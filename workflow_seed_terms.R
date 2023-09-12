## Full Seed Terms Workflow ##
##############################

## A high-level function incorporating the complete workflow to get seed terms
##  For testing and API purposes
##  Should not be used to process multiple samples/time frames, as it would unnecessarily reproduce certain steps

seed_terms_workflow <- function(
    tokens = get_latest_tokens_file(), # tokens object. Expects tokenized data returned by the tokenizer workflow
    ministries,      # ministry account list
    committees,      # committee account list
    dir = NULL,             # subdirectory to save results in. NULL to skip
    date = Sys.Date(), # date for filtering purposes (max date)
    time_frame_seeds = years(1), # length of the time frame for seed term extraction
    seed_replies = TRUE, # should replies be utilized?
    seed_mentions = FALSE, # should mentions be utilizued?
    seed_urls = TRUE, # should URLs be utilized?
    chi2_ministries = 150, # set chi^2 threshold for ministries
    max_results_ministries = 230, # number of maximum results for each ministry
    min_results_ministries = 100, # number of minimum results for each ministry
    chi2_committee_members = 250, # set chi^2 threshold for within-committee members
    max_results_committee_members = NULL, # number of maximum results for each committee member (NULL to skip)
    min_results_committee_members = NULL, # number of minimum results for each committee member (NULL to skip)
    chi2_committees = 40, # set chi^2 threshold between committees
    max_results_committees = 200, # number of maximum results for each committee (NULL to skip)
    min_results_committees = 35, # number of minimum results for each committee (NULL to skip)
    max_result_ties = TRUE, # should ties for max results be kept? If TRUE, may return more than the requested number of seed terms
    active_committees_only = TRUE, # should only active committees be considered? Expects a "begin", "end" and "`_source.created_at`" column to track activity. 
                                   #  Drops all documents of a committees not created in this range
    get_plots = FALSE, # should the chi^2 plots be saved within the object?
    save_seeds = FALSE, # should the Seed Terms be saved explicitly in 'dir'? If TRUE, saves data for ministries, committees, and committee members seperately. Does not save plots
    verbose = FALSE, # should output be printed that keeps track of the current analysis step? Additionally, verbose = F mutes messages from the parallel seed term functions
    random_seed = as.numeric(date) # random_seed to prevent RNG issues in parallelization. by default the numeric conversion of the date 
){
  
  # cat("\n ===== Preparations ===== \n")
  
  source("get_seed_terms.R")
  source("utils_text_processing.R")
  
  require(dplyr)
  require(furrr)
  require(data.table)
  require(utf8)
  require(vroom)
  require(future) # future allows us to parallelize certain calculations with %<-%
  
  ## some checks
  if (save_seeds & is.null(dir)) {
    stop("You need to set the directory with 'dir' in order to save the results with save_seeds. \n")
  }
  
  expected_tokens_cols <- c("doc_id", "lemma", "tag", "is_reply", "_source.created_at", "_source.author_id")
  if (any(!(expected_tokens_cols %in% colnames(tokens)))) {
    stop(
      cat("Expected column", 
          expected_tokens_cols[!(expected_tokens_cols %in% colnames(tokens))],
          "in 'tokens'-object not found. \n", sep = " ")
    )
  }
  
  expected_ministry_cols <- c("user_id", "official_name", "policy_field")
  if (any(!(expected_ministry_cols %in% colnames(ministries)))) {
    stop(
      cat("Expected column", 
          expected_ministry_cols[!(expected_ministry_cols %in% colnames(ministries))],
          "in 'ministry'-object not found. \n", sep = " ")
    )
  }
  
  expected_committee_cols <- c("user_id", "official_name", "policy_field", "committee")
  if (any(!(expected_committee_cols %in% colnames(committees)))) {
    stop(
      cat("Expected column", 
          expected_committee_cols[!(expected_committee_cols %in% colnames(committees))],
          "in 'committees'-object not found. \n", sep = " ")
    )
  }
  
  
  if (active_committees_only) {
    expected_active_cols_tokens <- c("_source.created_at")
    if (any(!(expected_active_cols_tokens %in% colnames(tokens)))) {
      stop(
        cat("Expected column", 
            expected_active_cols_tokens[!(expected_active_cols_tokens %in% colnames(tokens))],
            "in 'tokens'-object not found. Specify accordingly or set 'active_committees_only' to FALSE in order to skip the step requiring these columns. \n", sep = " ")
      )
    }
    expected_active_cols_committees <- c("begin", "end")
    if (any(!(expected_active_cols_committees %in% colnames(committees)))) {
      stop(
        cat("Expected column", 
            expected_active_cols_committees[!(expected_active_cols_committees %in% colnames(committees))],
            "in 'committees'-object not found. Specify accordingly or set 'active_committees_only' to FALSE in order to skip the step requiring these columns. \n", sep = " ")
      )
    }
  }

  
  # cat("\n ===== Filtering and Data Structuring ===== \n")
  
  
  ## Filter Accounts; time filtering; NEs only; data cleaning
  seed_NE <- tokens %>% dplyr::as_tibble() %>% 
    dplyr::filter(`_source.created_at` >= (date - time_frame_seeds) & # time frame filtering
                    `_source.created_at` <= date) %>%  
    dplyr::filter(`_source.author_id`  %in% ministries$user_id | # seed account tweets only
                    `_source.author_id`  %in% committees$user_id ) %>% 
    filter_tokens(tokens_col = "lemma", 
                  tags = c("NN", "NE"), # Noun words and NEs only
                  #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                  replies = seed_replies, # filter for reply condition (TRUE includes replies, FALSE does not)  
                  keep_mentions = seed_mentions, # should @-mentions be kept?
                  keep_urls = seed_urls # should URLs be kept?
    )
    
  
  
  # Data Structuring
  
  ministry_NE %<-% {ministries %>% 
      dplyr::inner_join(seed_NE, by = join_by(user_id == `_source.author_id`), # inner_join ensures we only keep accounts for which we have data
                        relationship = "many-to-many") %>% 
      dplyr::select(user_id, official_name, policy_field, doc_id, lemma)}
  
  
  if (active_committees_only) {
    
    committee_NE %<-% {committees %>% 
        dplyr::inner_join(seed_NE, by = join_by(user_id == `_source.author_id`), # inner_join ensures we only keep accounts for which we have data
                          relationship = "many-to-many") %>% 
        dplyr::filter((as_date(`_source.created_at`) >= as_date(begin) & # filter for active committee time periods
                         as_date(`_source.created_at`) <= as_date(end)) | 
                        is.na(end)) %>% 
        dplyr::select(user_id, official_name, policy_field, doc_id, lemma, committee)}
      
  } else {
    
    committee_NE %<-% {committees %>% 
        dplyr::inner_join(seed_NE, by = join_by(user_id == `_source.author_id`), # inner_join ensures we only keep accounts for which we have data
                          relationship = "many-to-many") %>% 
        dplyr::select(user_id, official_name, policy_field, doc_id, lemma, committee)}
  }
  
  
  if (verbose) {
    
    cat("\n")
    
    while(any(!(future::resolved(ministry_NE)) | !(future::resolved(committee_NE)))) {
      
      cat(".")
      
      Sys.sleep(0.5)
      
    }
    cat("Data Structuring done")
  } 
  
  # Calculate Chi^2 for Policy Fields
  
  
  ## Between Ministries
  
  # cat("\n ===== Calculate Ministry Seeds ===== \n")
  
  seed_terms_ministries <-  future::future(
    {get_seed_terms(data = ministry_NE,
                    doc_id = "doc_id",
                    tokens = "lemma",
                    grouping_var = "official_name",
                    policy_field = "policy_field",
                    threshold = chi2_ministries,
                    max_results =  max_results_ministries,
                    max_result_ties = max_result_ties,
                    min_results = min_results_ministries,
                    show_plots = F,
                    save_plots = get_plots)}, 
    seed = random_seed, 
    stdout = F)
  
  
  
  ## Between Committees
  
  # cat("\n ===== Calculate Committee Seeds ===== \n")
  
  seed_terms_committees <- future::future(
    {get_seed_terms(data = committee_NE,
                    doc_id = "doc_id",
                    tokens = "lemma",
                    grouping_var = "committee",
                    policy_field = "policy_field",
                    threshold = chi2_committees,
                    max_results =  max_results_committees,
                    max_result_ties = max_result_ties,
                    min_results = min_results_committees,
                    show_plots = F,
                    save_plots = get_plots)}, 
    seed = random_seed, 
    stdout = F)
  
  
  
  
  ## Between Members of each committee
  
  # cat("\n ===== Calculate Committee Member Seeds ===== \n")
  
  seed_terms_committee_members <- future::future({
    res <- committee_NE %>% # split datasets into committees and calculate keyness within committees via map()
      split(.$committee) %>% 
      furrr::future_imap(\(data, id) 
                         { committee_res <- get_seed_terms(data = data,
                                                           doc_id = "doc_id",
                                                           tokens = "lemma",
                                                           grouping_var = "official_name",
                                                           policy_field = "policy_field",
                                                           threshold = chi2_committee_members,
                                                           max_results =  max_results_committee_members,
                                                           max_result_ties = max_result_ties,
                                                           min_results = min_results_committee_members,
                                                           show_plots = F,
                                                           save_plots = get_plots)
                           
                           if (get_plots) { # make sure we add the committee name in the right place
                             committee_res$key_terms <- committee_res$key_terms %>% dplyr::mutate(committee = id)
                             
                           } else {
                             
                             committee_res <- committee_res %>% dplyr::mutate(committee = id)
                             
                           }
                           
                           return(committee_res)
      }
      ) 
    
    if (get_plots) {
      
      res <- list("key_terms" = res %>% map(\(dat) {dat %>% .[[1]]}) %>% rbindlist(),
                  "plots" = res %>% map(\(dat) {dat %>% .[[2]]}))
      
    } else {
      
      res <- res %>% data.table::rbindlist() %>% dplyr::as_tibble()
      
    }
    
  }, 
  seed = random_seed, 
  stdout = F)
  
  if (verbose) {
    # make internal indicators for the verbose loop
    ministries_done <-  FALSE 
    committees_done <- FALSE
    committee_members_done <- FALSE
    
    while(any(!(future::resolved(seed_terms_ministries)) |
              !(future::resolved(seed_terms_committees)) |
              !(future::resolved(seed_terms_committee_members)))) {
      cat(".")
      
      Sys.sleep(0.5)
      
      if (future::resolved(seed_terms_ministries) & 
          ministries_done == FALSE) {
        cat("Ministry Terms done")
        ministries_done <-  TRUE
      }
      
      if (future::resolved(seed_terms_committees) & 
          committees_done == FALSE) {
        cat("Committee Terms done")
        committees_done <- TRUE
      }
      
      if (future::resolved(seed_terms_committee_members) & 
          committee_members_done == FALSE) {
        cat("Committee Members Terms done")
        committee_members_done <- TRUE}
    }
  }
  
  
  ## save results (if desired)
  if (save_seeds == TRUE & !is.null(dir)) {
    
    # cat("\n ===== Save Results ===== \n")
    if(verbose) cat("...Writing Results")
    
    if(get_plots) {
      
      future::value(seed_terms_ministries)$key_terms %>%
        dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
        vroom::vroom_write(file = paste0(dir, "seed_terms_ministries_", date ,".csv.tar.gz"), delim = ",")
      
      future::value(seed_terms_committees)$key_terms %>%
        dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
        vroom::vroom_write(file = paste0(dir, "seed_terms_committees_", date ,".csv.tar.gz"), delim = ",")
      
      future::value(seed_terms_committee_members)$key_terms %>%
        dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
        vroom::vroom_write(file = paste0(dir, "seed_terms_committee_members_", date ,".csv.tar.gz"), delim = ",")
      
      } else {
      
      future::value(seed_terms_ministries) %>%
        dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
        vroom::vroom_write(file = paste0(dir, "seed_terms_ministries_", date ,".csv.tar.gz"), delim = ",")
      
      future::value(seed_terms_committees) %>%
        dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
        vroom::vroom_write(file = paste0(dir, "seed_terms_committees_", date ,".csv.tar.gz"), delim = ",")
      
      future::value(seed_terms_committee_members) %>%
        dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
        vroom::vroom_write(file = paste0(dir, "seed_terms_committee_members_", date ,".csv.tar.gz"), delim = ",")
      
      }
    
    if(verbose) cat("...done")
    
  }

  
  ## return results
  return(list("seed_terms_ministries" = future::value(seed_terms_ministries),
              "seed_terms_committees" = future::value(seed_terms_committees),
              "seed_terms_committee_members" = future::value(seed_terms_committee_members)))
  
}
