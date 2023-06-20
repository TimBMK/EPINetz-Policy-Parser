## Full Seed Terms Workflow ##
##############################

## A high-level function incorporating the complete workflow to get seed terms
##  For testing and API purposes
##  Should not be used to process multiple samples/time frames, as it would unnecessarily reproduce certain steps

seed_terms_workflow <- function(
    tokens,          # tokens object. Expects tokenized data returned by the tokenizer workflow
    ministries,      # ministry account list
    committees,      # committee account list
    dir = NULL,             # subdirectory to save results in. NULL to skip
    date = Sys.Date(), # date for filtering purposes (max date)
    time_frame_seeds = years(1), # length of the time frame for seed term extraction
    seed_replies = TRUE, # should replies be utilized?
    seed_mentions = FALSE, # should mentions be utilizued?
    seed_urls = TRUE, # should URLs be utilized?
    chi2_ministries = 500, # set chi^2 threshold for ministries
    chi2_committee_members = 250, # set chi^2 threshold for within-committee members
    chi2_committees = 30, # set chi^2 threshold between committees
    save_seeds = FALSE, # should the Seed Terms be saved explicitly in 'dir'? If TRUE, saves data for ministries, committees, and committee members seperately
    verbose = FALSE, # should output be printed that keeps track of the current analysis step? Additionally, verbose = F mutes messages from the parallel seed term functions 
    seed = as.numeric(date) # seed to prevent RNG issues in parallelization. by default the numeric conversion of the date 
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
  
  
  committee_NE %<-% {committees %>% 
    dplyr::inner_join(seed_NE, by = join_by(user_id == `_source.author_id`), # inner_join ensures we only keep accounts for which we have data
                      relationship = "many-to-many") %>% 
    dplyr::select(user_id, official_name, policy_field, doc_id, lemma, committee)}
  
  if (verbose) {
    
    cat("\n")
    
    while(any(!(resolved(ministry_NE)) | !(resolved(committee_NE)))) {
      
      cat(".")
      
      Sys.sleep(1)
      
    }
    cat("Data Structuring done")
  } 
  
  # Calculate Chi^2 for Policy Fields
  
  
  ## Between Ministries
  
  # cat("\n ===== Calculate Ministry Seeds ===== \n")
  
  seed_terms_ministries <-  future::future({get_seed_terms(data = ministry_NE,
                                          doc_id = "doc_id",
                                          tokens = "lemma",
                                          grouping_var = "official_name",
                                          policy_field = "policy_field",
                                          threshold = chi2_ministries,
                                          show_plots = F,
                                          save_plots = F)}, 
                                          seed = seed, 
                                          stdout = verbose)
  
  
  
  ## Between Committees
  
  # cat("\n ===== Calculate Committee Seeds ===== \n")
  
  seed_terms_committees <- future::future({get_seed_terms(data = committee_NE,
                                          doc_id = "doc_id",
                                          tokens = "lemma",
                                          grouping_var = "committee",
                                          policy_field = "policy_field",
                                          threshold = chi2_committees,
                                          show_plots = F,
                                          save_plots = F)}, 
                                          seed = seed, 
                                          stdout = verbose)
  
  
  
  
  ## Between Members of each committee
  
  # cat("\n ===== Calculate Committee Member Seeds ===== \n")
  
  seed_terms_committee_members <- future::future({
    committee_NE %>% # split datasets into committees and calculate keyness within committees via map()
    split(.$committee) %>% 
    furrr::future_imap(\(data, id) get_seed_terms(data = data,
                                                  doc_id = "doc_id",
                                                  tokens = "lemma",
                                                  grouping_var = "official_name",
                                                  policy_field = "policy_field",
                                                  threshold = chi2_committee_members,
                                                  show_plots = F,
                                                  save_plots = F) %>% 
                         dplyr::mutate(committee = id)) %>% 
    data.table::rbindlist()}, 
    seed = seed, 
    stdout = verbose)
  
  if (verbose) {
    # make internal indicators for the verbose loop
    ministries_done <-  FALSE 
    committees_done <- FALSE
    committee_members_done <- FALSE
    
    while(any(!(future::resolved(seed_terms_ministries)) |
              !(future::resolved(seed_terms_committees)) |
              !(future::resolved(seed_terms_committee_members)))) {
      cat(".")
      
      Sys.sleep(1)
      
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
  
  # if (verbose & future::resolved(seed_terms_ministries)) {cat("\n... Ministry Terms done \n")}
  # 
  # if (verbose & future::resolved(seed_terms_committees)) {cat("\n... Committee Terms done \n")}
  # 
  # if (verbose & future::resolved(seed_terms_committee_members)) {cat("\n... Committee Members Terms done \n")}
  
  
  ## save results (if desired)
  if (save_seeds == TRUE & !is.null(dir)) {
    
    # cat("\n ===== Save Results ===== \n")
    if(verbose) cat("...Writing Results")
    
    future::value(seed_terms_ministries) %>%
      dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
      vroom::vroom_write(file = paste0(dir, "seed_terms_ministries_", date ,".csv.tar.gz"), delim = ",")
    
    future::value(seed_terms_committees) %>%
      dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
      vroom::vroom_write(file = paste0(dir, "seed_terms_committees_", date ,".csv.tar.gz"), delim = ",")
    
    future::value(seed_terms_committee_members) %>%
      dplyr::mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
      vroom::vroom_write(file = paste0(dir, "seed_terms_committee_members_", date ,".csv.tar.gz"), delim = ",")
    
    if(verbose) cat("...done")
  }
  
  ## return results
  return(list("seed_terms_ministries" = future::value(seed_terms_ministries),
              "seed_terms_committees" = future::value(seed_terms_committees),
              "seed_terms_committee_members" = future::value(seed_terms_committee_members)))
  
}
