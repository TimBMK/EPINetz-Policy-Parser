## Full Walk Terms Workflow ##
##############################

## A high-level function incorporating the complete workflow to get random walk terms based on seeds
##  For testing and API purposes
##  Should not be used to process multiple samples/time frames, as it would unnecessarily reproduce certain steps

walk_terms_workflow <- function(
    tokens, # tokens object. Expects tokenized data returned by the tokenizer workflow
    seed_terms, # seeds objects. Expects a liste with the named dataframes "seed_terms_ministries", "seed_term_committees" and "seed_term_committee_members"
    date = Sys.Date(), # date for filtering purposes (max date
    walk_replies = FALSE, # should replies be utilized?
    walk_mentions = TRUE, # should mentions be utilizued?
    walk_urls = TRUE, # should URLs be utilized?
    time_frame_walks = weeks(12), # length of the time frame for random walk term extraction
    drop_quantile = 0.1, # what quantile should be dropped from the token counts? NULL to skip
    positive_scores_only = TRUE, # Drop Score below 0 before Normalization?
    normalize_score = c(NULL, "seeds", "group", "group_mean"), # How should scores be normalized? Always applied before score filtering
    walk_score = 0.9, # Minimum normalized Random Walk Score for Random Walk Terms
    keep_seed_terms = TRUE, # Should seed terms be kept, regardless of their their walk score?
    seed = as.numeric(date), # seed to prevent RNG issues in parallelization. by default the numeric conversion of the date 
    verbose = F
)

{  
  source("utils_text_processing.R") # text processing utils
  
  source("get_rwr_terms.R") # random walk functions
  
  require(purrr)
  require(future)
  require(dplyr)
  require(data.table)
  
  
  # some checks
  
  rlang::arg_match(normalize_score) # check for correct argument specification here
  
  expected_tokens_cols <- c("doc_id", "lemma", "tag", "is_reply", "_source.created_at", "_source.author_id")
  if (any(!(expected_tokens_cols %in% colnames(tokens)))) {
    stop(
      cat("Expected column", 
          expected_tokens_cols[!(expected_tokens_cols %in% colnames(tokens))],
          "not found in tokens. \n", sep = " ")
    )
  }
  
  expected_seed_dataframes <- c("seed_terms_ministries", "seed_terms_committees","seed_terms_committee_members")
  if(any(!(expected_seed_dataframes %in% names(seed_terms)))) {
    stop(cat("Expected Data Frame", 
             expected_seed_dataframes[!(expected_seed_dataframes %in% names(seed_terms))],
             "not found in seed_terms. \n", sep = " "))
  }
  
  expected_seed_cols <- c("feature", "policy_field")
  seed_terms %>% purrr::iwalk(\(dataframe, name)
                              {
                                try({ # try and silent to suppress the purrr error
                                  if (any(!(expected_seed_cols %in% colnames(dataframe)))) {
                                    stop(
                                      cat("Expected column", 
                                          expected_seed_cols[!(expected_seed_cols %in% colnames(dataframe))],
                                          "not found in",
                                          name,
                                          "\n", sep = " ")
                                    )
                                  }
                                }, silent = TRUE)
  })
  
  walk_NE <- tokens %>% dplyr::as_tibble() %>% 
    dplyr::filter(`_source.created_at` >= (date - time_frame_walks) & # time frame filtering
             `_source.created_at` <= date) %>%  
    filter_tokens(tokens_col = "lemma", 
                  tags = c("NN", "NE"), # Noun words and NEs only
                  #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                  replies = walk_replies, # filter for reply condition (TRUE includes replies, FALSE does not)  
                  keep_mentions = walk_mentions, # should @-mentions be kept?
                  keep_urls = walk_urls) # should URLs be kept?
  
  
  ## droppercentile of counts
  if (!is.null(drop_quantile))
  {
    walk_NE <- drop_quantile(walk_NE,
                             tokens = "lemma",
                             quantile = drop_quantile,
                             ignore_case = FALSE, # case is already lowered
                             group = "tag",
                             verbose = verbose)
  }
  
  # Prepare Network for Random Walks
  
  walk_network <- future::future({make_multiplex_objects(walk_NE,
                                         vertex_a = "doc_id",
                                         vertex_b = "lemma",
                                         directed = F,
                                         pmi_weight = T,
                                         keep_igraph_network = F,
                                         keep_multiplex_network = T,
                                         keep_adjacency_matrix = F,
                                         keep_normalized_adjacency_matrix = T)},
                                 seed = seed,
                                 stdout = FALSE)
  
  
  # Prepare Seeds
  
  seeds <- future::future({
    data.table::rbindlist(list(seed_terms$seed_terms_ministries, # bind seed terms of subsets together...
                   seed_terms$seed_terms_committees),
              fill = TRUE) %>% 
      dplyr::anti_join(seed_terms$seed_terms_committee_members, # drop seed terms prevalent for single committee members
                by = join_by(feature, committee)) %>% 
      dplyr::filter(feature %in% walk_NE$lemma) %>% # drop seed terms not in the walk network
      dplyr::distinct(feature, policy_field) %>% # drop features duplicated within policy fields (from committees etc)
      split(.$policy_field)},                        # ... and split by policy field
    seed = seed,
    stdout = FALSE)
  
  
  # Housekeeping
  
  rm(tokens)
  rm(walk_NE)
  gc(verbose = FALSE)
  
  
  # Compute Random Walks
  
  walk_terms <- get_rwr_terms(value(walk_network),
                              network_name = NULL,
                              seeds = value(seeds), 
                              seed_var = "feature",
                              match_var = NULL,
                              flatten_results = TRUE,
                              group_name = "policy_field",
                              normalize_score = normalize_score,
                              positive_scores_only = positive_scores_only,
                              walk_score = walk_score,
                              keep_seed_terms = keep_seed_terms,
                              progress = verbose) 
  
  ## The terms returned for 'normalize_score = "seeds"' include duplicates, esp. seed terms with keep_seed_terms = TRUE
  
  return(walk_terms)
}