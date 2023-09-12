## Full Walk Terms Workflow ##
##############################

## A high-level function incorporating the complete workflow to get random walk terms based on seeds
##  For testing and API purposes
##  Should not be used to process multiple samples/time frames, as it would unnecessarily reproduce certain steps

walk_terms_workflow <- function(
    tokens = get_latest_tokens_file(), # tokens object. Expects tokenized data returned by the tokenizer workflow
    seed_terms, # seeds objects. Expects a liste with the named dataframes "seed_terms_ministries", "seed_term_committees" and "seed_term_committee_members"
    date = Sys.Date(), # date for filtering purposes (max date
    walk_replies = FALSE, # should replies be utilized?
    walk_mentions = TRUE, # should mentions be utilizued?
    walk_urls = TRUE, # should URLs be utilized?
    time_frame_walks = weeks(12), # length of the time frame for random walk term extraction
    quantile_drop = 0.1, # what quantile should be dropped from the token counts? NULL to skip
    walk_score_normalization = "seeds", # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 
    calculate_means = TRUE, # should the means of the score be calculated and displayed? The can also be used for minimum walk_score filtering (see below)
    normalize_means = TRUE, # a second normalization of the means
    reduce_to_means = TRUE, # should only means be returned, dropping duplicated Nodes and their associated scores?
    positive_scores_only = TRUE, # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization
    walk_score = 0.9, # cutoff value for normalized random walk score. Non-Null Values require the selection of a measure to filter on if more than one walk_score_normalization method is picked
    walk_score_measure = "seeds_mean", # value to apply the walk_score filter on. 
    # Possible are: "default" (auto-pick), "raw" (non-normalized rwr score), "seeds" (seed normalized), "seeds_mean" (mean of seed normalized), "group" (group normalized), "group_mean" (mean of group normalized). Needs to be specified if more than one 
    walk_score_quantile = TRUE, # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value. Cutoff values may differ from group to group
    keep_seed_terms = TRUE, # should seed terms within the policy field of the same period always be kept, regardless of walk score?
    seedterm_value = NULL, # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip
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
                             quantile = quantile_drop,
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
  
  walk_terms <-  get_rwr_terms(walk_network = future::value(walk_network),
                               network_name = NULL, # not required for a single period
                               seeds = future::value(seeds),
                               seed_var = "feature",
                               match_var = NULL, # not required for a single period
                               flatten_results = TRUE,
                               group_name = "policy_field",
                               normalize_score = walk_score_normalization,
                               calculate_means = calculate_means,
                               normalize_means = normalize_means, # a second normalization of the means
                               reduce_to_means = reduce_to_means, # should only means be returned, dropping duplicated Nodes and their associated scores?
                               positive_scores_only = positive_scores_only,
                               walk_score = walk_score,
                               walk_score_measure = walk_score_measure,
                               walk_score_quantile = walk_score_quantile,
                               report_quantiles = verbose,
                               keep_seed_terms = keep_seed_terms,
                               seedterm_value = seedterm_value, # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip
                               progress = verbose) 
  
  ## The terms returned for 'normalize_score = "seeds"' include duplicates, esp. seed terms with keep_seed_terms = TRUE
  
  return(walk_terms)
}
