

microbenchmark::microbenchmark(
  {plan(multisession, workers = 4)
    seeds[[1]][1:100] %>% 
      dplyr::distinct(!!as.name(seed_var)) %>% dplyr::pull() %>%
      furrr::future_map(\(seed) # running parallelization over the seeds (instead of whole groups) might add stability
                        {seed_walk <- tryCatch(# capture non-standard errors thrown by Random.Walk.Restart.Multiplex
                          RandomWalkRestartMH::Random.Walk.Restart.Multiplex(
                            x = walk_network$AdjMatrixNorm,
                            MultiplexObject = walk_network$multiplex,
                            Seeds = seed
                          ),
                          error  = function(e) NULL # return NULL for errors...
                        ) %>% 
                            .[["RWRM_Results"]] %>% 
                            dplyr::as_tibble() %>% 
                            dplyr::mutate(seed_node = seed)
                          
                          if(positive_scores_only){
                            seed_walk <- seed_walk %>% 
                              dplyr::filter(Score > 0)
                          }
                          
                          # if (normalize_score == "seeds"){ # normalization and score filtering on Seed Level (for each Random Walk)
                          #   seed_walk <- seed_walk %>% 
                          #     dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
                          #   if (!is.null(walk_score)) {
                          #     seed_walk <- seed_walk %>% 
                          #       dplyr::filter(ScoreNorm >= walk_score)
                          #   }
                          # } 
                          
                          # if (normalize_score == FALSE & !is.null(walk_score)) { # score filtering without normalization
                          #   seed_walk <- seed_walk %>% 
                          #     dplyr::filter(Score >= walk_score)
                          # }
                          
                          return(seed_walk)
      }) %>% 
      purrr::compact() %>%  # ... and remove NULLs
      data.table::rbindlist()},
  {plan(multisession, workers = 2)
    seeds[[1]][1:100] %>% 
      dplyr::distinct(!!as.name(seed_var)) %>% dplyr::pull() %>%
      furrr::future_map(\(seed) # running parallelization over the seeds (instead of whole groups) might add stability
                        {seed_walk <- tryCatch(# capture non-standard errors thrown by Random.Walk.Restart.Multiplex
                          RandomWalkRestartMH::Random.Walk.Restart.Multiplex(
                            x = walk_network$AdjMatrixNorm,
                            MultiplexObject = walk_network$multiplex,
                            Seeds = seed
                          ),
                          error  = function(e) NULL # return NULL for errors...
                        ) %>% 
                            .[["RWRM_Results"]] %>% 
                            dplyr::as_tibble() %>% 
                            dplyr::mutate(seed_node = seed)
                          
                          if(positive_scores_only){
                            seed_walk <- seed_walk %>% 
                              dplyr::filter(Score > 0)
                          }
                          
                          # if (normalize_score == "seeds"){ # normalization and score filtering on Seed Level (for each Random Walk)
                          #   seed_walk <- seed_walk %>% 
                          #     dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
                          #   if (!is.null(walk_score)) {
                          #     seed_walk <- seed_walk %>% 
                          #       dplyr::filter(ScoreNorm >= walk_score)
                          #   }
                          # } 
                          
                          # if (normalize_score == FALSE & !is.null(walk_score)) { # score filtering without normalization
                          #   seed_walk <- seed_walk %>% 
                          #     dplyr::filter(Score >= walk_score)
                          # }
                          
                          return(seed_walk)
      }) %>% 
      purrr::compact() %>%  # ... and remove NULLs
      data.table::rbindlist()},
  {plan(sequential)
    seeds[[1]][1:100] %>% 
      dplyr::distinct(!!as.name(seed_var)) %>% dplyr::pull() %>%
      furrr::future_map(\(seed) # running parallelization over the seeds (instead of whole groups) might add stability
                        {seed_walk <- tryCatch(# capture non-standard errors thrown by Random.Walk.Restart.Multiplex
                          RandomWalkRestartMH::Random.Walk.Restart.Multiplex(
                            x = walk_network$AdjMatrixNorm,
                            MultiplexObject = walk_network$multiplex,
                            Seeds = seed
                          ),
                          error  = function(e) NULL # return NULL for errors...
                        ) %>% 
                            .[["RWRM_Results"]] %>% 
                            dplyr::as_tibble() %>% 
                            dplyr::mutate(seed_node = seed)
                          
                          if(positive_scores_only){
                            seed_walk <- seed_walk %>% 
                              dplyr::filter(Score > 0)
                          }
                          
                          # if (normalize_score == "seeds"){ # normalization and score filtering on Seed Level (for each Random Walk)
                          #   seed_walk <- seed_walk %>% 
                          #     dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
                          #   if (!is.null(walk_score)) {
                          #     seed_walk <- seed_walk %>% 
                          #       dplyr::filter(ScoreNorm >= walk_score)
                          #   }
                          # } 
                          
                          # if (normalize_score == FALSE & !is.null(walk_score)) { # score filtering without normalization
                          #   seed_walk <- seed_walk %>% 
                          #     dplyr::filter(Score >= walk_score)
                          # }
                          
                          return(seed_walk)
      }) %>% 
      purrr::compact() %>%  # ... and remove NULLs
      data.table::rbindlist()},
    times = 5)




plan(multisession, workers = 10)

microbenchmark::microbenchmark(get_rwr_terms(walk_network,
                                             network_name = NULL,
                                             seeds = seeds[1:2], 
                                             seed_var = "feature",
                                             match_var = NULL,
                                             flatten_results = TRUE,
                                             normalize_score = TRUE,
                                             positive_scores_only = TRUE,
                                             walk_score = walk_score,
                                             keep_seed_terms = keep_seed_terms,
                                             progress = FALSE),
                               get_rwr_terms(walk_network,
                                             network_name = NULL,
                                             seeds = seeds[1:2], 
                                             seed_var = "feature",
                                             match_var = NULL,
                                             flatten_results = TRUE,
                                             normalize_score = F,
                                             positive_scores_only = F,
                                             walk_score = NULL,
                                             keep_seed_terms = keep_seed_terms,
                                             progress = FALSE), 
                               times = 2
)

microbenchmark::microbenchmark(
  {
    plan(sequential)
    get_rwr_terms(walk_network,
                  network_name = NULL,
                  seeds = seeds[1:2], 
                  seed_var = "feature",
                  match_var = NULL,
                  flatten_results = TRUE,
                  normalize_score = "seeds",
                  positive_scores_only = TRUE,
                  walk_score = walk_score,
                  keep_seed_terms = keep_seed_terms,
                  progress = FALSE)
    },
  {
    plan(multisession, workers = 16)
    get_rwr_terms(walk_network,
                  network_name = NULL,
                  seeds = seeds[1:2], 
                  seed_var = "feature",
                  match_var = NULL,
                  flatten_results = TRUE,
                  normalize_score = "seeds",
                  positive_scores_only = TRUE,
                  walk_score = walk_score,
                  keep_seed_terms = keep_seed_terms,
                  progress = FALSE)
    }, 
  times = 2
)




get_rwr_terms_old <- function(walk_network, # an object made by make_multiplex_objects, contatining the multiplex network and the Normalized Adjacency Matrix
                          network_name = NULL, # name of the network, used for matching the match_var. Usually the index when implented in imap functions. NULL to skip Requires match_var
                          seeds, # a list of dataframes, where each entry represents a specific group of seeds, e.g. a single policy field
                          seed_var, # the variable in the seed dataframes containing the seed terms
                          match_var = NULL, # the variable in the seed dataframes to match on the network_name variable, e.g. specific time periods. NULL to skip. Requires network_name
                          flatten_results = TRUE, 
                          walk_score = NULL, # minimal normalized walk score for results to keep. Only available if flatten_results = TRUE. NULL to retain all results
                          keep_seed_terms = TRUE, # should seed terms always be kept, regardless of score? Only available if flatten_results = TRUE
                          progress = FALSE) # should the progress be shown for the two future_map functions? Only available for multisessions. See ?future_map(.progress)
{
  
  require(dplyr)
  require(furrr)
  require(purrr)
  require(RandomWalkRestartMH)
  require(data.table)
  require(scales)
  
  rwr_results <-
    seeds %>% furrr::future_map(\(seed_group)
                                {
                                  if (!is.null(match_var) & !is.null(network_name)) { # match network names and match var
                                    seed_group %>%
                                      mutate({{match_var}} := as.character({{match_var}})) %>% 
                                      dplyr::filter(!!as.name(match_var) == network_name)}
                                  
                                  seed_group %>% 
                                    dplyr::distinct(!!as.name(seed_var)) %>% dplyr::pull() %>%
                                    purrr::map(\(seed)
                                               tryCatch(# capture non-standard errors thrown by Random.Walk.Restart.Multiplex
                                                 RandomWalkRestartMH::Random.Walk.Restart.Multiplex(
                                                   x = walk_network$AdjMatrixNorm,
                                                   MultiplexObject = walk_network$multiplex,
                                                   Seeds = seed
                                                 ),
                                                 error  = function(e) NULL # return NULL for errors...
                                               )) %>% compact() # ... and remove NULLs
    },
    .progress = progress)
  
  if (flatten_results) {
    flattened_results <- rwr_results %>%
      purrr::list_flatten(name_spec = "{outer}") %>%
      furrr::future_imap(\(x, idx)
                         {
                           dplyr::tibble(x[[1]], seed_node = x[[2]], policy_field = idx) %>%
                             dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))# rescale scores for each seed term
      },
      .progress = progress) %>%
      data.table::rbindlist() %>%
      dplyr::group_by(policy_field) %>%
      dplyr::mutate(seed_term = dplyr::case_when(# mark seed terms of the policy field
        NodeNames %in% seed_node ~ TRUE, .default = FALSE)) %>%
      dplyr::ungroup()
    
    if (!is.null(walk_score) & keep_seed_terms == TRUE) {
      flattened_results <- flattened_results %>% 
        dplyr::filter(ScoreNorm >= walk_score | # drop results below the desired (normalized) walk score
                        seed_term == keep_seed_terms)    # but retain seed terms if desired
    }
    
    if (!is.null(walk_score) & keep_seed_terms == FALSE) {
      flattened_results <- flattened_results %>% 
        dplyr::filter(ScoreNorm >= walk_score) # drop results below the desired (normalized) walk score
    }
    
    
    if (!is.null(match_var) & !is.null(network_name)) { # add network names as match var
      flattened_results <- flattened_results %>% 
        dplyr::mutate({{match_var}} := network_name)}
    
    return(flattened_results)
  } else {
    return(rwr_results)
  }
}

walk_terms_old <- get_rwr_terms_old(walk_network,
                            network_name = NULL,
                            seeds = seeds[1], 
                            seed_var = "feature",
                            match_var = NULL,
                            flatten_results = TRUE,
                            walk_score = walk_score,
                            keep_seed_terms = keep_seed_terms,
                            progress = FALSE) 


microbenchmark::microbenchmark(get_rwr_terms(walk_network,
                                             network_name = NULL,
                                             seeds = seeds[1], 
                                             seed_var = "feature",
                                             match_var = NULL,
                                             flatten_results = TRUE,
                                             group_name = "policy_field",
                                             normalize_score = "seeds",
                                             positive_scores_only = TRUE,
                                             walk_score = walk_score,
                                             keep_seed_terms = keep_seed_terms,
                                             progress = FALSE) ,
                               get_rwr_terms(walk_network,
                                             network_name = NULL,
                                             seeds = seeds[1], 
                                             seed_var = "feature",
                                             match_var = NULL,
                                             flatten_results = TRUE,
                                             group_name = "policy_field",
                                             normalize_score = "group",
                                             positive_scores_only = TRUE,
                                             walk_score = walk_score,
                                             keep_seed_terms = keep_seed_terms,
                                             progress = FALSE) , 
                               times = 3
)

