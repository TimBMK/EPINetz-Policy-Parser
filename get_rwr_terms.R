get_rwr_terms <- function(walk_network, # an object made by make_multiplex_objects, contatining the multiplex network and the Normalized Adjacency Matrix
                          network_name = NULL, # name of the network, used for matching the match_var. Usually the index when implented in imap functions. NULL to skip. Requires match_var
                          seeds, # a list of dataframes, where each entry represents a specific group of seeds, e.g. a single policy field
                          seed_var, # the variable in the seed dataframes containing the seed terms
                          match_var = NULL, # the variable in the seed dataframes to match on the network_name variable, e.g. specific time periods. NULL to skip. Requires network_name
                          flatten_results = TRUE, # should the results be flattened into a single dataframe (with policy_field indicator) or a list of one dataframe per group? 
                          group_name = "policy_field", # name of the grouping variable if flatten_results = TRUE
                          positive_scores_only = FALSE, # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization
                          normalize_score = c(NULL, "seeds", "group"), # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 
                          walk_score = NULL, # minimal walk score for results to keep. Can be applied to normalized or raw scores, see normalize_score. Will always apply to normalized score if available
                          walk_score_quantile = FALSE, # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value
                          report_quantiles = FALSE, # should quantile values for each group be reported as printed output? Returns the quantiles of the selected walk_score_measure before any walk_score filtering
                          walk_score_measure = c("default", "raw", "seeds", "seeds_mean", "group", "group_mean"), # if more than one normalization method is selected, based which one should the walk_score be filtered? defaults to the chosen normalization
                          calculate_means = TRUE, # should score means be calculated?
                          normalize_means = TRUE,
                          reduce_to_means = FALSE,
                          keep_seed_terms = TRUE, # should seed terms always be kept, regardless of score? Only available if flatten_results = TRUE
                          seedterm_value = NULL, # overwrite the score of seed terms with a fixed value. Applies to all scores calculated. NULL to skip
                          progress = FALSE) # should the progress be shown for the map function?
{
  
  require(dplyr)
  require(furrr)
  require(purrr)
  require(RandomWalkRestartMH)
  require(data.table)
  require(scales)
  require(rlang)
  
  if (!is.null(normalize_score)){ # arg_match does not handle NULL values, thus we skip it if the score normalization is skipped via NULL
    normalize_score <- rlang::arg_match(normalize_score, multiple = TRUE) # check for correct argument specification here
  }
  walk_score_measure <- rlang::arg_match(walk_score_measure, multiple = FALSE) # check for correct argument specification here
  
  # format checks
  if (is.null(walk_network$multiplex) | is.null(walk_network$AdjMatrixNorm)) {
    stop("walk_network is missing the multiplex object or the adjacency matrix")
  }
  
  if (!RandomWalkRestartMH::isMultiplex(walk_network$multiplex)) {
    stop("walk_network$multiplex is not a valid multiplex object")
  }
  
  if (class(walk_network$AdjMatrixNorm) != "dgCMatrix") {
    stop("walk_network$AdjMatrixNorm is not a valid dgCMatrix")
  }
  
  
  # setting of variables
  if (reduce_to_means & !calculate_means) { # set reduce_to_means to FALSE if no means are calculated
    message("No means calculated. Not reducing output to means.")
    reduce_to_means <- FALSE
  }
  
#  if (!is.null(walk_score)) { # set the variable that the walk_score filter will be applied on
    
    if (length(normalize_score) > 1 & 
        walk_score_measure == "default") {
      stop("More than one normalization method selected, but no valid walk_score_measure selected. Select one of the normalization methods to filter the walk_score on, or set walk_score to NULL to skip")
    }
    
    if (walk_score_measure == "default") { # set default measure for filtering...
      
      if (is.null(normalize_score)) {
        filter_var <- "Score"
        message("Applying walk_score to raw RWR scores. You can change this by setting the walk_score_measure")
      } else {
        
        if (calculate_means) {
          
          if (normalize_score == "seeds") {
            filter_var <- "ScoreNormMean"
            message("Applying walk_score to mean seed scores. You can change this by setting the walk_score_measure")
          }
          
          if (normalize_score == "group" ) {
            filter_var <- "ScoreNormGroupMean"
            message("Applying walk_score to mean group scores. You can change this by setting the walk_score_measure")
          }
          
        } else {
          
          if (normalize_score == "seeds") {
            filter_var <- "ScoreNorm"
            message("Applying walk_score to seed-normalized scores. You can change this by setting the walk_score_measure")
          }
          
          if (normalize_score == "group" ) {
            filter_var <- "ScoreNormGroup"
            message("Applying walk_score to group-normalized scores. You can change this by setting the walk_score_measure")
          }
        }
      }
    } else { # ... or accommodate explicit settings
      
      if (walk_score_measure == "raw") {
        filter_var <- "Score"
      }
      
      if (walk_score_measure == "seeds") {
        filter_var <- "ScoreNorm"
      }
      
      if (walk_score_measure == "seeds_mean") {
        filter_var <- "ScoreNormMean"
      }
      
      if (walk_score_measure == "group" ) {
        filter_var <- "ScoreNormGroup"
      }
      
      if (walk_score_measure == "group_mean" ) {
        filter_var <- "ScoreNormGroupMean"
      }
    }
    
#  }
  
  if (!is.null(seedterm_value) & !is.numeric(seedterm_value)) {
    stop("seedterm_value must either be a numerical value to set the seed term mean to or NULL to skip")
  }
  
  # Run Random Walks
  rwr_results <-
    seeds %>% purrr::imap(\(seed_group, name)
                         {
                           if (!is.null(match_var) & !is.null(network_name)) { # match network names and match var
                             seed_group <- seed_group %>%
                               dplyr::mutate({{match_var}} := as.character(!!as.name(match_var))) %>% 
                               dplyr::filter(!!as.name(match_var) == network_name)}
                           
                           seed_group <- seed_group %>%  dplyr::filter(feature %in% walk_network$multiplex$Pool_of_Nodes) # drop seeds not in the network
                           
                           if (nrow(seed_group) > 0) { # make sure there are seeds available (esp. important utilizing a match var!)
                             
                             group_results <- seed_group %>% 
                               dplyr::distinct(!!as.name(seed_var)) %>% dplyr::pull() %>%
                               furrr::future_map(\(seed) # running parallelization over the seeds (instead of whole groups) might add stability
                                                 seed_walk(seed = seed, # calling an external functions is more robust in terms of not copying unnecessary object to the workers
                                                           walk_network = walk_network, 
                                                           normalize_score = normalize_score, 
                                                           positive_scores_only = positive_scores_only)) %>% 
                               purrr::compact() %>%  # remove NULLs introduced by tryCatch for erroneous walks
                               data.table::rbindlist(use.names = TRUE)
                             
                             group_results <- group_results %>% 
                               dplyr::mutate(seed_term = dplyr::case_when( # mark seed terms of the group/policy field
                                 NodeNames %in% seed_node ~ TRUE, .default = FALSE))
                             
                             if(calculate_means) { # calculate means for non-normalized score
                               group_results <- group_results %>% 
                                 dplyr::mutate(ScoreMean = mean(Score), .by = c(seed_term, NodeNames))
                               }
                             
                             if (!is.null(normalize_score)) { # calculate means for normalized score
                               if (("seeds" %in% normalize_score) & calculate_means) { # calculate means of normalized seed scores in group and normalize again
                                 group_results <- group_results %>% 
                                   dplyr::mutate(ScoreNormMean = mean(ScoreNorm), .by = c(seed_term, NodeNames))
                                 
                                 if (normalize_means) {
                                   group_results <- group_results %>% 
                                     dplyr::mutate(ScoreNormMean = scales::rescale(ScoreNormMean, to = c(0, 1)))
                                 }
                               } 
                               
                               if ("group" %in% normalize_score) { # normalization on Group Level (within each group, e.g. policy field)
                                 group_results <- group_results %>% 
                                   dplyr::mutate(ScoreNormGroup = scales::rescale(Score, to = c(0, 1)))
                                 
                                 if (calculate_means) { # calculation of means within group
                                   group_results <- group_results %>% 
                                     dplyr::mutate(ScoreNormGroupMean = mean(ScoreNormGroup), .by = NodeNames)
                                   
                                   if (normalize_means) {
                                     group_results <- group_results %>% 
                                       dplyr::mutate(ScoreNormGroupMean = scales::rescale(ScoreNormGroupMean, to = c(0, 1)))
                                   }
                                 }
                               }
                             }
                             
                             if (report_quantiles) {
                               cat(paste0(group_name, ": ", name, "\n"))
                               cat(paste(filter_var, "Quantiles:\n"))
                               group_results %>% dplyr::pull(!!as.name(filter_var)) %>% 
                                 stats::quantile() %>% print()
                               cat("\n")
                             }
                               
                             if (reduce_to_means) {
                               group_results <- group_results %>%
                                 dplyr::distinct(dplyr::across(c(NodeNames, seed_term,
                                                                 dplyr::ends_with("Mean"))))
                             }
                             
                             # overwrite seed term values with a fixed value if desired
                             if (!is.null(seedterm_value)) {
                               group_results <- group_results %>% 
                                 dplyr::mutate(dplyr::across(c(dplyr::starts_with("Score")),
                                                             dplyr::case_when(
                                                               seed_term == T ~ seedterm_value,
                                                               .default = .
                                                             )))
                             }
                             
                             # Score filtering
                             if (!is.null(walk_score) & !walk_score_quantile) {
                               
                               if (keep_seed_terms) {
                                 group_results <- group_results %>% 
                                   dplyr::filter(!!as.name(filter_var) >= walk_score |
                                                   seed_term == TRUE)
                               } else {
                                 group_results <- group_results %>% 
                                   dplyr::filter(!!as.name(filter_var) >= walk_score)
                               }
                               
                             }
                             
                             if (!is.null(walk_score) & walk_score_quantile) {
                               
                               quantile_value <- group_results %>% 
                                 dplyr::pull(!!as.name(filter_var)) %>% 
                                 stats::quantile(walk_score) %>% 
                                 .[[1]]
                               
                               cat(paste0("Quantile-based threshold (",
                                          walk_score, " quantile)",
                                          " for ",
                                          group_name, " ", 
                                          name, " in ",
                                          filter_var, ": ",
                                          quantile_value, "\n"))
                               
                               if (keep_seed_terms) {
                                 group_results <- group_results %>% 
                                   dplyr::filter(!!as.name(filter_var) >= quantile_value |
                                                   seed_term == TRUE)
                               } else {
                                 group_results <- group_results %>% 
                                   dplyr::filter(!!as.name(filter_var) >= quantile_value)
                               }
                               
                             }
                             
                             
                             
                             return(group_results)
                             
                           } else return(NULL) # return NULL if no seeds are available
    },
    .progress = progress)
  
  if (flatten_results) { # flatten results into a single dataframe if desired
    rwr_results %>% 
      purrr::compact() %>%  # remove NULLs introduced by missing seeds
      data.table::rbindlist(idcol = group_name, use.names = TRUE) %>% 
      dplyr::relocate(dplyr::any_of(c("Score", "ScoreMean", # set a more convenient col order
                                      "ScoreNorm", "ScoreNormMean",
                                      "ScoreNormGroup", "ScoreNormGroupMean")),
                      .after = dplyr::last_col()) %>% 
      return()
  } else {
    return(rwr_results)
  }

}


seed_walk <- function(seed, walk_network, normalize_score, positive_scores_only){ # convenience function for the seed walks to be used in get_rwr_terms()
  
  res <- tryCatch(# capture non-standard errors thrown by Random.Walk.Restart.Multiplex
    RandomWalkRestartMH::Random.Walk.Restart.Multiplex(
      x = walk_network$AdjMatrixNorm,
      MultiplexObject = walk_network$multiplex,
      Seeds = seed
    ),
    error  = function(e) NULL # return NULL for errors
  ) 
  
  if (!is.null(res)) { # process further if the walk was successfull
    
    res <- res %>% 
      .[["RWRM_Results"]] %>%  # pull relevant data
      dplyr::as_tibble() %>% 
      dplyr::mutate(seed_node = seed) # and set the seed node indicator
    
    if (positive_scores_only) {
      res <- res %>% 
        dplyr::filter(Score > 0)
    }
    
    if (!is.null(normalize_score)) {
      
      if ("seeds" %in% normalize_score){ # normalization and score filtering on Seed Level (for each Random Walk)
        res <- res %>%
          dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
      }
      
    }
    
  }
  
  return(res)
}

# this function makes the multiplex objects to be passed on to the random walk function
make_multiplex_objects <- function(dat,              # data to be passed to calculate_network()
                                   vertex_a,         # vertex A column to be passed to calculate_network()
                                   vertex_b,         # vertex B column to be passed to calculate_network()
                                   directed = FALSE, # is the network directed?
                                   pmi_weight = TRUE,# should PMI weights be calculated? If TRUE, make sure vertex A and B are specified correctly
                                   network = NULL,   # can be used to pass already-calculated networks. If NULL, a network will be calculated from the vertices
                                   keep_igraph_network = FALSE, # should the igraph network be kept as a seperate object? Note
                                   keep_multiplex_network = TRUE, # should the multiplex network be kept as a separate object?
                                   keep_adjacency_matrix = FALSE, # should the non-normalized adjacency matrix be kept?
                                   keep_normalized_adjacency_matrix = TRUE # should the normalized adjacency matrix be calculated and kept?
)
  
{
  require(RandomWalkRestartMH)
  
  if (is.null(network)) {
    network <- calculate_network(
      dat,
      vertex_a = vertex_a,
      vertex_b = vertex_b,
      directed = directed,
      pmi_weight = pmi_weight,
      as_data_frame = F # return as igraph object
    ) 
  }
  
  multiplex <- RandomWalkRestartMH::create.multiplex(list(network = network)) # make multiplex object for random walks
  
  if(keep_adjacency_matrix | # calculate adjacency matrix if needed
     keep_normalized_adjacency_matrix) {
    AdjMatrix <- compute.adjacency.matrix.mono(multiplex)
  }
  
  if(keep_normalized_adjacency_matrix){ # calculate normalized adjacency matrix if needed
    AdjMatrixNorm <- RandomWalkRestartMH::normalize.multiplex.adjacency(AdjMatrix)
  }
  
  return(c(
    if(keep_igraph_network) {
      list(network = network)
    },
    if(keep_multiplex_network) {
      list(multiplex = multiplex)
    }, 
    if(keep_adjacency_matrix){
      list(AdjMatrix = AdjMatrix)
    }, 
    if(keep_normalized_adjacency_matrix){
      list(AdjMatrixNorm = AdjMatrixNorm)
    }
  ))
  
}


compute.adjacency.matrix.mono <- function(x) # delta is no longer needed for monoplex networks
  
  ### An adjusted version of the compute.adjacency.matrix() function from 
  ###  the RandomWalkRestartMH package, making monoplex network preparation 
  ###  more efficient by dropping unnecessary overhead
  ###  Specifically, it vastly reduces the use of RAM (which would require 
  ###  manual flushing with gc() every time) by dropping everything connected
  ###  to the line "offdiag <- (delta/(L-1))*Idem_Matrix" which is not needed
  ###  for monoplex networks
  
{
  
  require(igraph)
  require(Matrix)
  require(RandomWalkRestartMH)
  
  if (!isMultiplex(x) & !isMultiplexHet(x)) {
    stop("Not a Multiplex or Multiplex Heterogeneous object")
  }
  
  
  N <- x$Number_of_Nodes_Multiplex
  L <- x$Number_of_Layers
  
  Layers_Names <- names(x)[seq(L)]
  
  ## IDEM_MATRIX.
  Idem_Matrix <- Matrix::Diagonal(N, x = 1)
  
  counter <- 0 
  Layers_List <- lapply(x[Layers_Names],function(x){
    
    counter <<- counter + 1;    
    if (is_weighted(x)){ 
      Adjacency_Layer <-  igraph::as_adjacency_matrix(x,sparse = TRUE, 
                                                      attr = "weight")
    } else {
      Adjacency_Layer <-  igraph::as_adjacency_matrix(x,sparse = TRUE)
    }
    
    Adjacency_Layer <- Adjacency_Layer[order(rownames(Adjacency_Layer)),
                                       order(colnames(Adjacency_Layer))]
    colnames(Adjacency_Layer) <- 
      paste0(colnames(Adjacency_Layer),"_",counter)
    rownames(Adjacency_Layer) <- 
      paste0(rownames(Adjacency_Layer),"_",counter)
    Adjacency_Layer
  })
  
  MyColNames <- unlist(lapply(Layers_List, function (x) unlist(colnames(x))))
  MyRowNames <- unlist(lapply(Layers_List, function (x) unlist(rownames(x))))
  names(MyColNames) <- c()
  names(MyRowNames) <- c()
  SupraAdjacencyMatrix <- Matrix::bdiag(unlist(Layers_List))
  colnames(SupraAdjacencyMatrix) <-MyColNames
  rownames(SupraAdjacencyMatrix) <-MyRowNames
  
  SupraAdjacencyMatrix <- as(SupraAdjacencyMatrix, "dgCMatrix")
  return(SupraAdjacencyMatrix)
}



# this function calculates a (pmi weighted) network. Based on https://github.com/TimBMK/Tools-Scripts/blob/master/Tools%20%26%20Scripts/network_snapshots.R
calculate_network <-             # function to be mapped over timeframes
  function(data,
           vertex_a,
           vertex_b,
           directed = FALSE, # is the network directed?
           pmi_weight = TRUE, # should PMI weights be calculated? If TRUE, make sure vertex A and B are specified correctly
           as_data_frame = FALSE, # should the output be returned as a data frame?
           ...) {
    
    require(dplyr)
    require(tidyr)
    require(igraph)
    require(widyr)
    
    if (pmi_weight == TRUE) {
      suppressWarnings({ # suppress warnings about deprecated matrix function in pmi calculation
        slice <-
          data %>%
          dplyr::select({{ vertex_a }}, {{ vertex_b }}) %>%
          widyr::pairwise_pmi_(feature =  {{vertex_a}}, item = {{vertex_b}}, sort = F) %>% dplyr::rename(weight = pmi) %>% # calculate PMI as weight (use pairwise_pmi_() avoid problems with column specification)
          igraph::graph_from_data_frame(directed = F) # make igraph object for slice
      })
      
    } else {
      # unweighted if not pmi-weighted
      slice <-
        data %>% dplyr::as_tibble() %>%
        dplyr::select({{ vertex_a }}, {{ vertex_b }}) %>%
        igraph::graph_from_data_frame(directed = directed) # make igraph object for slice
    }
    
    if (as_data_frame == T) {
      slice <- igraph::as_data_frame(slice, what = "edges") 
    } 
    
    return(slice)
    
  }



    