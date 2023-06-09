get_rwr_terms <- function(walk_network, # an object made by make_multiplex_objects, contatining the multiplex network and the Normalized Adjacency Matrix
                          network_name = NULL, # name of the network, used for matching the match_var. Usually the index when implented in imap functions. NULL to skip. Requires match_var
                          seeds, # a list of dataframes, where each entry represents a specific group of seeds, e.g. a single policy field
                          seed_var, # the variable in the seed dataframes containing the seed terms
                          match_var = NULL, # the variable in the seed dataframes to match on the network_name variable, e.g. specific time periods. NULL to skip. Requires network_name
                          flatten_results = TRUE, # should the results be flattened into a single dataframe (with policy_field indicator) or a list of one dataframe per group? 
                          group_name = "policy_field", # name of the grouping variable if flatten_results = TRUE
                          positive_scores_only = FALSE, # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization
                          normalize_score = c(NULL, "seeds", "group"), # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization
                          walk_score = NULL, # minimal normalized walk score for results to keep. Can be applied to normalized or raw scores, see normalize_score. Will always apply to normalized score if available
                          keep_seed_terms = TRUE, # should seed terms always be kept, regardless of score? Only available if flatten_results = TRUE
                          progress = FALSE) # should the progress be shown for the map function?
{
  
  require(dplyr)
  require(furrr)
  require(purrr)
  require(RandomWalkRestartMH)
  require(data.table)
  require(scales)
  
  match.arg(normalize_score, several.ok = FALSE) # check for correct argument specification here
  
  rwr_results <-
    seeds %>% purrr::map(\(seed_group)
                         {
                           if (!is.null(match_var) & !is.null(network_name)) { # match network names and match var
                             seed_group <- seed_group %>%
                               mutate({{match_var}} := as.character({{match_var}})) %>% 
                               dplyr::filter(!!as.name(match_var) == network_name)}
                           
                           group_results <- seed_group %>% 
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
                                                 
                                                 if (normalize_score == "seeds"){ # normalization and score filtering on Seed Level (for each Random Walk)
                                                   seed_walk <- seed_walk %>%
                                                     dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
                                                 }
                                                 
                                                 return(seed_walk)
                             }) %>% 
                             purrr::compact() %>%  # ... and remove NULLs
                             data.table::rbindlist(use.names = TRUE)
                           
                           group_results <- group_results %>% 
                             dplyr::mutate(seed_term = dplyr::case_when(# mark seed terms of the group/policy field
                               NodeNames %in% seed_node ~ TRUE, .default = FALSE))
                           
                           if (!is.null(normalize_score)) { 
                             
                             if (normalize_score == "group") { # normalization and score filtering on Group Level (within each group, e.g. policy field)
                               group_results <- group_results %>% 
                                 dplyr::mutate(ScoreNorm = scales::rescale(Score, to = c(0, 1)))
                             }
                             
                             if (!is.null(walk_score) & keep_seed_terms == TRUE) {
                               group_results <- group_results %>% 
                                 dplyr::filter(ScoreNorm >= walk_score | # drop results below the desired (normalized) walk score
                                                 seed_term == keep_seed_terms)    # but retain seed terms if desired
                             }
                             
                             if (!is.null(walk_score) & keep_seed_terms == FALSE) {
                               group_results <- group_results %>% 
                                 dplyr::filter(ScoreNorm >= walk_score) # drop results below the desired (normalized) walk score
                             }
                             
                           } else { # Score filtering without normalization
                             
                             if (!is.null(walk_score) & keep_seed_terms == TRUE) {
                               group_results <- group_results %>% 
                                 dplyr::filter(Score >= walk_score | # drop results below the desired (normalized) walk score
                                                 seed_term == keep_seed_terms)    # but retain seed terms if desired
                             }
                             
                             if (!is.null(walk_score) & keep_seed_terms == FALSE) {
                               group_results <- group_results %>% 
                                 dplyr::filter(Score >= walk_score) # drop results below the desired (normalized) walk score
                               
                             } 
                           }
                           return(group_results)
    },
    .progress = progress)
  
  if (flatten_results) { # flatten results into a single dataframe if desired
    rwr_results %>% 
      rbindlist(idcol = group_name, use.names = TRUE) %>% 
      return()
  } else {
    return(rwr_results)
  }

}


# this function makes the multiplex objects to be passed on to the random walk function
make_multiplex_objects <- function(dat,              # data to be passed to calculate_network()
                                   vertex_a,         # vertex A column to be passed to calculate_network()
                                   vertex_b,         # vertex B column to be passed to calculate_network()
                                   directed = FALSE, # is the network directed?
                                   pmi_weight = TRUE,# should PMI weights be calculated? If TRUE, make sure vertex A and B are specified correctly
                                   network = NULL,   # can be used to pass already-calculated networks. If NULL, a network will not be calculated
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
  
  multiplex <- create.multiplex(list(network = network)) # make multiplex object for random walks
  
  if(keep_adjacency_matrix | # calculate adjacency matrix if needed
     keep_normalized_adjacency_matrix) {
    AdjMatrix <- compute.adjacency.matrix.mono(multiplex)
  }
  
  if(keep_normalized_adjacency_matrix){ # calculate normalized adjacency matrix if needed
    AdjMatrixNorm <- normalize.multiplex.adjacency(AdjMatrix)
  }
  
  # if (is.null(save_to_path)) {
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
  # }
  
  # if (!is.null(save_to_path)) {
  #   
  #   if(keep_igraph_network) {
  #     
  #   }
  #   
  #   if(keep_multiplex_network) {
  #     
  #   }
  #   
  #   if(keep_adjacency_matrix){
  #   
  #   }
  #   
  #   if(keep_normalized_adjacency_matrix){
  #   
  #   }
  #   
  # }
  
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



    