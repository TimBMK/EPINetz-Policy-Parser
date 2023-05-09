calculate_network <-             # function to be mapped over timeframes
  function(data,
           vertex_a,
           vertex_b,
           directed,
           pmi_weight,
           as_data_frame = F,               # should the output be returned as a data frame?
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