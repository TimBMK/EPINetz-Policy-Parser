
drop_quantile <- function(data,   # data
                          tokens, # column with tokens 
                          quantile = 0.1, # quantile to be dropped
                          ignore_case = TRUE, # ignore casing?
                          group,  # grouping column, e.g. type of word, for report statistics
                          verbose = TRUE # report staistics
)
{
  
  require(dplyr)
  require(stringr)
  require(scales)
  require(stats)
  
  if (ignore_case == TRUE) {
    data <- data %>% 
      dplyr::mutate(
        orig_tokens = !!as.name(tokens), # preserve original tokens
        {{tokens}} := stringr::str_to_lower(!!as.name(tokens)))  # convert to lower case to ignore case
  }
  
  # add count for tokens
  res <- data %>% 
    dplyr::group_by(!!as.name(tokens)) %>% 
    dplyr::mutate(entity_count = dplyr::n()) %>% 
    dplyr::ungroup() 
  
  # save min and max counts for verbose output
  if (verbose == TRUE) {
    range <- tibble(min = min(res$entity_count),
                    max = max(res$entity_count))
  }
  
  # calculate quantile for filtering
  threshold <- res %>% 
    dplyr::distinct(!!as.name(tokens), .keep_all = T) %>% 
    dplyr::pull(entity_count) %>% 
    stats::quantile(probs = quantile) %>% 
    .[[1]]
  
  # drop quantile
  res <- res %>%  
    dplyr::filter(entity_count > threshold) 
  
  if (verbose == TRUE) {
    
    cat(paste(scales::percent(quantile), "Quantile:", threshold, "\n"))
    
    cat(paste("Count Range:", range$min, "-", range$max, "\n"))
    
    original_count <- data %>% # save unprocessed count statistics
      dplyr::distinct(!!as.name(tokens), !!as.name(group)) %>% 
      dplyr::summarise(distinct_unprocessed = n(), .by = {{group}}) 
    
    res %>% 
      dplyr::distinct(!!as.name(tokens),!!as.name(group)) %>%
      dplyr::group_by(!!as.name(group)) %>% 
      dplyr::summarise(distinct_processed = n()) %>%
      dplyr::left_join(original_count, by = group) %>% 
      dplyr::arrange(tag) %>% 
      dplyr::add_row(
        {{group}} := "total",
        distinct_unprocessed = sum(.$distinct_unprocessed),
        distinct_processed = sum(.$distinct_processed)
      ) %>%
      dplyr::mutate(
        dropped = distinct_unprocessed - distinct_processed,
        reduction =  scales::percent(1 - distinct_processed / distinct_unprocessed)
      ) %>% 
      print()
  }
  
  if (ignore_case == TRUE) {
    res <- res %>% mutate({{tokens}} := orig_tokens) %>% select(!orig_tokens) # restore original tokens
  }
  
  return(res)
  
}


