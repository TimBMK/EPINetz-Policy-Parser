
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


filter_tokens <- function(tokens, 
                          tokens_col = "lemma", # which column should be used for token filtering. Usually "token" or "lemma"
                          tags = c("NN", "NE"), # tags of a tokens object to keep. Expects a "tag" column. NULL to skip
                          min_str_length = 2, # minimum string length to keep
                          tolower = TRUE, # should tokens be set to lower? Ignores URLs and all-caps acronyms. Note that this is executed before stopword filtering, which is case-sensitive
                          stopword_languages = c("en", "de", "fr"), # stopword dictionaries to be removed. NULL to skip
                          stopword_dictionary = "snowball", # stopword dictionary to be used. See stopwords::stopwords()
                          additional_stopwords = c("amp", "&amp", "RT", "rt", "--", "---"), # provide additional stopwords to drop here
                          replies = NULL, # should replies be considered for the random walks? Expects a column named is_reply. NULL to skip  
                          keep_mentions = TRUE, # should @-mentions be filtered out?
                          keep_urls = TRUE # should URLs be filtered out?
                          ) {
  
  require(dplyr)
  require(stopwords)
  
  # some checks
  
  if (!is.null(tags) & !("tag" %in% colnames(tokens))){
    stop("Missing 'tag' column for tag filtering")
  }
  
  if (!is.null(replies) & !("is_reply" %in% colnames(tokens))){
    stop("Missing 'is_reply' column for reply filtering")
  }
  
  if (any(!(stopword_languages %in% stopwords::stopwords_getlanguages(stopword_dictionary)))) {
    stop(
      cat("Language '",
            stopword_languages[!stopword_languages %in%
                                 stopwords::stopwords_getlanguages(stopword_dictionary)],
      "' not provided in stopword dictionary '",
      stopword_dictionary, "'.\n", sep = "")
    )
  }
  
  # Filtering Sequences
  
  if (!is.null(tags)) {
    tokens <- tokens %>% 
      filter(tag %in% tags)
  }
  
  tokens <- tokens %>% 
    filter(str_length(!!as.name(tokens_col)) >= min_str_length) 
    
  if (tolower) {
    tokens <- tokens %>%
      mutate(lemma = # lower case - 
               case_when(str_detect(!!as.name(tokens_col), "http") # except for URLs (so they don't break) ...
                         ~ !!as.name(tokens_col), 
                         !!as.name(tokens_col) == toupper(!!as.name(tokens_col)) # ... and acronyms (all caps), e.g. "UN", "IT" etc
                         ~ !!as.name(tokens_col),
                 .default = tolower(!!as.name(tokens_col))))
  }
  
  if (!is.null(stopword_languages)) {
    
    stopwords <- stopword_languages %>% # combine stopwords dicts
      map(\(lang) stopwords::stopwords(language = lang, 
                                       source = stopword_dictionary)) %>% 
      unlist()
    
    tokens <- tokens %>% 
      filter(!(!!as.name(tokens_col) %in% stopwords))
  }
  
  if (!is.null(additional_stopwords)) {
    tokens <- tokens %>% 
      filter(!(!!as.name(tokens_col) %in% additional_stopwords))
  }
  

  
  if (!is.null(replies)) {
    tokens <- tokens %>%
    filter(is_reply == replies | is_reply == FALSE) # filter for reply condition (TRUE includes replies, FALSE does not)
  }
  
  if (keep_mentions == FALSE) {
    tokens <- tokens %>%
      filter(!str_detect(!!as.name(tokens_col), "@")) # drop all lemmas containing "@" - that is, all mentions
  }

  if (keep_urls == FALSE) {
    tokens <- tokens %>%
      filter(!str_detect(!!as.name(tokens_col), "http"))  # drop all URLs
  }
  
  return(tokens)
}


get_latest_tokens_file <- function(path, 
                                   pattern ="tokens.csv.tar.gz", # regular updates of the tokens file will always be named "tokens.csv.tar.gz"
                                   fallback = "tokens_init") {  # fallback pattern to look for, with only the latest returned 
                                                                #  if there are multiple matches according to the pattern "tokens_init_[date]"
  
  files <- list.files(path)
  
  if (pattern %in% files) { # check for latest tokens file and load
    
    latest_tokens <- pattern 
    
  } else { # else, fall back to initial tokenization
    
    latest_tokens <- file.path("Tokenizer", files[str_detect(files, fallback)])
                         
  }
  
  return(file.path(path, latest_tokens))
  
}


