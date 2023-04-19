## Extract Seed Terms from Seed Accounts ##
###########################################


get_seed_terms <- function(data,              # dataframe containing tokens, grouping_var and policy_field
                           doc_id = NULL,     # doc_id of the documents. Optional, but can speed up computation
                           tokens,            # tokens variable (can also be lemmas)
                           grouping_var,      # What variable should the tokens be accumulated and compared by? E.g. policy field, committee, name...
                           remove_tokens = c(stopwords("german"), stopwords("english"), "rt", "RT"), # Tokens to be removed in the process, typically stopwords
                           policy_field = NULL,     # the name of the policy field variable to be added to results. Can be skipped if policy_field = NULL
                           measure = "chi2",        # keyness measure. See textstat_keyness for options
                           threshold = NULL,        # keyness threshold to filter results. Optional, no threshold with NULL
                           show_plots = FALSE,       # should the keyness results for each group be plotted? 
                           save_plots = FALSE)      # should the plots be saved and returned with the keyterm data?
{
  require(dplyr)
  require(quanteda)
  require(quanteda.textstats)
  require(quanteda.textplots)
  
  # prepare corpus
  if(!is.null(doc_id)) {
    corpus <- data %>% 
      dplyr::summarise(text = paste(!!as.name(tokens), collapse = " "), .by = c(!!as.name(doc_id), !!as.name(grouping_var))) %>% # not strictly necessary, but speeds up computation
      quanteda::corpus(text_field = "text") # as no docid is given, they are generated, allowing for duplicates (i.e. same tweet in multiple fields)
  } else { # if no doc_id is given, each row becomes a document
    corpus <- data %>% 
      corpus(text_field = tokens) # as no docid is given, they are generated, allowing for duplicates (i.e. same tweet in multiple fields)
  }
  
  # prepare DFM (incl. grouping and stopword removal)
  dfm <- corpus %>% 
    quanteda::tokens(remove_punct = TRUE, remove_symbols = TRUE,
                     remove_numbers = TRUE,  remove_url = TRUE) %>% 
    quanteda::tokens_remove(remove_tokens) %>%  
    quanteda::tokens_group(groups = docvars(., grouping_var)) %>% # by ministry
    quanteda::dfm()
  
  # Calculate Keyness
  keyness <- dplyr::tibble() # container
  
  if (save_plots == T) {
    plots <- vector(mode = "list", length = length(dfm %>% quanteda::docvars() %>% dplyr::distinct() %>% dplyr::pull())) %>%  # container
      setNames(dfm %>% quanteda::docvars() %>% dplyr::distinct() %>% dplyr::pull()) # with names
  }
  
  for (group in (dfm %>% quanteda::docvars() %>% dplyr::distinct() %>% dplyr::pull())) { # calculate keyness 
    
    textstat <- quanteda.textstats::textstat_keyness(dfm, measure = measure, target = group)
    
    if (show_plots == TRUE) {
      print(quanteda.textplots::textplot_keyness(textstat))
      readline(prompt="Press [enter] to show next plot")
    }
    
    if (save_plots) {
      plots[[group]] <- quanteda.textplots::textplot_keyness(textstat)
    }
    
    keyness <- keyness %>% 
      dplyr::bind_rows(textstat  %>% 
                         dplyr::as_tibble() %>% 
                         dplyr::mutate({{grouping_var}} := group))
    
  }
  
  # Filter results by keyness value
  if (!is.null(threshold)) {
    keyness_res <- keyness %>% dplyr::filter(!!as.name(measure) >= threshold) # threshold filter
  } else {
    keyness_res <- keyness
  }
  
  
  if (!is.null(policy_field) && grouping_var != policy_field) { # add policy field if necessary
    keyness_res <- keyness_res %>% 
      dplyr::left_join(data %>% distinct(!!as.name(grouping_var), !!as.name(policy_field)),
                by = grouping_var)
  }
  
  if (save_plots == TRUE) {
    result <- list(keyness_res,
                        plots)
    names(result) <- c("key_terms", "plots")
  } else {
    result <- keyness_res
  }
  
  return(result)
  }
  
