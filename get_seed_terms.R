## Extract Seed Terms from Seed Accounts ##
###########################################


get_seed_terms <- function(data,              # dataframe containing tokens, grouping_var and policy_field
                           doc_id = NULL,     # doc_id of the documents. Optional, but can speed up computation
                           tokens,            # tokens variable (can also be lemmas)
                           grouping_var,      # What variable should the tokens be accumulated and compared by? E.g. policy field, committee, name...
                           #remove_tokens = c(stopwords("german"), stopwords("english"), stopwords("french"), "rt", "RT"), # Tokens to be removed in the process, typically stopwords | done in a different step now
                           policy_field = NULL,     # the name of the policy field variable to be added to results. Can be skipped if policy_field = NULL
                           measure = "chi2",        # keyness measure. See textstat_keyness for options
                           threshold = NULL,        # keyness threshold to filter results. Optional, no threshold with NULL
                           max_results = NULL,      # maximum number of terms returned per group, sorted (desc) to the selected measure. Threshold still applies. See drop_max_result_ties for handling of tied values
                           max_result_ties = FALSE, # should tied values be dropped when max_results is applied? May return more rows than requested when TRUE
                           min_results = NULL,      # minimum number of terms returned per group, sorted (desc) to the selected measure. Overwrites threshold if necessary
                           show_plots = FALSE,       # should the keyness results for each group be plotted? 
                           save_plots = FALSE)      # should the plots be saved and returned with the keyterm data?
{
  require(dplyr)
  require(purrr)
  require(quanteda)
  require(quanteda.textstats)
  require(quanteda.textplots)
  
  # prepare corpus
  
  if(!is.null(doc_id)) {
    corpus <- data %>% 
      dplyr::summarise(text = paste(!!as.name(tokens), collapse = " "), .by = c(!!as.name(doc_id), !!as.name(grouping_var))) %>% # not strictly necessary, but speeds up computation
      dplyr::mutate(temp_id = 1:nrow(.)) %>% # generate a temporary ID to allow for duplicates (i.e. same tweet in multiple fields)
      quanteda::corpus(text_field = "text", docid_field = "temp_id") 
  } else { # if no doc_id is given, each row becomes a document
    corpus <- data %>% 
      dplyr::mutate(temp_id = 1:nrow(.)) %>% # generate a temporary ID to allow for duplicates (i.e. same tweet in multiple fields)
      quanteda::corpus(text_field = "text", docid_field = "temp_id") 
  }
  
  # prepare DFM (incl. grouping and stopword removal)
  dfm <- corpus %>% 
    quanteda::tokens(remove_punct = TRUE, remove_symbols = TRUE,
                     remove_numbers = TRUE,  remove_url = TRUE) %>% 
    # quanteda::tokens_remove(remove_tokens) %>%  # done during preprocessing / filter_tokens() now
    quanteda::tokens_group(groups = quanteda::docvars(., grouping_var)) %>% # by ministry
    quanteda::dfm()
  
  # Calculate Keyness
  dfm_groups <- dfm %>% quanteda::docvars() %>% dplyr::distinct() %>% dplyr::pull()
  
  if (save_plots == T) {
    plots <- vector(mode = "list", length = length(dfm_groups)) %>%  # container
      setNames(dfm_groups) # with names
  }
  
  keyness <- dfm_groups %>% 
    purrr:::map(~ tryCatch({
      
      group <- .
      
      textstat <- quanteda.textstats::textstat_keyness(dfm, 
                                                       measure = measure, 
                                                       target = group)
      
      if (show_plots == TRUE) {
        print(quanteda.textplots::textplot_keyness(textstat))
        readline(prompt="Press [enter] to show next plot")
      }
      
      if (save_plots == TRUE) {
        plots[[group]] <<- tryCatch(quanteda.textplots::textplot_keyness(textstat), # assign to plots object outside of function
                                    error = function(e) NULL) # failsafe
      }
      
      textstat_res <- textstat %>% 
        dplyr::as_tibble() %>% 
        dplyr::mutate({{grouping_var}} := group) 
      
      
      # Filter results
      
      if (!is.null(max_results)) {
        textstat_res <- textstat_res %>% 
          dplyr::slice_max(!!as.name(measure), 
                           n = max_results,
                           with_ties = max_result_ties)
      }
      
      if (!is.null(threshold)) {
        
        # if min_results are specified, overwrite threshold as needed
        if (!is.null(min_results)) { # check potential nr of results
          number_results <- textstat_res %>% 
            dplyr::filter(!!as.name(measure) >= threshold) %>% nrow()
        }
        
        if (!is.null(min_results) & number_results < min_results) {
          textstat_res <- textstat_res %>% 
            dplyr::slice_max(!!as.name(measure), 
                             n = min_results,
                             with_ties = TRUE)
          
        } else { # if no min results are specified or there are sufficient results, simply filter for keyness
          textstat_res <- textstat_res %>% dplyr::filter(!!as.name(measure) >= threshold)
        }
        
      } 
      
      return(textstat_res)
      
    }), error = function(e) NULL) %>% 
    compact() %>% 
    purrr::list_rbind()
  
  
  
  # add policy field and handle plot saving
  if (!is.null(policy_field) && grouping_var != policy_field) { # add policy field if necessary
    keyness <- keyness %>% 
      dplyr::left_join(data %>% distinct(!!as.name(grouping_var), !!as.name(policy_field)),
                by = grouping_var)
  }
  
  if (save_plots == TRUE) {
    result <- list(keyness,
                        plots)
    names(result) <- c("key_terms", "plots")
  } else {
    result <- keyness
  }
  
  return(result)
  }
  
