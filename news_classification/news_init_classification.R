## Initial Classification ##
############################

# Classify News Articles #

{
  library(tidyverse)
  library(quanteda)
  library(quanteda.textstats)
  library(data.table)
  library(furrr)
  library(vroom)
  library(igraph)
  library(RandomWalkRestartMH)
  library(scales)
}

source("utils_text_processing.R")
source("classify_documents.R")


dir = "news_classification"

## settings

classification_timeframe = "weeks" # length of the timeframe for the classification, e.g. one week before the RWR timeframe
classification_before_after = "before" # set if the classification_timeframe is before or after the rwr_timeframe, e.g. the week before or after. "after" to for after, "before" for before
switch_before_after_for_missing = TRUE # should the before/after timeframe be switched if the walk terms are missing in a period, i.e. use "after" when there was no valid walk data for "before" (and vice versa)?


classification_measure = "ScoreNormMean" # one of ScoreNorm, ScoreNormMean, ScoreNormGroup, ScoreNormGroupMean, or (for raw scores) Score or ScoreMean

classification_cutoff = NULL # Should an additional cutoff be set? Applies to classification measure. NULL to skip

seedterm_value = NULL # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only

keep_seed_terms = NULL

cut_frequent_policy_terms = NULL  # Should terms appearing in numerous policy field be cut? 
                                  #  "auto" to cut terms appearing in more than 50% of the policy fields
                                  #  numeric value for a specific number
                                  #  NULL to skip

cutoff_value = 0.5 # a numerical value to set. Scores below will be set to 0. NULL to skip

cutoff_quantile = FALSE # if TRUE, the cutoff_value specifies a quantile, rather than a fixed value

cutoff_normalized_scores = TRUE # if TRUE, the cutoff is applied to the normalized scores. Otherwise, normalization is applied after the cutoff

minimum_results = 50 # Numerical minimum number of results for each group to be returned. Bypasses the cutoff_value as needed. NULL to skip

normalize_scores = "doc" # should the score in the documents be normalized between 0 and 1? Can be "doc" (normalize within each document), "group" (normalize for each group), or NULL to skip
# normalizing the scores per document seems beneficial for longer docs, "group" is better suited for short documents like tweets

return_walk_terms = TRUE # should the processed walk terms be returned for further analysis and transparency?

return_unclassified_docs = TRUE # should the IDs of the unlassified docs be returned? 


# Classify Documents

## read in data

walk_data <- list.files(file.path(dir, "walk_terms")) %>% str_remove(".csv") # all available timeframes

news_docs <- list.files(file.path(dir, "data"), full.names = T) %>% 
  map(\(file) vroom(file)) %>% rbindlist(fill = TRUE)

news_docs <- news_docs %>% # add outlet indicator
  mutate(outlet = str_remove_all(`_source.host`,
                                 paste0(c("^www\\.", "\\.de$", "\\.net$", 
                                          "\\.co.uk$", "\\.com$"), 
                                        collapse = "|")))

news_docs <- news_docs %>% # remove a small number of non-german outlets accidentially in the data / only keep select outlets (drops 20 docs)
  filter(outlet %in% c("faz", "welt", "bild", "spiegel", 
                       "zeit", "sueddeutsche", "stuttgarter-zeitung"))

classification_NE <- list.files(dir, 
                                pattern = "tokens_news_", # only read the required tokens object
                                full.names = TRUE) %>% 
  map(\(file) { # reading the files and filtering is mapped to minimize RAM usage
    file %>% 
      vroom() %>% 
      filter_tokens(tokens_col = "lemma", 
                    tags = c("NN", "NE"), # Noun words and NEs only
                    #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                    replies = NULL, # not needed
                    keep_mentions = NULL, # not needed
                    keep_urls = NULL) # not needed
  }) %>% 
  rbindlist(fill = TRUE) %>% 
  right_join(news_docs %>% select(`_id`, `_source.estimated_date`), 
            join_by(doc_id == `_id`)) %>% # add estimated publication date. The right_join drops all observations not in the new_docs, esp. the removed outlets
  split_timeframes(datetime_var = "_source.estimated_date", 
                   timeframe = classification_timeframe,
                   before_after = classification_before_after)


## Classify

classification_result <- classification_NE %>% 
  imap(\(NE, timeframe)
       {
         cat(paste0(timeframe, "\n"))
         
         filename <- paste0(dir, "/walk_terms/", timeframe, ".csv")
         
         if (!file.exists(filename)){ # check if the file does not exist
           
           warning(paste(filename, "does not exist in the specified directory")) # print a warning if the file does not exist
           
           if (switch_before_after_for_missing) { # try the respective other before/after timeframe if desired
             
             if (classification_before_after == "before") {
               switched_before_after <- "after"
               switched_timeframe <- as_date(timeframe) + 
                 as.period(classification_timeframe)
             }
             if (classification_before_after == "after") {
               switched_before_after <- "before"
               switched_timeframe <- as_date(timeframe) - 
                 as.period(classification_timeframe)
             }
             
             warning(paste("... trying", paste0("'",switched_before_after,"'"), "timeframe",
                           switched_timeframe, "instead"))
             
             filename <- paste0(dir, "/walk_terms/", switched_timeframe, ".csv") # replace filename with switched timeframe
           }
         } 
         
         if (file.exists(filename)){ # check if the (updated) filename exists
           res <- classify_documents(
             walk_terms = suppressMessages(vroom(filename)), # data with terms generated by get_rwr_terms()
             group_name = "policy_field", # name of the group variable which specifies the classes the documents will be sorted into
             document_tokens = NE, # data with the documents to be classified. Expects tokenized data, with a doc_id and one token per row
             tokens_var = "lemma", # name of the tokens variable within the documents dataframe. Usually "tokens", "lemma", etc.
             doc_id = "doc_id", # name of the doc_id variable in document_tokens
             classification_measure = classification_measure, # set the measure to use for classification here. Must be present in the data
             classification_cutoff = classification_cutoff, # Should a cutoff be set to filter the walk_terms?? Applies to classification measure. NULL to skip. This is useful if a more strict cutoff is desired than in get_rwr_terms()
             keep_seed_terms = keep_seed_terms, # should seed terms be kept even if their score is lower than the cutoff? only applies if a cutoff is specified
             seedterm_value = seedterm_value, # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only
             normalize_scores = normalize_scores, # should the score in the documents be normalized between 0 and 1? Can be doc (normalize within each document), group (normalize for each group), or NULL to skip
             cutoff_value = cutoff_value, # a numerical value to set. Scores below will be set to 0. NULL to skip
             cutoff_quantile = cutoff_quantile, # if TRUE, the cutoff_value specifies a quantile, rather than a fixed value
             cutoff_normalized_scores = cutoff_normalized_scores, # if TRUE, the cutoff is applied to the normalized scores. Otherwise, normalization is applied after the cutoff
             minimum_results = minimum_results, # Numerical minimum number of results for each group to be returned. Bypasses the cutoff_value as needed. NULL to skip    
             cut_frequent_group_terms = cut_frequent_policy_terms,
             return_walk_terms = return_walk_terms, # should the processed walk terms be returned for further analysis and transparency?
             return_unclassified_docs = return_unclassified_docs, # should the IDs of the unlassified docs be returned? 
             # setting return_walk_terms or return_unclassified_docs to TURE returns a list of dataframes rather than a single dataframe
             verbose = TRUE)
           return(res)
         } else {
           if (switch_before_after_for_missing) {
             warning("Switched timeframe does not exist in directory either.")
           }
         }
         
         cat("\n")
  }
  ) 

# Save

classification_result %>% # sum missing results
  iwalk(\(result, timeframe) 
       {if(is.null(result)){
         cat(paste("No result for timeframe", timeframe))
       }})

classification_result <- classification_result %>% compact() # remove missing timeframes


saveRDS(classification_result, file = file.path(dir, "init_classified_news.RDS"))
  

# check results

classified_docs <- classification_result %>% 
  map_vec(\(dat)
          {
            dat$classified_documents %>% distinct(doc_id) %>% nrow()}) %>% 
  sum()

unclassified_docs <- classification_result %>% 
  map_vec(\(dat)
          {dat$unclassified_documents %>% distinct(doc_id) %>% nrow()}) %>% 
  sum()

cat(paste(unclassified_docs, "out of", 
          (unclassified_docs + classified_docs), 
          "documents classified",
          paste0("(", percent(unclassified_docs/
                                (unclassified_docs + classified_docs),
                              accuracy = 0.01), 
                 " unclassified)")))

unclassified_docs <- classification_result %>% 
  map(\(result)
      get_unclassified_documents( 
        result, # the result of classify_documents(). requires the walk_terms data
        documents = news_docs, # the full document data to be matched to the classification result for printout
        doc_id = join_by(doc_id == `_id`), # the name of the doc_id used for matching. Can be a join_by() function where classification_result = a and documents = b
        n = 20, # the number of documents to print/return 
        mode = "return" # should the results be printed out a dataframe of results be returned?
      ) 
  ) %>% compact() %>% rbindlist() #remove NULLs (timeframes without missing docs) and bind together
 # almost all unclassified docs are very short - seemingly failed scrapes that returned only single paragraphs or sentences


# make csv dataframe of tweets to pass to HD
classified_documents <- classification_result %>%
  map(\(dat)
      dat$classified_documents) %>%
  rbindlist()

classified_documents %>% 
  select(!score) %>%  # only pass the normalized score
  rename(tweet_id = doc_id) %>% # rename for clarity
  pivot_wider(names_from = policy_field, values_from = score_norm) %>% # wide format (1 column per policy field)
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
  vroom_write(file.path(dir, "init_classified_news_wide.csv.tar.gz"))
