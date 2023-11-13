## Initial Classification ##
############################

# Classify Tweets #

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

dir = "init_classification"

## settings

classification_timeframe = "weeks" # weeks(1) bugs out (lubridate bug) # length of the timeframe for the classification, e.g. one week before the RWR timeframe
classification_before_after = "before" # set if the classification_timeframe is before or after the rwr_timeframe, e.g. the week before or after. "after" to for after, "before" for before

# What data should be used in the classification? This defaults to the RWR settings
classification_replies = FALSE # should replies get classified?
classification_mentions = TRUE # should mentions be utilized? adviced to keep this in line with walk_mentions (TRUE)
classification_urls = TRUE # should urls be utilized? adviced to keep this in line with walk_urls (TRUE)

classification_measure = "ScoreNormMean" # one of ScoreNorm, ScoreNormMean, ScoreNormGroup, ScoreNormGroupMean, or (for raw scores) Score or ScoreMean

classification_cutoff = NULL # Should an additional cutoff be set? Applies to classification measure. NULL to skip

seedterm_value = NULL # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only

keep_seed_terms = NULL
                    
cut_frequent_policy_terms = NULL  # Should terms appearing in numerous policy field be cut? 
                                  #  "auto" to cut terms appearing in more than 50% of the policy fields
                                  #  numeric value for a specific number
                                  #  NULL to skip

cutoff_value = 0.1 # a numerical value to set. Scores below will be set to 0. NULL to skip

cutoff_quantile = TRUE # if TRUE, the cutoff_value specifies a quantile, rather than a fixed value

cutoff_normalized_scores = TRUE # if TRUE, the cutoff is applied to the normalized scores. Otherwise, normalization is applied after the cutoff

minimum_results = NULL # Numerical minimum number of results for each group to be returned. Bypasses the cutoff_value as needed. NULL to skip

normalize_scores = "group" # should the score in the documents be normalized between 0 and 1? Can be "doc" (normalize within each document), "group" (normalize for each group), or NULL to skip

return_walk_terms = TRUE # should the processed walk terms be returned for further analysis and transparency?

return_unclassified_docs = TRUE # should the IDs of the unlassified docs be returned? 




# Classify Documents

## read in data
walk_data <- list.files(file.path(dir, "walk_terms")) %>% str_remove(".csv") # all available timeframes

# walk_terms <- vroom(paste0("init_classification/walk_terms/", rwr_timeframe, ".csv"))

classification_NE <- get_latest_tokens_file(path = "Tokenizer", 
                                            pattern = "tokens_full.csv.tar.gz") %>% 
  vroom(col_types = list(doc_id = "c")) %>% 
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = classification_replies, # should replies get classified?
                keep_mentions = classification_mentions, # should @-mentions be kept?
                keep_urls = classification_urls) %>%   # should URLs be kept?
  split_timeframes(datetime_var = "_source.created_at", 
                   timeframe = classification_timeframe,
                   before_after = classification_before_after)


tweets <- vroom(file = "Tokenizer/data_init_tweets_2023-06-22.csv.tar.gz",
                col_types = list(`_id`= "c", `_source.author_id` = "c",
                                 `_source.conversation_id` = "c",
                                 `_source.in_reply_to_user_id` = "c",
                                 `_source.withheld.scope` = "c",
                                 `_source.withheld.country_codes` = "c"),
                guess_max = 1000000) %>% 
  filter(is_reply == classification_replies | is_reply == FALSE)# %>%  # only get replies if desired
  # split_timeframes(datetime_var = "_source.created_at", 
  #                  timeframe = classification_timeframe,
  #                  before_after = classification_before_after)


# Classify

classification_result <- classification_NE %>% 
  imap(\(NE, timeframe)
       {
         cat(paste0(timeframe, "\n"))
         
         filename <- paste0(dir, "/walk_terms/", timeframe, ".csv")
         
         if (file.exists(filename)){ #check if the file exists
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
           } else { # print a warning if the file does not exist
             warning(paste(filename, "does not exist in the specified directory"))
           }
         
         cat("\n")
         
         
  }
  ) 


# save

saveRDS(classification_result, file = file.path(dir, "init_classified_tweets.RDS"))

# check results

classified_docs <- classification_result %>% 
  map_vec(\(dat)
      {dat$classified_documents %>% distinct(doc_id) %>% nrow()}) %>% 
  sum()

unclassified_docs <- classification_result %>% 
  map_vec(\(dat)
          {dat$unclassified_documents %>% distinct(doc_id) %>% nrow()}) %>% 
  sum()

cat(paste(unclassified_docs, "out of", 
          (unclassified_docs + classified_docs), 
          "documents classified",
          paste0("(", percent(unclassified_docs/
                                (unclassified_docs + classified_docs)), 
                 " unclassified)")))


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
  vroom_write(file.path(dir, "init_classified_tweets_wide.csv.tar.gz"))

# # Additional checks
# classification_result$classified_documents %>%
#   inner_join(tweets %>% filter(!str_detect(`_source.text`, "^RT ")) %>% # drops RTs
#                select(`_id`, `_source.text`, `_source.created_at`),
#              by = join_by(doc_id == `_id`)) %>%
#   slice_max(policy_score, n = 20, # set number of documents returned per field with n = ...
#             by = policy_field) %>%
#   View()
# 
# #### to normalize or not to normalize?
# classification_result$classified_documents %>%
#   inner_join(tweets %>% filter(!str_detect(`_source.text`, "^RT ")) %>% # drops RTs
#                select(`_id`, `_source.text`, `_source.created_at`),
#              by = join_by(doc_id == `_id`)) %>%
#   slice_max(policy_score_norm, n = 20, # set number of documents returned per field with n = ...
#             by = policy_field) %>%
#   View()
# 
# 
# 
# classification_result$unclassified_documents %>%
#   left_join(tweets %>% select(`_id`, `_source.text`, `_source.created_at`),
#              by = join_by(doc_id == `_id`)) %>%
#   View()
# 

