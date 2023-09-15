# Regular Workflow for Classification #

## Classify Documents ##
### look up Tweets of the IDs passed to the API, tokenize and classify

{
  library(tidyverse)
  library(elastic)
  library(quanteda)
  library(quanteda.textstats)
  library(spacyr)
  library(data.table)
  library(scales)
  library(vroom)
  library(furrr)
}

source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R") # scrolled search function

source("utils_text_processing.R")

source("classify_documents.R")

tweet_ids <- "..." # a character vector of tweet IDs of tweets to be classified

# setup parallel computing session
threads = 16 # nr of threads to use

if (installr::is.RStudio()){
  plan(multisession, workers = threads) 
  # Multisession for R Studio Sessions
} else {
  plan(multicore, workers = threads)
  # multicore for scripts / non-rstudio processes
}


save_results = TRUE # should the results be saved?

if (save_results){
  # subdirectory to save results in
  date <- Sys.Date()
  dir <- file.path("regular_classification/classified_documents", date) 
  dir.create(dir)
  # the subdirectory only indicates the day of the classification and is not necessarily coherent with the classification terms!
  #  instead, this classification routine always uses the LATEST classification terms, as it is intended for continous classification with regular term updates
}



## settings

# classification_timeframe = "weeks" # weeks(1) bugs out (lubridate bug) # length of the timeframe for the classification, e.g. one week before the RWR timeframe
# classification_before_after = "before" # set if the classification_timeframe is before or after the rwr_timeframe, e.g. the week before or after. "after" to for after, "before" for before

# What data should be used in the classification? This defaults to the RWR settings
classification_replies = FALSE # should replies get classified?
classification_mentions = TRUE # should mentions be utilized? adviced to keep this in line with walk_mentions (TRUE)
classification_urls = TRUE # should urls be utilized? adviced to keep this in line with walk_urls (TRUE)

classification_measure = "ScoreNormMean" # one of ScoreNorm, ScoreNormMean, ScoreNormGroup, ScoreNormGroupMean, or (for raw scores) Score or ScoreMean
                                         #  this should correspond to (one of) the score(s) calculated during the random walks
cutoff = NULL # Should an additional cutoff be set? Applies to classification measure. NULL to skip
seedterm_value = NULL # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only
keep_seed_terms = NULL
cut_frequent_policy_terms = NULL  # Should terms appearing in numerous policy field be cut? 
#  "auto" to cut terms appearing in more than 50% of the policy fields
#  numeric value for a specific number
#  NULL to skip
cut_lower_quantile_fields = 0.1 # Should policy classifications within a document be cut if they fall below a certain quantile? NULL to skip. Else numerical value to specify the quantile
normalize_scores = "group" # should the score in the documents be normalized between 0 and 1? Can be "doc" (normalize within each document), "group" (normalize for each group), or NULL to skip

return_walk_terms = TRUE # should the processed walk terms be returned for further analysis and transparency? 
return_unclassified_docs = TRUE # should unclassified docs be returned for further analysis and transparency?

print_walk_terms = 20 # should a number of top-ranking terms for each policy field be printed? NULL to skip
print_unclassified_docs = 30 # should a random sample of the unlassified docs be printed? 
print_example_docs = 10 # should a number of top-ranking documents for each policy field be printed? NULL to skip


verbose = TRUE # set verbosity of classify_tweets()



# Read lists
ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))
committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))
epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_11.RDS") # list of all EPINetz Accounts

# Get Terms
latest_results <- tibble(folder = list.dirs("regular_classification/terms")) %>% 
  filter(str_detect(folder, "\\d+.*")) %>% # filter for folders with a date
  slice_max(folder) %>%  # pick the latest walk_terms file available
  pull()

latest_walk_terms <- latest_results %>% list.files() %>% 
  .[str_detect(., "walk_terms")]

walk_terms <- vroom(file.path(latest_results, 
                              latest_walk_terms))


# some checks

if (!(classification_measure %in% names(walk_terms))) {
  stop(paste(classification_measure, "not found in latest walk terms.", 
              "Respecify the classification_measure or make sure the walk terms contain the desired measure."))
}

if (return_walk_terms == FALSE & print_walk_terms == TRUE) {
  warning("Set return_walk_terms to TRUE if a sample of walk terms should be printed.")
}

if (return_unclassified_docs == FALSE & print_unclassified_docs == TRUE) {
  warning("Set return_unclassified_docs to TRUE if a sample of unclassified documents should be printed.")
}

# connect to Heidelberg Database via Tunnel. In Bash, use:
# ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@adrastea.ifi.uni-heidelberg.de
# Alternative: # ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@wega.ifi.uni-heidelberg.de

credentials <- readRDS("/data/koenigt/elastic_credentials.RDS")

conn <- connect(
  path = "",
  port = 9201,
  user = credentials$user,
  pwd = credentials$password,
  host = "localhost"
)

conn$ping() # check connection


#### API call (live data) ####

tweets <- tibble() # data container

for (i in seq(1, length(tweet_ids %>% unique()), 100)) { # the search query needs to be chopped up into smaller bits (100 IDs per chunk)
  
  # cat(paste("Rows", i, "to", i+(100-1), "\n"))
  
  dat <- na.omit((tweet_ids %>% unique())[i:(i+(100-1))]) # ID chunks of 100 each 
  
  # make query:
  q <- paste0("_id:(", paste(dat, collapse = " OR "), ")")
  
  res <- Search(conn, q =  q, index = "twitter_v2_tweets", source = NULL, asdf = T, size = 10000)# get the full Tweets 
  
  tweets <- bind_rows(tweets, res$hits$hits)
  
}

# add reply indicator
tweets <- tweets %>% mutate(is_reply = case_when(!is.na(`_source.in_reply_to_user_id`) ~ TRUE,
                                                         .default = FALSE))


# filter for reply condition
tweets <- tweets %>% filter(is_reply == classification_replies | is_reply == FALSE) 



# Tokenization, Lemmatization, Noun-Word Filtering

corpus <- corpus(tweets, docid_field = "_id", text_field = "_source.text", 
                      meta = list(names(tweets)), # preserve all vars as metadata
                      unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

classification_tokens <-
  spacy_parse(
    corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

spacy_finalize() # end spacy

classification_NE <- classification_tokens %>% # filter for NEs
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = NULL, # filtering for reply condition is done before tokenization to save on computing and can thus be skipped with NULL
                keep_mentions = classification_mentions, # should @-mentions be kept?
                keep_urls = classification_urls) # should URLs be kept?



#### Classify Documents ####

classification_result <- classify_documents(
  walk_terms = walk_terms, # data with terms generated by get_rwr_terms()
  group_name = "policy_field", # name of the group variable which specifies the classes the documents will be sorted into
  document_tokens = classification_NE, # data with the documents to be classified. Expects tokenized data, with a doc_id and one token per row
  tokens_var = "lemma", # name of the tokens variable within the documents dataframe. Usually "tokens", "lemma", etc.
  doc_id = "doc_id", # name of the doc_id variable in document_tokens
  classification_measure = classification_measure, # set the measure to use for classification here. Must be present in the data
  cutoff = cutoff, # Should a cutoff be set to filter the walk_terms?? Applies to classification measure. NULL to skip. This is useful if a more strict cutoff is desired than in get_rwr_terms()
  keep_seed_terms = keep_seed_terms, # should seed terms be kept even if their score is lower than the cutoff? only applies if a cutoff is specified
  seedterm_value = seedterm_value, # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only
  normalize_scores = normalize_scores, # should the score in the documents be normalized between 0 and 1? Can be doc (normalize within each document), group (normalize for each group), or NULL to skip
  cut_lower_quantile_fields = cut_lower_quantile_fields, # Should policy classifications within a document be cut if they fall below a certain quantile? NULL to skip. Else numerical value to specify the quantile
  cut_frequent_group_terms = cut_frequent_policy_terms,
  return_walk_terms = return_walk_terms, # should the processed walk terms be returned for further analysis and transparency?
  return_unclassified_docs = return_unclassified_docs, # should the IDs of the unlassified docs be returned? 
  # setting return_walk_terms or return_unclassified_docs to TURE returns a list of dataframes rather than a single dataframe
  verbose = verbose)


if (!is.null(normalize_scores)){ # if the scores were normalized, results will contain only the normalized score in export and print
  score <- "score_norm"
} else {
  score <- "score"
}

# return walk terms (quality control)
if (!is.null(print_walk_terms)) {
  top_group_terms(classification_result,
                  group_name = "policy_field",
                  classification_measure = classification_measure,
                  print_seed_terms = TRUE,
                  n = print_walk_terms,
                  mode = "print")
}

# Check highest ranking policy fields for tweets (quality control)
if (!is.null(print_example_docs)) {
  
  top_group_documents(classification_result,
                            documents = tweets %>% 
                              filter(!str_detect(`_source.text`, "^RT ")) %>% # drops RTs
                              select(`_id`, `_source.text`, `_source.created_at`), # and only keep a few variables
                            doc_id = join_by(doc_id == `_id`),
                            group_name = "policy_field",
                            classification_score = score,
                            n = print_example_docs,
                            mode = "print"
                            )
}

# Check unclassified tweets
if (!is.null(print_unclassified_docs)) {
  
  get_unclassified_documents(classification_result,
                             documents = tweets %>% 
                               select(`_id`, `_source.text`, `_source.created_at`), # and only keep a few variables)
                             doc_id = join_by(doc_id == `_id`),
                             n = print_unclassified_docs,
                             mode = "print")
}


#### save ####
if (save_results) {
  classification_result %>% # export full object as RDS (for troubleshooting etc)
    saveRDS(file = file.path(dir, paste0("classification_terms_", date, ".RDS")))
  
  # export
  if (!is.data.frame(classification_result)){ # only export classified documents if additional results (policy terms/unclassified documents) were calculated
    classified_documents <- classification_result[["classified_documents"]] 
  } else {
    classified_documents <- classification_result
    }
  
  classified_documents %>% 
    select(doc_id, policy_field, {{score}}) %>%  # only pass one score
    rename(tweet_id = doc_id) %>% # rename for clarity
    pivot_wider(names_from = policy_field, values_from = score_norm) %>% # wide format (1 column per policy field)
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
    vroom_write("init_classification/init_classified_tweets_wide.csv.tar.gz")
}

plan(sequential) # end multisession
