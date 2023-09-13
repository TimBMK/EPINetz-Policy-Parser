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

source("classify_documents.R")

tweet_ids <- "..." # tweet IDs of tweets to be classified

plan(multisession, workers = 4) # set up future multisession for future_map functions


## settings

classification_timeframe = "weeks" # weeks(1) bugs out (lubridate bug) # length of the timeframe for the classification, e.g. one week before the RWR timeframe
classification_before_after = "before" # set if the classification_timeframe is before or after the rwr_timeframe, e.g. the week before or after. "after" to for after, "before" for before

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

return_unclassified_docs = TRUE # should the IDs of the unlassified docs be returned? 




# Read lists
ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))
committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))
epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_11.RDS") # list of all EPINetz Accounts

# Get Terms
latest_results <- tibble(folder = list.dirs("regular_classification")) %>% 
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


# connect to Heidelberg Database via Tunnel. In Bash, use:
# ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@adrastea.ifi.uni-heidelberg.de

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

for (i in seq(1, length(tweet_ids %>% distinct(`_id`) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 IDs per chunk)
  
  # cat(paste("Rows", i, "to", i+(100-1), "\n"))
  
  dat <- na.omit((tweet_ids %>% distinct(`_id`) %>% pull())[i:(i+(100-1))]) # ID chunks of 100 each 
  
  # make query:
  q <- paste0("_id:(", paste(dat, collapse = " OR "), ")")
  
  res <- Search(conn, q =  q, index = "twitter_v2_tweets", source = NULL, asdf = T, size = 10000)# get the full Tweets 
  
  tweets <- bind_rows(tweets, res$hits$hits)
  
}

# add reply indicator
tweets <- tweets %>% mutate(is_reply = case_when(!is.na(`_source.in_reply_to_user_id`) ~ TRUE,
                                                         .default = FALSE))


# filter for reply condition
tweets <- tweets %>% filter(is_reply == seed_replies | is_reply == FALSE) 



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



#### Classify Documents ####
classification_NE <- classification_tokens %>% 
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = NULL, # filtering for reply condition is done before tokenization to save on computing and can thus be skipped with NULL
                keep_mentions = classification_mentions, # should @-mentions be kept?
                keep_urls = classification_urls) # should URLs be kept?

classification_terms <- classification_NE %>% 
  semi_join(walk_terms_means, # filter for lemmas in the walk terms 
            join_by(lemma == NodeNames)) %>%
  left_join(walk_terms_means %>% distinct(NodeNames, mean_score_norm, policy_field),
            join_by(lemma == NodeNames), # add classification attributes
            relationship = "many-to-many") %>% # multi-matches for a) terms in multiple docs, b) terms in multiple policy fields
  filter(lemma != "--", lemma != "---")

classified_documents <- classification_terms %>% 
  summarize(policy_score = sum(mean_score_norm), .by = c(doc_id, policy_field)) %>% # sum policy scores by field and document
  complete(doc_id, policy_field, fill = list(policy_score = 0)) %>% # fill missing values
  mutate(policy_score_norm = policy_score/sum(policy_score), .by = doc_id) # and normalize between 0 and 1
  

# Check highest ranking policy fields for tweets (quality control)
top_field_tweets <- tweets %>% 
  left_join(classified_documents %>% 
              slice_max(policy_score_norm, by = doc_id, with_ties = F), 
            join_by(`_id` == doc_id)) %>% 
  select(`_id`, `_source.text`, policy_field, policy_score_norm, policy_score)

# Check unclassified tweets
unclassified_documents <- tweets %>% anti_join(classified_documents, by = join_by(`_id` == doc_id))



plan(sequential) # end multisession
