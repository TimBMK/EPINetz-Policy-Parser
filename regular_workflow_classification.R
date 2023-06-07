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

tweet_ids <- "..." # tweet IDs of tweets to be classified

plan(multisession, workers = 4) # set up future multisession for future_map functions

classification_replies <- FALSE # should replies get classified?

# Read lists
ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))
committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))
epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_11.RDS") # list of all EPINetz Accounts

# Get Terms
files <- list.files("regular_classification/")
latest_terms <- tibble(file = files) %>% 
  filter(str_detect(files, "walk_terms_\\d+.*")) %>% # filter for walk_terms files with a date
  slice_max(file) %>%  # pick the latest walk_terms file available
  pull()

walk_terms_means <- vroom(file.path("regular_classification/", latest_terms))





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
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(lemma) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(lemma != "amp", lemma != "&amp", lemma != "RT", lemma != "rt", lemma != "--", lemma != "---") %>% 
  filter(!(tolower(lemma) %in% stopwords(language = "en")) & # drop stopwords
           !(tolower(lemma) %in% stopwords(language = "de")) &
           !(tolower(lemma) %in% stopwords(language = "fr"))) %>% 
  mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # lower case - except for URLs (so they don't break)
                           .default = lemma))

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
