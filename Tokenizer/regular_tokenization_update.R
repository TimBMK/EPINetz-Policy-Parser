### Update Tokens ###
#####################

### run this regularly to keep the tokens database up-to-date

{
  library(tidyverse)
  library(elastic)
  library(quanteda)
  library(spacyr)
  library(data.table)
  library(vroom)
}

source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R") # scrolled search function

date <- Sys.Date() # current date
date_range <- date - years(1) # This is used for making an additional, smaller data set of only 1 year (required for the policy parser)

verbose = TRUE

save_tweets = FALSE

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

if(verbose) conn$ping() # check connection


if(verbose) cat("Reading Data...")

# Read database

epinetz_list <- readRDS("EPINetz_full_collection_list_update_11.RDS")

# committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c")) # only necessary for min date


# load existing tokens

init_tokens <- get_latest_tokens_file(path = "Tokenizer") %>% 
  vroom(col_types = list(doc_id = "c", `_source.author_id` = "c")) 


#### API call ####

if(verbose) cat("Making API Call...")

get_replies <- TRUE

start_date <- Sys.Date() - years(1) # minimum Date: One year back (max Range for Policy Parser (Seed Terms))
# start_date <- min(committees$begin) # minimum Date: beginning of the first WP of interest

account_list <- epinetz_list %>% 
  filter(!is.na(user_id)) %>% 
  distinct(user_id)


# Get all Tweet IDs for one year (more efficiant than getting full tweets)

all_ids <- tibble() # data container

for (i in seq(1, length(account_list %>% distinct(user_id) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 accounts per chunk)
  
  cat(paste("Rows", i, "to", i+(100-1), "\n"))
  
  dat <- na.omit((account_list %>% distinct(user_id) %>% pull())[i:(i+(100-1))]) # account chunks of 100 each 
  
  # make query:
  if (get_replies == TRUE) {
    q <- paste0("created_at:[\"", start_date, "T00:00:00Z\" TO *] AND author_id:(", # timeframe with * as wildcard (no end date)
                paste(dat, collapse = " OR "), ")") # Accounts in the Seed Account List only
  }
  
  if (get_replies == FALSE) {
    q <- paste0("created_at:[\"", start_date, "T00:00:00Z\" TO *] AND author_id:(", # timeframe with * as wildcard (no end date)
                paste(dat, collapse = " OR "), ") NOT in_reply_to_user_id:*") # Accounts in the Seed Account List only
  }
  
  res <- full_scroll(conn, q = q, index = "twitter_v2_tweets", source = F) # get the Tweet IDs
  
  all_ids <- bind_rows(all_ids, res)
  
}


# Check which tweets are not yet tokenized
new_ids <- all_ids %>% anti_join(init_tokens %>% distinct(doc_id), 
                                      by = join_by(`_id` == doc_id))


# Get full tweets

if(verbose) cat("Pulling Full Tweets...")

new_tweets <- tibble() # data container

for (i in seq(1, length(new_ids %>% distinct(`_id`) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 IDs per chunk)
  
  # cat(paste("Rows", i, "to", i+(100-1), "\n"))
  
  dat <- na.omit((new_ids %>% distinct(`_id`) %>% pull())[i:(i+(100-1))]) # ID chunks of 100 each 
  
  # make query:
  q <- paste0("_id:(", paste(dat, collapse = " OR "), ")")
  
  res <- Search(conn, q =  q, index = "twitter_v2_tweets", source = NULL, asdf = T, size = 10000)# get the full Tweets 
  
  new_tweets <- bind_rows(new_tweets, res$hits$hits)
  
}

# add reply indicator
new_tweets <- new_tweets %>% mutate(is_reply = case_when(!is.na(`_source.in_reply_to_user_id`) ~ TRUE,
                                                         .default = FALSE))



#### Tokenize ####

if(verbose) cat("Preparing Data and Starting Spacy...")

corpus <- corpus(new_tweets, docid_field = "_id", text_field = "_source.text", 
                 meta = list(names(new_tweets)), # preserve all vars as metadata
                 unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

tokens <-
  spacy_parse(
    corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

if(verbose) cat("Tokenization complete. Finalizing spacy...")

spacy_finalize() # end spacy


# add reply indicator, creation date and author account ID
if(verbose) cat("Adding additional data...")
  
tokens <- tokens %>%
  left_join(new_tweets %>% 
              distinct(`_id`, is_reply, 
                       `_source.created_at`, 
                       `_source.author_id`), 
            by = join_by(doc_id == `_id`)) %>% 
  mutate(`_source.created_at` = as_datetime(`_source.created_at`))


if(verbose) cat("Saving...")
# add to existing tokens & save
updated_tokens <- rbindlist(list(init_tokens, tokens)) %>% # bind together
  distinct(doc_id, sentence_id, token_id, token, .keep_all = TRUE) # double-check duplicates
  
updated_tokens %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "Tokenizer/tokens_full.csv.tar.gz", delim = ",")

# smaller dataset of only 1 year for faster read-in
updated_tokens %>% 
  filter(`_source.created_at` >= date_range) %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "Tokenizer/tokens.csv.tar.gz", delim = ",")

if(save_tweets) {
  new_tweets %>% 
    rowwise() %>% 
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x)), # text as utf8
           across(.cols = where(is.list), ~ str_c(unlist(.x), collapse = ", "))) %>%  # unlist lists (otherwise lost in export!)
    vroom_write(paste0("Tokenizer/data_new_tweets_", date, ".csv.tar.gz"))
}