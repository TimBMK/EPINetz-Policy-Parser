# Get data from HD Server #


library(tidyverse)
library(elastic)
library(ssh)
library(quanteda)
library(spacyr)

source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R") # scrolled search function

# user = readline(prompt = "Enter Heidelberg Username: ") # enter credentials in console
# password = readline(prompt = "Enter Password: ") # user prompts for username and password, so the password is not visible in the script

# ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@adrastea.ifi.uni-heidelberg.de

# host = "@wega.ifi.uni-heidelberg.de" # wega or adrastea to jump
# 
# jump <- ssh_connect(host = paste0(user, host),
#             passwd = password)
# 
# tunnel <- ssh_tunnel(jump,  port = 9200, target = "erinome.ifi.uni-heidelberg.de:9200") # this command freezes the session?!

credentials <- readRDS("data/koenigt/elastic_credentials.RDS")

conn <- connect(
  path = "",
  port = 9201,
  user = credentials$suser,
  pwd = credentials$password,
  host = "localhost"
)

# conn$ping()


# read data

ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))


# API call

date_range <- tibble(until = Sys.Date(), from = Sys.Date() - years(1))

account_list <- ministries %>% bind_rows(committees) %>% filter(!is.na(user_id)) %>% distinct(user_id)


seed_tweets <- tibble() # data container

for (i in seq(1, length(account_list %>% distinct(user_id) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 accounts per chunk)
  
  dat <- na.omit((account_list %>% distinct(user_id) %>% pull())[i:(i+(100-1))]) # account chunks of 100 each 
  
  # make query:
  q <- paste0("created_at:[\"", date_range$from, "T00:00:00Z\" TO \"", date_range$until, "T00:00:00Z\"] AND author_id:(", # timeframe
              paste(dat, collapse = " OR "), ")") # Accounts in the EPINetz List only
  
  tweets <- full_scroll(conn, q = q, index = "twitter_v2_tweets")
  
  seed_tweets <- bind_rows(seed_tweets, tweets)
  
}


# Tokenization, Lemmatization, Noun-Word Filtering

seed_corpus <- corpus(seed_tweets, docid_field = "_id", text_field = "_source.text", 
                         meta = list(names(seed_tweets)), # preserve all vars as metadata
                         unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

seed_tokens <-
  spacy_parse(
    seed_corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

spacy_finalize() # end spacy

seed_NE <- seed_tokens %>% as_tibble() %>% 
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(token) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(token != "amp", token != "&amp")


# Data Structuring

ministry_tweets <- ministries %>% left_join(seed_tweets, 
                                            by = join_by(user_id == `_source.author_id`), 
                                            relationship = "many-to-many")

committee_tweets <- committees %>% left_join(seed_tweets, 
                                             by = join_by(user_id == `_source.author_id`), 
                                             relationship = "many-to-many") # tweets for accounts in multiple fields are duplicated


