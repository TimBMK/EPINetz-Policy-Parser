## Initial Classification ##
############################

# Get data from HD Server & tokenize #

{
  library(tidyverse)
  library(elastic)
  library(quanteda)
  library(spacyr)
  library(data.table)
  library(vroom)
}


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

credentials <- readRDS("/data/koenigt/elastic_credentials.RDS")

conn <- connect(
  path = "",
  port = 9201,
  user = credentials$user,
  pwd = credentials$password,
  host = "localhost"
)

conn$ping()


# read data

ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))


# API call

get_replies <- TRUE

date_range <- tibble(until = ymd("2023-04-10"), from = min(committees$begin)) # from beginning of first WP until 10th of April 2023

account_list <- ministries %>% bind_rows(committees) %>% 
  filter(!is.na(user_id)) %>% 
  distinct(user_id)

seed_tweets <- tibble() # data container

for (i in seq(1, length(account_list %>% distinct(user_id) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 accounts per chunk)
  
  dat <- na.omit((account_list %>% distinct(user_id) %>% pull())[i:(i+(100-1))]) # account chunks of 100 each 
  
  # make query:
  if (get_replies == TRUE) {
    q <- paste0("created_at:[\"", date_range$from, "T00:00:00Z\" TO \"", date_range$until, "T00:00:00Z\"] AND author_id:(", # timeframe
                paste(dat, collapse = " OR "), ")") # Accounts in the Seed Account List only
  }
  
  if (get_replies == FALSE) {
    q <- paste0("created_at:[\"", date_range$from, "T00:00:00Z\" TO \"", date_range$until, "T00:00:00Z\"] AND author_id:(", # timeframe
                paste(dat, collapse = " OR "), ") NOT in_reply_to_user_id:*") # Accounts in the Seed Account List only
  }
  
  tweets <- full_scroll(conn, q = q, index = "twitter_v2_tweets")
  
  seed_tweets <- bind_rows(seed_tweets, tweets)
  
}

vroom_write(seed_tweets %>% rowwise() %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x)), # text as utf8
                                                 across(.cols = where(is.list), ~ str_c(unlist(.x), collapse = ", "))), # unlist lists (otherwise lost in export!)
            delim = ",", file = "init_classification/data_init_seeds_2023-04-10.csv.tar.gz")




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

saveRDS(seed_tokens, "init_classification/tokens_init_seeds_2023-04-10.RDS")
seed_tokens %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% vroom_write(file = "init_classification/tokens_init_seeds_2023-04-10.csv.tar.gz", delim = ",")

