### Initial Tokenization - Data ###
###################################

### run this once to get a base stock of tweet to tokenize


{
  library(tidyverse)
  library(elastic)
  library(data.table)
  library(vroom)
}

source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R") # scrolled search function

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


# Read database
epinetz_list <- readRDS("EPINetz_full_collection_list_update_11.RDS")

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c")) # only necessary for min date



#### API call ####

get_replies <- TRUE

date_range <- tibble(until = ymd("2023-06-12"), from = min(committees$begin)) # from beginning of first WP until 10th of April 2023

account_list <- epinetz_list %>% 
  filter(!is.na(user_id)) %>% 
  distinct(user_id)

init_tweets <- tibble() # data container

for (i in seq(1, length(account_list %>% distinct(user_id) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 accounts per chunk)
  
  cat(paste("Rows", i, "to", i+(100-1), "\n"))
  
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
  
  init_tweets <- bind_rows(init_tweets, tweets)
  
}

# add reply indicator
init_tweets <- init_tweets %>% mutate(is_reply = case_when(!is.na(`_source.in_reply_to_user_id`) ~ TRUE,
                                                           .default = FALSE))

# Save
init_tweets %>% rowwise() %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x)), # text as utf8
         across(.cols = where(is.list), ~ str_c(unlist(.x), collapse = ", "))) %>%  # unlist lists (otherwise lost in export!)
  vroom_write(paste0("Tokenizer/data_init_tweets_", date_range$until, ".csv.tar.gz"))



