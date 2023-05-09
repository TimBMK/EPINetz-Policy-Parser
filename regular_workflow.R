# Regular Workflow for Classification #

{
  library(tidyverse)
  library(elastic)
  library(quanteda)
  library(quanteda.textstats)
  library(spacyr)
  library(data.table)
  library(RandomWalkRestartMH)
  library(scales)
  library(vroom)
  library(igraph)
  library(furrr)
}

source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R") # scrolled search function

source("get_seed_terms.R") # seed term extraction function

source("/data/koenigt/Tools-Scripts/Tools & Scripts/network_snapshots.R") # network function

source("utils_nested_data.R") # nested data utils

source("utils_text_processing.R") # text processing utils

source("utils_networks.R") # networks utils

plan(multisession, workers = 4) # set up future multisession for future_map functions

chi2_ministries <- 500 # set chi^2 threshold for ministries
chi2_committee_members <- 250 # set chi^2 threshold for within-committee members
chi2_committees <- 30 # set chi^2 threshold between committees

time_frame_seeds <- years(1) # length of the time frame for seed term extraction
time_frame_walks <- months(3) # length of the time frame for random walk term extraction

walk_score <- 0.9 # Minimum normalized Random Walk Score for Random Walk Terms

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

# conn$ping()


#### Extract Seed Terms ####

# read data

ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))


# API call I (seed data)

date_range <- tibble(until = Sys.Date(), from = until - time_frame_seeds)

account_list <- ministries %>% bind_rows(committees) %>%
  filter(!is.na(user_id)) %>%
  group_by(begin, end) %>%
  mutate(
    relevant = case_when( # dummy variable for relevance...
      int_overlaps( # ... if the intervals overlap ...
        interval(begin, end),
        interval(date_range$from, date_range$until)
      )
      ~ TRUE,
      is.na(begin) & is.na(end) ~ TRUE, # ... both are NA (i.e. ministries) ...
      is.na(end) & # ... or the period is still running (i.e. NA) ...
        begin <= date_range$from ~ TRUE, # ... AND beginning of the period is before the interval
      .default = FALSE
    )
  ) %>% 
  filter(relevant == TRUE) %>% # only relevant accounts (i.e. in the observation period)
  distinct(user_id, begin, end, period)


seed_tweets <- tibble() # data container

for (i in seq(1, length(account_list %>% distinct(user_id) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 accounts per chunk)
  
  dat <- na.omit((account_list %>% distinct(user_id) %>% pull())[i:(i+(100-1))]) # account chunks of 100 each 
  
  # make query:
  q <- paste0("created_at:[\"", date_range$from, "T00:00:00Z\" TO \"", date_range$until, "T00:00:00Z\"] AND author_id:(", # timeframe
              paste(dat, collapse = " OR "), ")") # Accounts in the Seed Account List only
  
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


## save tokens

vroom_write(seed_tokens, file = paste0("regular_classification/seed_tokens_", Sys.Date(), ".csv.tar.gz"))


## NEs only
seed_NE <- seed_tokens %>% as_tibble() %>% 
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(token) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(token != "amp", token != "&amp")



# Data Structuring & Cleaning

ministry_tweets <- ministries %>% left_join(seed_tweets, 
                                            by = join_by(user_id == `_source.author_id`), 
                                            relationship = "many-to-many")

ministry_NE <- ministry_tweets %>% 
  left_join(seed_NE, by = join_by(`_id` == doc_id), 
            relationship = "many-to-many") %>% 
  select(user_id, official_name, policy_field, `_id`, lemma) %>% 
  filter(!str_detect(lemma, "@")) # drop all lemmas containing "@" - that is, all mentions


committee_tweets <- committees %>% left_join(seed_tweets, 
                                             by = join_by(user_id == `_source.author_id`), 
                                             relationship = "many-to-many") # tweets for accounts in multiple fields are duplicated

committee_NE <- committee_tweets %>% 
  left_join(seed_NE, by = join_by(`_id` == doc_id), 
            relationship = "many-to-many") %>% 
  select(user_id, official_name, policy_field, committee, `_id`, lemma) %>% 
  filter(!str_detect(lemma, "@")) # drop all lemmas containing "@" - that is, all mentions



# Calculate Chi^2 for Policy Fields


## Between Ministries

seed_terms_ministries <- get_seed_terms(data = ministry_NE,
                                        doc_id = "_id",
                                        tokens = "lemma",
                                        grouping_var = "official_name",
                                        policy_field = "policy_field",
                                        threshold = chi2_ministries,
                                        show_plots = F,
                                        save_plots = F)



## Between Committees

seed_terms_committees <- get_seed_terms(data = committee_NE,
                                        doc_id = "_id",
                                        tokens = "lemma",
                                        grouping_var = "committee",
                                        policy_field = "policy_field",
                                        threshold = chi2_committees,
                                        show_plots = F,
                                        save_plots = F)



## Between Members of each committee

seed_terms_committee_members <- committee_NE %>% # split datasets into committees and calculate keyness within committees via map()
  split(.$committee) %>% 
  map(\(data) get_seed_terms(data = data,
                           doc_id = "_id",
                           tokens = "lemma",
                           grouping_var = "official_name",
                           policy_field = "policy_field",
                           threshold = chi2_committee_members,
                           show_plots = F,
                           save_plots = F)) %>% 
  rbindlist()
  

## save results

seed_terms_ministries %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/seed_terms_ministries_", Sys.Date() ,".csv.tar.gz"), delim = ",")

seed_terms_committees %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/seed_terms_committees_", Sys.Date() ,".csv.tar.gz"), delim = ",")

seed_terms_committee_members %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/seed_terms_committee_members_", Sys.Date() ,".csv.tar.gz"), delim = ",")




#### Additional Terms through Random Walks ####


# API call II (random walk data)

date_range <- tibble(until = Sys.Date(), from = until - time_frame_walks)

epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_10.RDs") # list of all EPINetz Accounts

account_list <- epinetz_accounts %>% # currently, this considers tweets of all accounts in the observation period, regardless of incumbency. This may change in the future
  filter(!is.na(user_id)) # drop entries without user IDs


walk_tweets <- tibble() # data container

for (i in seq(1, length(account_list %>% distinct(user_id) %>% pull()), 100)) { # the search query needs to be chopped up into smaller bits (100 accounts per chunk)
  
  dat <- na.omit((account_list %>% distinct(user_id) %>% pull())[i:(i+(100-1))]) # account chunks of 100 each 
  
  # make query:
  q <- paste0("created_at:[\"", date_range$from, "T00:00:00Z\" TO \"", date_range$until, "T00:00:00Z\"] AND author_id:(", # timeframe
              paste(dat, collapse = " OR "), ")") # Accounts in the EPINetz List only
  
  tweets <- full_scroll(conn, q = q, index = "twitter_v2_tweets")
  
  walk_tweets <- bind_rows(walk_tweets, tweets)
  
}


# Tokenization, Lemmatization, Noun-Word Filtering

walk_corpus <- corpus(walk_tweets, docid_field = "_id", text_field = "_source.text", 
                      meta = list(names(walk_tweets)), # preserve all vars as metadata
                      unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

walk_tokens <-
  spacy_parse(
    walk_corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

spacy_finalize() # end spacy

## save tokens

vroom_write(walk_tokens, file = paste0("regular_classification/walk_tokens_", Sys.Date(), ".csv.tar.gz"))


## NEs only
walk_NE <- walk_tokens %>% as_tibble() %>% 
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(lemma) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(lemma != "amp", lemma != "&amp", lemma != "RT", lemma != "rt") %>% 
  filter(!(tolower(lemma) %in% stopwords(language = "en")) & # drop stopwords
           !(tolower(lemma) %in% stopwords(language = "de"))) %>% 
  filter(!str_detect(lemma, "@")) %>% # drop all lemmas containing "@" - that is, all mentions
  filter(!str_detect(lemma, "http")) # drop all URLs


## drop 10% percentile of counts
walk_NE <- drop_quantile(walk_NE,
                         tokens = "lemma",
                         quantile = 0.1,
                         ignore_case = TRUE, # case is ignored, but not modified
                         group = "tag",
                         verbose = T)

# walk_NE <- walk_NE %>% mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # convert to lower,
#                                                 .default = lemma)) # but preserve URLs as is (to not break twitter links)

walk_NE <- walk_NE %>% mutate(lemma = tolower(lemma)) # set case to lower to ignore casing in networks

# Prepare Network for Random Walks

walk_network <- snapshots(walk_NE %>% mutate(time = "full"), # add dummy variable to calculate only 1 snapshot
                          vertex_a = "doc_id",
                          vertex_b = "lemma", 
                          time = "time", 
                          output = "networks",
                          directed = F, 
                          pmi_weight = T) %>% 
  select(!time) %>%  # remove time dummy
  # filter(weight > 0 ) %>% # remove edges with negative weight - not necessary, as negative weights are accepted by the random walk algorithm
  graph_from_data_frame(directed = F) # make igraph object 


multiplex <- create.multiplex(list(walk_network = walk_network)) # make multiplex object for random walks

# {
#   AdjMatrix <- compute.adjacency.matrix(multiplex) # make adjacency matrix
#   gc() # this is important - the computation of the adjacency matrix is very costly and doesn't flush automatically!
# }

AdjMatrix <- compute.adjacency.matrix.mono(multiplex) # an efficient adjacency matrix implementation for monoplex networks

AdjMatrixNorm <- normalize.multiplex.adjacency(AdjMatrix) # normalize the adjacency matrix



# Compute Random Walks

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees, 
                        seed_terms_committee_members),
                   fill = TRUE) %>% 
  filter(feature %in% walk_NE$lemma) %>% # drop seed terms not in the walk network
  split(.$policy_field)                        # ... and split by policy field

rwr_results <- seeds %>% future_map(\(seed_group)
                                    seed_group$feature %>%
                                      map(\(seed)
                                          tryCatch( # capture non-standard errors thrown by Random.Walk.Restart.Multiplex
                                            Random.Walk.Restart.Multiplex(
                                              x = AdjMatrixNorm,
                                              MultiplexObject = multiplex,
                                              Seeds = seed
                                            ), 
                                            error  = function(e) NULL) # return NULL for errors...
                                          ) %>% compact(), # ... and remove NULLs
                                    .progress = T) 

flattened_results <- rwr_results %>%
  list_flatten(name_spec = "{outer}") %>%
  future_imap(\(x, idx)
    tibble(x[[1]], seed_node = x[[2]], policy_field = idx) %>% 
      mutate(ScoreNorm = rescale(Score, to = c(0, 1))), # rescale scores
  .progress = T) %>% 
  rbindlist() %>% 
  filter(ScoreNorm >= walk_score)

vroom_write(flattened_results, file = paste0("regular_classification/walk_terms_", Sys.Date(), ".csv.tar.gz"))

plan(sequential) # end multisession
