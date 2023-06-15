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

source("get_rwr_terms.R") # random walk functions

plan(multisession, workers = 4) # set up future multisession for future_map functions

chi2_ministries <- 500 # set chi^2 threshold for ministries
chi2_committee_members <- 250 # set chi^2 threshold for within-committee members
chi2_committees <- 30 # set chi^2 threshold between committees

# date <- Sys.Date()
date <- ymd("2023-04-25") # testing

get_replies <- FALSE

time_frame_seeds <- years(1) # length of the time frame for seed term extraction
time_frame_walks <- weeks(12) # length of the time frame for random walk term extraction

walk_score <- 0.9 # Minimum normalized Random Walk Score for Random Walk Terms
keep_seed_terms <- TRUE

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

conn <- elastic::connect(
  path = "",
  port = 9201,
  user = credentials$user,
  pwd = credentials$password,
  host = "localhost"
)

conn$ping()


#### Extract Seed Terms ####

# read data

ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))


# API call I (seed data)

date_range <- tibble(until = date, from = until - time_frame_seeds)

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

# seed_tweets %>%
#   mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
#   vroom_write(file = paste0("regular_classification/no_replies/seed_tweets_",
#                             date, ".csv.tar.gz"))

# Tokenization, Lemmatization, Noun-Word Filtering

seed_corpus <- corpus(seed_tweets, docid_field = "_id", text_field = "_source.text", 
                         meta = list(names(seed_tweets)), # preserve all vars as metadata
                         unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy
          ### if there is an error in normalizePath(), try temporarily setting the wd to the user directory with the miniconda installation

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
seed_tokens %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/no_replies/seed_tokens_", 
                            date, ".csv.tar.gz"))


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
  imap(\(data, id) get_seed_terms(data = data,
                           doc_id = "_id",
                           tokens = "lemma",
                           grouping_var = "official_name",
                           policy_field = "policy_field",
                           threshold = chi2_committee_members,
                           show_plots = F,
                           save_plots = F) %>% 
  mutate(committee = id)) %>% 
  rbindlist()
  

## save results

seed_terms_ministries %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/no_replies/seed_terms_ministries_", date ,".csv.tar.gz"), delim = ",")

seed_terms_committees %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/no_replies/seed_terms_committees_", date ,".csv.tar.gz"), delim = ",")

seed_terms_committee_members %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/no_replies/seed_terms_committee_members_", date ,".csv.tar.gz"), delim = ",")




#### Additional Terms through Random Walks ####


# API call II (random walk data)

date_range <- tibble(until = date, from = until - time_frame_walks)

epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_10.RDs") # list of all EPINetz Accounts

account_list <- epinetz_accounts %>% # currently, this considers tweets of all accounts in the observation period, regardless of incumbency. This may change in the future
  filter(!is.na(user_id)) # drop entries without user IDs


walk_tweets <- tibble() # data container

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
walk_tokens %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/no_replies/walk_tokens_", date, ".csv.tar.gz"))


## NEs only
walk_NE <- walk_tokens %>% as_tibble() %>% 
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(lemma) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(lemma != "amp", lemma != "&amp", lemma != "RT", lemma != "rt", lemma != "--", lemma != "---") %>% 
  filter(!(tolower(lemma) %in% stopwords(language = "en")) & # drop stopwords
           !(tolower(lemma) %in% stopwords(language = "de")) &
           !(tolower(lemma) %in% stopwords(language = "fr"))) %>% 
  # filter(!str_detect(lemma, "@")) %>% # drop all lemmas containing "@" - that is, all mentions | mentions may be relevant for context
  # filter(!str_detect(lemma, "http")) %>%  # drop all URLs
  # mutate(lemma = tolower(lemma)) %>%  # set case to lower to ignore casing in networks
  mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # lower case - except for URLs (so they don't break)
                           .default = lemma)) %>%
  left_join(walk_tweets %>% distinct(`_id`, `_source.created_at`), 
            join_by(doc_id == `_id`)) %>%  # add time
  mutate(week = ceiling_date(as_datetime(`_source.created_at`), # make week indicator (last day of the week)
                             unit = "week"))


## drop 10% percentile of counts
walk_NE <- drop_quantile(walk_NE,
                         tokens = "lemma",
                         quantile = 0.1,
                         ignore_case = FALSE, # case is already lowered
                         group = "tag",
                         verbose = T)

# walk_NE <- walk_NE %>% mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # convert to lower,
#                                                 .default = lemma)) # but preserve URLs as is (to not break twitter links)


# Prepare Network for Random Walks

walk_network <- make_multiplex_objects(walk_NE,
                                       vertex_a = "doc_id",
                                       vertex_b = "lemma",
                                       directed = F,
                                       pmi_weight = T,
                                       keep_igraph_network = F,
                                       keep_multiplex_network = T,
                                       keep_adjacency_matrix = F,
                                       keep_normalized_adjacency_matrix = T)
  
saveRDS(walk_network, file = paste0("regular_classification/no_replies/walk_network_", date, ".RDS"))


# Compute Random Walks

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, # drop seed terms prevalent for single committee members
            by = join_by(feature, committee)) %>% 
  filter(feature %in% walk_NE$lemma) %>% # drop seed terms not in the walk network
  distinct(feature, policy_field) %>% # drop features duplicated within policy fields (from committees etc)
  split(.$policy_field)                        # ... and split by policy field

# rwr_results <- seeds %>% future_map(\(seed_group)
#                                     seed_group$feature %>%
#                                       map(\(seed)
#                                           tryCatch( # capture non-standard errors thrown by Random.Walk.Restart.Multiplex
#                                             Random.Walk.Restart.Multiplex(
#                                               x = AdjMatrixNorm,
#                                               MultiplexObject = multiplex,
#                                               Seeds = seed
#                                             ), 
#                                             error  = function(e) NULL) # return NULL for errors...
#                                           ) %>% compact(), # ... and remove NULLs
#                                     .progress = T) 
# 
# flattened_results <- rwr_results %>%
#   list_flatten(name_spec = "{outer}") %>%
#   future_imap(\(x, idx)
#     tibble(x[[1]], seed_node = x[[2]], policy_field = idx) %>% 
#       mutate(ScoreNorm = rescale(Score, to = c(0, 1))), # rescale scores
#   .progress = T) %>% 
#   rbindlist() %>% 
#   filter(ScoreNorm >= walk_score)

walk_terms <- get_rwr_terms(walk_network,
                            network_name = NULL,
                            seeds = seeds, 
                            seed_var = "feature",
                            match_var = NULL,
                            flatten_results = TRUE,
                            walk_score = walk_score,
                            keep_seed_terms = keep_seed_terms,
                            progress = FALSE) 

    ## The terms returned include duplicates, esp. seed terms with keep_seed_terms = TRUE


## calculate means, re-normalize
walk_terms_means <- walk_terms %>% 
  summarise(mean_score = mean(ScoreNorm), 
            .by = c(NodeNames, policy_field, seed_term)) %>% 
  group_by(policy_field, seed_term) %>% # rescale scores within policy fields (keep seed_term indicator for setting their score to 1)
  mutate(mean_score_norm = case_when(seed_term == TRUE ~ 1,  # score of 1 for seed terms
                                     seed_term == FALSE ~ rescale(mean_score))) %>% # rescaling for all other terms
  # tidytext::bind_tf_idf(NodeNames, policy_field, mean_score_norm) %>% # calculate IDF over scores of terms in multiple policy fields
  # filter(idf > quantile(.$idf, 0.1)[[1]]) %>% # drop 10% quantile
  ungroup()


## save
walk_terms_means %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
  vroom_write(file = paste0("regular_classification/no_replies/walk_terms_", 
                            date, ".csv.tar.gz"), append = F)
  


#### Classify Documents ####
### this is a testing workflow that requires modification for the live API 
###  live API calls would load the term data to classify documents as needed

# API call III (live data)

date_range <- tibble(until = date + days(7), from = date + days(1))

epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_11.RDS") # list of all EPINetz Accounts

account_list <- epinetz_accounts %>% # currently, this considers tweets of all accounts in the observation period, regardless of incumbency. This may change in the future
  filter(!is.na(user_id)) # drop entries without user IDs


classification_tweets <- tibble() # data container

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
  
  classification_tweets <- bind_rows(classification_tweets, tweets)
  
}

classification_tweets %>% 
  rowwise() %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x)), # text as utf8
         across(.cols = where(is.list), ~ str_c(unlist(.x), collapse = ", "))) %>% # unlist nested lists
  vroom_write(paste0("regular_classification/no_replies/classification_tweets_", date, ".csv.tar.gz"))


# Tokenization, Lemmatization, Noun-Word Filtering

classification_corpus <- corpus(classification_tweets, docid_field = "_id", text_field = "_source.text", 
                      meta = list(names(classification_tweets)), # preserve all vars as metadata
                      unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

classification_tokens <-
  spacy_parse(
    classification_corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

spacy_finalize() # end spacy

## save tokens
classification_tokens %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("regular_classification/no_replies/classification_tokens_", date, ".csv.tar.gz"))



# Classify Documents
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
top_field_tweets <- classification_tweets %>% 
  left_join(classified_documents %>% 
              slice_max(policy_score_norm, by = doc_id, with_ties = F), 
            join_by(`_id` == doc_id)) %>% 
  select(`_id`, `_source.text`, policy_field, policy_score_norm, policy_score)

# Check unclassified tweets
unclassified_documents <- classification_tweets %>% anti_join(classified_documents, by = join_by(`_id` == doc_id))



# Test: Classify Documents via Random Walks in heterogeneous networks
classification_network <- make_multiplex_objects(classification_terms,
                                       vertex_a = "doc_id",
                                       vertex_b = "lemma",
                                       directed = F,
                                       pmi_weight = T,
                                       keep_igraph_network = F,
                                       keep_multiplex_network = T,
                                       keep_adjacency_matrix = F,
                                       keep_normalized_adjacency_matrix = T)

classification_doc_network <- make_multiplex_objects(classification_terms,
                                                 vertex_a = "lemma",
                                                 vertex_b = "doc_id",
                                                 directed = F,
                                                 pmi_weight = T,
                                                 keep_igraph_network = F,
                                                 keep_multiplex_network = T,
                                                 keep_adjacency_matrix = F,
                                                 keep_normalized_adjacency_matrix = T)

het_net <- create.multiplexHet(classification_network$multiplex, 
                               classification_doc_network$multiplex, 
                               classification_terms %>% distinct(lemma, doc_id) %>% # distinct drops duplicates form policy fields
                                 filter(lemma %in% V(classification_network$multiplex$network)$name) %>% # make sure IDs and lemmas exist in the data
                                 filter(doc_id %in% V(classification_doc_network$multiplex$network)$name))

TranMatrix <- compute.transition.matrix(het_net)

classification_walks <- Random.Walk.Restart.MultiplexHet(TranMatrix, het_net, Multiplex1_Seeds = seeds$aeusseres$feature)

plan(sequential) # end multisession
