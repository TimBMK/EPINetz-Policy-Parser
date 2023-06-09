# Regular Workflow for Classification #

## Make Lists of Terms via Seeds and Random Walks ##
### Uses pre-tokenized data
{
  library(tidyverse)
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

source("get_seed_terms.R") # seed term extraction function

source("utils_nested_data.R") # nested data utils

source("utils_text_processing.R") # text processing utils

source("get_rwr_terms.R") # random walk functions

plan(multisession, workers = 16) # set up future multisession for future_map functions

chi2_ministries <- 500 # set chi^2 threshold for ministries
chi2_committee_members <- 250 # set chi^2 threshold for within-committee members
chi2_committees <- 30 # set chi^2 threshold between committees

# date <- Sys.Date()
date <- ymd("2023-04-25") # testing

seed_replies <- TRUE
walk_replies <- FALSE

save_seeds <- FALSE # should the Seed Terms be saved explicitly?

time_frame_seeds <- years(1) # length of the time frame for seed term extraction
time_frame_walks <- weeks(12) # length of the time frame for random walk term extraction

walk_score <- 0.9 # Minimum normalized Random Walk Score for Random Walk Terms
keep_seed_terms <- TRUE # always keep seed terms, ignore their walk score

# Read lists
ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))
committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))
epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_11.RDS") # list of all EPINetz Accounts

# Get Tokenized Data
tokens <- vroom("Tokenizer/tokens.csv.tar.gz", 
                col_types = list(doc_id = "c",
                                 `_source.author_id` = "c")) 

#### Extract Seed Terms ####

## Filter Accounts; time filtering; NEs only; data cleaning
seed_NE <- tokens %>% as_tibble() %>% 
  filter(`_source.created_at` >= (date - time_frame_seeds) & # time frame filtering
           `_source.created_at` <= date) %>%  
  filter(`_source.author_id`  %in% ministries$user_id | # seed account tweets only
           `_source.author_id`  %in% committees$user_id ) %>% 
  filter(is_reply == seed_replies | is_reply == FALSE) %>% # filter for reply condition (TRUE includes replies, FALSE does not)
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(token) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(!str_detect(lemma, "@")) %>%  # drop all lemmas containing "@" - that is, all mentions
  # filter(!str_detect(lemma, "http")) %>%  # drop all URLs
  filter(!(tolower(lemma) %in% stopwords(language = "en")) & # drop stopwords
           !(tolower(lemma) %in% stopwords(language = "de")) &
           !(tolower(lemma) %in% stopwords(language = "fr"))) %>% 
  filter(lemma != "amp", lemma != "&amp", lemma != "RT", lemma != "rt", 
         lemma != "--", lemma != "---") %>% # drop additional stopwords
  mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # lower case - except for URLs (so they don't break)
                           .default = lemma))

  


# Data Structuring

ministry_NE <- ministries %>% 
  inner_join(seed_NE, by = join_by(user_id == `_source.author_id`), # inner_join ensures we only keep accounts for which we have data
            relationship = "many-to-many") %>% 
  select(user_id, official_name, policy_field, doc_id, lemma) 


committee_NE <- committees %>% 
  inner_join(seed_NE, by = join_by(user_id == `_source.author_id`), # inner_join ensures we only keep accounts for which we have data
            relationship = "many-to-many") %>% 
  select(user_id, official_name, policy_field, doc_id, lemma, committee) 



# Calculate Chi^2 for Policy Fields


## Between Ministries

seed_terms_ministries <- get_seed_terms(data = ministry_NE,
                                        doc_id = "doc_id",
                                        tokens = "lemma",
                                        grouping_var = "official_name",
                                        policy_field = "policy_field",
                                        threshold = chi2_ministries,
                                        show_plots = F,
                                        save_plots = F)



## Between Committees

seed_terms_committees <- get_seed_terms(data = committee_NE,
                                        doc_id = "doc_id",
                                        tokens = "lemma",
                                        grouping_var = "committee",
                                        policy_field = "policy_field",
                                        threshold = chi2_committees,
                                        show_plots = F,
                                        save_plots = F)



## Between Members of each committee

seed_terms_committee_members <- committee_NE %>% # split datasets into committees and calculate keyness within committees via map()
  split(.$committee) %>% 
  future_imap(\(data, id) get_seed_terms(data = data,
                                         doc_id = "doc_id",
                                         tokens = "lemma",
                                         grouping_var = "official_name",
                                         policy_field = "policy_field",
                                         threshold = chi2_committee_members,
                                         show_plots = F,
                                         save_plots = F) %>% 
  mutate(committee = id)) %>% 
  rbindlist()
  

## save results

if (save_seeds == TRUE) {
  seed_terms_ministries %>%
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
    vroom_write(file = paste0("regular_classification/no_replies/seed_terms_ministries_", date ,".csv.tar.gz"), delim = ",")
  
  seed_terms_committees %>%
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
    vroom_write(file = paste0("regular_classification/no_replies/seed_terms_committees_", date ,".csv.tar.gz"), delim = ",")
  
  seed_terms_committee_members %>%
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
    vroom_write(file = paste0("regular_classification/no_replies/seed_terms_committee_members_", date ,".csv.tar.gz"), delim = ",")
}




#### Additional Terms through Random Walks ####

walk_NE <- tokens %>% as_tibble() %>% 
  filter(`_source.created_at` >= (date - time_frame_walks) & # time frame filtering
           `_source.created_at` <= date) %>%  
  filter(is_reply == walk_replies | is_reply == FALSE) %>% # filter for reply condition (TRUE includes replies, FALSE does not)
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(token) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  # filter(!str_detect(lemma, "@")) %>%  # drop all lemmas containing "@" - that is, all mentions
  filter(!(tolower(lemma) %in% stopwords(language = "en")) & # drop stopwords
           !(tolower(lemma) %in% stopwords(language = "de")) &
           !(tolower(lemma) %in% stopwords(language = "fr"))) %>% 
  filter(lemma != "amp", lemma != "&amp", lemma != "RT", lemma != "rt", 
         lemma != "--", lemma != "---") %>% # drop additional stopwords
  mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # lower case - except for URLs (so they don't break)
                           .default = lemma))



## drop 10% percentile of counts
walk_NE <- drop_quantile(walk_NE,
                         tokens = "lemma",
                         quantile = 0.1,
                         ignore_case = FALSE, # case is already lowered
                         group = "tag",
                         verbose = T)


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
  


# Prepare Seeds

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, # drop seed terms prevalent for single committee members
            by = join_by(feature, committee)) %>% 
  filter(feature %in% walk_NE$lemma) %>% # drop seed terms not in the walk network
  distinct(feature, policy_field) %>% # drop features duplicated within policy fields (from committees etc)
  split(.$policy_field)                        # ... and split by policy field


# Housekeeping

rm(tokens)
rm(seed_NE)
rm(ministry_NE)
rm(committee_NE)
rm(walk_NE)
gc()


# Compute Random Walks

walk_terms <- get_rwr_terms(walk_network,
                            network_name = NULL,
                            seeds = seeds, 
                            seed_var = "feature",
                            match_var = NULL,
                            flatten_results = TRUE,
                            group_name = "policy_field",
                            normalize_score = "seeds",
                            positive_scores_only = TRUE,
                            walk_score = walk_score,
                            keep_seed_terms = keep_seed_terms,
                            progress = FALSE) 

    
## The terms returned include duplicates, esp. seed terms with keep_seed_terms = TRUE


## calculate means, re-normalize
walk_terms_means <- walk_terms %>% 
  # complete(NodeNames, policy_field, seed_term, fill = list(ScoreNorm = 0)) %>% # add 0 indicator for terms not found by random walkers
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
  vroom_write(file = paste0("regular_classification/walk_terms_", 
                            date, ".csv.tar.gz"), append = F)
  

