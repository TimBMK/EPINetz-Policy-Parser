## Initial Classification ##
############################

# Classify Additional Terms #

{
  library(tidyverse)
  library(quanteda)
  library(quanteda.textstats)
  library(data.table)
  library(furrr)
  library(vroom)
  library(igraph)
  library(RandomWalkRestartMH)
  library(scales)
}

options(future.globals.maxSize = (100000*1024^2)) # 10 gb Max Size for Parallelization Processes. 
                                                  #  This is necessary for making the dat_list in parallel and should be treated with care!

# source("/data/koenigt/Tools-Scripts/Tools & Scripts/network_snapshots.R") # single snapshot function included in utils_networks.R

source("get_rwr_terms.R")

source("utils_text_processing.R")

cat("\n ======= Preparations ======= \n")

# plan(multisession, workers = 4) # start multisession (for mapping processes) - Multisession for R Studio Sessions
plan(multicore, workers = 4) # start multisession (for mapping processes) - ! DO NOT USE MULTICORE IN RSTUDIO SESSION. ONLY WHEN CALLING THE SCRIPT DIRECTLY !
#                               # !!multicore plans can be unstable!!



walk_score = 0.5 # cutoff value for normalized random walk score

keep_seed_terms = TRUE # should seed terms within the policy field of the same period always be kept, regardless of walk score?

# read seed terms

seed_terms_ministries <- vroom("init_classification/no_replies/seed_terms_ministries.csv.tar.gz", 
                                    col_names = c("feature", "chi2", "p", # colnames need to be specified during read-in
                                                  "n_target", "n_reference", 
                                                  "ministry", "policy_field", 
                                                  "period"))

seed_terms_committees <- vroom("init_classification/no_replies/seed_terms_committees.csv.tar.gz") # this data has columns pre-specified

seed_terms_committee_members <- vroom("init_classification/seed_terms_committee_members.csv.tar.gz") # this data has columns pre-specified


# read data

walk_tweets <-
  vroom(
    file = "init_classification/no_replies/data_init_walk_2023-04-10.csv.tar.gz",
    # Important! specify coltypes to preserve correct IDs
    col_types = list(
      `_id` = "c",
      `_source.author_id` = "c",
      `_source.conversation_id` = "c",
      `_source.in_reply_to_user_id`= "c",
      `_source.attachments.poll_ids` = "c",
      `_source.withheld.scope` = "c",
      `_source.withheld.country_codes` = "c",
      `_source.entities.cashtags` = "c"
    )
  ) 


walk_tokens <- vroom("init_classification/no_replies/tokens_init_walk_2023-04-10.csv.tar.gz", col_types = list(doc_id = "c"))


## NEs only
walk_NE <- walk_tokens %>% as_tibble() %>% 
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(lemma) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(lemma != "amp", lemma != "&amp", lemma != "RT", lemma != "rt", lemma != "--", lemma != "---") %>% 
  filter(!(tolower(lemma) %in% stopwords(language = "en")) & # drop stopwords
           !(tolower(lemma) %in% stopwords(language = "de")) &
           !(tolower(lemma) %in% stopwords(language = "fr"))) %>% 
  filter(!str_detect(lemma, "@")) %>% # drop all lemmas containing "@" - that is, all mentions
  filter(!str_detect(lemma, "http")) %>%  # drop all URLs
  mutate(lemma = tolower(lemma)) %>%  # set case to lower to ignore casing in networks
  left_join(walk_tweets %>% distinct(`_id`, `_source.created_at`), 
            join_by(doc_id == `_id`)) %>%  # add time
  mutate(week = ceiling_date(as_datetime(`_source.created_at`), # make week indicator (last day of the week)
                             unit = "week"))
gc()

# List of Timeframes
  ## in contrast to the seed terms, we directly use the NE data for the list of 
  ##  frames, since we need all NEs (not specific to ministries etc.) anyways

cat("\n ======= Make List of Timeframes ======= \n")

dat_list <- future_map(walk_NE$week %>% unique(),
                       ~ {
                         walk_NE %>% 
                           filter(week >= (.x - weeks(12)) & # beginning: 12 weeks (3 months) before
                                    week <= .x)          # end: week of interest
                       })

names(dat_list) <- walk_NE$week %>% unique() # name the dataframes in the list

gc()

## drop 10% percentile of counts for each time frame

# dat_list <- dat_list %>% 
#   future_map(\(dat)
#              dat %>% drop_quantile(.,
#                            tokens = "lemma",
#                            quantile = 0.1,
#                            ignore_case = F, # already transformed to lower case
#                            group = "tag",
#                            verbose = F),
#              .progress = T)

cat("\n ======= Drop 10% Quantiles ======= \n")

for (i in 1:length(dat_list)){ # somehow this is faster and more stable than mapping the function
  
  # print(i)
  
  dat_list[[i]] <-  drop_quantile(dat_list[[i]],
                tokens = "lemma",
                quantile = 0.1,
                ignore_case = F, # already transformed to lower case
                group = "tag",
                verbose = F)
  
}

gc()

# walk_NE <- walk_NE %>% mutate(lemma = case_when(!str_detect(lemma, "http") ~ tolower(lemma), # convert to lower,
#                                                 .default = lemma)) # but preserve URLs as is (to not break twitter links)


# Housekeeping #
cat("\n ====== Remove unneeded Objects =======  \n")

rm(walk_tokens)
rm(walk_tweets)

gc()

# Prepare Network for Random Walks

cat("\n ======= Make Networks for Random Walks ======= \n")

plan(sequential) # no multisession - seems unstable for certain processes here, such as making the networks

# walk_networks <- dat_list %>% 
#   future_map(\(dat)
#              make_multiplex_objects(dat,
#                                     vertex_a = "doc_id",
#                                     vertex_b = "lemma",
#                                     directed = F,
#                                     pmi_weight = T,
#                                     keep_igraph_network = F,
#                                     keep_multiplex_network = T,
#                                     keep_adjacency_matrix = F,
#                                     keep_normalized_adjacency_matrix = T),
#              .progress = F)

dat_list %>% 
  future_iwalk(\(dat, name) # saving the results as .RDS to read in later is more stable than keeping them in the environment
             make_multiplex_objects(dat,
                                    vertex_a = "doc_id",
                                    vertex_b = "lemma",
                                    directed = F,
                                    pmi_weight = T,
                                    keep_igraph_network = F,
                                    keep_multiplex_network = T,
                                    keep_adjacency_matrix = F,
                                    keep_normalized_adjacency_matrix = T) %>% 
               saveRDS(file = paste0("init_classification/no_replies/walk_network_data/",
                                     name, ".RDS")),
             .progress = F)


gc()


# Make Seedlist

cat("\n ======= Make Seedlist ======= \n")

# plan(multicore, workers = 4) # restart multisession (for mapping processes) 

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, by = join_by(feature, period, committee)) %>% # drop seed terms prevalent for single committee members
  filter(feature %in% walk_NE$lemma) %>% # drop seed terms not in the walk network
  distinct(feature, policy_field, period) %>% # drop features duplicated within policy fields (from committees etc)
  split(.$policy_field)                        # ... and split by policy field


# Housekeeping #
cat("\n ====== Remove unneeded Objects =======  \n")

rm(dat_list)
rm(walk_NE)

gc()


# Compute Random Walks

cat("\n ======= Compute Random Walks ======= \n")

walk_networks_list <- list.files("init_classification/no_replies/walk_network_data/")

walk_networks_list %>% 
  walk(\(walk_network)
       { network_name <- str_remove(walk_network, ".RDS") # reading in the data saved above
           
         readRDS(file.path("init_classification/no_replies/walk_network_data", walk_network)) %>% 
           get_rwr_terms(network_name = network_name,
                         seeds = seeds,
                         seed_var = "feature",
                         match_var = "period",
                         flatten_results = TRUE,
                         group_name = "policy_field",
                         normalize_score = "seeds",
                         positive_scores_only = TRUE,
                         walk_score = walk_score,
                         keep_seed_terms = keep_seed_terms,
                         progress = FALSE) %>% 
            # write results to disk rather than saving them in the environment
            mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
            vroom_write(file = paste0("init_classification/no_replies/walk_terms/",
                                      network_name, ".csv"), append = F)

  }, .progress = F)

gc()

walk_terms <- lapply(list.files("init_classification/no_replies/walk_terms/", 
                                full.names = T), vroom) %>% rbindlist()

walk_terms %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "init_classification/no_replies/init_walk_terms.csv.tar.gz", delim = ",")

plan(sequential)
