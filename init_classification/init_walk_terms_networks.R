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


source("get_rwr_terms.R")

source("utils_text_processing.R")

cat("\n ======= Preparations ======= \n")

walk_replies = FALSE # should replies be considered for the random walks?

walk_mentions = TRUE # should @-mentions be used for the random walks?

walk_urls = TRUE # should URLs be used for the random walks?

time_frame_walks = weeks(12) # length of the time frame for random walks

# read data

tokens <- get_latest_tokens_file(path = "Tokenizer", pattern = "tokens_full.csv.tar.gz") %>% 
  vroom(col_types = list(doc_id = "c", `_source.author_id` = "c")) 


## NEs only
walk_NE <- filter_tokens(tokens,
                         tokens_col = "lemma", 
                         tags = c("NN", "NE"), # Noun words and NEs only
                         #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                         replies = walk_replies, # filter for reply condition (TRUE includes replies, FALSE does not)  
                         keep_mentions = walk_mentions, # should @-mentions be kept?
                         keep_urls = walk_urls # should URLs be kept?
) %>% 
  mutate(week = ceiling_date(as_datetime(`_source.created_at`), # make week indicator (last day of the week)
                             unit = "week"))

gc()

# List of Timeframes
  ## in contrast to the seed terms, we directly use the NE data for the list of 
  ##  frames, since we need all NEs (not specific to ministries etc.) anyways

cat("\n ======= Make List of Timeframes ======= \n")

dat_list <- map(walk_NE$week %>% unique(), # parallelization overhead is larger than the gain, so we use a simple map() here
                ~ {
                  walk_NE %>% 
                    filter(week >= (.x - time_frame_walks) & # beginning: set above
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

rm(tokens)

gc()

# Prepare Network for Random Walks

cat("\n ======= Make Networks for Random Walks ======= \n")

#plan(sequential) # no multisession - seems unstable for certain processes here, such as making the networks [we can rather use a simple iwalk here than changing the plan]

dat_list %>% 
  iwalk(\(dat, name) # saving the results as .RDS to read in later is more stable than keeping them in the environment
        {make_multiplex_objects(dat,
                                vertex_a = "doc_id",
                                vertex_b = "lemma",
                                directed = F,
                                pmi_weight = T,
                                keep_igraph_network = F,
                                keep_multiplex_network = T,
                                keep_adjacency_matrix = F,
                                keep_normalized_adjacency_matrix = T) %>% 
            saveRDS(file = paste0("init_classification/walk_network_data/",
                                  name, ".RDS"))},
               .progress = F)


gc()
