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

dir <- "news_classification"

cat("\n ======= Preparations ======= \n")

time_frame_walks = weeks(12) # length of the time frame for random walks

# read data

tokens <- vroom(list.files(dir, pattern = "tokens_news", full.names = T))

news_data <- vroom("news_classification/data_news_2019-2021.csv.tar.gz")
# tokens <- vroom("news_classification/tokens_news_2021.csv.tar.gz")

## NEs only
walk_NE <- filter_tokens(tokens,
                         tokens_col = "lemma", 
                         tags = c("NN", "NE"), # Noun words and NEs only
                         #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                         replies = NULL, # filter for reply condition (TRUE includes replies, FALSE does not)  
                         keep_mentions = NULL, # should @-mentions be kept?
                         keep_urls = NULL # should URLs be kept?
) %>% 
  left_join(news_data %>% select(`_id`, `_source.estimated_date`), by = join_by(doc_id == `_id`)) %>% # add date indicator
  mutate(week = ceiling_date(as_datetime(`_source.estimated_date`), # make week indicator (last day of the week)
                             unit = "week"))

gc()

# Housekeeping #
cat("\n ====== Remove unneeded Objects =======  \n")

rm(tokens)
rm(news_data)

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



cat("\n ====== Remove unneeded Objects =======  \n")

rm(walk_NE)

gc()


# Prepare Network for Random Walks

cat("\n ======= Make Networks for Random Walks ======= \n")

#plan(sequential) # no multisession - seems unstable for certain processes here, such as making the networks [we can rather use a simple iwalk here than changing the plan]

if (!dir.exists(file.path(dir, "walk_network_data"))) {
  dir.create(file.path(dir, "walk_network_data"))
}

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
            saveRDS(file = paste0(dir, "/walk_network_data/",
                                  name, ".RDS"))},
               .progress = F)


gc()
