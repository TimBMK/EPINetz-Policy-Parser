# Making network snapshots

if(!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
if(!require("igraph")) install.packages("igraph")
  library(igraph)
if(!require("furrr")) install.packages("furrr")
  library(furrr)
if(!require("vroom")) install.packages("vroom")
  library(vroom)
if(!require("lubridate")) install.packages("lubridate")
  library(lubridate)
if(!require("widyr")) install.packages("widyr")
  library(widyr)
if(!require("data.table")) install.packages("data.table")
  library(data.table)

source("/data/wolfeswenker/Projekte/Policy_Parser/network_snapshots_EW.R") # function to make snapshots
source("/data/koenigt/Tools-Scripts/Tools & Scripts/network_snapshots.R") # function to make snapshots

## read in data
twitter_NE <-
  vroom("/data/EPINetz/IssueAttention/data_graph_twitter.csv.tar.gz", # this data uses lemmas rather than tokens!
        col_types = list(doc_id = "c", author_id = "c")) %>%
  as_tibble()
        # data_graph_combined.csv.tar.gz would be the combined twitter/news NEs


## make time / snapshot indicator
twitter_NE <-  twitter_NE %>%  mutate(
  week = floor_date(created_at, unit = "week"),              # for combined news and twitter data, "created_at" must be replaced with "date" 
  month = floor_date(created_at, unit = "month"),
  quarter = floor_date(created_at, unit = "quarter")         # any other date intervals are possible as long as you stick to the format
)


## make snapshots

plan(multisession, workers = 4) # set up a multisession with future/furr to parallelize the snapshotting. plan(sequential) is the the standard (linear) method. see future::plan() documentation

twitter_NE_subsample <- twitter_NE %>% 
  filter(type == "NE" | type == "hashtag") %>% # we can subset the input data, e.g. for certain entitiy types like NEs and hashtags ...
  filter(created_at >= ymd("2019-01-01"), created_at <= ymd("2019-04-01")) # ... or timeframes (in order to speed up the snapshotting)

twitter_snapshots <- snapshots(twitter_NE_subsample,
                               vertex_a = "doc_id", # order of these vertices is importan for the PMI calculation
                               vertex_b = "lemma",
                               time = "quarter", # here goes the indicator of the variable that will determine the snapshots. In this case, the "week" variable created above. 
                                              #   Possible alternatives with the mutation above would be "month" or "quarter" (3 months)
                               directed = F, # we want an undirected network
                               pmi_weight = T, # we want to calculate the PMI and use it as weights in the network
                               output = "networks", # we want to output the network data, not the community or page_rank metrics
                               seed = 20230131 # setting a seed prevents RNG issues in the parallelization
                               )

        # this results in a data.frame where the pmi-weighted connections between all nodes in a snapshot (to, from, weight) are contained
        #     The "time" variable indicates the snapshot. All snapshots are contained in this data.frame

plan(sequential) # end the multisession


## save

setwd("/data/wolfeswenker/Projekte/Policy_Parser/")

twitter_snapshots %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% # I would advice saving it this way to prevent encoding issues
  vroom_write(file = "topic_graphs_quarter201901.csv.tar.gz", delim = ",") # vroom_write is very fast

twitter_NE_hashtags <-
  vroom("/data/wolfeswenker/Projekte/Policy_Parser/topic_graphs_quarter201901.csv.tar.gz", # this data uses lemmas rather than tokens!
        col_types = list(from = "c", to = "c")) %>%
  as_tibble()

twitter_graph <- graph_from_data_frame(twitter_NE_hashtags, directed = F)

write_graph(twitter_NE_hashtags, file = "twitter_NE_hashtags.graphml", format = "graphml") # save

