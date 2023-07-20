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

cat("\n ======= Preparations ======= \n")

if (installr::is.RStudio()){
  plan(list(tweak(multisession, workers = 2), # A nested plan, where the first level is the number of networks processed in parallel, 
            tweak(multisession, workers = 16))) # and the second level is the parallelization of the random walks 
  # Multisession for R Studio Sessions
} else {
  plan(list(tweak(multicore, workers = 2), # A nested plan, where the first level is the number of networks processed in parallel, 
            tweak(multicore, workers = 16))) # and the second level is the parallelization of the random walks 
  # multicore for scripts / non-rstudio processes
}


# settings

walk_score_normalization = c("seeds","group") # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 

calculate_means = TRUE # should the means of the score be calculated and displayed? The can also be used for minimum walk_score filtering (see below)

positive_scores_only = TRUE # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization

walk_score = 0.5 # cutoff value for normalized random walk score. Non-Null Values require the selection of a measure to filter on if more than one walk_score_normalization method is picked

walk_score_measure = "seeds" # value to apply the walk_score filter on. 
# Possible are: "default" (auto-pick), "raw" (non-normalized rwr score), "seeds" (seed normalized), "seeds_mean" (mean of seed normalized), "group" (group normalized), "group_mean" (mean of group normalized). Needs to be specified if more than one 

walk_score_quantile = FALSE # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value. Cutoff values may differ from group to group

report_quantiles = TRUE # should the quantiles of the selected walk_score_measure for each group be printed out? This is independent of walk_score_quantiles and calculated before walk_score filtering

keep_seed_terms = TRUE # should seed terms within the policy field of the same period always be kept, regardless of walk score?

seedterm_value = NULL # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip



# read seed terms

seed_terms_ministries <- vroom("init_classification/seed_terms_ministries.csv.tar.gz")

seed_terms_committees <- vroom("init_classification/seed_terms_committees.csv.tar.gz") 

seed_terms_committee_members <- vroom("init_classification/seed_terms_committee_members.csv.tar.gz") 


# Make Seedlist

cat("\n ======= Make Seedlist ======= \n")

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, by = join_by(feature, period, committee)) %>% # drop seed terms prevalent for single committee members
  distinct(feature, policy_field, period) %>% # drop features duplicated within policy fields (from committees etc)
  split(.$policy_field)                        # ... and split by policy field


# Compute Random Walks

cat("\n ======= Compute Random Walks ======= \n")


walk_networks_list <- list.files("init_classification/walk_network_data/")
# walk_networks_list <- walk_networks_list[150:155] # testing

walk_networks_list %>% 
  future_walk(\(walk_network) 
              { network_name <- str_remove(walk_network, ".RDS") # reading in the data saved above
                
                if(report_quantiles) {cat(paste0("\n\n", network_name, "\n"))} # report time period (name of the network) if quantiles are reported
                
                readRDS(file.path("init_classification/walk_network_data", walk_network)) %>% 
                  get_rwr_terms(network_name = network_name,
                                seeds = seeds,
                                seed_var = "feature",
                                match_var = "period",
                                flatten_results = TRUE,
                                group_name = "policy_field",
                                normalize_score = walk_score_normalization,
                                calculate_means = calculate_means,
                                normalize_means = TRUE, # a second normalization of the means
                                reduce_to_means = FALSE, # should only means be returned, dropping duplicated Nodes and their associated scores?
                                positive_scores_only = positive_scores_only,
                                walk_score = walk_score,
                                walk_score_measure = walk_score_measure,
                                walk_score_quantile = walk_score_quantile,
                                report_quantiles = report_quantiles,
                                keep_seed_terms = keep_seed_terms,
                                seedterm_value = seedterm_value, # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip
                                progress = F) %>% 
                  # write results to disk rather than saving them in the environmen
                  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
                  vroom_write(file = paste0("init_classification/walk_terms/",
                                            network_name, ".csv"), append = F)
                
                
                
  }, .progress = F)

  
gc()

walk_terms <- lapply(list.files("init_classification/walk_terms/", 
                                full.names = T), vroom) %>% rbindlist()

walk_terms %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "init_classification/init_walk_terms.csv.tar.gz", delim = ",")

plan(sequential)
