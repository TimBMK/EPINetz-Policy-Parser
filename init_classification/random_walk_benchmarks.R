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
  library(microbenchmark)
}


source("get_rwr_terms.R")

cat("\n ======= Preparations ======= \n")

# if (installr::is.RStudio()){
#   plan(list(tweak(multisession, workers = 2), # A nested plan, where the first level is the number of networks processed in parallel, 
#             tweak(multisession, workers = 16))) # and the second level is the parallelization of the random walks 
#   # Multisession for R Studio Sessions
# } else {
#   plan(list(tweak(multicore, workers = 2), # A nested plan, where the first level is the number of networks processed in parallel, 
#             tweak(multicore, workers = 16))) # and the second level is the parallelization of the random walks 
#   # multicore for scripts / non-rstudio processes
# }


# plan(multicore, workers = 4) # start multisession (for mapping processes) - ! DO NOT USE MULTICORE IN RSTUDIO SESSION. ONLY WHEN CALLING THE SCRIPT DIRECTLY !
#                               # !!multicore plans can be unstable!!
# options(future.globals.maxSize = (100000*1024^2)) # 10 gb Max Size for Parallelization Processes


walk_score = 0.5 # cutoff value for normalized random walk score

keep_seed_terms = TRUE # should seed terms within the policy field of the same period always be kept, regardless of walk score?


# read seed terms

seed_terms_ministries <- vroom("init_classification/seed_terms_ministries.csv.tar.gz", 
                               col_names = c("feature", "chi2", "p", # colnames need to be specified during read-in
                                             "n_target", "n_reference", 
                                             "ministry", "policy_field", 
                                             "period"))

seed_terms_committees <- vroom("init_classification/seed_terms_committees.csv.tar.gz") # this data has columns pre-specified

seed_terms_committee_members <- vroom("init_classification/seed_terms_committee_members.csv.tar.gz") # this data has columns pre-specified


# Make Seedlist

cat("\n ======= Make Seedlist ======= \n")

# plan(multicore, workers = 4) # restart multisession (for mapping processes) 

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, by = join_by(feature, period, committee)) %>% # drop seed terms prevalent for single committee members
  distinct(feature, policy_field, period) %>% # drop features duplicated within policy fields (from committees etc)
  split(.$policy_field)                        # ... and split by policy field


# Compute Random Walks

cat("\n ======= Compute Random Walks ======= \n")


walk_networks_list <- list.files("init_classification/walk_network_data/")
walk_networks_list <- walk_networks_list[150:154] # testing

microbenchmark(
  
  {plan(list(tweak(sequential), 
             tweak(multicore, workers = 16))) 
    walk_networks_list %>% 
      future_walk(\(walk_network) 
                  { network_name <- str_remove(walk_network, ".RDS") # reading in the data saved above
                    
                    readRDS(file.path("init_classification/walk_network_data", walk_network)) %>% 
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
                                    progress = F) %>% 
                      # write results to disk rather than saving them in the environmen
                      mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
                      vroom_write(file = paste0("init_classification/walk_terms/",
                                                network_name, ".csv"), append = F)
                    
      }, .progress = F)},
  
  {plan(list(tweak(sequential), 
            tweak(multicore, workers = 32))) 
  walk_networks_list %>% 
    future_walk(\(walk_network) 
                { network_name <- str_remove(walk_network, ".RDS") # reading in the data saved above
                  
                  readRDS(file.path("init_classification/walk_network_data", walk_network)) %>% 
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
                                  progress = F) %>% 
                    # write results to disk rather than saving them in the environmen
                    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
                    vroom_write(file = paste0("init_classification/walk_terms/",
                                              network_name, ".csv"), append = F)
                  
    }, .progress = F)},
  
  {plan(list(tweak(multicore, workers = 2), 
            tweak(multicore, workers = 16))) 
  walk_networks_list %>% 
    future_walk(\(walk_network) 
                { network_name <- str_remove(walk_network, ".RDS") # reading in the data saved above
                  
                  readRDS(file.path("init_classification/walk_network_data", walk_network)) %>% 
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
                                  progress = F) %>% 
                    # write results to disk rather than saving them in the environmen
                    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
                    vroom_write(file = paste0("init_classification/walk_terms/",
                                              network_name, ".csv"), append = F)
                  
    }, .progress = F)},
  
  {plan(list(tweak(multicore, workers = 4), 
             tweak(multicore, workers = 8))) 
    walk_networks_list %>% 
      future_walk(\(walk_network) 
                  { network_name <- str_remove(walk_network, ".RDS") # reading in the data saved above
                    
                    readRDS(file.path("init_classification/walk_network_data", walk_network)) %>% 
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
                                    progress = F) %>% 
                      # write results to disk rather than saving them in the environmen
                      mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
                      vroom_write(file = paste0("init_classification/walk_terms/",
                                                network_name, ".csv"), append = F)
                    
      }, .progress = F)},
  times = 2
  )

gc()

plan(sequential)
