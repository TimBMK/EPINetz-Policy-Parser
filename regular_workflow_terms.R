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
source("utils_text_processing.R") # text processing utils
source("get_rwr_terms.R") # random walk functions

source("workflow_seed_terms.R") # full seed term workflow
source("workflow_walk_terms.R") # full walk term workflow


plan(multisession, workers = 16) # set up future multisession for future_map functions


# Set date
date <- Sys.Date()

# subdirectory to save results in
dir <- "regular_classification/"


# set verbosity
verbose <- TRUE

# Read lists
ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))
committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))
epinetz_accounts <- readRDS("EPINetz_full_collection_list_update_11.RDS") # list of all EPINetz Accounts

# Get Tokenized Data
tokens <- get_latest_tokens_file(path = "Tokenizer") %>% 
  vroom(col_types = list(doc_id = "c", `_source.author_id` = "c")) 

#### Extract Seed Terms ####

seeds <- seed_terms_workflow(tokens, # tokens object. Expects tokenized data returned by the tokenizer workflow
                             ministries,      # ministry account list
                             committees,      # committee account list
                             dir = dir,             # subdirectory to save results in. NULL to skip
                             date = date, # date for filtering purposes (max date)
                             time_frame_seeds = years(1), # length of the time frame for seed term extraction
                             seed_replies = TRUE, # should replies be utilized?
                             seed_mentions = FALSE, # should mentions be utilizued?
                             seed_urls = TRUE, # should URLs be utilized?
                             chi2_ministries = 150, # set chi^2 threshold for ministries
                             max_results_ministries = 230, # number of maximum results for each ministry
                             min_results_ministries = 100, # number of minimum results for each ministry
                             chi2_committee_members = 250, # set chi^2 threshold for within-committee members
                             max_results_committee_members = NULL, # number of maximum results for each committee member (NULL to skip)
                             min_results_committee_members = NULL, # number of minimum results for each committee member (NULL to skip)
                             chi2_committees = 40, # set chi^2 threshold between committees
                             max_results_committees = 200, # number of maximum results for each committee (NULL to skip)
                             min_results_committees = 35, # number of minimum results for each committee (NULL to skip)
                             max_result_ties = TRUE, # should ties for max results be kept? If TRUE, may return more than the requested number of seed terms
                             active_committees_only = TRUE, # should only active committees be considered? Expects a "begin", "end" and "`_source.created_at`" column to track activity. 
                             #  Drops all documents of a committees not created in this range
                             get_plots = FALSE, # should the chi^2 plots be saved within the object?
                             save_seeds = FALSE, # should the Seed Terms be saved explicitly in 'dir'? If TRUE, saves data for ministries, committees, and committee members seperately. Does not save plots
                             verbose = verbose, # should output be printed that keeps track of the current analysis step? Additionally, verbose = F mutes messages from the parallel seed term functions
                             random_seed = as.numeric(date))


# Housekeeping


gc()




#### Additional Terms through Random Walks ####

walk_terms <- walk_terms_workflow(tokens = tokens, # tokens object. Expects tokenized data returned by the tokenizer workflow
                                  seed_terms = seeds, # seeds objects. Expects a liste with the named dataframes "seed_terms_ministries", "seed_term_committees" and "seed_term_committee_members"
                                  date = date, # date for filtering purposes (max date
                                  walk_replies = FALSE, # should replies be utilized?
                                  walk_mentions = TRUE, # should mentions be utilizued?
                                  walk_urls = TRUE, # should URLs be utilized?
                                  time_frame_walks = weeks(12), # length of the time frame for random walk term extraction
                                  quantile_drop = 0.1, # what quantile should be dropped from the token counts? NULL to skip
                                  walk_score_normalization = "seeds", # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 
                                  calculate_means = TRUE, # should the means of the score be calculated and displayed? The can also be used for minimum walk_score filtering (see below)
                                  normalize_means = TRUE, # a second normalization of the means
                                  reduce_to_means = TRUE, # should only means be returned, dropping duplicated Nodes and their associated scores?
                                  positive_scores_only = TRUE, # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization
                                  walk_score = 0.9, # cutoff value for normalized random walk score. Non-Null Values require the selection of a measure to filter on if more than one walk_score_normalization method is picked
                                  walk_score_measure = "seeds_mean", # value to apply the walk_score filter on. 
                                  # Possible are: "default" (auto-pick), "raw" (non-normalized rwr score), "seeds" (seed normalized), "seeds_mean" (mean of seed normalized), "group" (group normalized), "group_mean" (mean of group normalized). Needs to be specified if more than one 
                                  walk_score_quantile = TRUE, # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value. Cutoff values may differ from group to group
                                  keep_seed_terms = TRUE, # should seed terms within the policy field of the same period always be kept, regardless of walk score?
                                  seedterm_value = NULL, # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip
                                  seed = as.numeric(date), # seed to prevent RNG issues in parallelization. by default the numeric conversion of the date 
                                  verbose = F
)


#### save ####

path <- file.path("regular_classification", date)

dir.create(path)

walk_terms %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
  vroom_write(file.path(path, paste0("policy_terms_", date, ".csv")))
  

  
# Housekeeping
plan(sequential) # end multisession
gc()