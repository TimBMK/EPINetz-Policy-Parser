## Initial Classification ##
############################

# Classify Seed Terms #

{
  library(tidyverse)
  library(quanteda)
  library(quanteda.textstats)
  library(data.table)
  library(furrr)
  library(vroom)
}

options(future.globals.maxSize = (100000*1024^2)) # 10 gb Max Size for Parallelization Processes

source("get_seed_terms.R")

source("utils_nested_data.R")

# plan(multisession, workers = 4) # start multisession (for mapping processes) - Multisession for R Studio Sessions
plan(multicore, workers = 4) # start multisession (for mapping processes) - ! DO NOT USE MULTICORE IN RSTUDIO SESSION. ONLY WHEN CALLING THE SCRIPT DIRECTLY !
#                              # !!multicore plans can be unstable!!

chi2_ministries <- 500 # set chi^2 threshold for ministries
chi2_committee_members <- 250 # set chi^2 threshold for within-committee members
chi2_committees <- 30 # set chi^2 threshold between committees

seed <- 20230425 # seed for parallelization (prevents RNG issues)

cat("\n preparations \n\n")

# read data

ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))


# seed_tweets <-
#   vroom(
#     file = "init_classification/data_init_seeds_2023-04-10.csv.tar.gz",
#     # Important! specify coltypes to preserve correct IDs
#     col_types = list(
#       `_id` = "c",
#       `_source.author_id` = "c",
#       `_source.conversation_id` = "c",
#       `_source.in_reply_to_user_id`= "c",
#       `_source.attachments.poll_ids` = "c",
#       `_source.withheld.scope` = "c",
#       `_source.withheld.country_codes` = "c",
#       `_source.entities.cashtags` = "c"
#     )
#   ) 


seed_NE <- get_latest_tokens_file(path = "Tokenizer") %>% 
  vroom(col_types = list(doc_id = "c", `_source.author_id` = "c"))  %>% 
  as_tibble() %>% 
  filter(`_source.author_id`  %in% ministries$user_id | # seed account tweets only
           `_source.author_id`  %in% committees$user_id ) %>% 
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = seed_replies, # filter for reply condition (TRUE includes replies, FALSE does not)  
                keep_mentions = seed_mentions, # should @-mentions be kept?
                keep_urls = seed_urls # should URLs be kept?
  )


# Data Structuring & Cleaning

committee_tweets <- committees %>% left_join(seed_tweets, 
                                             by = join_by(user_id == `_source.author_id`), 
                                             relationship = "many-to-many") # tweets for accounts in multiple fields are duplicated

committee_NE <- committee_tweets %>% 
  left_join(seed_NE, by = join_by(`_id` == doc_id), 
            relationship = "many-to-many") %>% 
  dplyr::filter((as_date(`_source.created_at`) >= as_date(begin) & # filter for active committee time periods
                   as_date(`_source.created_at`) <= as_date(end)) | 
                  is.na(end)) %>%   select(user_id, official_name, policy_field, committee, `_id`, lemma) %>% 
  filter(!str_detect(lemma, "@")) # drop all lemmas containing "@" - that is, all mentions


# Split Data into weekly one-year-frames (back to beginning of the set date range)

  # min <- date_range$from + years(1) # do not go back behind the first LP (for committees) -- only necessary for data ranging beyond the LP (then, only ministry data could be used)

dat_list <- future_map(ceiling_date(seed_tweets$`_source.created_at`, 
                                    unit = "week") %>% unique(),
                   ~ {
                     # ministry_tweets %>% mutate(type = "ministry") %>%                # ministry + committee tweets combined ... ! only necessary for min date filtering
                     #   bind_rows(committee_tweets %>% mutate(type = "committee")) %>% # ... with indicator for filtering ...
                     #   distinct(`_id`, .keep_all = T) %>% # ... and dropped  duplicates
                     # select(`_id`, `_source.created_at`, type) %>% 
                     seed_tweets %>% 
                       select(`_id`, `_source.created_at`) %>% 
                       mutate(week = ceiling_date(`_source.created_at`, 
                                                  unit = "week")) %>% # make week indicator (last day of the week)
                       filter(week >= (.x - years(1)) & # beginning: 1 year before
                                week <= .x)             # end: week of interest
                       # filter(!(type == "committee" & `_source.created_at` <= min)) # committees only after min date (before: only ministries) -- only necessary for data ranging beyond the LP 
                   })

names(dat_list) <- ceiling_date(seed_tweets$`_source.created_at`, 
                                unit = "week") %>% 
  unique() # name the dataframes in the list


# Calculate Keyness for each Frame

  ### saving nested objects is rather inefficient, so we flatten/save them as different objects and pack them up


## Committees
cat("\n Committees \n\n")
dat_list %>% 
  future_iwalk(\(x, idx)
               {
                 dat <- x %>%
                   inner_join(committee_NE, by = "_id") %>% # add NEs
                   get_seed_terms(
                     doc_id = "_id",
                     tokens = "lemma",
                     grouping_var = "committee",
                     policy_field = "policy_field",
                     threshold = chi2_committees,
                     show_plots = F,
                     save_plots = T
                   )
                 
                 dat %>% .[[1]] %>% mutate(period = idx) %>%
                   mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>%
                   vroom_write(file = paste0("init_classification/seed_terms_committees/", idx, ".csv"), append = F)
                 
                 # dat %>% .[[2]] %>% iwalk(\(plot, plotid)
                 #                          suppressMessages(ggsave(
                 #                            plot = plot,
                 #                            width = 18,
                 #                            height = 10,
                 #                            filename = paste0(plotid, ".png"),
                 #                            path = file.path("init_classification/seed_terms_committees_plots", idx)
                 #                          )))
  },
  .options = furrr_options(seed = seed), # set seed to prevent RNG issues
  .progress = F)

# cat("\n Calculations finished. Saving...")
# 
# save_nested_tarball(seed_terms_committees,
#                     index = 1,
#                     file_name = "init_classification/committee_seeds.gz")
# 
# save_nested_plots_tarball(seed_terms_committees,
#                           index = 2,
#                           file_name = "init_classification/committee_seed_plots.gz",
#                           progress = F)

# saveRDS(seed_terms_committees, file = "init_classification/init_seed_terms_committees.RDS")

seed_term_committes_list <- lapply(list.files("init_classification/seed_terms_committees/", full.names = T), vroom, 
                                   guess_max = 300, show_col_types = F) 

seed_terms_committees <- seed_terms_committees_list %>% rbindlist()

seed_terms_committees %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "init_classification/seed_terms_committees.csv.tar.gz", delim = ",")

plan(sequential) # end multisession
