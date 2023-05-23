# Classify Terms #
## Readme for Evaluation ##

{
  library(tidyverse)
  library(quanteda)
  library(quanteda.textstats)
  library(data.table)
  library(vroom)
}

# source("utils_nested_data.R")

## old:
# init_seed_terms_ministries <- load_nested_tarball("init_classification/ministry_seeds.gz")
## or (slow, but has plots also):
# init_seed_terms_ministries <- readRDS("init_classification/init_seed_terms_ministries.RDS")


### all seed terms are saved as .csv 
### One file per group (ministries, committees, committee members)
### The Period (that is, the week - 1 year for which it was analyzed) is a column, allowing easy filtering 
### (e.g.: init_seed_terms_ministries %>% filter(period == ymd("2022-12-11")) )

init_seed_terms_ministries <- vroom("init_classification/seed_terms_ministries.csv.tar.gz", 
                                    col_names = c("feature", "chi2", "p", # colnames need to be specified during read-in
                                                  "n_target", "n_reference", 
                                                  "ministry", "policy_field", 
                                                  "period"))

init_seed_terms_committees <- vroom("init_classification/seed_terms_committees.csv.tar.gz") # this data has column pre-specified

init_seed_terms_committee_members <- vroom("init_classification/seed_terms_committee_members.csv.tar.gz") # this data has column pre-specified

## All Plots are available in the respective folder under init_classification/
## => can be downloaded and viewed (this should be easier than loading them into R)


# Re-Calculate Seed Terms

source("get_seed_terms.R")

chi2_ministries <- 500 # set chi^2 threshold for ministries
chi2_committee_members <- 250 # set chi^2 threshold for within-committee members
chi2_committees <- 30 # set chi^2 threshold between committees



# read data

ministries <- read_csv("Seed_Accounts/ministry_seeds_2023-04-06.csv", col_types = list(user_id = "c"))

committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c"))


seed_tweets <-
  vroom(
    file = "init_classification/data_seeds_init_2023-04-10.csv.tar.gz",
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


seed_tokens <- vroom("init_classification/tokens_init_2023-04-10.csv.tar.gz", col_types = list(doc_id = "c"))


seed_NE <- seed_tokens %>% as_tibble() %>% 
  filter(tag == "NE" | tag == "NN") %>% # Noun words and NEs only
  filter(str_length(token) > 1) %>% # drop very short tokens, e.g. wrongly classified "#"
  filter(token != "amp", token != "&amp")



# ! if you want to calculate seed terms for a specific time frame, e.g. one year,, subset here ! 

tweets_subset <- seed_tweets %>% filter(`_source.created_at` <= ymd("2023-03-01") & 
                                          `_source.created_at` >= (ymd("2023-03-01") - years(1)))

NE_subset <- seed_NE %>% filter(doc_id %in% tweets_subset$`_id`)



# Data Structuring & Cleaning

ministry_tweets <- ministries %>% left_join(tweets_subset, 
                                            by = join_by(user_id == `_source.author_id`), 
                                            relationship = "many-to-many")

ministry_NE <- ministry_tweets %>% 
  left_join(NE_subset, by = join_by(`_id` == doc_id), 
            relationship = "many-to-many") %>% 
  select(user_id, official_name, policy_field, `_id`, lemma) %>% 
  filter(!str_detect(lemma, "@")) # drop all lemmas containing "@" - that is, all mentions


committee_tweets <- committees %>% left_join(tweets_subset, 
                                             by = join_by(user_id == `_source.author_id`), 
                                             relationship = "many-to-many") # tweets for accounts in multiple fields are duplicated

committee_NE <- committee_tweets %>% 
  left_join(NE_subset, by = join_by(`_id` == doc_id), 
            relationship = "many-to-many") %>% 
  select(user_id, official_name, policy_field, committee, `_id`, lemma) %>% 
  filter(!str_detect(lemma, "@")) # drop all lemmas containing "@" - that is, all mentions



# Calculate Chi^2 for Policy Fields

## to accumulate the terms by policy field and then calculate chi^2, set the grouping var to "policy_field"

## Between Ministries

seed_terms_ministries <- get_seed_terms(data = ministry_NE, # dataframe containing tokens, grouping_var and policy_field
                                        doc_id = "_id",   # doc_id of the documents. Optional, but can speed up computation
                                        tokens = "lemma", # tokens variable (here we use lemmas)
                                        grouping_var = "official_name", # What variable should the tokens be accumulated and compared by? E.g. policy field, committee, name...
                                        policy_field = "policy_field", # the name of the policy field variable to be added to results. Can be skipped if policy_field = NULL
                                        threshold = chi2_ministries, # keyness threshold to filter results. Optional, no threshold with NULL
                                        measure = "chi2",        # keyness measure. See textstat_keyness for options
                                        remove_tokens = c(stopwords("german"), stopwords("english"), "rt", "RT"), # Tokens to be removed in the process, typically stopwords
                                        show_plots = T, # to print the plot for each group in the process, set to TRUE
                                        save_plots = F) # to save the plots in the same object, set to TRUE. Note that terms will be accessible under ...$key_terms and plots under ...$plots



## Between Committees

seed_terms_committees <- get_seed_terms(data = committee_NE,
                                        doc_id = "_id",
                                        tokens = "lemma",
                                        grouping_var = "committee",
                                        policy_field = "policy_field",
                                        threshold = chi2_committees,
                                        measure = "chi2",        
                                        remove_tokens = c(stopwords("german"), stopwords("english"), "rt", "RT"), 
                                        show_plots = T, 
                                        save_plots = F) 



## Between Members of each committee

seed_terms_committee_members <- committee_NE %>% # split datasets into committees and calculate keyness within committees via map()
  split(.$committee) %>% 
  map(\(data) get_seed_terms(data = data,
                             doc_id = "_id",
                             tokens = "lemma",
                             grouping_var = "official_name",
                             policy_field = "policy_field",
                             threshold = chi2_committee_members,
                             measure = "chi2",        
                             remove_tokens = c(stopwords("german"), stopwords("english"), "rt", "RT"), 
                             show_plots = T, 
                             save_plots = F)) %>% 
  rbindlist()

