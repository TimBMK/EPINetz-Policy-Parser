## News Classification Prototype ##
###################################

# Uses the twitter Seed Terms to run random walks or news articles and classify them

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

source("utils_text_processing.R") # text processing utils
source("get_rwr_terms.R") # random walk functions
source("classify_documents.R") # classification function

# setup parallel computing session
threads = 8 # nr of threads to use

if (installr::is.RStudio()){
  plan(multisession, workers = threads) 
  # Multisession for R Studio Sessions
} else {
  plan(multicore, workers = threads)
  # multicore for scripts / non-rstudio processes
}

options(future.globals.maxSize = (100000*1024^2)) # 10 gb Max Size for Parallelization Processes


# Settings

walk_score_normalization = "seeds" # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 
calculate_means = TRUE # should the means of the score be calculated and displayed? The can also be used for minimum walk_score filtering (see below)
positive_scores_only = TRUE # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization
walk_score = 0.9 # cutoff value for normalized random walk score. Non-Null Values require the selection of a measure to filter on if more than one walk_score_normalization method is picked
walk_score_measure = "seeds_mean" # value to apply the walk_score filter on. 
# Possible are: "default" (auto-pick), "raw" (non-normalized rwr score), "seeds" (seed normalized), "seeds_mean" (mean of seed normalized), "group" (group normalized), "group_mean" (mean of group normalized). Needs to be specified if more than one 
walk_score_quantile = TRUE # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value. Cutoff values may differ from group to group
report_quantiles = TRUE # should the quantiles of the selected walk_score_measure for each group be printed out? This is independent of walk_score_quantiles and calculated before walk_score filtering
keep_seed_terms = TRUE # should seed terms within the policy field of the same period always be kept, regardless of walk score?
seedterm_value = NULL # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip

classification_measure = "ScoreNormMean" # one of ScoreNorm, ScoreNormMean, ScoreNormGroup, ScoreNormGroupMean, or (for raw scores) Score or ScoreMean
#  this should correspond to (one of) the score(s) calculated during the random walks
cutoff = NULL # Should an additional cutoff be set? Applies to classification measure. NULL to skip
seedterm_value = NULL # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only
keep_seed_terms_classification = NULL
cut_frequent_policy_terms = NULL  # Should terms appearing in numerous policy field be cut? 
  #  "auto" to cut terms appearing in more than 50% of the policy fields
  #  numeric value for a specific number
  #  NULL to skip
cut_lower_quantile_fields = 0.1 # Should policy classifications within a document be cut if they fall below a certain quantile? NULL to skip. Else numerical value to specify the quantile
normalize_scores = "doc" # should the score in the documents be normalized between 0 and 1? Can be "doc" (normalize within each document), "group" (normalize for each group), or NULL to skip
  # normalizing the scores per document seems beneficial for longer docs, "group" is better suited for short documents like tweets
return_walk_terms = TRUE # should the processed walk terms be returned for further analysis and transparency? 
return_unclassified_docs = TRUE # should unclassified docs be returned for further analysis and transparency?



# Get and prepare Seedterms
seed_terms_ministries <- vroom("init_classification/seed_terms_ministries.csv.tar.gz") %>% 
  mutate(policy_field = case_when(policy_field == "finanzen_haushalt" ~ "haushalt_finanzen", # rename for consistency with committee policy fields
                                  .default = policy_field))

seed_terms_committees <- vroom("init_classification/seed_terms_committees.csv.tar.gz") %>% 
  filter(!is.na(policy_field)) # drop committees without an associated policy field, that is "Recht und Verbraucherschutz"

seed_terms_committee_members <- vroom("init_classification/seed_terms_committee_members.csv.tar.gz") %>% 
  filter(!is.na(policy_field)) # drop committees without an associated policy field, that is "Recht und Verbraucherschutz"

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, by = join_by(feature, period, committee)) %>% # drop seed terms prevalent for single committee members
  distinct(feature, policy_field, period) %>% # drop features duplicated within policy fields (from committees etc)
  split(.$policy_field)                        # ... and split by policy field


# Load News Data

news_tokens <- vroom("/data/EPINetz/IssueAttention/tokens_news_articlelevel/tokens_news_2021.csv.tar.gz")

news_docs <- vroom("/data/EPINetz/IssueAttention/data_news_2019-2021.csv.tar.gz")


# Make test data

date <- ymd("2021-06-20") # a date for testing purposes

seeds_test <- seeds %>% 
  map(\(seeds)
      {seeds %>% filter(period == date)})

test_tokens <- news_docs %>%
  filter(`_source.estimated_date` <= date &
           `_source.estimated_date` >= (date - weeks(12))) %>%
  select(`_id`) %>%
  rename(doc_id = `_id`) %>% 
  left_join(news_tokens, by = join_by(doc_id)) 
  
  
test_NE <- test_tokens %>% 
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = NULL, # not available
                keep_mentions = NULL, # not available
                keep_urls = NULL) %>%  # not available 
  drop_quantile(tokens = "lemma", # drop 10% quantile
                quantile = 0.1,
                ignore_case = FALSE, # case is already lowered
                group = "tag",
                verbose = TRUE)
  
test_docs <- news_docs %>%
  filter(`_source.estimated_date` <= date &
           `_source.estimated_date` >= (date - weeks(1)))
  



# calculate random walk network
walk_network <- make_multiplex_objects(test_NE,
                                       vertex_a = "doc_id",
                                       vertex_b = "lemma",
                                       directed = F,
                                       pmi_weight = T,
                                       keep_igraph_network = F,
                                       keep_multiplex_network = T,
                                       keep_adjacency_matrix = F,
                                       keep_normalized_adjacency_matrix = T)


saveRDS(walk_network,
        file = paste0("news_classification/walk_network_data/",
                      as.character(date), ".RDS"))

gc()

# Run Random Walks 

walk_terms <- get_rwr_terms(walk_network,
                            network_name = NULL,
                            seeds = seeds_test,
                            seed_var = "feature",
                            match_var = NULL,
                            flatten_results = TRUE,
                            group_name = "policy_field",
                            normalize_score = walk_score_normalization,
                            calculate_means = calculate_means,
                            normalize_means = TRUE, # a second normalization of the means
                            reduce_to_means = TRUE, # should only means be returned, dropping duplicated Nodes and their associated scores?
                            positive_scores_only = positive_scores_only,
                            walk_score = walk_score,
                            walk_score_measure = walk_score_measure,
                            walk_score_quantile = walk_score_quantile,
                            report_quantiles = report_quantiles,
                            keep_seed_terms = keep_seed_terms,
                            seedterm_value = seedterm_value, # should the actual value of seed terms be overwritten by a default value, e.g. 1? NULL to skip
                            progress = T) 

walk_terms %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = paste0("news_classification/walk_terms/",
                          date, ".csv"), append = F)


# Compare Walk Terms between Twitter and News

walk_terms_twitter <- vroom("init_classification/walk_terms/2021-06-20.csv")

walk_terms %>% filter(seed_term == FALSE) %>%
  as.data.table() %>% split(by = "policy_field") %>%
  imap(\(data, group)
       {
         data %>% mutate(overlap = case_when(
           NodeNames %in% (
             walk_terms_twitter %>% filter(seed_term == FALSE &
                                             policy_field == group) %>%
               pull(NodeNames)
           ) ~ TRUE,
           .default = FALSE
         )) %>%
           summarise(n = n(), .by = overlap) %>% 
           mutate(policy_field = group)
  }) %>% 
  rbindlist() %>% 
  ggplot(aes(y = n, x = policy_field, fill = overlap)) +
  geom_col()



# Classify Documents

classification_NE <- test_NE %>% inner_join(test_docs %>% select(`_id`), 
                                            by = join_by(doc_id == `_id`))

classification_result <- classify_documents(
  walk_terms = walk_terms, # data with terms generated by get_rwr_terms()
  group_name = "policy_field", # name of the group variable which specifies the classes the documents will be sorted into
  document_tokens = classification_NE, # data with the documents to be classified. Expects tokenized data, with a doc_id and one token per row
  tokens_var = "lemma", # name of the tokens variable within the documents dataframe. Usually "tokens", "lemma", etc.
  doc_id = "doc_id", # name of the doc_id variable in document_tokens
  classification_measure = classification_measure, # set the measure to use for classification here. Must be present in the data
  cutoff = cutoff, # Should a cutoff be set to filter the walk_terms?? Applies to classification measure. NULL to skip. This is useful if a more strict cutoff is desired than in get_rwr_terms()
  keep_seed_terms = keep_seed_terms_classification, # should seed terms be kept even if their score is lower than the cutoff? only applies if a cutoff is specified
  seedterm_value = seedterm_value, # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only
  normalize_scores = normalize_scores, # should the score in the documents be normalized between 0 and 1? Can be doc (normalize within each document), group (normalize for each group), or NULL to skip
  cut_lower_quantile_fields = cut_lower_quantile_fields, # Should policy classifications within a document be cut if they fall below a certain quantile? NULL to skip. Else numerical value to specify the quantile
  cut_frequent_group_terms = cut_frequent_policy_terms,
  return_walk_terms = return_walk_terms, # should the processed walk terms be returned for further analysis and transparency?
  return_unclassified_docs = return_unclassified_docs, # should the IDs of the unlassified docs be returned? 
  # setting return_walk_terms or return_unclassified_docs to TURE returns a list of dataframes rather than a single dataframe
  verbose = TRUE
)


# Check results

top_group_terms(classification_result,
                group_name = "policy_field",
                classification_measure = classification_measure,
                print_seed_terms = TRUE,
                n = 20,
                mode = "print")


top_docs <- top_group_documents(classification_result,
                                documents = test_docs,
                                doc_id = join_by(doc_id == `_id`),
                                group_name = "policy_field",
                                classification_score = "score_norm",
                                n = 10,
                                with_ties = TRUE,
                                mode = "return")
