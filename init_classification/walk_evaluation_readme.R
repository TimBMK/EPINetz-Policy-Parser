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
  library(patchwork)
}


source("get_rwr_terms.R")
source("utils_text_processing.R")

# Set time frames

rwr_timeframe = "2020-11-01" # Time frame of the RWR

classification_timeframe = weeks(1) # length of the timeframe for the classification, e.g. one week before the RWR timeframe
classification_before_after = "before" # set if the classification_timeframe is before or after the rwr_timeframe, e.g. the week before or after. "after" to for after, "before" for before

# What data should be used in the classification? This defaults to the RWR settings
classification_replies <- FALSE # should replies get classified?
classification_mentions <- TRUE # should mentions be utilized? adviced to keep this in line with walk_mentions
classification_urls <- TRUE # should urls be utilized? adviced to keep this in line with walk_urls



## Quantile Reports before walk score filtering can be found for each policy field in each period under init_classification/init_walk_statistics.txt (you can use strg+f to look for a specific period in the document)

walk_terms <- vroom(paste0("init_classification/walk_terms/", rwr_timeframe, ".csv"))
  

# Some general distribution checks
{walk_terms %>% 
    ggplot(aes(x = ScoreNorm, fill = seed_term)) + 
    geom_histogram(bins = 20, position = "dodge") + # note that the data is being binned, that is, subsumed into broader data ranges
    facet_wrap(~ seed_term, labeller = "label_both") + 
    guides(fill = "none") +
    scale_y_continuous(labels = label_scientific()) +
    theme_bw()} +
  {walk_terms %>% 
      distinct(policy_field, NodeNames, ScoreNormMean, seed_term) %>%  # display only mean values
      ggplot(aes(x = ScoreNormMean, fill = seed_term)) + 
      geom_histogram(bins = 20, position = "dodge") + # note that the data is being binned, that is, subsumed into broader data ranges
      facet_wrap(~ seed_term, labeller = "label_both") +
      guides(fill = "none") +
      scale_y_continuous(labels = label_scientific()) +
      theme_bw()} +
  {walk_terms %>% 
      ggplot(aes(x = ScoreNormGroup, fill = seed_term)) + 
      geom_histogram(bins = 20, position = "dodge") + # note that the data is being binned, that is, subsumed into broader data ranges
      facet_wrap(~ seed_term, labeller = "label_both") + 
      guides(fill = "none") +
      scale_y_continuous(labels = label_scientific())+
      theme_bw()} +
  {walk_terms %>% 
      distinct(policy_field, NodeNames, ScoreNormGroupMean, seed_term) %>% # display only mean values
      ggplot(aes(x = ScoreNormGroupMean, fill = seed_term)) + 
      geom_histogram(bins = 20, position = "dodge") + # note that the data is being binned, that is, subsumed into broader data ranges
      facet_wrap(~ seed_term, labeller = "label_both") +
      guides(fill = "none") +
      scale_y_continuous(labels = label_scientific()) +
      theme_bw()}


# Classify Documents

## read in data
classification_NE <- get_latest_tokens_file(path = "Tokenizer", 
                                 pattern = "tokens_full.csv.tar.gz") %>% 
  read_timelimited_data(col_types = list(doc_id = "c", `_source.author_id` = "c"),
                        filter_var = "_source.created_at",
                        starting_point = rwr_timeframe,
                        timeframe = classification_timeframe,
                        before_after = classification_before_after) %>% 
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = classification_replies, # should replies get classified?
                keep_mentions = classification_mentions, # should @-mentions be kept?
                keep_urls = classification_urls) # should URLs be kept?



tweets <- read_timelimited_data(file = "Tokenizer/data_init_tweets_2023-06-22.csv.tar.gz",
                                col_types = list(`_id`= "c", `_source.author_id` = "c",
                                                 `_source.conversation_id` = "c",
                                                 `_source.in_reply_to_user_id` = "c",
                                                 `_source.withheld.scope` = "c",
                                                 `_source.withheld.country_codes` = "c"),
                                guess_max = 1000000,
                                filter_var = "_source.created_at",
                                starting_point = rwr_timeframe,
                                timeframe = classification_timeframe,
                                before_after = classification_before_after) %>% 
  filter(is_reply == classification_replies | is_reply == FALSE) # only get replies if desired


## settings

classification_measure <- "ScoreNorm" # can also be ScoreNormMean, ScoreNormGroup, ScoreNormGroupMean, or (for raw scores) Score or ScoreMean
                                      # Try different measure to see what performs best in classification

seedterm_value = NULL # Should Seed Term Scores values be set to a fixed value for classification? NULL to skip. Otherwise enter a numerical value. Applies to classification_measure only
                      # walk_terms-Data needs to be reloaded to restore original values!

## calculate means (if necessary) to gain one score for each term in a policy field (rather then one for each seedterm-term connection)
### Note that if we calculate the means only now, means are calculated AFTER the initial filtering, and scores will accordingly be higher (all values below the walk_score threshold have already been dropped!)
if (str_detect(classification_measure, "Mean")) { # if one of the mean scores is set as classification_measure, we simply use that
classification_terms <- classification_NE %>% 
  semi_join(walk_terms, # filter for lemmas in the walk terms 
            join_by(lemma == NodeNames)) %>%
  left_join(walk_terms %>% distinct(NodeNames, !!as.name(classification_measure), policy_field),
            join_by(lemma == NodeNames), # add classification attributes
            relationship = "many-to-many")  # multi-matches for a) terms in multiple docs, b) terms in multiple policy fields
} else { # else, we need to calculate the mean for each term within a policy field first, in order to handle duplicates from different seed terms
  classification_terms <- classification_NE %>% 
    semi_join(walk_terms, # filter for lemmas in the walk terms 
              join_by(lemma == NodeNames)) %>%
    left_join(walk_terms %>% 
                summarise(!!as.name(classification_measure) := mean(!!as.name(classification_measure)), 
                          .by = c(NodeNames, policy_field)),
              join_by(lemma == NodeNames), # add classification attributes
              relationship = "many-to-many") 
}

if(!is.null(seedterm_value)) { 
  walk_terms <- walk_terms %>% 
    mutate(!!as.name(classification_measure) := case_when(seed_term == TRUE ~ seedterm_value,
                                                          .default = !!as.name(classification_measure)))
}

## classify

classified_documents <- classification_terms %>% 
  summarize(policy_score = sum(!!as.name(classification_measure)), .by = c(doc_id, policy_field)) %>% # sum policy scores by field and document  
                 ## there might be a better/different way of cumulating the scores, rather than just calculating the sum (with sum()) for each policy field in a document....
  complete(doc_id, policy_field, fill = list(policy_score = 0)) %>% # fill missing values
  mutate(policy_score_norm = policy_score/sum(policy_score), .by = doc_id) # and normalize between 0 and 1


## Check highest ranking policy fields for tweets (quality control)
top_field_tweets <- tweets %>% 
  left_join(classified_documents %>% 
              slice_max(policy_score_norm, by = doc_id, with_ties = F), 
            join_by(`_id` == doc_id)) %>% 
  select(`_id`, `_source.text`, policy_field, policy_score_norm, policy_score)


## Check unclassified tweets
unclassified_documents <- tweets %>% anti_join(classified_documents, by = join_by(`_id` == doc_id))


## some data overviews
{classified_documents %>% # average policy_field scores
    summarise(average_score = mean(policy_score_norm),
              .by = policy_field) %>% 
    ggplot(aes(x = policy_field, y = average_score, fill = policy_field)) +
    geom_col() +
    labs(x = "", title = "Mean Policy Field Scores") +
    guides(x = "none", fill = "none") +
    theme_bw()} +
  {classified_documents %>% # median policy_field scores
      summarise(median_score = median(policy_score_norm),
                .by = policy_field) %>% 
      ggplot(aes(x = policy_field, y = median_score, fill = policy_field)) +
      geom_col() +
      labs(x = "", title = "Median Policy Field Scores") +
      guides(x = "none") +
      theme_bw()} +
  {classified_documents %>% # distribution of policy scores in documents
      ggplot(aes(x = policy_score_norm, fill = policy_field)) +
      geom_histogram(bins = 20) +
      guides(fill = "none") +
      labs(title = "Distribution of Policy Field Scores in Documents") +
      theme_bw()} +
  {classified_documents %>% # number of highest-ranking documents per policy field
      slice_max(policy_score_norm, by = doc_id, with_ties = F) %>% 
      summarise(n = n(), .by = policy_field) %>% 
      ggplot(aes(x = policy_field, y = n, fill = policy_field)) +
      geom_col() +
      labs(x = "", title = "Nr Documents with the highest Score per Policy Field") +
      guides(x = "none", fill = "none") +
      theme_bw()} +
  {classified_documents %>% # number of policy_fields in documents
      filter(policy_score_norm > 0) %>% 
      summarise(policy_fields = n(), .by = doc_id) %>% 
      ggplot(aes(x = policy_fields)) +
      geom_histogram(bins = distinct(classified_documents, policy_field) %>% nrow()) +
      labs(title = "Nr of Policy Fields in Documents") +
      theme_bw()} +
  {tibble(n = c(distinct(unclassified_documents, `_id`)%>% nrow(),
                distinct(classified_documents, doc_id) %>% nrow()),
          documents = c("unclassified", "classified")) %>% 
      ggplot(aes(x = documents, y = n, fill = documents)) +
      geom_col() +
      scale_fill_manual(values = c("blue", "red")) +
      guides(fill = "none") +
      labs(title = "Nr of Classified and Unclassified Documents") +
      theme_bw()} +
  plot_layout(ncol = 2, guides = "collect")




#### Re-Calculate the Random Walk #####

## Setting of the initial walks:
# walk_score_normalization = c("seeds","group") # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 
# calculate_means = TRUE # should the means of the score be calculated and displayed? The can also be used for minimum walk_score filtering (see below)
# positive_scores_only = TRUE # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization
# walk_score = 0.5 # cutoff value for normalized random walk score. Non-Null Values require the selection of a measure to filter on if more than one walk_score_normalization method is picked
# walk_score_measure = "seeds" # filtering applied on normalized seed scores 
# walk_score_quantile = FALSE # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value. Cutoff values may differ from group to group
# report_quantiles = TRUE # should the quantiles of the selected walk_score_measure for each group be printed out? This is independent of walk_score_quantiles and calculated before walk_score filtering
# keep_seed_terms = TRUE # should seed terms within the policy field of the same period always be kept, regardless of walk score?
# seedterm_value = NULL # Seed Term values were not overwritten, i.e. are displayed as their actual result


# Random Walk Settings

walk_score_normalization = c("seeds","group") # Should scores be normalized? "seeds" to normalize the scores for each seed walk. "group" to normalize within grouping vars. Set to NULL for no normalization. 

calculate_means = TRUE # should the means of the score be calculated and displayed? The can also be used for minimum walk_score filtering (see below)

positive_scores_only = TRUE # should negative Walk Scores (i.e. very unlikely connection due to negative weights) and 0 scores be dropped? Applied before normalization

walk_score = 0.5 # cutoff value for normalized random walk score. Non-Null Values require the selection of a measure to filter on if more than one walk_score_normalization method is picked

walk_score_measure = "seeds" # value to apply the walk_score filter on. 
    # Possible are: "default" (auto-pick), "raw" (non-normalized rwr score), "seeds" (seed normalized), "seeds_mean" (mean of seed normalized), "group" (group normalized), "group_mean" (mean of group normalized). Needs to be specified if more than one 

walk_score_quantile = FALSE # Should the quantile be calculated as a dynamic minimum walk_score for each group? If TRUE, walk_score specifies the quantile, rather than a fixed value. Cutoff values may differ from group to group

report_quantiles = TRUE # should the quantiles of the selected walk_score_measure for each group be printed out? This is independent of walk_score_quantiles and calculated before walk_score filtering

keep_seed_terms = TRUE # should seed terms within the policy field of the same period always be kept, regardless of walk score?



## Prepare Seed Terms (this only needs to be done once) ##
# read seed terms

seed_terms_ministries <- vroom("init_classification/seed_terms_ministries.csv.tar.gz")

seed_terms_committees <- vroom("init_classification/seed_terms_committees.csv.tar.gz") 

seed_terms_committee_members <- vroom("init_classification/seed_terms_committee_members.csv.tar.gz") 


# Make Seedlist

seeds <- rbindlist(list(seed_terms_ministries, # bind seed terms of subsets together...
                        seed_terms_committees),
                   fill = TRUE) %>% 
  anti_join(seed_terms_committee_members, by = join_by(feature, period, committee)) %>% # ...drop seed terms prevalent for single committee members...
  distinct(feature, policy_field, period) %>% # ...drop features duplicated within policy fields (from committees etc)...
  split(.$policy_field)                        # ... and split by policy field




# Compute Random Walks

{
  plan(multisession, workers = 12) # set up multisession for faster random walk calculations
  
  walk_terms <- readRDS(file.path("init_classification/walk_network_data", paste0(rwr_timeframe, ".RDS"))) %>% 
    get_rwr_terms(network_name = rwr_timeframe,
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
                  seedterm_value = NULL, # should the actual value of seed terms be overwritten by a default value, e.g. 1?
                  progress = F)
  
  
  gc(verbose = FALSE) # clear memory
  
  plan(sequential) # end multisession
}




