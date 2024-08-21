
{
  library(tidyverse)
  library(vroom)
  library(data.table)
  library(openxlsx)
  library(scales)
  library(caret)
}


# Read results

## Twitter

### Classification

tweet_classification <- readRDS("init_classification/init_classified_tweets.RDS")


### Manual Coding

twitter_sample_1_eval <- read.xlsx("evaluation_samples/CAP/evaluated/twitter_sample_1.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_1_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 1) 

twitter_sample_2_eval <- read_csv("evaluation_samples/CAP/evaluated/twitter_sample_2.csv",
                                  col_types = list(`_id` = "c")) %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  # bind_rows(read_csv("evaluation_samples/evaluated/twitter_sample_2_missing.csv",
  #                    col_types = list(`_id` = "c"))) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`, `...24`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 2)

twitter_sample_3_eval <- read.xlsx("evaluation_samples/CAP/evaluated/twitter_sample_3.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_3_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 3) 

## recoded samples
twitter_sample_recode_1 <- read.xlsx("evaluation_samples/CAP/evaluated/twitter_recode_sample_1.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_1_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 1) 

twitter_sample_1_eval <- twitter_sample_1_eval %>% # replace old evaluation with recoding
  filter(!(`_id` %in% twitter_sample_recode_1$`_id`)) %>% 
  bind_rows(twitter_sample_recode_1)


twitter_sample_recode_2 <- read_csv("evaluation_samples/CAP/evaluated/twitter_recode_sample_2.csv",
                                    col_types = list(`_id` = "c")) %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  # bind_rows(read_csv("evaluation_samples/evaluated/twitter_sample_2_missing.csv",
  #                    col_types = list(`_id` = "c"))) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 2)

twitter_sample_2_eval <- twitter_sample_2_eval %>% # replace old evaluation with recoding
  filter(!(`_id` %in% twitter_sample_recode_2$`_id`)) %>% 
  bind_rows(twitter_sample_recode_2) %>% 
  mutate(intercoder_sample = case_when(intercoder_sample == 1 ~ TRUE, # recode intercoder var
                                       intercoder_sample == 0 ~ FALSE))


twitter_sample_recode_3 <- read.xlsx("evaluation_samples/CAP/evaluated/twitter_recode_sample_3.xlsx") %>% 
  #filter(!is.na(`_source.text`)) %>% # drop unclassified due to missing text
  #bind_rows(read.xlsx("evaluation_samples/evaluated/twitter_sample_3_missing.xlsx")) %>% 
  select(!c(`_source.created_at`, `_source.real_name`, `_source.text`)) %>% # drop unnecessary variables
  mutate(across(!c(`_id`, intercoder_sample), ~ case_when(!is.na(.) ~ TRUE, .default = FALSE))) %>% # TRUE/FALSE indicator
  mutate(coder = 3) 

twitter_sample_3_eval <- twitter_sample_3_eval %>% # replace old evaluation with recoding
  filter(!(`_id` %in% twitter_sample_recode_3$`_id`)) %>% 
  bind_rows(twitter_sample_recode_3)


### process intercoder sample

twitter_intercoder_sample <- twitter_sample_1_eval %>% filter(intercoder_sample) %>% 
  bind_rows(twitter_sample_2_eval %>% filter(intercoder_sample)) %>% 
  bind_rows(twitter_sample_3_eval %>% filter(intercoder_sample)) %>% 
  summarise(across(!c(intercoder_sample, coder), # if any coder rated a category, set to TRUE
                   ~ case_when(any(.) ~ TRUE, .default = FALSE)), 
            intercoder_sample = TRUE, .by = `_id`)

twitter_coding_sample <- bind_rows(twitter_sample_1_eval,
                                   twitter_sample_2_eval,
                                   twitter_sample_3_eval) %>%
  filter(!intercoder_sample) %>% # drop intercoder tweets
  bind_rows(twitter_intercoder_sample) %>% # add processed intercoder sample
  #filter(intercoder_sample) %>% # testing
  filter(rowSums(select(., !c(`_id`, intercoder_sample, coder))) > 0) %>% # drop rows with all FALSE values (uncoded)
  mutate(coder = case_when(intercoder_sample == TRUE ~ "intercoder",
                           .default = as.character(coder))) %>% 
  pivot_longer(!c(`_id`, intercoder_sample, coder), 
               names_to = "policy_field", 
               values_to = "coder_score")
  

# Analyse fit

threshold = 0.7 # set treshold for classifier here (what value should be considered a prediction?)
threshold_quantile = 0.75
n_highest = 3
threshold_method = "threshold" # set to "threshold" (fixed threshold),
                               # "threshold_quantile" (quantile of non-normalized score,
                               # or "highest" (top n categories per doc) to switch between methods

## Twitter

twitter_eval_sample <- tweet_classification %>%
  map(\(dat) dat$classified_documents) %>%
  rbindlist()
if (threshold_method == "threshold_quantile"){
  twitter_eval_sample <- twitter_eval_sample %>% 
    mutate(classification = case_when(
      score >= quantile(score, # set quantile-based threshold classification
                        probs = threshold_quantile)[[1]] ~ TRUE,
      .default = FALSE
    ))
}
twitter_eval_sample <- twitter_eval_sample %>% # drop documents not in the coded sample
  filter(doc_id %in% twitter_coding_sample$`_id`) # some unclassified tweets are missing here, but appear in the manually coded sample

twitter_eval <- twitter_coding_sample %>% 
  rename(doc_id = `_id`) %>% 
  left_join(twitter_eval_sample, by = c("doc_id", "policy_field")) %>% 
  mutate(across(c(score, score_norm), ~ case_when(is.na(.) ~ 0, # fill NA values for unclassified tweets
                                                  .default = .))) %>% 
  mutate(across(c(score, score_norm), ~ case_when(sum(.) == 0 & policy_field == "none" ~ 1, # set none to 1 when no scoring for any policy fields in a doc
                                                  .default = .)), .by = doc_id) 

if (threshold_method == "threshold"){
  twitter_eval <- twitter_eval %>%
    mutate(classification = case_when(score_norm > threshold ~ TRUE, .default = FALSE)) # add T/F classification indicator with threshold
}

if (threshold_method == "highest"){
  twitter_eval <- twitter_eval %>%
    mutate(classification = case_when(rank(desc(score_norm)) <= n_highest ~ TRUE,  # select n highest policy scores in a doc
                                      .default = F), .by = doc_id)
}


## Precision and Recall 

### total
{ cat(paste("Method:", threshold_method, "\n")) 
  confusionMatrix(twitter_eval %>% 
                    pull(classification) %>% as.factor(),
                  twitter_eval %>% pull(coder_score) %>% as.factor(),
                  mode = "everything", positive = "TRUE")}

### full printout by field 
for (field in unique(twitter_eval$policy_field)){
  
  cat("\n\n")
  cat(field)
  cat("\n")
  
  dat <- twitter_eval %>% filter(policy_field == field)
  
  truth <- as.factor(dat$coder_score)
  prediction <- as.factor(dat$classification)
  
  metrics <- confusionMatrix(prediction, truth, 
                             mode = "everything",
                             positive = "TRUE")

  print(metrics)
}

### overview table
twitter_fields <- twitter_eval %>% split(twitter_eval$policy_field) %>% 
  imap(\(dat, field)
      {
        truth <- as.factor(dat$coder_score)
        prediction <- as.factor(dat$classification)
        
        metrics <- confusionMatrix(prediction, truth, 
                                   mode = "everything",
                                   positive = "TRUE")
        
        out <- tibble(policy_field = field,
                      metric = names(metrics$overall),
                      value = metrics$overall) %>% 
          bind_rows(tibble(policy_field = field,
                           metric = names(metrics$byClass),
                           value = metrics$byClass))
        
        return(out)
  }) %>% 
  rbindlist()

twitter_fields %>% pivot_wider(names_from = policy_field, 
                               values_from = value)
  
twitter_fields %>% filter(metric == "F1" & !is.na(value)) %>% pull(value) %>% mean() 

# for (field in unique(twitter_eval$policy_field)) {
#   cat(paste("\n\n", field, "\n"))
#   twitter_eval %>% filter(policy_field == field) %>% 
#     pull(score) %>% quantile() %>% print()
# }

## Other evaluations

twitter_eval %>% summarise(mean_score = mean(score_norm), 
                           .by = c(policy_field, coder_score)) %>% 
  ggplot(aes(x = coder_score, y = mean_score)) +
  geom_col() +
  facet_wrap(vars(policy_field)) +
  labs(title = "Mean Scores of Policy Classification over Manual Coding, by Policy Field")

twitter_eval %>% 
  ggplot(aes(x = coder_score, y = score_norm)) +
  geom_point() +
  facet_wrap(vars(policy_field)) +
  labs(title = "Scores of Policy Classification over Manual Coding, by Policy Field")

twitter_eval %>% 
  ggplot(aes(x = policy_field, y = score_norm, color = coder_score)) +
  geom_point() +
  labs(title = "Scores of Policy Classification over Policy Field")


### number of overlapping categories (for threshold_method = "highest")
twitter_eval %>% 
  summarise(overlap = sum(coder_score & classification), .by = doc_id) %>% 
  ggplot(aes(x = overlap)) +
  geom_histogram()

twitter_eval %>% 
  summarise(overlap = sum(coder_score & classification), .by = doc_id) %>% 
  summarise(percentage = n() / nrow(.), .by = overlap)
