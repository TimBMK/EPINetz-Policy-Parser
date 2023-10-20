# Pulling Samples for the Policy Parser Evaluation with manually-coded data

{
  library(tidyverse)
  library(vroom)
  library(data.table)
  library(openxlsx)
}

seed_sample <- function(data, # data
                        seed, # seed
                        ... # arguments to pass to slice_sample()
                        ) { # slice_sample needs the seed specified before each run (?!), so we build a small wrapper function
  set.seed(seed)
  res <- data %>% slice_sample(...)
  return(res)
}

seed <- 20231017
set.seed(seed)


## Read data
news_data <- list.files("news_classification/data", full.names = T) %>% 
  map(\(file) vroom(file)) %>% rbindlist(fill = TRUE)

tweet_data <- vroom(file = "Tokenizer/data_init_tweets_2023-06-22.csv.tar.gz",
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
                    ), guess_max = 10000)

tweet_data <- tweet_data %>% mutate(is_retweet = case_when(
  str_detect(`_source.text`, "^RT") ~ TRUE,
  .default = FALSE)) %>% 
  mutate(week = ceiling_date(`_source.created_at`, unit = "weeks")) # add week indicator

tweet_data <- tweet_data %>% filter(!is_retweet & !is_reply) # drop replies and retweets for the samples
            # (replies were not classified, retweets need not be classified)


tweet_classification <- readRDS("init_classification/init_classified_tweets.RDS")

tweets_highest <- tweet_classification %>%  # reduce to highest score per document to determine its policy field
  imap(\(week, date) 
       {week %>% .[["classified_documents"]]} %>% 
         slice_max(score_norm, by = doc_id) %>% 
         mutate(week = as_date(date))) %>% 
  rbindlist() %>% 
  rename(`_id` = doc_id) %>% # add variables
  left_join(tweet_data %>% 
              select(`_id`, `_source.created_at`, 
                     `_source.real_name`, `_source.text`),
            by = join_by(`_id`)) 


## Pull Samples

### Twitter 
#### 1000 tweets per coder, 300 tweets intercoder sample = 2.400 total tweets classified (~0.01%) with 12.5% of tweets coded by all coders
#### 60% of sampled data are pulled over the timeframes (weeks), 40% are pulled over tweets associated with policy fields (highest association) to not underrepresent small fields

tweet_sample_1 <- 
  tweet_data %>% # temporal sample
  seed_sample(seed, n = 2, by = week)  %>% 
  mutate(sample = "temporal") %>% 
  bind_rows(
    tweets_highest %>% # policy field sample
      filter(!(`_id` %in% (tweet_data %>% # not in temporal sample
                 seed_sample(seed, n = 2, by = week) %>% pull(`_id`)))) %>% 
      seed_sample(seed, n = 24, by = policy_field) %>% 
      mutate(sample = "policy")
  ) %>% 
  select(`_id`, `_source.created_at`, `_source.real_name`, `_source.text`, sample)

tweet_sample_1 %>% summarise(n = n(), .by = sample) # a roughly 60/40 ratio

intercoder_sample <- tweet_sample_1 %>% # make intercoder sample
  seed_sample(seed, n = 150, by = sample) # .. with even amounts of temporal and policy sourced tweets

tweet_sample_2 <-  tweet_data %>% # temporal sample
  filter(!(`_id` %in% tweet_sample_1$`_id`)) %>% # not in previous samples
  seed_sample(seed, n = 2, by = week)  %>% 
  mutate(sample = "temporal") %>% 
  bind_rows(
    tweets_highest %>% # policy field sample
      filter(!(`_id` %in% tweet_sample_1$`_id`), # not in previous samples
             !(`_id` %in% (tweet_data %>% # not in temporal sample
                             seed_sample(seed, n = 2, by = week) %>% pull(`_id`)))) %>% 
      seed_sample(seed, n = 24, by = policy_field) %>% 
      mutate(sample = "policy")
  ) %>% 
  seed_sample(seed, n = 700, weight_by = ifelse(sample == "temporal", 0.6, 0.4)) %>% # pull 700 of the 1000 documents, maintaining the 60/50 ratio
  bind_rows(intercoder_sample) %>% 
  select(`_id`, `_source.created_at`, `_source.real_name`, `_source.text`, sample)

tweet_sample_2 %>% summarise(n = n(), .by = sample)

tweet_sample_3 <- tweet_data %>% # temporal sample
  filter(!(`_id` %in% tweet_sample_1$`_id`), # not in previous samples
         !(`_id` %in% tweet_sample_2$`_id`)) %>% 
  seed_sample(seed, n = 2, by = week)  %>% 
  mutate(sample = "temporal") %>% 
  bind_rows(
    tweets_highest %>% # policy field sample
      filter(!(`_id` %in% tweet_sample_1$`_id`), # not in previous samples
             !(`_id` %in% tweet_sample_2$`_id`),
             !(`_id` %in% (tweet_data %>% # not in temporal sample
                             seed_sample(seed, n = 2, by = week) %>% pull(`_id`)))) %>% 
      seed_sample(seed, n = 24, by = policy_field) %>% 
      mutate(sample = "policy")
  ) %>% 
  seed_sample(seed, n = 700, weight_by = ifelse(sample == "temporal", 0.6, 0.4)) %>% # pull 700 of the 1000 documents, maintaining the 60/50 ratio
  bind_rows(intercoder_sample) %>% 
  select(`_id`, `_source.created_at`, `_source.real_name`, `_source.text`, sample)

tweet_sample_3 %>% summarise(n = n(), .by = sample)

#### export samples

export_sample <- function(data, filename) {
  data[c(unique(tweets_highest$policy_field), "none")] <- NA # add empty columns for policy indicators
  data %>% select(!sample) %>% # drop sample indicator
    arrange(`_source.created_at`) %>% # order by date
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% # utf8 conversion
    write.xlsx(file = filename)
}

export_sample(intercoder_sample, "evaluation_samples/twitter_sample_intercoder.xlsx")

export_sample(tweet_sample_1, "evaluation_samples/twitter_sample_1.xlsx")

export_sample(tweet_sample_2, "evaluation_samples/twitter_sample_2.xlsx")

export_sample(tweet_sample_3, "evaluation_samples/twitter_sample_3.xlsx")
