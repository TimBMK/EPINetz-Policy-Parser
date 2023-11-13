# Pulling Samples for the Policy Parser Evaluation with manually-coded data

{
  library(tidyverse)
  library(vroom)
  library(data.table)
  library(openxlsx)
  library(scales)
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

### Twitter
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

tweet_data <- tweet_data %>% mutate(is_retweet = case_when( # add RT indicator
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


### News

news_data <- list.files("news_classification/data", full.names = T) %>% 
  map(\(file) vroom(file)) %>% rbindlist(fill = TRUE)

news_data <- news_data %>% # add outlet indicator
  mutate(outlet = str_remove_all(`_source.host`,
                                 paste0(c("^www\\.", "\\.de$", "\\.net$", 
                                          "\\.co.uk$", "\\.com$"), 
                                        collapse = "|"))) %>% 
  mutate(week = ceiling_date(`_source.estimated_date`, unit = "weeks")) # add week indicator

news_data <- news_data %>% # remove a small number of non-german outlets accidentially in the data / only keep select outlets (drops 20 docs)
  filter(outlet %in% c("faz", "welt", "bild", "spiegel", 
                       "zeit", "sueddeutsche", "stuttgarter-zeitung"))

news_classification <- readRDS("news_classification/init_classified_news.RDS")

news_highest <- news_classification %>%  # reduce to highest score per document to determine its policy field
  imap(\(week, date) 
       {week %>% .[["classified_documents"]]} %>% 
         slice_max(score_norm, by = doc_id) %>% 
         mutate(week = as_date(date))) %>% 
  rbindlist() %>% 
  rename(`_id` = doc_id) %>% # add variables
  left_join(news_data %>% 
              select(`_id`, `_source.estimated_date`, 
                     outlet, `_source.body`),
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


twitter_intercoder_sample <- tweet_sample_1 %>% # make intercoder sample
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
  seed_sample(seed, n = 700, weight_by = ifelse(sample == "temporal", 0.6, 0.4)) %>% # pull 700 of the 1000 documents, maintaining the 60/40 ratio
  bind_rows(twitter_intercoder_sample) %>% 
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
  seed_sample(seed, n = 700, weight_by = ifelse(sample == "temporal", 0.6, 0.4)) %>% # pull 700 of the 1000 documents, maintaining the 60/40 ratio
  bind_rows(twitter_intercoder_sample) %>% 
  select(`_id`, `_source.created_at`, `_source.real_name`, `_source.text`, sample)

tweet_sample_3 %>% summarise(n = n(), .by = sample)

#### export samples

export_sample <- function(data, sort_col, filename) {
  data <- as.data.frame(data)
  data[c(unique(tweets_highest$policy_field), "none")] <- NA # add empty columns for policy indicators
  data %>% select(!sample) %>% # drop sample indicator
    arrange(!!as.name(sort_col)) %>% # order by date
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% # utf8 conversion
    write.xlsx(file = filename)
}
export_sample(twitter_intercoder_sample, "_source.created_at", "evaluation_samples/twitter_sample_intercoder.xlsx")

export_sample(tweet_sample_1, "_source.created_at", "evaluation_samples/twitter_sample_1.xlsx")

export_sample(tweet_sample_2, "_source.created_at", "evaluation_samples/twitter_sample_2.xlsx")

export_sample(tweet_sample_3, "_source.created_at", "evaluation_samples/twitter_sample_3.xlsx")





### News
#### 437 articles per coder, 129 docs intercoder sample = 1.049 total docs classified (~0.001%) with 12.5% of docs coded by all coders
#### 66% of sampled data are pulled over the timeframes (weeks), 
####  20% are pulled over docs associated with policy fields (highest association) to not underrepresent small fields,
####  14% are pulled over outlet, to represent all outlets

news_sample_1 <- 
  news_data %>% # temporal sample
  seed_sample(seed, n = 1, by = week)  %>% 
  mutate(sample = "temporal") %>% 
  bind_rows( # policy field sample
    news_highest %>% 
      filter(!(`_id` %in% (news_data %>% # not in temporal sample
                             seed_sample(seed, n = 1, by = week) %>% pull(`_id`)))) %>% 
      seed_sample(seed, n = 5, by = policy_field) %>% 
      mutate(sample = "policy")
  ) %>% 
  bind_rows( # outlet sample
    news_data %>% 
      filter(!(`_id` %in% (news_data %>% # not in temporal sample
                             seed_sample(seed, n = 1, by = week) %>% 
                             pull(`_id`)))) %>% 
      filter(!(`_id` %in% (news_highest %>% # not in policy sample
                             filter(!(`_id` %in% (news_data %>% 
                                                    seed_sample(seed, n = 1, 
                                                                by = week) %>% 
                                                    pull(`_id`)))) %>% 
                             seed_sample(seed, n = 5, by = policy_field)))) %>% 
      seed_sample(seed, n = 9, by = outlet) %>% 
      mutate(sample = "outlet")
  ) %>% 
  select(`_id`, `_source.estimated_date`, outlet, `_source.body`, sample)

news_sample_1 %>% summarise(n = n(), .by = sample) %>% 
  mutate(percent = percent(n/sum(n), accuracy = 0.01)) # a roughly 63/19/18 ratio


news_intercoder_sample <- news_sample_1 %>% # make intercoder sample
  seed_sample(seed, n = 43, by = sample) # .. with even amounts of temporal and policy sourced tweets


news_sample_2 <-  news_data %>% # temporal sample
  filter(!(`_id` %in% news_sample_1$`_id`)) %>% # not in previous samples
  seed_sample(seed, n = 1, by = week)  %>% 
  mutate(sample = "temporal") %>% 
  bind_rows(
    news_highest %>% # policy field sample
      filter(!(`_id` %in% news_sample_1$`_id`)) %>%  # not in previous samples
      filter(!(`_id` %in% (news_data %>% # not in temporal sample
                             seed_sample(seed, n = 1, by = week) %>% 
                             pull(`_id`)))) %>% 
      seed_sample(seed, n = 5, by = policy_field) %>% 
      mutate(sample = "policy")
  ) %>% 
  bind_rows( # outlet sample
    news_data %>% 
      filter(!(`_id` %in% news_sample_1$`_id`)) %>%  # not in previous samples
      filter(!(`_id` %in% (news_data %>% # not in temporal sample
                             seed_sample(seed, n = 1, by = week) %>% 
                             pull(`_id`)))) %>% 
      filter(!(`_id` %in% (news_highest %>% # not in policy sample
                             filter(!(`_id` %in% (news_data %>% 
                                                    seed_sample(seed, n = 1, 
                                                                by = week) %>% 
                                                    pull(`_id`)))) %>% 
                             seed_sample(seed, n = 5, by = policy_field)))) %>% 
      seed_sample(seed, n = 9, by = outlet) %>% 
      mutate(sample = "outlet")
  ) %>% 
  seed_sample(seed, n = 308, weight_by = ifelse(sample == "temporal", 0.66, 
                                                ifelse(sample == "policy", 0.20, 0.14))) %>% # pull 700 of the 1000 documents, maintaining the 60/40 ratio
  bind_rows(news_intercoder_sample) %>% 
  select(`_id`, `_source.estimated_date`, outlet, `_source.body`, sample)

news_sample_2 %>% summarise(n = n(), .by = sample) %>% 
  mutate(percent = percent(n/sum(n), accuracy = 0.01)) # a roughly 66/19/15 ratio


news_sample_3 <-  news_data %>% # temporal sample
  filter(!(`_id` %in% news_sample_1$`_id`), # not in previous samples
         !(`_id` %in% news_sample_2$`_id`)) %>% 
  seed_sample(seed, n = 1, by = week)  %>% 
  mutate(sample = "temporal") %>% 
  bind_rows(
    news_highest %>% # policy field sample
      filter(!(`_id` %in% news_sample_1$`_id`), # not in previous samples
             !(`_id` %in% news_sample_2$`_id`)) %>% 
      filter(!(`_id` %in% (news_data %>% # not in temporal sample
                             seed_sample(seed, n = 1, by = week) %>% 
                             pull(`_id`)))) %>% 
      seed_sample(seed, n = 5, by = policy_field) %>% 
      mutate(sample = "policy")
  ) %>% 
  bind_rows( # outlet sample
    news_data %>% 
      filter(!(`_id` %in% news_sample_1$`_id`), # not in previous samples
             !(`_id` %in% news_sample_2$`_id`)) %>% 
      filter(!(`_id` %in% (news_data %>% # not in temporal sample
                             seed_sample(seed, n = 1, by = week) %>% 
                             pull(`_id`)))) %>% 
      filter(!(`_id` %in% (news_highest %>% # not in policy sample
                             filter(!(`_id` %in% (news_data %>% 
                                                    seed_sample(seed, n = 1, 
                                                                by = week) %>% 
                                                    pull(`_id`)))) %>% 
                             seed_sample(seed, n = 5, by = policy_field)))) %>% 
      seed_sample(seed, n = 9, by = outlet) %>% 
      mutate(sample = "outlet")
  ) %>% 
  seed_sample(seed, n = 308, weight_by = ifelse(sample == "temporal", 0.66, 
                                                ifelse(sample == "policy", 0.20, 0.14))) %>% # pull 700 of the 1000 documents, maintaining the 60/40 ratio
  bind_rows(news_intercoder_sample) %>% 
  select(`_id`, `_source.estimated_date`, outlet, `_source.body`, sample)

news_sample_3 %>% summarise(n = n(), .by = sample) %>% 
  mutate(percent = percent(n/sum(n), accuracy = 0.01)) # a roughly 66/19/15 ratio


#### export samples

export_sample <- function(data, sort_col, filename) {
  data <- as.data.frame(data)
  data[c(unique(news_highest$policy_field), "none")] <- NA # add empty columns for policy indicators
  data <- data %>% mutate(`_source.body` = case_when( # truncate long documents
    str_length(`_source.body`) > 32767 ~ str_trunc(`_source.body`, 
                                                   width = 32767, 
                                                   side = "right"), 
    .default = `_source.body`))
  data %>% select(!sample) %>% # drop sample indicator
    arrange(!!as.name(sort_col)) %>% # order by date
    mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% # utf8 conversion
    write.xlsx(file = filename)
}

export_sample(news_intercoder_sample, "_source.estimated_date", "evaluation_samples/news_sample_intercoder.xlsx")

export_sample(news_sample_1, "_source.estimated_date", "evaluation_samples/news_sample_1.xlsx")

export_sample(news_sample_2, "_source.estimated_date", "evaluation_samples/news_sample_2.xlsx")

export_sample(news_sample_3, "_source.estimated_date", "evaluation_samples/news_sample_3.xlsx")
