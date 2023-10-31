library(tidyverse)
library(vroom)
library(data.table)


# Twitter

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

news_data <- list.files("news_classification/data", full.names = T) %>% 
  map(\(file) vroom(file)) %>% rbindlist(fill = TRUE)

tweet_data <- tweet_data %>% mutate(is_retweet = case_when(
  str_detect(`_source.text`, "^RT") ~ TRUE,
  .default = FALSE))

tweet_data %>% nrow()
tweet_data %>% summarise(n = n(), .by = is_retweet)
tweet_data %>% summarise(n = n(), .by = is_reply)
tweet_data %>% filter(!is_reply & !is_retweet) %>% nrow()

tweet_data %>% 
  mutate(week = ceiling_date(`_source.created_at`, unit = "weeks")) %>% 
  summarise(documents = n(), source = "tweets", .by = week) %>% 
  add_row(
    news_data %>% 
      mutate(week = ceiling_date(`_source.estimated_date`, unit = "weeks")) %>% 
      summarise(documents = n(), source = "news", .by = week)
    ) %>% 
  ggplot(aes(y = documents, x = week, color = source)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Number of Documents over Time") +
  theme_bw()



tweet_classification <- readRDS("init_classification/init_classified_tweets.RDS")

# reduce to highest score per document to determine its policy field
tweets_classified <- tweet_classification %>% 
  imap(\(week, date) 
       {week %>% .[["classified_documents"]]} %>% 
        slice_max(score_norm, by = doc_id) %>% 
         mutate(week = date)) %>% 
  rbindlist()

tweets_classified %>% 
  summarise(tweets = n(), .by = policy_field) %>% 
  ggplot(aes(x = policy_field, y = tweets, fill = policy_field)) +
  geom_col() +
  labs(title = "Number of Documents most highly associated with each Policy Field",
       x = NULL) +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# News

news_data <- list.files(file.path("news_classification", "data"), 
                        "data_news", full.names = T) %>% 
  map(\(file) vroom(file)) %>% rbindlist(fill = T)

news_data <- news_data %>% 
  mutate(outlet = str_remove_all(`_source.host`,
                                 paste0(c("^www\\.", "\\.de$", "\\.net$", 
                                          "\\.co.uk$", "\\.com$"), 
                                        collapse = "|")))

news_data %>% summarise(n = n(), 
                        start = min(`_source.estimated_date`), 
                        end = max(`_source.estimated_date`), 
                        .by = outlet)
  # only faz, welt, bild and spiegel for the full time period
  # english outlets neglectable (and only on a single day)

news_data %>% filter(outlet == "faz" | outlet == "welt" | 
                       outlet == "bild" | outlet == "spiegel") %>% 
  nrow()

news_data %>% filter(outlet == "faz" | outlet == "welt" | 
                       outlet == "bild" | outlet == "spiegel") %>% 
  summarise(n = n(), .by = outlet)

news_data %>% 
  summarise(n = n(), .by = outlet) %>% 
  filter(n > 10) 

news_data %>% 
  summarise(n = n(), .by = outlet) %>% 
  filter(n > 10) %>% 
  summarise(total = sum(n))
