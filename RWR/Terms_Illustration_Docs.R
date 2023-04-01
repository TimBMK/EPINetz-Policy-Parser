#Clean working space
rm(list = ls())

## Prep ####

if(!require("tidyverse")) install.packages("tidyverse")
  library(tidyverse)
if(!require("vroom")) install.packages("vroom")
  library(vroom)

list_cols <- # columns that were originally lists, but needed to be flattened for .csv export
  c(
    "hashtags",
    "ext_urls",
    "ext_urls_expanded",
    "ext_urls_unwound",
    "ext_urls_title",
    "ext_urls_description",
    "context.domain.id",
    "context.domain.name",
    "context.domain.description",
    "context.entity.id",
    "context.entity.name",
    "context.entity.description",
    "mentions_username",
    "mentions_user_id",
    "annotation_probability",
    "annotation_type",            
    "annotation_entity"
  )

setwd("/data/EPINetz/IssueAttention/")

twitter_data <-
  vroom(
    file = "data_twitter_2019-2021.csv.tar.gz",
    # Important! specify coltypes to preserve correct IDs
    col_types = list(
      tweet_id = "c",
      conversation_id = "c",
      author_id = "c",
      in_reply_to_user_id = "c",
      user_pinned_tweet_id = "c",
      sourcetweet_id = "c",
      sourcetweet_author_id = "c",
      hashtags = "c"
    )
  ) %>%
  # transform flattened columns into lists again
  mutate(across(.cols = any_of(list_cols), ~ ifelse(
    is.na(.x), NA, str_split(.x, pattern = ", ") %>% as.list()
  )))

#Adapt snapshot
twitter_data_2019_Q1 <- twitter_data %>% filter(created_at < '2019-03-31')
twitter_data_2019_Q2 <- twitter_data %>% filter(created_at > '2019-04-01' & created_at < '2019-06-30')

twitter_NE <- vroom("data_graph_twitter.csv.tar.gz",
                    col_types = list(doc_id = "c", author_id = "c")) %>%
  as_tibble()

#Adapt snapshot
twitter_NE_2019_Q1 <- twitter_NE %>% filter(created_at < '2019-03-31')
twitter_NE_2019_Q2 <- twitter_NE %>% filter(created_at > '2019-04-01' & created_at < '2019-06-30')

#load("/data/koenigt/terms_list_äußeres.RDA") 

###### Äußeres

terms <- terms_äußeres_sum %>% left_join(twitter_NE_2019_Q2 %>% select(doc_id, lemma) %>% 
                                           filter(lemma %in% (terms_äußeres_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                         by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 10) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q2, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen

###### Entwicklung

terms <- terms_entwicklung_sum %>% left_join(twitter_NE_2019_Q1 %>% select(doc_id, lemma) %>% 
                                           filter(lemma %in% (terms_entwicklung_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                         by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 20) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q1, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen

###### Bildung & Forschung

terms <- terms_bildung_forschung_sum %>% left_join(twitter_NE_2019_Q1 %>% select(doc_id, lemma) %>% 
                                               filter(lemma %in% (terms_bildung_forschung_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                             by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 20) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q1, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen

###### Digitalisierung & Technik

terms <- terms_digitalisierung_technik_sum %>% left_join(twitter_NE_2019_Q1 %>% select(doc_id, lemma) %>% 
                                                     filter(lemma %in% (terms_digitalisierung_technik_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                                   by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 20) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q1, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen

###### Gesundheit

terms <- terms_gesundheit_sum %>% left_join(twitter_NE_2019_Q1 %>% select(doc_id, lemma) %>% 
                                                           filter(lemma %in% (terms_gesundheit_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                                         by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 10) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q1, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen

###### Landwirtschaft & Ernährung

terms <- terms_landwirtschaft_ernährung_sum %>% left_join(twitter_NE_2019_Q2 %>% select(doc_id, lemma) %>% 
                                              filter(lemma %in% (terms_landwirtschaft_ernährung_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                            by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 11) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q2, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen


###### Umwelt

terms <- terms_umwelt_sum %>% left_join(twitter_NE_2019_Q2 %>% select(doc_id, lemma) %>% 
                                              filter(lemma %in% (terms_umwelt_sum %>% distinct(NodeNames) %>% pull())), # Filter verringert hier nur bereits die Anzahl an Zeilen, auf die gejoint wird. nicht zwingend nötig, aber etwas performanter
                                            by = c("NodeNames" = "lemma"), multiple = "all") # im Prinzip matchen wir alle Terme in unserem DF mit den Dokumenten, in denen sie vorkommen


doc_scores <- terms %>% group_by(doc_id) %>% # nach doc_id gruppieren
  summarise(sum_score = sum(as.numeric(ScoreNorm2))) %>% # wir fassen mittels summarise() die Gesamtscores aller Terme pro Dokument zusammen. Wir könnten z.B. auch mean() statt sum() verwenden
  ungroup() # degruppieren nicht vergessen!

top_docs <- doc_scores %>% slice_max(sum_score, n = 10) # slice_max gibt uns jetzt die n (hier: 10) Dokumente mit der höchsten Score aus

top_docs_full <- top_docs %>% left_join(twitter_data_2019_Q2, by = c("doc_id" = "tweet_id")) # jetzt können wir einfach auf die vollen Tweets joinen
