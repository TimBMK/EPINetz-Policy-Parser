library(tidyverse)
library(vroom)
library(spacyr)
library(quanteda)

walk_tweets <-
  vroom(
    file = "init_classification/data_init_walk_2023-04-10.csv.tar.gz",
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


walk_corpus <- corpus(walk_tweets, docid_field = "_id", text_field = "_source.text", 
                      meta = list(names(walk_tweets)), # preserve all vars as metadata
                      unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

walk_tokens <-
  spacy_parse(
    walk_corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

spacy_finalize() # end spacy

saveRDS(walk_tokens, "init_classification/tokens_init_walk_2023-04-10.RDS")
walk_tokens %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% vroom_write(file = "init_classification/tokens_init_walk_2023-04-10.csv.tar.gz", delim = ",")

