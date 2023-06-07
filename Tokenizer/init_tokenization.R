### Initial Tokenization - Tokenizer ###
########################################

### run this once to get a base stock of tokenized tweets

{
  library(tidyverse)
  library(quanteda)
  library(spacyr)
  library(data.table)
  library(vroom)
}

init_tweets <- vroom(file = "Tokenizer/data_init_tweets_2023-05-31.csv.tar.gz",
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
                     ))


# Tokenization, Lemmatization, Noun-Word Filtering

corpus <- corpus(init_tweets, docid_field = "_id", text_field = "_source.text", 
                      meta = list(names(init_tweets)), # preserve all vars as metadata
                      unique_docnames = T) # we could also use the conversation IDs to treat conversations as single documents

spacy_initialize(model = "de_core_news_lg") # start python spacy

init_tokens <-
  spacy_parse(
    corpus,
    pos = T,
    tag = T,
    lemma = T,
    entity = T
  )

spacy_finalize() # end spacy


# add reply indicator, creation date and author account ID
init_tokens <- init_tokens %>% left_join(init_tweets %>% 
                                 distinct(`_id`, is_reply, 
                                          `_source.created_at`, 
                                          `_source.author_id`), 
                               by = join_by(doc_id == `_id`))


# save
init_tokens %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "Tokenizer/tokens_init_2023-05-31.csv.tar.gz", delim = ",")



