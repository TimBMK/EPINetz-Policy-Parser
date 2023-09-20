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

cat("\nReading Data...\n")

news_data <- vroom(file = "news_classification/data/data_news_2017-2018.csv.tar.gz") %>%
  select(
    # drop unnecessary columns
    where(~ !all(is.na(.x))) &  # drop columns containing only NAs
      !c(
        "_index",
        "_type",
        "_score",
        "_source.nlp_pipeline_applied",     # drop db internal information
        "_source.lang",
        "_source.publication_date",
        "_source.crawled",
        "_source.extracted"
      ) # drop redundant information (estimated date is the only important date)
  ) %>%
  rename_with( ~ str_remove(.x, pattern = "_source.")) %>%  # drop "_source." from variable names
  rename("id" = "_id") # drop "_" from id

# Tokenization, Lemmatization, Noun-Word Filtering

cat("\nPreparing Data...\n")

news_corpus <- corpus(news_data, docid_field = "id", text_field = "body",
                      meta = list("host", "estimated_date", "url"), # preserve meta data (drop summary column)
                      unique_docnames = T)


cat("\nRunning Spacy....\n")

spacy_initialize(model = "de_core_news_lg") # start python spacy

news_tokens_201718 <- spacy_parse(news_corpus, 
                                  pos = T, tag = T, lemma = T, 
                                  entity = T, nounphrase = T,
                                  additional_attributes = c("like_url", "like_num", "is_currency"))


cat("\nTokenization complete. Finalizing spacy...\n")

spacy_finalize() # end spacy



# save

cat("\nSaving...\n")

news_tokens_201718 %>% 
  mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  vroom_write(file = "news_classification/tokens_news_2017-2018.csv.tar.gz", delim = ",")


cat("\nDone.")
