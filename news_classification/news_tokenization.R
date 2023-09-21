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

data_files <- list.files("news_classification/data", pattern = "data_news", full.names = T)

# drop data already tokenized
tokenized <- list.files("news_classification", pattern = "tokens_news", full.names = T)

data_files <- data_files[!str_extract(data_files, "[\\d-]+") %in% str_extract(tokenized, "[\\d-]+")]

news_data <- data_files %>% 
  map(\(file)
      {
        data <- vroom(file = file) %>%
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
        return(data)
  })

names(news_data) <- str_extract(data_files, "[\\d-]+") # name the dataframes in the list

cat(paste("\nFound data", names(news_data), "to tokenize.\n"))

# Tokenization, Lemmatization, Noun-Word Filtering


cat("\nRunning Mapped Spacy....\n")

spacy_initialize(model = "de_core_news_lg") # start python spacy

news_data %>% 
  iwalk(\(data, name)
        {
          data %>% 
            corpus(docid_field = "id", text_field = "body", 
                   unique_docnames = T) %>% 
            spacy_parse(pos = T,
                        tag = T,
                        lemma = T,
                        entity = T,
                        nounphrase = T,
                        additional_attributes = c("like_url", "like_num", "is_currency")
            ) %>% 
            mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
            vroom_write(file = paste0("news_classification/tokens_news_", name,".csv.tar.gz"), delim = ",")
  })


cat("\nTokenization complete. Finalizing spacy...\n")

spacy_finalize() # end spacy
