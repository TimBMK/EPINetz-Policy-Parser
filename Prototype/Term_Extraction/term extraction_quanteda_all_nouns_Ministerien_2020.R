#Clean working space
  rm(list=ls())

### Termextraktion mit quanteda

if(!require("quanteda")) install.packages("quanteda")
  library(quanteda)
if(!require("quanteda.textstats")) install.packages("quanteda.textstats")
  library(quanteda.textstats)
if(!require("quanteda.textplots")) install.packages("quanteda.textplots")
  library(quanteda.textplots)
if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
if(!require("stringi")) install.packages("stringi")
  library(stringi)
if(!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
if(!require("readr")) install.packages("readr")
  library(readr)
  
## load data into df
  
folder_loc <- "/data/wolfeswenker/Projekte/Policy_Parser/nouns_Ministerien_year/2020/RDA/"
files <- paste0(folder_loc,list.files(folder_loc))[1:26]

names <- gsub('_neg.RDA', '', files)
names <- gsub('/data/wolfeswenker/Projekte/Policy_Parser/nouns_Ministerien_year/2020/RDA/', '', names)

setwd("/data/wolfeswenker/Projekte/Policy_Parser/Term_Extraction_nouns_Ministerien_year")
i=0

while (i <26) {
  i<-i+1
  j<-i+1
  load(files[j])
  load(files[i])
  
  timelines_filtered_pos$key<-'key'
  timelines_filtered_neg$key<-'ref'
  
  timelines_filtered_neg <- timelines_filtered_neg %>% filter(!doc_id %in% timelines_filtered_pos$doc_id)
  
  ## prepare data for corpus creation
  timelines_filtered_pos<-distinct(timelines_filtered_pos)
  timelines_filtered_neg<-distinct(timelines_filtered_neg)
  timelines_filtered_pos$lemmas <- tolower(timelines_filtered_pos$lemmas)
  timelines_filtered_neg$lemmas <- tolower(timelines_filtered_neg$lemmas)
  timelines_filtered_pos$lemmas <- gsub("@[a-z0-9_]{2,15}", "", timelines_filtered_pos$lemmas) # this removes all @-mentions which otherwise dominate keywords, de-select if unnecessary (for bith corpora!!)
  timelines_filtered_neg$lemmas <- gsub("@[a-z0-9_]{2,15}", "", timelines_filtered_neg$lemmas)
  timelines_filtered_pos$lemmas <- stri_encode(timelines_filtered_pos$lemmas, "", "UTF-8") # re-mark encodings
  timelines_filtered_neg$lemmas <- stri_encode(timelines_filtered_neg$lemmas, "", "UTF-8") # re-mark encodings
  
  corpus_key <- corpus(timelines_filtered_pos, docid_field = 'doc_id', text_field = 'lemmas')
  
  corpus_ref <- corpus(timelines_filtered_neg, docid_field = 'doc_id', text_field = 'lemmas')
  
  corpus_key<- corpus_key+corpus_ref
  
  corpus_key_t<-tokens(corpus_key)
  
  dfm_key <- dfm(corpus_key_t, remove = c(stopwords("german"), stopwords("english")),
                 remove_punct = TRUE, remove_symbols = TRUE,
                 remove_numbers = TRUE,  remove_url = TRUE)
  
  dfm_key <- dfm_group(dfm_key, dfm_key$key)
  
  tstat_key <- textstat_keyness(dfm_key,
                                target = "key")
  save(tstat_key, file=paste0('tstat_',names[i],'.RDA'))
  write_csv2(tstat_key, file=paste0('tstat',names[i],'.CSV'))
  
  tplot_key <- textplot_keyness(tstat_key,
                                margin = 0.2,
                                n = 20)
  
  ggsave(paste0('plot', names[i],'.pdf'), width = 10, height = 8)
  
  i<-j
}
