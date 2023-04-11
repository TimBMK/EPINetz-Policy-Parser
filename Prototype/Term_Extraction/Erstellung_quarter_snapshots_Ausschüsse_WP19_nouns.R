#Clean working space
  rm(list = ls())
  
##Filter timelines by committee
  
#Install/Load packages

if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)

if(!require("fuzzyjoin")) install.packages("fuzzyjoin")
  library(fuzzyjoin)

if(!require("tidyr")) install.packages("tidyr")
  library(tidyr)

if(!require("readr")) install.packages("readr")
  library(readr)

#Read timelines
setwd("/data/EPINetz/IssueAttention")
timelines <- readRDS("data_graph_twitter.RDS")

#Save quarter snapshots
setwd("~/Projekte/Policy_Parser/nouns_quarter_snapshots")

timelines_2019_Q1 <- timelines %>% 
  filter(created_at < '2019-03-31') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2019_Q1
save(timelines_snapshot, file=paste0('timelines_2019_Q1.RDA'))

timelines_2019_Q2 <- timelines %>% 
  filter(created_at > '2019-04-01' & created_at < '2019-06-30') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2019_Q2
save(timelines_snapshot, file=paste0('timelines_2019_Q2.RDA'))

timelines_2019_Q3 <- timelines %>% 
  filter(created_at > '2019-07-01' & created_at < '2019-09-30') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2019_Q3
save(timelines_snapshot, file=paste0('timelines_2019_Q3.RDA'))

timelines_2019_Q4 <- timelines %>% 
  filter(created_at > '2019-10-01' & created_at < '2019-12-31') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2019_Q4
save(timelines_snapshot, file=paste0('timelines_2019_Q4.RDA'))

timelines_2020_Q1 <- timelines %>% 
  filter(created_at > '2020-01-01' & created_at < '2020-03-31') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2020_Q1
save(timelines_snapshot, file=paste0('timelines_2020_Q1.RDA'))

timelines_2020_Q2 <- timelines %>% 
  filter(created_at > '2020-04-01' & created_at < '2020-06-30') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2020_Q2
save(timelines_snapshot, file=paste0('timelines_2020_Q2.RDA'))

timelines_2020_Q3 <- timelines %>% 
  filter(created_at > '2020-07-01' & created_at < '2020-09-30') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2020_Q3
save(timelines_snapshot, file=paste0('timelines_2020_Q3.RDA'))

timelines_2020_Q4 <- timelines %>% 
  filter(created_at > '2020-10-01' & created_at < '2020-12-31') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2020_Q4
save(timelines_snapshot, file=paste0('timelines_2020_Q4.RDA'))

timelines_2021_Q1 <- timelines %>% 
  filter(created_at > '2021-01-01' & created_at < '2021-03-31') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()
timelines_snapshot <- timelines_2021_Q1
save(timelines_snapshot, file=paste0('timelines_2021_Q1.RDA'))


