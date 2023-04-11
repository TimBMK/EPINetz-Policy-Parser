#Clean working space
  rm(list=ls())
  
##Filter timelines by ministry
  
#Install/Load packages
  
if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
if(!require("fuzzyjoin")) install.packages("fuzzyjoin")
  library(fuzzyjoin)
  
if(!require("tidyr")) install.packages("tidyr")
  library(tidyr)
  
if(!require("readr")) install.packages("readr")
  library(readr)
  
if(!require("openxlsx")) install.packages("openxlsx")
  library(openxlsx)

if(!require("vroom")) install.packages("vroom")
  library(vroom)
  
#Read timelines
setwd("/data/EPINetz/IssueAttention")
#timelines_NE <- readRDS("data_graph_twitter.RDS")
timelines_NE <-
  vroom("/data/EPINetz/IssueAttention/data_graph_twitter.csv.tar.gz", # this data uses lemmas rather than tokens!
        col_types = list(doc_id = "c", author_id = "c")) %>%
  as_tibble()
timelines_NE <- timelines_NE %>% filter(created_at < '2021-03-31') %>% group_by(doc_id) %>% summarise(lemmas = paste(lemma, collapse=", "), author_id)
timelines_NE <- distinct(timelines_NE)
timelines_NE["author_id"][timelines_NE["author_id"] == "1121700961125314600"] <- "1121700961125314560"#replace damaged ID (scientific notation) of Bundesministeriums Verteidigung

#Read list of ministries
setwd("/data/wolfeswenker")
ministries <- read.xlsx("ministries_accounts.xlsx")
ministries <- subset(ministries, select = -c(X1))
names(ministries)[names(ministries)=="id"] <- "author_id"

ministries_users <- list()

for (i in 1:15) {
  ministries_users[[i]] <- ministries[i,]
}

ministries_names <- as.list(ministries$username)

setwd("/data/wolfeswenker/Projekte/Policy_Parser/nouns_Ministerien")

#Filter for committee and others, extract files and text (pos. / neg. as TXT)
for (i in 1:15) {
  min_pos <- ministries_users[[i]]
  min_neg <- ministries_users
  min_neg[[i]]<-NULL
  min_neg <- bind_rows(min_neg)
  timelines_filtered_pos <- filter(timelines_NE, author_id == min_pos$author_id)
  timelines_filtered_neg <- stringdist_join(timelines_NE, min_neg, by="author_id", max_dist = 0,
                                            method = "lv", mode = "inner", distance_col = "string_distance")
  timelines_filtered_pos <- timelines_filtered_pos[,1:3]
  timelines_filtered_neg <- timelines_filtered_neg[,1:3]
  
  pos_txt <- paste(timelines_filtered_pos$lemmas, sep = "**")
  neg_txt <- paste(timelines_filtered_neg$lemmas, sep = "**")
  
  save(timelines_filtered_pos, file=paste0(ministries_names[i],"_pos.RDA"))
  save(timelines_filtered_neg, file=paste0(ministries_names[i],"_neg.RDA"))
  
  write.table(pos_txt, file=paste0(ministries_names[i],"_pos.TXT"))
  write.table(neg_txt, file=paste0(ministries_names[i],"_neg.TXT"))
}
