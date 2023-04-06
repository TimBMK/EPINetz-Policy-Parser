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

timelines <- timelines %>% 
  filter(created_at < '2021-03-26') %>% 
  group_by(doc_id) %>% 
  summarise(lemmas = paste(lemma, collapse=", "), author) %>%
  distinct()

#Read list of committee members
setwd("/data/wolfeswenker/Ausschussmitglieder_WP19")

folder_loc <- "/data/wolfeswenker/Ausschussmitglieder_WP19/"
files <- paste0(folder_loc,list.files(folder_loc), '/ordentliche_mitglieder.csv')[1:22]

committee_members <- list()

names <- list()

for (i in 1:22) {
  
  committee_members[[i]] <- as.data.frame(read.csv(files[i], row.names=NULL, header=TRUE, sep=";"))
  
  names[i]<-gsub('/ordentliche_mitglieder.csv', '', files[i])
  names[i]<-gsub('/data/wolfeswenker/Ausschussmitglieder_WP19/','', names[i])
}

names(committee_members) <- names

setwd("~/Projekte/Policy_Parser/nouns_Ausschussmitglieder_WP19")

#Create username column for matching
timelines_matching <- timelines
timelines_matching["user_name_match"] <- timelines_matching["author"]
pattern_dr <- "Dr. "
pattern_hc <- "h. c. "
pattern_prof <- "Prof. "
pattern_mdb <- "MdB"
timelines_matching$user_name_match <- gsub(pattern_dr, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub(pattern_hc, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub(pattern_prof, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub(pattern_mdb, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\,.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\*.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\|.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\#.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\- .*", "", timelines_matching$user_name_match)
#timelines_matching$user_name_match <- gsub("[^-Ã¤Ã¶Ã¼ÃÃÃA-Za-z ]", "", timelines_matching$user_name_match)

#Create full name column for matching
for (i in 1:22) {
  committee_members[[i]]$fullname_match <- gsub(pattern_dr, "", committee_members[[i]]$fullname)
  committee_members[[i]]$fullname_match <- gsub(pattern_hc, "", committee_members[[i]]$fullname_match)
  committee_members[[i]]$fullname_match <- gsub(pattern_prof, "", committee_members[[i]]$fullname_match)
  committee_members[[i]]$fullname_match <- trimws(committee_members[[i]]$fullname_match)
  colnames(committee_members[[i]])[5] <- "user_name_match"
}

#Filter for committee member and others, extract files and text (pos. / neg. as TXT)
for (i in 1:22) {
  memb <- stringdist_join(committee_members[[i]], timelines_matching, by="user_name_match", max_dist = 1,
                                            method = "lv", mode = "inner", distance_col = "string_distance")
  memb <- data.frame(memb$user_name_match.x) %>% rename(user_name_match = 1) %>% distinct()
  for (j in 1:length(memb$user_name_match)){
    memb_pos <- as.list(memb$user_name_match[[j]])
    memb_pos <- data.frame(memb_pos) %>% rename(user_name_match = 1)
    memb_neg <- as.list(memb$user_name_match)
    memb_neg[[j]]<-NULL
    memb_neg <- memb_neg %>% unlist(memb_neg)
    memb_neg <- data.frame(memb_neg) %>% rename(user_name_match = 1)
    timelines_filtered_pos <- stringdist_join(timelines_matching, memb_pos, by="user_name_match", max_dist = 1,
                                              method = "lv", mode = "inner", distance_col = "string_distance")
    timelines_filtered_neg <- stringdist_join(timelines_matching, memb_neg, by="user_name_match", max_dist = 1,
                                              method = "lv", mode = "inner", distance_col = "string_distance")
    timelines_filtered_pos <- timelines_filtered_pos[,1:3]
    timelines_filtered_neg <- timelines_filtered_neg[,1:3]
    
    pos_txt <- paste(timelines_filtered_pos$lemmas, sep = "**")
    neg_txt <- paste(timelines_filtered_neg$lemmas, sep = "**")
    
    save(timelines_filtered_pos, file=paste0(names[i],"_",timelines_filtered_pos$author[[1]],"_pos.RDA"))
    save(timelines_filtered_neg, file=paste0(names[i],"_",timelines_filtered_pos$author[[1]],"_neg.RDA"))
    
    write.table(pos_txt, file=paste0(names[i],"_",timelines_filtered_pos$author[[1]],"_pos.TXT"))
    write.table(neg_txt, file=paste0(names[i],"_",timelines_filtered_pos$author[[1]],"_neg.TXT"))
  }
}

