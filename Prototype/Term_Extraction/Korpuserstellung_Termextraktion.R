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
timelines <- readRDS("data_twitter_timelines_2019-2021.RDS")

#Read list of committee members
setwd("/data/wolfeswenker/Ausschussmitglieder_WP20")

folder_loc <- "/data/wolfeswenker/Ausschussmitglieder_WP20/"
files <- paste0(folder_loc,list.files(folder_loc), '/ordentliche_mitglieder.csv')[1:22]

committee_members <- list()

names<- list()

for (i in 1:22) {

  committee_members[[i]] <- as.data.frame(read.csv(files[i], row.names=NULL, header=TRUE, sep=";"))

  names[i]<-gsub('_Mitglieder/ordentliche_mitglieder.csv', '', files[i])
  names[i]<-gsub('/data/wolfeswenker/Ausschussmitglieder_WP20/','', names[i])
}

names(committee_members) <- names

setwd("~/Projekte/Policy_Parser")

#Create username column for matching
timelines_matching <- timelines
timelines_matching["user_name_match"] <- timelines_matching["user_name"]
pattern_dr <- "Dr. "
pattern_prof <- "Prof. "
pattern_mdb <- "MdB"
timelines_matching$user_name_match <- gsub(pattern_dr, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub(pattern_prof, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub(pattern_mdb, "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\,.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\*.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\|.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\#.*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("\\- .*", "", timelines_matching$user_name_match)
timelines_matching$user_name_match <- gsub("[^-Ã¤Ã¶Ã¼ÃÃÃA-Za-z ]", "", timelines_matching$user_name_match)

#Create full name column for matching
for (i in 1:22) {
  committee_members[[i]]$fullname_match <- gsub(pattern_dr, "", committee_members[[i]]$fullname)
  committee_members[[i]]$fullname_match <- gsub(pattern_prof, "", committee_members[[i]]$fullname_match)
  colnames(committee_members[[i]])[5] <- "user_name_match"
}

#Filter for committee and others, extract files and text (pos. / neg. as TXT)
for (i in 1:22) {
  comm_pos <- committee_members[[i]]
  comm_neg<-committee_members
  comm_neg[[i]]<-NULL
  comm_neg<-bind_rows(comm_neg)
  timelines_filtered_pos <- stringdist_join(timelines_matching, committee_members[[i]], by="user_name_match", max_dist = 1,
                                      method = "lv", mode = "inner", distance_col = "string_distance")
  timelines_filtered_neg <- stringdist_join(timelines_matching, comm_neg, by="user_name_match", max_dist = 1,
                                            method = "lv", mode = "inner", distance_col = "string_distance")
  timelines_filtered_pos <- timelines_filtered_pos[,1:3]
  timelines_filtered_neg <- timelines_filtered_neg[,1:3]
  
  pos_txt <- paste(timelines_filtered_pos$text, sep = '**')
  neg_txt <- paste(timelines_filtered_neg$text, sep = '**')
  
  save(timelines_filtered_pos, file=paste0(names[i],'_pos.RDA'))
  save(timelines_filtered_neg, file=paste0(names[i],'_neg.RDA'))
  
  write.table(pos_txt, file=paste0(names[i],'_pos.TXT'))
  write.table(neg_txt, file=paste0(names[i],'_neg.TXT'))

}
