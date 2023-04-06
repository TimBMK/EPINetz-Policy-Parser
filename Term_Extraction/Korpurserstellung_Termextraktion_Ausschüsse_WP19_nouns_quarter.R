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

## load data into df

folder_loc <- "/data/wolfeswenker/Projekte/Policy_Parser/nouns_quarter_snapshots/"
files <- paste0(folder_loc,list.files(folder_loc))[1:9]

names_files <- gsub('.RDA', '', files)
names_files <- gsub('/data/wolfeswenker/Projekte/Policy_Parser/nouns_quarter_snapshots/', '', names_files)

pattern_dr <- "Dr. "
pattern_hc <- "h. c. "
pattern_prof <- "Prof. "
pattern_mdb <- "MdB"

#Add matching column to timeline snapshots
setwd("~/Projekte/Policy_Parser/nouns_quarter_snapshots")

i=0

while (i <9) {
  i<-i+1
  load(files[i])
  timelines_matching <- timelines_snapshot
  timelines_matching["user_name_match"] <- timelines_matching["author"]
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
  save(timelines_matching, file=paste0(names_files[i],'_matching.RDA'))
}

#Create full name column for matching
for (i in 1:22) {
  committee_members[[i]]$fullname_match <- gsub(pattern_dr, "", committee_members[[i]]$fullname)
  committee_members[[i]]$fullname_match <- gsub(pattern_hc, "", committee_members[[i]]$fullname_match)
  committee_members[[i]]$fullname_match <- gsub(pattern_prof, "", committee_members[[i]]$fullname_match)
  committee_members[[i]]$fullname_match <- trimws(committee_members[[i]]$fullname_match)
  colnames(committee_members[[i]])[5] <- "user_name_match"
}

setwd("~/Projekte/Policy_Parser/nouns_quarter_snapshots")

folder_loc <- "/data/wolfeswenker/Projekte/Policy_Parser/nouns_quarter_snapshots/"
files <- paste0(folder_loc,list.files(folder_loc))[1:18]

files
load(files[3])
load(files[5])
load(files[7])
load(files[9])
load(files[11])
load(files[13])
load(files[15])
load(files[17])

setwd("~/Projekte/Policy_Parser/nouns_Ausschüsse_quarter")

i=0

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
  
  pos_txt <- paste(timelines_filtered_pos$lemmas, sep = '**')
  neg_txt <- paste(timelines_filtered_neg$lemmas, sep = '**')
  
  save(timelines_filtered_pos, file=paste0(names[i],'_pos.RDA'))
  save(timelines_filtered_neg, file=paste0(names[i],'_neg.RDA'))
  
  write.table(pos_txt, file=paste0(names[i],'_pos.TXT'))
  write.table(neg_txt, file=paste0(names[i],'_neg.TXT'))
  
}
