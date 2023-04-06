## Creation of Seed Account Lists ##
####################################

{
  library(tidyverse)
  library(data.table)
}

setwd("Seed_Accounts/")

# List of all Politician Twitter Accounts
account_list <- read_csv("EPINetz_seedlist_2023-01-02.csv",
                         col_types = list(ID = "c",
                                          user_id = "c",
                                          abgeordnetenwatch_id = "c"))

# WP 19

## Read list of committee members 

folder_loc <- "Ausschussmitglieder_WP19/"
files <- paste0(folder_loc,list.files(folder_loc), '/ordentliche_mitglieder.csv')

data_19 <- lapply(files, read_delim, delim = ";", trim_ws = TRUE)
setattr(data_19, "names", list.files(folder_loc))

committee_members_19 <- rbindlist(data_19, idcol = "committee")


## Clean data for matching

remove_pattern <- "\\b(?:Dr\\.|Prof\\.|h\\. c\\.|MdB)(?:\\s+Dr\\.|\\s+h\\. c\\.|\\.)*\\s*" # Remove "Dr.", Prof.", "h. c.", and "MdB", as well as the ensuing whitespace

committee_members_19 <- committee_members_19 %>% 
  mutate(official_name = str_remove(fullname, remove_pattern)) %>% 
  distinct() # remove any duplicates in the data


## Match Twitter IDs

committee_members_19 <- committee_members_19 %>% 
  left_join(account_list %>% distinct(official_name, user_id), 
            by = "official_name",
            relationship = "many-to-many") # as a politician may have multiple accounts (e.g. older ones or for different roles) this may produce intentional duplicates


## add WP indicator

committee_members_19 <- committee_members_19 %>% mutate(period = "WP_19",
                                                        begin = ymd("2017-10-24"),
                                                        end = ymd("2021-10-26"))



# WP 20

## Read list of committee members 

folder_loc <- "Ausschussmitglieder_WP20/"
files <- paste0(folder_loc,list.files(folder_loc), '/ordentliche_mitglieder.csv')

data_20 <- lapply(files, read_delim, delim = ";", trim_ws = TRUE)
setattr(data_20, "names", list.files(folder_loc))

committee_members_20 <- rbindlist(data_20, idcol = "committee")


## Clean data for matching

remove_pattern <- "\\b(?:Dr\\.|Prof\\.|h\\. c\\.|MdB)(?:\\s+Dr\\.|\\s+h\\. c\\.|\\.)*\\s*" # Remove "Dr.", Prof.", "h. c.", and "MdB", as well as the ensuing whitespace

committee_members_20 <- committee_members_20 %>% 
  mutate(official_name = str_remove(fullname, remove_pattern)) %>% 
  distinct() # remove any duplicates in the data


## Match Twitter IDs

committee_members_20 <- committee_members_20 %>% 
  left_join(account_list %>% distinct(official_name, user_id), 
            by = "official_name",
            relationship = "many-to-many") # as a politician may have multiple accounts (e.g. older ones or for different roles) this may produce intentional duplicates


## add WP indicator

committee_members_20 <- committee_members_20 %>% mutate(period = "WP_20",
                                                        begin = ymd("2021-10-26"),
                                                        end = NA)


## bind

committee_members <- committee_members_19 %>% bind_rows(committee_members_20)


# save

write_csv(committee_members, "committee_members_19-20_2023-04-05.csv")
