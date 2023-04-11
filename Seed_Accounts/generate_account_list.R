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


#### Ministries ####

ministries <- read_csv("ministries_accounts.csv", 
                       col_types = list(id = "c")) %>% 
  select(!`...1`)


## add policy field

ministries <- ministries %>% mutate(policy_field = case_when(
  username == "BMAS_Bund" ~ "arbeit",
  username == "AuswaertigesAmt" ~ "aeusseres",
  username == "BMZ_Bund" ~ "entwicklung",
  username == "BMBF_Bund" ~ "bildung_forschung",
  username == "bmj_bund" ~ "gesellschaft",
  username == "BMG_Bund" ~ "gesundheit",
  username == "BMF_Bund" ~ "finanzen_haushalt",
  username == "BMI_Bund" ~ "innere_sicherheit",
  username == "bmel" ~ "landwirtschaft_ernaehrung",
  username == "BMFSFJ" ~ "soziales",
  username == "BMUV" ~ "umwelt",
  username == "bmdv" ~ "verkehr",
  username == "BMVg_Bundeswehr" ~ "verteidigung",
  username == "BMWK" ~ "wirtschaft",
  username == "BMWSB_Bund" ~ "soziales"
))


## clean and save

ministries <- ministries %>% rename(official_name = name, user_id = id) %>% 
  select(!username)

ministries %>% mutate(across(.cols = where(is.character),  ~ utf8::as_utf8(.x))) %>% 
  write_csv("ministry_seeds_2023-04-06.csv")
                        


##### Committees #####

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

## add policy field

committee_members_19 <- committee_members_19 %>% mutate(policy_field = case_when(
  committee == "Arbeit und Soziales" ~ "arbeit",
  committee == "Auswärtiger Ausschuss" ~ "aeusseres",
  committee == "Menschenrechte und humanitäre Hilfe" ~ "entwicklung",
  committee == "Wirtschaftliche Zusammenarbeit und Entwicklung" ~ "entwicklung",
  committee == "Europäische Angelegenheiten" ~ "europa",
  committee == "Bildung, Forschung und Technikfolgenabschätzung" ~ "bildung_forschung",
  committee == "Digitale Agenda" ~ "digitalisierung_technik",
  committee == "Gesundheit" ~ "gesundheit",
  committee == "Finanzen" ~ "haushalt_finanzen",
  committee == "Haushalt" ~ "haushalt_finanzen",
  committee == "Inneres" ~ "innere_sicherheit",
  committee == "Kultur und Medien" ~ "kultur_medien_sport",
  committee == "Sport" ~ "kultur_medien_sport",
  committee == "Tourismus" ~ "kultur_medien_sport",
  committee == "Ernährung und Landwirtschaft" ~ "landwirtschaft_ernaehrung",
  committee == "Bau, Wohnen, Stadtentwicklung und Kommunen" ~ "soziales",
  committee == "Familie, Senioren, Frauen und Jugend" ~ "soziales",
  committee == "Umwelt, Naturschutz und nukleare Sicherheit" ~ "umwelt",
  committee == "Verkehr und digitale Infrastruktur" ~ "verkehr",
  committee == "Verteidigung" ~ "verteidigung",
  committee == "Wirtschaft und Energie" ~ "wirtschaft"
))


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

committee_members_20 <- committee_members_20 %>% 
  mutate(committee = str_remove(committee, "_Mitglieder")) # remove "_Mitglieder" string from committee name

## Match Twitter IDs

committee_members_20 <- committee_members_20 %>% 
  left_join(account_list %>% distinct(official_name, user_id), 
            by = "official_name",
            relationship = "many-to-many") # as a politician may have multiple accounts (e.g. older ones or for different roles) this may produce intentional duplicates


## add WP indicator

committee_members_20 <- committee_members_20 %>% mutate(period = "WP_20",
                                                        begin = ymd("2021-10-26"),
                                                        end = NA)


## add policy field

committee_members_20 <- committee_members_20 %>% mutate(policy_field = case_when(
  committee == "Arbeit und Soziales" ~ "arbeit",
  committee == "Auswärtiger Ausschuss" ~ "aeusseres",
  committee == "Menschenrechte und humanitäre Hilfe" ~ "entwicklung",
  committee == "Wirtschaftliche Zusammenarbeit und Entwicklung" ~ "entwicklung",
  committee == "Europäische Union" ~ "europa",
  committee == "Bildung, Forschung und Technikfolgenabschätzung" ~ "bildung_forschung",
  committee == "Digitales" ~ "digitalisierung_technik",
  committee == "Gesundheit" ~ "gesundheit",
  committee == "Finanzen" ~ "haushalt_finanzen",
  committee == "Haushalt" ~ "haushalt_finanzen",
  committee == "Inneres und Heimat" ~ "innere_sicherheit",
  committee == "Kultur und Medien" ~ "kultur_medien_sport",
  committee == "Sport" ~ "kultur_medien_sport",
  committee == "Tourismus" ~ "kultur_medien_sport",
  committee == "Ernährung und Landwirtschaft" ~ "landwirtschaft_ernaehrung",
  committee == "Wohnen, Stadtentwicklung, Bauwesen und Kommunen" ~ "soziales",
  committee == "Familie, Senioren, Frauen und Jugend" ~ "soziales",
  committee == "Umwelt, Naturschutz, nukleare Sicherheit und Verbraucherschutz" ~ "umwelt",
  committee == "Klimaschutz und Energie" ~ "umwelt",
  committee == "Verteidigung" ~ "verteidigung",
  committee == "Wirtschaft und Energie" ~ "wirtschaft",
  committee == "Wirtschaft" ~ "wirtschaft",
  committee == "Verkehr" ~ "verkehr"
))


## bind

committee_members <- committee_members_19 %>% bind_rows(committee_members_20)


# save

write_csv(committee_members, "committee_seeds_19-20_2023-04-06.csv")
















