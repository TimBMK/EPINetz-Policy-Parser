#Clean working space
  rm(list = ls())

#---------------------------------#
# 0.Install/Load Packages #########
#---------------------------------#

if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
if(!require("rlist")) install.packages("rlist")
  library(rlist)
if(!require("stringr")) install.packages("stringr")
  library(stringr)
if(!require("igraph")) install.packages("igraph")
  library(igraph)
if(!require("RandomWalkRestartMH")) install.packages("RandomWalkRestartMH")
  library(RandomWalkRestartMH)
if(!require("vroom")) install.packages("vroom")
  library(vroom)
if(!require("readr")) install.packages("readr")
  library(readr)

#---------------------------------#
# 1.Read Network ##################
#---------------------------------#

#Network data - 1st quarter 2019
  
twitter_NE_hashtags <-
  vroom("/data/wolfeswenker/Projekte/Policy_Parser/topic_graphs_quarter201901.csv.tar.gz", # this data uses lemmas rather than tokens!
        col_types = list(from = "c", to = "c")) %>%
  as_tibble()
  
twitter_graph <- graph_from_data_frame(twitter_NE_hashtags, directed = F)

#---------------------------------#
# 2. Seed Set Preparation #########
#---------------------------------#

#Committees seed set (policy specific) - read data
folder_loc <- "/data/wolfeswenker/Projekte/Policy_Parser/Term_Extraction_nouns_Ausschüsse_WP19_Q1-2019_prototype/"
files <- paste0(folder_loc,list.files(folder_loc))[23:44]

names_committees <- gsub('.RDA', '', files)
names_committees <- gsub('/data/wolfeswenker/Projekte/Policy_Parser/Term_Extraction_nouns_Ausschüsse_WP19_Q1-2019_prototype/tstat_', '', names_committees)

#Committees seed set (policy specific) - create seed set

i=0
mat <- matrix(ncol = 0, nrow = 0)
seeds_committees <- data.frame(mat)

while (i <22) {
  i <- i+1
  load(files[i])
  tstat_key <- tstat_key[tstat_key$chi2>30, ]
  #tstat_key <- tstat_key %>% slice(1:round(nrow(tstat_key)*1/100))
  #tstat_key <- tstat_key[tstat_key$n_target>5, ]
  #tstat_key <- tstat_key %>% slice(1:round(nrow(tstat_key)*0.5/100))
  tstat_key <- tstat_key %>% mutate(entity=names_committees[[i]])
  seeds_committees <- rbind(seeds_committees, tstat_key)
}

###Filter out committee member specific terms

#Committees seed set (committee specific) - read data
folder_loc <- "/data/wolfeswenker/Projekte/Policy_Parser/Term_Extraction_nouns_Ausschussmitglieder_WP19/"
files <- paste0(folder_loc,list.files(folder_loc))[1:521]

#Committees seed set (committee specific) - create seed set

setwd("/data/wolfeswenker/Projekte/Policy_Parser/top_terms_Ausschussmitglieder")
i=0

terms <- list()

while (i <521) {
  i<-i+1
  load(files[i])
  tstat_key <- tstat_key[tstat_key$chi2>250, ]
  #tstat_key <- tstat_key %>% slice(1:round(nrow(tstat_key)*1/100))
  #tstat_key <- tstat_key %>% slice(1:round(nrow(tstat_key)*0.5/100))
  terms <- append(terms, as.list(tstat_key$feature))
}

terms <- terms %>% unlist(terms)
terms_df <- data.frame(terms)
terms_df <- distinct(terms_df)

members_terms <- as.character(terms_df$terms)
#At this point terms are filtered out
seeds_committees <- subset(seeds_committees, !(feature %in% members_terms))

#Check results
table(seeds_committees$entity)
#mean(table(seeds_committees$entity))

#seeds_committees <- seeds_committees %>% distinct(feature, .keep_all = TRUE)
#seeds$feature[duplicated(seeds$feature)]

#Ministries seed set - read data

folder_loc <- "/data/wolfeswenker/Projekte/Policy_Parser/Term_Extraction_nouns_Ministerien_WP19_2019_prototype/"
files <- paste0(folder_loc,list.files(folder_loc))[14:26]

names_ministries <- gsub('.RDA', '', files)
names_ministries <- gsub('/data/wolfeswenker/Projekte/Policy_Parser/Term_Extraction_nouns_Ministerien_WP19_2019_prototype/tstat_', '', names_ministries)

#Ministries seed set - create seed set

i=0
mat <- matrix(ncol = 0, nrow = 0)
seeds_ministries <- data.frame(mat)

while (i <13) {
  i<-i+1
  load(files[i])
  tstat_key <- tstat_key[tstat_key$chi2>500, ]
  #tstat_key <- tstat_key %>% slice(1:round(nrow(tstat_key)*1/100))
  #tstat_key <- tstat_key[tstat_key$n_target>5, ]
  #tstat_key <- tstat_key %>% slice(1:round(nrow(tstat_key)*0.5/100))
  tstat_key <- tstat_key %>% mutate(entity=names_ministries[[i]])
  seeds_ministries <- rbind(seeds_ministries,tstat_key)
}

#seeds_ministries <- seeds_ministries %>% distinct(feature, .keep_all = TRUE)

#Check results
table(seeds_ministries$entity)
mean(table(seeds_committees$entity))

###Create master seed list

seeds_arbeit <- rbind(subset(seeds_committees, entity=="Arbeit und Soziales"), subset(seeds_ministries, entity=="BMAS_Bund"))
seeds_arbeit_info <- seeds_arbeit %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_arbeit <- seeds_arbeit %>% distinct(feature, .keep_all = TRUE)
seeds_arbeit <- subset(seeds_arbeit, select = -c(entity))
seeds_arbeit <- merge(seeds_arbeit, seeds_arbeit_info, by = "feature")

#seeds_äußeres_europa <- rbind(subset(seeds_committees, entity=="Auswärtiger Ausschuss"), subset(seeds_committees, entity=="Europäische Angelegenheiten"), subset(seeds_committees, entity=="Menschenrechte und humanitäre Hilfe"), subset(seeds_committees, entity=="Wirtschaftliche Zusammenarbeit und Entwicklung"), subset(seeds_ministries, entity=="AuswaertigesAmt"), subset(seeds_ministries, entity=="BMZ_Bund"))
seeds_äußeres <- rbind(subset(seeds_committees, entity=="Auswärtiger Ausschuss"), subset(seeds_ministries, entity=="AuswaertigesAmt"))
seeds_äußeres_info <- seeds_äußeres %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_äußeres <- seeds_äußeres %>% distinct(feature, .keep_all = TRUE)
seeds_äußeres <- subset(seeds_äußeres, select = -c(entity))
seeds_äußeres <- merge(seeds_äußeres, seeds_äußeres_info, by = "feature")

seeds_entwicklung <- rbind(subset(seeds_committees, entity=="Menschenrechte und humanitäre Hilfe"), subset(seeds_committees, entity=="Wirtschaftliche Zusammenarbeit und Entwicklung"), subset(seeds_ministries, entity=="BMZ_Bund"))
seeds_entwicklung_info <- seeds_entwicklung %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_entwicklung <- seeds_entwicklung %>% distinct(feature, .keep_all = TRUE)
seeds_entwicklung <- subset(seeds_entwicklung, select = -c(entity))
seeds_entwicklung <- merge(seeds_entwicklung, seeds_entwicklung_info, by = "feature")

seeds_europa <- rbind(subset(seeds_committees, entity=="Europäische Angelegenheiten"))
seeds_europa_info <- seeds_europa %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_europa <- seeds_europa %>% distinct(feature, .keep_all = TRUE)
seeds_europa <- subset(seeds_europa, select = -c(entity))
seeds_europa <- merge(seeds_europa, seeds_europa_info, by = "feature")

seeds_bildung_forschung <- rbind(subset(seeds_committees, entity=="Bildung, Forschung und Technikfolgenabschätzung"), subset(seeds_ministries, entity=="BMBF_Bund"))
seeds_bildung_forschung_info <- seeds_bildung_forschung %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_bildung_forschung <- seeds_bildung_forschung %>% distinct(feature, .keep_all = TRUE)
seeds_bildung_forschung <- subset(seeds_bildung_forschung, select = -c(entity))
seeds_bildung_forschung <- merge(seeds_bildung_forschung, seeds_bildung_forschung_info, by = "feature")

seeds_digitalisierung_technik <- rbind(subset(seeds_committees, entity=="Digitale Agenda"), subset(seeds_ministries, entity==""))
seeds_digitalisierung_technik_info <- seeds_digitalisierung_technik %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_digitalisierung_technik <- seeds_digitalisierung_technik %>% distinct(feature, .keep_all = TRUE)
seeds_digitalisierung_technik <- subset(seeds_digitalisierung_technik, select = -c(entity))
seeds_digitalisierung_technik <- merge(seeds_digitalisierung_technik, seeds_digitalisierung_technik_info, by = "feature")

seeds_gesellschaft <- rbind(subset(seeds_ministries, entity=="bmj_bund"))
seeds_gesellschaft_info <- seeds_gesellschaft %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_gesellschaft <- seeds_gesellschaft %>% distinct(feature, .keep_all = TRUE)
seeds_gesellschaft <- subset(seeds_gesellschaft, select = -c(entity))
seeds_gesellschaft <- merge(seeds_gesellschaft, seeds_gesellschaft_info, by = "feature")

seeds_gesundheit <- rbind(subset(seeds_committees, entity=="Gesundheit"), subset(seeds_ministries, entity=="BMG_Bund"))
seeds_gesundheit_info <- seeds_gesundheit %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_gesundheit <- seeds_gesundheit %>% distinct(feature, .keep_all = TRUE)
seeds_gesundheit <- subset(seeds_gesundheit, select = -c(entity))
seeds_gesundheit <- merge(seeds_gesundheit, seeds_gesundheit_info, by = "feature")

seeds_haushalt_finanzen <- rbind(subset(seeds_committees, entity=="Finanzen"), subset(seeds_committees, entity=="Haushalt"), subset(seeds_ministries, entity=="BMF_Bund"))
seeds_haushalt_finanzen_info <- seeds_haushalt_finanzen %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_haushalt_finanzen <- seeds_haushalt_finanzen %>% distinct(feature, .keep_all = TRUE)
seeds_haushalt_finanzen <- subset(seeds_haushalt_finanzen, select = -c(entity))
seeds_haushalt_finanzen <- merge(seeds_haushalt_finanzen, seeds_haushalt_finanzen_info, by = "feature")

seeds_innere_sicherheit <- rbind(subset(seeds_committees, entity=="Inneres"), subset(seeds_ministries, entity=="BMI_Bund"))
seeds_innere_sicherheit_info <- seeds_innere_sicherheit %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_innere_sicherheit <- seeds_innere_sicherheit %>% distinct(feature, .keep_all = TRUE)
seeds_innere_sicherheit <- subset(seeds_innere_sicherheit, select = -c(entity))
seeds_innere_sicherheit <- merge(seeds_innere_sicherheit, seeds_innere_sicherheit_info, by = "feature")

seeds_kultur_medien_sport <- rbind(subset(seeds_committees, entity=="Kultur und Medien"), subset(seeds_committees, entity=="Sport"), subset(seeds_committees, entity=="Tourismus"))
seeds_kultur_medien_sport_info <- seeds_kultur_medien_sport %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_kultur_medien_sport <- seeds_kultur_medien_sport %>% distinct(feature, .keep_all = TRUE)
seeds_kultur_medien_sport <- subset(seeds_kultur_medien_sport, select = -c(entity))
seeds_kultur_medien_sport <- merge(seeds_kultur_medien_sport, seeds_kultur_medien_sport_info, by = "feature")

seeds_landwirtschaft_ernährung <- rbind(subset(seeds_committees, entity=="Ernährung und Landwirtschaft"), subset(seeds_ministries, entity=="bmel"))
seeds_landwirtschaft_ernährung_info <- seeds_landwirtschaft_ernährung %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_landwirtschaft_ernährung <- seeds_landwirtschaft_ernährung %>% distinct(feature, .keep_all = TRUE)
seeds_landwirtschaft_ernährung <- subset(seeds_landwirtschaft_ernährung, select = -c(entity))
seeds_landwirtschaft_ernährung <- merge(seeds_landwirtschaft_ernährung, seeds_landwirtschaft_ernährung_info, by = "feature")

seeds_soziales <- rbind(subset(seeds_committees, entity=="Bau, Wohnen, Stadtentwicklung und Kommunen"), subset(seeds_committees, entity=="Familie, Senioren, Frauen und Jugend"), subset(seeds_ministries, entity=="BMFSFJ"))
seeds_soziales_info <- seeds_soziales %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_soziales <- seeds_soziales %>% distinct(feature, .keep_all = TRUE)
seeds_soziales <- subset(seeds_soziales, select = -c(entity))
seeds_soziales <- merge(seeds_soziales, seeds_soziales_info, by = "feature")

seeds_umwelt <- rbind(subset(seeds_committees, entity=="Umwelt, Naturschutz und nukleare Sicherheit"), subset(seeds_ministries, entity=="BMUV"))
seeds_umwelt_info <- seeds_umwelt %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_umwelt <- seeds_umwelt %>% distinct(feature, .keep_all = TRUE)
seeds_umwelt <- subset(seeds_umwelt, select = -c(entity))
seeds_umwelt <- merge(seeds_umwelt, seeds_umwelt_info, by = "feature")

seeds_verkehr <- rbind(subset(seeds_committees, entity=="Verkehr und digitale Infrastruktur"), subset(seeds_ministries, entity=="bmdv"))
seeds_verkehr_info <- seeds_verkehr %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_verkehr <- seeds_verkehr %>% distinct(feature, .keep_all = TRUE)
seeds_verkehr <- subset(seeds_verkehr, select = -c(entity))
seeds_verkehr <- merge(seeds_verkehr, seeds_verkehr_info, by = "feature")

seeds_verteidigung <- rbind(subset(seeds_committees, entity=="Verteidigung"), subset(seeds_ministries, entity=="BMVg_Bundeswehr"))
seeds_verteidigung_info <- seeds_verteidigung %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_verteidigung <- seeds_verteidigung %>% distinct(feature, .keep_all = TRUE)
seeds_verteidigung <- subset(seeds_verteidigung, select = -c(entity))
seeds_verteidigung <- merge(seeds_verteidigung, seeds_verteidigung_info, by = "feature")

seeds_wirtschaft <- rbind(subset(seeds_committees, entity=="Wirtschaft und Energie"))
seeds_wirtschaft_info <- seeds_wirtschaft %>% group_by(feature) %>% summarize(entity=paste(entity, collapse=", "))
seeds_wirtschaft <- seeds_wirtschaft %>% distinct(feature, .keep_all = TRUE)
seeds_wirtschaft <- subset(seeds_wirtschaft, select = -c(entity))
seeds_wirtschaft <- merge(seeds_wirtschaft, seeds_wirtschaft_info, by = "feature")

#seeds_list <- list(seeds_arbeit, seeds_äußeres_europa, seeds_bildung_forschung,
#                   seeds_digitalisierung_technik, seeds_gesellschaft, 
#                   seeds_gesundheit, seeds_haushalt_finanzen, 
#                   seeds_innere_sicherheit, seeds_kultur_medien_sport,
#                   seeds_landwirtschaft_ernährung, seeds_soziales, seeds_umwelt,
#                   seeds_verkehr, seeds_verteidigung, seeds_wirtschaft)
seeds_list <- list(seeds_arbeit, 
                   seeds_äußeres, seeds_entwicklung, seeds_europa, 
                   seeds_bildung_forschung,
                   seeds_digitalisierung_technik, seeds_gesellschaft, 
                   seeds_gesundheit, seeds_haushalt_finanzen, 
                   seeds_innere_sicherheit, seeds_kultur_medien_sport,
                   seeds_landwirtschaft_ernährung, seeds_soziales, seeds_umwelt,
                   seeds_verkehr, seeds_verteidigung, seeds_wirtschaft)

#names <- c("arbeit", "äußeres_europa", "bildung_forschung", 
#           "digitalisierung_technik", "gesellschaft", "gesundheit",
#           "haushalt_finanzen", "innere_sicherheit", "kultur_medien_sport",
#           "landwirtschaft_ernährung", "soziales", "umwelt", "verkehr", 
#           "verteidigung", "wirtschaft")

names <- c("arbeit", "äußeres", "entwicklung", "europa", "bildung_forschung", 
           "digitalisierung_technik", "gesellschaft", "gesundheit",
           "haushalt_finanzen", "innere_sicherheit", "kultur_medien_sport",
           "landwirtschaft_ernährung", "soziales", "umwelt", "verkehr", 
           "verteidigung", "wirtschaft")

names(seeds_list) <- names

#Ausschuss "Recht und Verbraucherschutz"?
#bmdv -> Verkehr <-> Digitalisierung und Technik?
#bmj_bund -> Gesellschaft
#seeds_energie?
#seeds_migration?
#seeds_öffentliche_verwaltung?

seeds_list_vec <- list()

for (i in 1:17) {
  seeds <- as.data.frame(seeds_list[[i]])
  seeds <- as.vector(seeds$feature)
  net_seeds <- c()
  for (j in V(twitter_graph)$name) {
    if (j %in% seeds) {
      net_seeds <- c(net_seeds, j)
    }
  }
  #assign(paste0("net_seeds_", names[i]), net_seeds)
  name <- names[i]
  seeds_list_vec[[name]] <- assign(paste0("net_seeds_", names[i]), net_seeds)
}

#---------------------------------#
# 3. RWR - Single Seeds ###########
#---------------------------------#

#Create a Multiplex object composed of 1 layer (It's a Monoplex Network)
net_MultiplexObject <- create.multiplex(list(Graph_NE_Hashtags=twitter_graph))

#To apply the RWR on a monoplex network, we need to compute the adjacency matrix 
#of the network and normalize it by column
AdjMatrix_net <- compute.adjacency.matrix(net_MultiplexObject)
AdjMatrixNorm_net <- normalize.multiplex.adjacency(AdjMatrix_net)

setwd("/data/wolfeswenker/Projekte/Policy_Parser/RWR_res/Prototype2")

#custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#---------------------------------#
# Arbeit ##########################
#---------------------------------#

#Create empty df (which collects the RWR identified terms from different seed
#node RWR computations)
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_arbeit <- data.frame(mat)

#Compute RWRs
#Iterate through the seed nodes of a policy specific seed term set (here policy
#area Arbeit)
for (j in seeds_list_vec[[1]]) {
  #Define seed node
  SeedNode <- j
  #Depart from the seed node, compute RWR scores
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  #Transform the results into df, columns NodeNames (RWR identified terms) and 
  #Score (proximity level)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  #Normalize scores
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  #Rename columns
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  #Only extract top terms (high affinity score)
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  #Add column that contains the name of the seed node from which the terms were
  #identified (for traceability reasons)
  top_affinity_scores['SeedNode'] = SeedNode
  #Append sub result to a policy specific master list
  terms_arbeit <- rbind(terms_arbeit,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_arbeit
#Remove RWR terms that equal seed nodes
terms_arbeit <- subset(terms_arbeit, !(NodeNames %in% seeds_list_vec[[1]]))
#terms_arbeit <- terms_arbeit[ ! terms_arbeit$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_arbeit (and give them the score value 1)
terms_seeds_arbeit <- as.data.frame(seeds_arbeit$feature)
names(terms_seeds_arbeit)[names(terms_seeds_arbeit)=="seeds_arbeit$feature"] <- "NodeNames"
terms_seeds_arbeit <- terms_seeds_arbeit %>% mutate(Score = NA)
terms_seeds_arbeit <- terms_seeds_arbeit %>% mutate(ScoreNorm = 1)
terms_seeds_arbeit <- terms_seeds_arbeit %>% mutate(SeedNode = NA)
terms_arbeit <- rbind(terms_arbeit,terms_seeds_arbeit)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_arbeit_sum <- terms_arbeit %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_arbeit$Score <- round(terms_arbeit$Score, 10)
terms_arbeit_info <- terms_arbeit %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_arbeit_info2 <- terms_arbeit %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_arbeit_sum <- merge(terms_arbeit_sum, terms_arbeit_info, by = "NodeNames")
terms_arbeit_sum <- merge(terms_arbeit_sum, terms_arbeit_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_arbeit_sum <- cbind(terms_arbeit_sum,as.data.frame(lapply(terms_arbeit_sum[2], minMax)))
#Rename the respective column
names(terms_arbeit_sum)[5] <- "ScoreNorm2"
#Rearrange columns
terms_arbeit_sum <- terms_arbeit_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_arbeit_sum, file=paste0('terms_list_',names[1],'.RDA'))
write_csv2(terms_arbeit_sum, file=paste0('terms_list_',names[1],'.CSV'))

#---------------------------------#
# Äußeres #########################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_äußeres <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[2]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_äußeres <- rbind(terms_äußeres,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_äußeres
#Remove RWR terms that equal seed nodes
terms_äußeres <- subset(terms_äußeres, !(NodeNames %in% seeds_list_vec[[2]]))
#terms_äußeres <- terms_äußeres[ ! terms_äußeres$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_äußeres (and give them the score value 1)
terms_seeds_äußeres <- as.data.frame(seeds_äußeres$feature)
names(terms_seeds_äußeres)[names(terms_seeds_äußeres)=="seeds_äußeres$feature"] <- "NodeNames"
terms_seeds_äußeres <- terms_seeds_äußeres %>% mutate(Score = NA)
terms_seeds_äußeres <- terms_seeds_äußeres %>% mutate(ScoreNorm = 1)
terms_seeds_äußeres <- terms_seeds_äußeres %>% mutate(SeedNode = NA)
terms_äußeres <- rbind(terms_äußeres,terms_seeds_äußeres)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_äußeres_sum <- terms_äußeres %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_äußeres$Score <- round(terms_äußeres$Score, 10)
terms_äußeres_info <- terms_äußeres %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_äußeres_info2 <- terms_äußeres %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_äußeres_sum <- merge(terms_äußeres_sum, terms_äußeres_info, by = "NodeNames")
terms_äußeres_sum <- merge(terms_äußeres_sum, terms_äußeres_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_äußeres_sum <- cbind(terms_äußeres_sum,as.data.frame(lapply(terms_äußeres_sum[2], minMax)))
names(terms_äußeres_sum)[5] <- "ScoreNorm2"
terms_äußeres_sum <- terms_äußeres_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_äußeres_sum, file=paste0('terms_list_',names[2],'.RDA'))
write_csv2(terms_äußeres_sum, file=paste0('terms_list_',names[2],'.CSV'))

#---------------------------------#
# Entwicklung #####################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_entwicklung <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[3]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_entwicklung <- rbind(terms_entwicklung,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_entwicklung
#Remove RWR terms that equal seed nodes
terms_entwicklung <- subset(terms_entwicklung, !(NodeNames %in% seeds_list_vec[[3]]))
#terms_entwicklung <- terms_entwicklung[ ! terms_entwicklung$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_entwicklung (and give them the score value 1)
terms_seeds_entwicklung <- as.data.frame(seeds_entwicklung$feature)
names(terms_seeds_entwicklung)[names(terms_seeds_entwicklung)=="seeds_entwicklung$feature"] <- "NodeNames"
terms_seeds_entwicklung <- terms_seeds_entwicklung %>% mutate(Score = NA)
terms_seeds_entwicklung <- terms_seeds_entwicklung %>% mutate(ScoreNorm = 1)
terms_seeds_entwicklung <- terms_seeds_entwicklung %>% mutate(SeedNode = NA)
terms_entwicklung <- rbind(terms_entwicklung,terms_seeds_entwicklung)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_entwicklung_sum <- terms_entwicklung %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_entwicklung$Score <- round(terms_entwicklung$Score, 10)
terms_entwicklung_info <- terms_entwicklung %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_entwicklung_info2 <- terms_entwicklung %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_entwicklung_sum <- merge(terms_entwicklung_sum, terms_entwicklung_info, by = "NodeNames")
terms_entwicklung_sum <- merge(terms_entwicklung_sum, terms_entwicklung_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_entwicklung_sum <- cbind(terms_entwicklung_sum,as.data.frame(lapply(terms_entwicklung_sum[2], minMax)))
names(terms_entwicklung_sum)[5] <- "ScoreNorm2"
terms_entwicklung_sum <- terms_entwicklung_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_entwicklung_sum, file=paste0('terms_list_',names[3],'.RDA'))
write_csv2(terms_entwicklung_sum, file=paste0('terms_list_',names[3],'.CSV'))

#---------------------------------#
# Europa ##########################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_europa <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[4]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_europa <- rbind(terms_europa,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_europa
#Remove RWR terms that equal seed nodes
terms_europa <- subset(terms_europa, !(NodeNames %in% seeds_list_vec[[4]]))
#terms_europa <- terms_europa[ ! terms_europa$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_europa (and give them the score value 1)
terms_seeds_europa <- as.data.frame(seeds_europa$feature)
names(terms_seeds_europa)[names(terms_seeds_europa)=="seeds_europa$feature"] <- "NodeNames"
terms_seeds_europa <- terms_seeds_europa %>% mutate(Score = NA)
terms_seeds_europa <- terms_seeds_europa %>% mutate(ScoreNorm = 1)
terms_seeds_europa <- terms_seeds_europa %>% mutate(SeedNode = NA)
terms_europa <- rbind(terms_europa,terms_seeds_europa)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_europa_sum <- terms_europa %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_europa$Score <- round(terms_europa$Score, 10)
terms_europa_info <- terms_europa %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_europa_info2 <- terms_europa %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_europa_sum <- merge(terms_europa_sum, terms_europa_info, by = "NodeNames")
terms_europa_sum <- merge(terms_europa_sum, terms_europa_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_europa_sum <- cbind(terms_europa_sum,as.data.frame(lapply(terms_europa_sum[2], minMax)))
names(terms_europa_sum)[5] <- "ScoreNorm2"
terms_europa_sum <- terms_europa_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_europa_sum, file=paste0('terms_list_',names[4],'.RDA'))
write_csv2(terms_europa_sum, file=paste0('terms_list_',names[4],'.CSV'))

#---------------------------------#
# Bildung & Forschung #############
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_bildung_forschung <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[5]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_bildung_forschung <- rbind(terms_bildung_forschung,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_bildung_forschung
#Remove RWR terms that equal seed nodes
terms_bildung_forschung <- subset(terms_bildung_forschung, !(NodeNames %in% seeds_list_vec[[5]]))
#terms_bildung_forschung <- terms_bildung_forschung[ ! terms_bildung_forschung$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_bildung_forschung (and give them the score value 1)
terms_seeds_bildung_forschung <- as.data.frame(seeds_bildung_forschung$feature)
names(terms_seeds_bildung_forschung)[names(terms_seeds_bildung_forschung)=="seeds_bildung_forschung$feature"] <- "NodeNames"
terms_seeds_bildung_forschung <- terms_seeds_bildung_forschung %>% mutate(Score = NA)
terms_seeds_bildung_forschung <- terms_seeds_bildung_forschung %>% mutate(ScoreNorm = 1)
terms_seeds_bildung_forschung <- terms_seeds_bildung_forschung %>% mutate(SeedNode = NA)
terms_bildung_forschung <- rbind(terms_bildung_forschung,terms_seeds_bildung_forschung)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_bildung_forschung_sum <- terms_bildung_forschung %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_bildung_forschung$Score <- round(terms_bildung_forschung$Score, 10)
terms_bildung_forschung_info <- terms_bildung_forschung %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_bildung_forschung_info2 <- terms_bildung_forschung %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_bildung_forschung_sum <- merge(terms_bildung_forschung_sum, terms_bildung_forschung_info, by = "NodeNames")
terms_bildung_forschung_sum <- merge(terms_bildung_forschung_sum, terms_bildung_forschung_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_bildung_forschung_sum <- cbind(terms_bildung_forschung_sum,as.data.frame(lapply(terms_bildung_forschung_sum[2], minMax)))
names(terms_bildung_forschung_sum)[5] <- "ScoreNorm2"
terms_bildung_forschung_sum <- terms_bildung_forschung_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_bildung_forschung_sum, file=paste0('terms_list_',names[5],'.RDA'))
write_csv2(terms_bildung_forschung_sum, file=paste0('terms_list_',names[5],'.CSV'))

#---------------------------------#
# Digitalisierung & Technik #######
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_digitalisierung_technik <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[6]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_digitalisierung_technik <- rbind(terms_digitalisierung_technik,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_digitalisierung_technik
#Remove RWR terms that equal seed nodes
terms_digitalisierung_technik <- subset(terms_digitalisierung_technik, !(NodeNames %in% seeds_list_vec[[6]]))
#terms_digitalisierung_technik <- terms_digitalisierung_technik[ ! terms_digitalisierung_technik$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_digitalisierung_technik (and give them the score value 1)
terms_seeds_digitalisierung_technik <- as.data.frame(seeds_digitalisierung_technik$feature)
names(terms_seeds_digitalisierung_technik)[names(terms_seeds_digitalisierung_technik)=="seeds_digitalisierung_technik$feature"] <- "NodeNames"
terms_seeds_digitalisierung_technik <- terms_seeds_digitalisierung_technik %>% mutate(Score = NA)
terms_seeds_digitalisierung_technik <- terms_seeds_digitalisierung_technik %>% mutate(ScoreNorm = 1)
terms_seeds_digitalisierung_technik <- terms_seeds_digitalisierung_technik %>% mutate(SeedNode = NA)
terms_digitalisierung_technik <- rbind(terms_digitalisierung_technik,terms_seeds_digitalisierung_technik)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_digitalisierung_technik_sum <- terms_digitalisierung_technik %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_digitalisierung_technik$Score <- round(terms_digitalisierung_technik$Score, 10)
terms_digitalisierung_technik_info <- terms_digitalisierung_technik %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_digitalisierung_technik_info2 <- terms_digitalisierung_technik %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_digitalisierung_technik_sum <- merge(terms_digitalisierung_technik_sum, terms_digitalisierung_technik_info, by = "NodeNames")
terms_digitalisierung_technik_sum <- merge(terms_digitalisierung_technik_sum, terms_digitalisierung_technik_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_digitalisierung_technik_sum <- cbind(terms_digitalisierung_technik_sum,as.data.frame(lapply(terms_digitalisierung_technik_sum[2], minMax)))
names(terms_digitalisierung_technik_sum)[5] <- "ScoreNorm2"
terms_digitalisierung_technik_sum <- terms_digitalisierung_technik_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_digitalisierung_technik_sum, file=paste0('terms_list_',names[6],'.RDA'))
write_csv2(terms_digitalisierung_technik_sum, file=paste0('terms_list_',names[6],'.CSV'))

#---------------------------------#
# Gesellschaft ####################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_gesellschaft <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[7]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_gesellschaft <- rbind(terms_gesellschaft,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_gesellschaft
#Remove RWR terms that equal seed nodes
terms_gesellschaft <- subset(terms_gesellschaft, !(NodeNames %in% seeds_list_vec[[7]]))
#terms_gesellschaft <- terms_gesellschaft[ ! terms_gesellschaft$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_gesellschaft (and give them the score value 1)
terms_seeds_gesellschaft <- as.data.frame(seeds_gesellschaft$feature)
names(terms_seeds_gesellschaft)[names(terms_seeds_gesellschaft)=="seeds_gesellschaft$feature"] <- "NodeNames"
terms_seeds_gesellschaft <- terms_seeds_gesellschaft %>% mutate(Score = NA)
terms_seeds_gesellschaft <- terms_seeds_gesellschaft %>% mutate(ScoreNorm = 1)
terms_seeds_gesellschaft <- terms_seeds_gesellschaft %>% mutate(SeedNode = NA)
terms_gesellschaft <- rbind(terms_gesellschaft,terms_seeds_gesellschaft)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_gesellschaft_sum <- terms_gesellschaft %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_gesellschaft$Score <- round(terms_gesellschaft$Score, 10)
terms_gesellschaft_info <- terms_gesellschaft %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_gesellschaft_info2 <- terms_gesellschaft %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_gesellschaft_sum <- merge(terms_gesellschaft_sum, terms_gesellschaft_info, by = "NodeNames")
terms_gesellschaft_sum <- merge(terms_gesellschaft_sum, terms_gesellschaft_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_gesellschaft_sum <- cbind(terms_gesellschaft_sum,as.data.frame(lapply(terms_gesellschaft_sum[2], minMax)))
names(terms_gesellschaft_sum)[5] <- "ScoreNorm2"
terms_gesellschaft_sum <- terms_gesellschaft_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_gesellschaft_sum, file=paste0('terms_list_',names[7],'.RDA'))
write_csv2(terms_gesellschaft_sum, file=paste0('terms_list_',names[7],'.CSV'))

#---------------------------------#
# Gesundheit ######################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_gesundheit <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[8]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_gesundheit <- rbind(terms_gesundheit,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_gesundheit
#Remove RWR terms that equal seed nodes
terms_gesundheit <- subset(terms_gesundheit, !(NodeNames %in% seeds_list_vec[[8]]))
#terms_gesundheit <- terms_gesundheit[ ! terms_gesundheit$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_gesundheit (and give them the score value 1)
terms_seeds_gesundheit <- as.data.frame(seeds_gesundheit$feature)
names(terms_seeds_gesundheit)[names(terms_seeds_gesundheit)=="seeds_gesundheit$feature"] <- "NodeNames"
terms_seeds_gesundheit <- terms_seeds_gesundheit %>% mutate(Score = NA)
terms_seeds_gesundheit <- terms_seeds_gesundheit %>% mutate(ScoreNorm = 1)
terms_seeds_gesundheit <- terms_seeds_gesundheit %>% mutate(SeedNode = NA)
terms_gesundheit <- rbind(terms_gesundheit,terms_seeds_gesundheit)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_gesundheit_sum <- terms_gesundheit %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_gesundheit$Score <- round(terms_gesundheit$Score, 10)
terms_gesundheit_info <- terms_gesundheit %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_gesundheit_info2 <- terms_gesundheit %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_gesundheit_sum <- merge(terms_gesundheit_sum, terms_gesundheit_info, by = "NodeNames")
terms_gesundheit_sum <- merge(terms_gesundheit_sum, terms_gesundheit_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_gesundheit_sum <- cbind(terms_gesundheit_sum,as.data.frame(lapply(terms_gesundheit_sum[2], minMax)))
names(terms_gesundheit_sum)[5] <- "ScoreNorm2"
terms_gesundheit_sum <- terms_gesundheit_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_gesundheit_sum, file=paste0('terms_list_',names[8],'.RDA'))
write_csv2(terms_gesundheit_sum, file=paste0('terms_list_',names[8],'.CSV'))

#---------------------------------#
# Haushalt & Finanzen #############
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_haushalt_finanzen <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[9]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_haushalt_finanzen <- rbind(terms_haushalt_finanzen,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_haushalt_finanzen
#Remove RWR terms that equal seed nodes
terms_haushalt_finanzen <- subset(terms_haushalt_finanzen, !(NodeNames %in% seeds_list_vec[[9]]))
#terms_haushalt_finanzen <- terms_haushalt_finanzen[ ! terms_haushalt_finanzen$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_haushalt_finanzen (and give them the score value 1)
terms_seeds_haushalt_finanzen <- as.data.frame(seeds_haushalt_finanzen$feature)
names(terms_seeds_haushalt_finanzen)[names(terms_seeds_haushalt_finanzen)=="seeds_haushalt_finanzen$feature"] <- "NodeNames"
terms_seeds_haushalt_finanzen <- terms_seeds_haushalt_finanzen %>% mutate(Score = NA)
terms_seeds_haushalt_finanzen <- terms_seeds_haushalt_finanzen %>% mutate(ScoreNorm = 1)
terms_seeds_haushalt_finanzen <- terms_seeds_haushalt_finanzen %>% mutate(SeedNode = NA)
terms_haushalt_finanzen <- rbind(terms_haushalt_finanzen,terms_seeds_haushalt_finanzen)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_haushalt_finanzen_sum <- terms_haushalt_finanzen %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_haushalt_finanzen$Score <- round(terms_haushalt_finanzen$Score, 10)
terms_haushalt_finanzen_info <- terms_haushalt_finanzen %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_haushalt_finanzen_info2 <- terms_haushalt_finanzen %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_haushalt_finanzen_sum <- merge(terms_haushalt_finanzen_sum, terms_haushalt_finanzen_info, by = "NodeNames")
terms_haushalt_finanzen_sum <- merge(terms_haushalt_finanzen_sum, terms_haushalt_finanzen_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_haushalt_finanzen_sum <- cbind(terms_haushalt_finanzen_sum,as.data.frame(lapply(terms_haushalt_finanzen_sum[2], minMax)))
names(terms_haushalt_finanzen_sum)[5] <- "ScoreNorm2"
terms_haushalt_finanzen_sum <- terms_haushalt_finanzen_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_haushalt_finanzen_sum, file=paste0('terms_list_',names[9],'.RDA'))
write_csv2(terms_haushalt_finanzen_sum, file=paste0('terms_list_',names[9],'.CSV'))

#---------------------------------#
# Innere Sicherheit ###############
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_innere_sicherheit <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[10]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_innere_sicherheit <- rbind(terms_innere_sicherheit,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_innere_sicherheit
#Remove RWR terms that equal seed nodes
terms_innere_sicherheit <- subset(terms_innere_sicherheit, !(NodeNames %in% seeds_list_vec[[10]]))
#terms_innere_sicherheit <- terms_innere_sicherheit[ ! terms_innere_sicherheit$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_innere_sicherheit (and give them the score value 1)
terms_seeds_innere_sicherheit <- as.data.frame(seeds_innere_sicherheit$feature)
names(terms_seeds_innere_sicherheit)[names(terms_seeds_innere_sicherheit)=="seeds_innere_sicherheit$feature"] <- "NodeNames"
terms_seeds_innere_sicherheit <- terms_seeds_innere_sicherheit %>% mutate(Score = NA)
terms_seeds_innere_sicherheit <- terms_seeds_innere_sicherheit %>% mutate(ScoreNorm = 1)
terms_seeds_innere_sicherheit <- terms_seeds_innere_sicherheit %>% mutate(SeedNode = NA)
terms_innere_sicherheit <- rbind(terms_innere_sicherheit,terms_seeds_innere_sicherheit)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_innere_sicherheit_sum <- terms_innere_sicherheit %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_innere_sicherheit$Score <- round(terms_innere_sicherheit$Score, 10)
terms_innere_sicherheit_info <- terms_innere_sicherheit %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_innere_sicherheit_info2 <- terms_innere_sicherheit %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_innere_sicherheit_sum <- merge(terms_innere_sicherheit_sum, terms_innere_sicherheit_info, by = "NodeNames")
terms_innere_sicherheit_sum <- merge(terms_innere_sicherheit_sum, terms_innere_sicherheit_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_innere_sicherheit_sum <- cbind(terms_innere_sicherheit_sum,as.data.frame(lapply(terms_innere_sicherheit_sum[2], minMax)))
names(terms_innere_sicherheit_sum)[5] <- "ScoreNorm2"
terms_innere_sicherheit_sum <- terms_innere_sicherheit_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_innere_sicherheit_sum, file=paste0('terms_list_',names[10],'.RDA'))
write_csv2(terms_innere_sicherheit_sum, file=paste0('terms_list_',names[10],'.CSV'))

#---------------------------------#
# Kultur, Medien und Sport ########
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_kultur_medien_sport <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[11]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_kultur_medien_sport <- rbind(terms_kultur_medien_sport,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_kultur_medien_sport
#Remove RWR terms that equal seed nodes
terms_kultur_medien_sport <- subset(terms_kultur_medien_sport, !(NodeNames %in% seeds_list_vec[[11]]))
#terms_kultur_medien_sport <- terms_kultur_medien_sport[ ! terms_kultur_medien_sport$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_kultur_medien_sport (and give them the score value 1)
terms_seeds_kultur_medien_sport <- as.data.frame(seeds_kultur_medien_sport$feature)
names(terms_seeds_kultur_medien_sport)[names(terms_seeds_kultur_medien_sport)=="seeds_kultur_medien_sport$feature"] <- "NodeNames"
terms_seeds_kultur_medien_sport <- terms_seeds_kultur_medien_sport %>% mutate(Score = NA)
terms_seeds_kultur_medien_sport <- terms_seeds_kultur_medien_sport %>% mutate(ScoreNorm = 1)
terms_seeds_kultur_medien_sport <- terms_seeds_kultur_medien_sport %>% mutate(SeedNode = NA)
terms_kultur_medien_sport <- rbind(terms_kultur_medien_sport,terms_seeds_kultur_medien_sport)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_kultur_medien_sport_sum <- terms_kultur_medien_sport %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_kultur_medien_sport$Score <- round(terms_kultur_medien_sport$Score, 10)
terms_kultur_medien_sport_info <- terms_kultur_medien_sport %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_kultur_medien_sport_info2 <- terms_kultur_medien_sport %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_kultur_medien_sport_sum <- merge(terms_kultur_medien_sport_sum, terms_kultur_medien_sport_info, by = "NodeNames")
terms_kultur_medien_sport_sum <- merge(terms_kultur_medien_sport_sum, terms_kultur_medien_sport_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_kultur_medien_sport_sum <- cbind(terms_kultur_medien_sport_sum,as.data.frame(lapply(terms_kultur_medien_sport_sum[2], minMax)))
names(terms_kultur_medien_sport_sum)[5] <- "ScoreNorm2"
terms_kultur_medien_sport_sum <- terms_kultur_medien_sport_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_kultur_medien_sport_sum, file=paste0('terms_list_',names[11],'.RDA'))
write_csv2(terms_kultur_medien_sport_sum, file=paste0('terms_list_',names[11],'.CSV'))

#---------------------------------#
# Landwirtschaft & Ernährung ######
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_landwirtschaft_ernährung <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[12]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_landwirtschaft_ernährung <- rbind(terms_landwirtschaft_ernährung,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_landwirtschaft_ernährung
#Remove RWR terms that equal seed nodes
terms_landwirtschaft_ernährung <- subset(terms_landwirtschaft_ernährung, !(NodeNames %in% seeds_list_vec[[12]]))
#terms_landwirtschaft_ernährung <- terms_landwirtschaft_ernährung[ ! terms_landwirtschaft_ernährung$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_landwirtschaft_ernährung (and give them the score value 1)
terms_seeds_landwirtschaft_ernährung <- as.data.frame(seeds_landwirtschaft_ernährung$feature)
names(terms_seeds_landwirtschaft_ernährung)[names(terms_seeds_landwirtschaft_ernährung)=="seeds_landwirtschaft_ernährung$feature"] <- "NodeNames"
terms_seeds_landwirtschaft_ernährung <- terms_seeds_landwirtschaft_ernährung %>% mutate(Score = NA)
terms_seeds_landwirtschaft_ernährung <- terms_seeds_landwirtschaft_ernährung %>% mutate(ScoreNorm = 1)
terms_seeds_landwirtschaft_ernährung <- terms_seeds_landwirtschaft_ernährung %>% mutate(SeedNode = NA)
terms_landwirtschaft_ernährung <- rbind(terms_landwirtschaft_ernährung,terms_seeds_landwirtschaft_ernährung)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_landwirtschaft_ernährung_sum <- terms_landwirtschaft_ernährung %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_landwirtschaft_ernährung$Score <- round(terms_landwirtschaft_ernährung$Score, 10)
terms_landwirtschaft_ernährung_info <- terms_landwirtschaft_ernährung %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_landwirtschaft_ernährung_info2 <- terms_landwirtschaft_ernährung %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_landwirtschaft_ernährung_sum <- merge(terms_landwirtschaft_ernährung_sum, terms_landwirtschaft_ernährung_info, by = "NodeNames")
terms_landwirtschaft_ernährung_sum <- merge(terms_landwirtschaft_ernährung_sum, terms_landwirtschaft_ernährung_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_landwirtschaft_ernährung_sum <- cbind(terms_landwirtschaft_ernährung_sum,as.data.frame(lapply(terms_landwirtschaft_ernährung_sum[2], minMax)))
names(terms_landwirtschaft_ernährung_sum)[5] <- "ScoreNorm2"
terms_landwirtschaft_ernährung_sum <- terms_landwirtschaft_ernährung_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_landwirtschaft_ernährung_sum, file=paste0('terms_list_',names[12],'.RDA'))
write_csv2(terms_landwirtschaft_ernährung_sum, file=paste0('terms_list_',names[12],'.CSV'))

#---------------------------------#
# Soziales ########################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_soziales <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[13]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_soziales <- rbind(terms_soziales,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_soziales
#Remove RWR terms that equal seed nodes
terms_soziales <- subset(terms_soziales, !(NodeNames %in% seeds_list_vec[[13]]))
#terms_soziales <- terms_soziales[ ! terms_soziales$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_soziales (and give them the score value 1)
terms_seeds_soziales <- as.data.frame(seeds_soziales$feature)
names(terms_seeds_soziales)[names(terms_seeds_soziales)=="seeds_soziales$feature"] <- "NodeNames"
terms_seeds_soziales <- terms_seeds_soziales %>% mutate(Score = NA)
terms_seeds_soziales <- terms_seeds_soziales %>% mutate(ScoreNorm = 1)
terms_seeds_soziales <- terms_seeds_soziales %>% mutate(SeedNode = NA)
terms_soziales <- rbind(terms_soziales,terms_seeds_soziales)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_soziales_sum <- terms_soziales %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_soziales$Score <- round(terms_soziales$Score, 10)
terms_soziales_info <- terms_soziales %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_soziales_info2 <- terms_soziales %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_soziales_sum <- merge(terms_soziales_sum, terms_soziales_info, by = "NodeNames")
terms_soziales_sum <- merge(terms_soziales_sum, terms_soziales_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_soziales_sum <- cbind(terms_soziales_sum,as.data.frame(lapply(terms_soziales_sum[2], minMax)))
names(terms_soziales_sum)[5] <- "ScoreNorm2"
terms_soziales_sum <- terms_soziales_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_soziales_sum, file=paste0('terms_list_',names[13],'.RDA'))
write_csv2(terms_soziales_sum, file=paste0('terms_list_',names[13],'.CSV'))

#---------------------------------#
# Umwelt ##########################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_umwelt <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[14]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_umwelt <- rbind(terms_umwelt,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_umwelt
#Remove RWR terms that equal seed nodes
terms_umwelt <- subset(terms_umwelt, !(NodeNames %in% seeds_list_vec[[14]]))
#terms_umwelt <- terms_umwelt[ ! terms_umwelt$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_umwelt (and give them the score value 1)
terms_seeds_umwelt <- as.data.frame(seeds_umwelt$feature)
names(terms_seeds_umwelt)[names(terms_seeds_umwelt)=="seeds_umwelt$feature"] <- "NodeNames"
terms_seeds_umwelt <- terms_seeds_umwelt %>% mutate(Score = NA)
terms_seeds_umwelt <- terms_seeds_umwelt %>% mutate(ScoreNorm = 1)
terms_seeds_umwelt <- terms_seeds_umwelt %>% mutate(SeedNode = NA)
terms_umwelt <- rbind(terms_umwelt,terms_seeds_umwelt)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_umwelt_sum <- terms_umwelt %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_umwelt$Score <- round(terms_umwelt$Score, 10)
terms_umwelt_info <- terms_umwelt %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_umwelt_info2 <- terms_umwelt %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_umwelt_sum <- merge(terms_umwelt_sum, terms_umwelt_info, by = "NodeNames")
terms_umwelt_sum <- merge(terms_umwelt_sum, terms_umwelt_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_umwelt_sum <- cbind(terms_umwelt_sum,as.data.frame(lapply(terms_umwelt_sum[2], minMax)))
names(terms_umwelt_sum)[5] <- "ScoreNorm2"
terms_umwelt_sum <- terms_umwelt_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_umwelt_sum, file=paste0('terms_list_',names[14],'.RDA'))
write_csv2(terms_umwelt_sum, file=paste0('terms_list_',names[14],'.CSV'))

#---------------------------------#
# Verkehr #########################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_verkehr <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[15]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_verkehr <- rbind(terms_verkehr,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_verkehr
#Remove RWR terms that equal seed nodes
terms_verkehr <- subset(terms_verkehr, !(NodeNames %in% seeds_list_vec[[15]]))
#terms_verkehr <- terms_verkehr[ ! terms_verkehr$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_verkehr (and give them the score value 1)
terms_seeds_verkehr <- as.data.frame(seeds_verkehr$feature)
names(terms_seeds_verkehr)[names(terms_seeds_verkehr)=="seeds_verkehr$feature"] <- "NodeNames"
terms_seeds_verkehr <- terms_seeds_verkehr %>% mutate(Score = NA)
terms_seeds_verkehr <- terms_seeds_verkehr %>% mutate(ScoreNorm = 1)
terms_seeds_verkehr <- terms_seeds_verkehr %>% mutate(SeedNode = NA)
terms_verkehr <- rbind(terms_verkehr,terms_seeds_verkehr)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_verkehr_sum <- terms_verkehr %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_verkehr$Score <- round(terms_verkehr$Score, 10)
terms_verkehr_info <- terms_verkehr %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_verkehr_info2 <- terms_verkehr %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_verkehr_sum <- merge(terms_verkehr_sum, terms_verkehr_info, by = "NodeNames")
terms_verkehr_sum <- merge(terms_verkehr_sum, terms_verkehr_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_verkehr_sum <- cbind(terms_verkehr_sum,as.data.frame(lapply(terms_verkehr_sum[2], minMax)))
names(terms_verkehr_sum)[5] <- "ScoreNorm2"
terms_verkehr_sum <- terms_verkehr_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_verkehr_sum, file=paste0('terms_list_',names[15],'.RDA'))
write_csv2(terms_verkehr_sum, file=paste0('terms_list_',names[15],'.CSV'))

#---------------------------------#
# Verteidigung ####################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_verteidigung <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[16]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_verteidigung <- rbind(terms_verteidigung,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_verteidigung
#Remove RWR terms that equal seed nodes
terms_verteidigung <- subset(terms_verteidigung, !(NodeNames %in% seeds_list_vec[[16]]))
#terms_verteidigung <- terms_verteidigung[ ! terms_verteidigung$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_verteidigung (and give them the score value 1)
terms_seeds_verteidigung <- as.data.frame(seeds_verteidigung$feature)
names(terms_seeds_verteidigung)[names(terms_seeds_verteidigung)=="seeds_verteidigung$feature"] <- "NodeNames"
terms_seeds_verteidigung <- terms_seeds_verteidigung %>% mutate(Score = NA)
terms_seeds_verteidigung <- terms_seeds_verteidigung %>% mutate(ScoreNorm = 1)
terms_seeds_verteidigung <- terms_seeds_verteidigung %>% mutate(SeedNode = NA)
terms_verteidigung <- rbind(terms_verteidigung,terms_seeds_verteidigung)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_verteidigung_sum <- terms_verteidigung %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_verteidigung$Score <- round(terms_verteidigung$Score, 10)
terms_verteidigung_info <- terms_verteidigung %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_verteidigung_info2 <- terms_verteidigung %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_verteidigung_sum <- merge(terms_verteidigung_sum, terms_verteidigung_info, by = "NodeNames")
terms_verteidigung_sum <- merge(terms_verteidigung_sum, terms_verteidigung_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_verteidigung_sum <- cbind(terms_verteidigung_sum,as.data.frame(lapply(terms_verteidigung_sum[2], minMax)))
names(terms_verteidigung_sum)[5] <- "ScoreNorm2"
terms_verteidigung_sum <- terms_verteidigung_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_verteidigung_sum, file=paste0('terms_list_',names[16],'.RDA'))
write_csv2(terms_verteidigung_sum, file=paste0('terms_list_',names[16],'.CSV'))

#---------------------------------#
# Wirtschaft ######################
#---------------------------------#

#Create empty df
i=0
mat <- matrix(ncol = 0, nrow = 0)
terms_wirtschaft <- data.frame(mat)

#Compute RWRs
for (j in seeds_list_vec[[17]]) {
  SeedNode <- j
  RWR_net_Results <- Random.Walk.Restart.Multiplex(AdjMatrixNorm_net,
                                                   net_MultiplexObject,SeedNode)
  affinity_scores <- cbind(as.data.frame(RWR_net_Results[["RWRM_Results"]][["NodeNames"]]),as.data.frame(RWR_net_Results[["RWRM_Results"]][["Score"]]))
  affinity_scores <- cbind(affinity_scores,as.data.frame(lapply(affinity_scores[2], minMax)))
  names(affinity_scores)[1] <- "NodeNames"
  names(affinity_scores)[2] <- "Score"
  names(affinity_scores)[3] <- "ScoreNorm"
  top_affinity_scores <- affinity_scores[affinity_scores$ScoreNorm>0.9, ]
  top_affinity_scores['SeedNode'] = SeedNode
  terms_wirtschaft <- rbind(terms_wirtschaft,top_affinity_scores)
  #save(affinity_scores, file=paste0(names[i],'_',SeedNode,'.RDA'))
  #write_csv2(affinity_scores, file=paste0(names[i],'_',SeedNode,'.CSV'))
}

#Add seed nodes to terms_wirtschaft
#Remove RWR terms that equal seed nodes
terms_wirtschaft <- subset(terms_wirtschaft, !(NodeNames %in% seeds_list_vec[[17]]))
#terms_wirtschaft <- terms_wirtschaft[ ! terms_wirtschaft$NodeNames %in% seeds_list_vec[[1]], ]
#Add seed nodes to terms_wirtschaft (and give them the score value 1)
terms_seeds_wirtschaft <- as.data.frame(seeds_wirtschaft$feature)
names(terms_seeds_wirtschaft)[names(terms_seeds_wirtschaft)=="seeds_wirtschaft$feature"] <- "NodeNames"
terms_seeds_wirtschaft <- terms_seeds_wirtschaft %>% mutate(Score = NA)
terms_seeds_wirtschaft <- terms_seeds_wirtschaft %>% mutate(ScoreNorm = 1)
terms_seeds_wirtschaft <- terms_seeds_wirtschaft %>% mutate(SeedNode = NA)
terms_wirtschaft <- rbind(terms_wirtschaft,terms_seeds_wirtschaft)

#Summarize duplicated rows and add column with SeedNode terms for every duplicated row
terms_wirtschaft_sum <- terms_wirtschaft %>% group_by(NodeNames) %>% summarize(ScoreNorm=mean(ScoreNorm))
terms_wirtschaft$Score <- round(terms_wirtschaft$Score, 10)
terms_wirtschaft_info <- terms_wirtschaft %>% group_by(NodeNames) %>% summarize(Score=paste0(Score, collapse=", "))
terms_wirtschaft_info2 <- terms_wirtschaft %>% group_by(NodeNames) %>% summarize(SeedNode=paste(SeedNode, collapse=", "))
terms_wirtschaft_sum <- merge(terms_wirtschaft_sum, terms_wirtschaft_info, by = "NodeNames")
terms_wirtschaft_sum <- merge(terms_wirtschaft_sum, terms_wirtschaft_info2, by = "NodeNames")

#Normalize ScoreNorm again
terms_wirtschaft_sum <- cbind(terms_wirtschaft_sum,as.data.frame(lapply(terms_wirtschaft_sum[2], minMax)))
names(terms_wirtschaft_sum)[5] <- "ScoreNorm2"
terms_wirtschaft_sum <- terms_wirtschaft_sum[ , c(1, 5, 2, 3, 4)] 

#Save result
save(terms_wirtschaft_sum, file=paste0('terms_list_',names[17],'.RDA'))
write_csv2(terms_wirtschaft_sum, file=paste0('terms_list_',names[17],'.CSV'))