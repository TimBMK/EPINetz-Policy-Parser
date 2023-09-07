## Visualize Policy Field Contents ##
#####################################

{
  library(tidyverse)
  library(quanteda)
  library(quanteda.textstats)
  library(data.table)
  library(furrr)
  library(vroom)
  library(igraph)
  library(RandomWalkRestartMH)
  library(scales)
  library(ggraph)
}

source("utils_text_processing.R")
source("get_rwr_terms.R")

n_terms <- 20 # number of top terms to be extracted per timeframe and policy field

classification_measure = "ScoreNormMean" # classification measure found in the classification results

drop_negative_pmi = TRUE # should edges negative PMI weights be dropped to help with graph layouting?

rescale_pmi = TRUE # should the PMI in each network rescaled between 0.01 and 1?

# note that the PMI is only calculated between the top terms and does therefore not represent the true PMI

classification_result <- readRDS("init_classification/init_classified_tweets.RDS")

classification_NE <- get_latest_tokens_file(path = "Tokenizer", 
                                            pattern = "tokens_full.csv.tar.gz") %>% 
  vroom(col_types = list(doc_id = "c")) %>% 
  filter_tokens(tokens_col = "lemma", 
                tags = c("NN", "NE"), # Noun words and NEs only
                #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                replies = classification_replies, # should replies get classified?
                keep_mentions = classification_mentions, # should @-mentions be kept?
                keep_urls = classification_urls) %>%   # should URLs be kept?
  split_timeframes(datetime_var = "_source.created_at", 
                   timeframe = classification_timeframe,
                   before_after = classification_before_after)


docs <- classification_result %>% # pull relevant doc_ids
  imap(\(classification_data, timeframe)
       {
         classification_data %>% 
           .[["classified_documents"]] %>% 
           distinct(doc_id) %>%
           left_join(classification_NE %>% 
                       .[[as.character(timeframe)]] %>% 
                       select(doc_id, lemma), join_by(doc_id))
  })


top_terms <- classification_result %>% 
  imap(\(result, timeframe) 
      {
        result %>%.[["walk_terms"]] %>% 
          filter(lemma %in% (docs %>% .[[as.character(timeframe)]] %>% 
                               distinct(lemma) %>% pull(lemma))) %>% 
                   slice_max(order_by = !!as.name(classification_measure), 
                                       n = n_terms,
                                       by = policy_field
                                       )
  })


# make policy field networks of the top terms, including their policy scores and PMI for documents in the target week
policy_networks <- top_terms %>% 
  imap(\(terms, timeframe)
       {
         dat <- terms %>% left_join(docs %>% .[[as.character(timeframe)]],
                             by = join_by(lemma),
                             relationship = "many-to-many") # duplicated terms over policy fields; duplicated terms over documents
         
         networks <- dat %>% as.data.table() %>% split(by = "policy_field") %>% 
           map(\(group) 
               { 
                 network <- group %>% # note that the PMI is only calculated between the top terms!
                 calculate_network(vertex_a = "doc_id", 
                                   vertex_b = "lemma",
                                   pmi_weight = TRUE,
                                   as_data_frame = FALSE) 
                 
                 if(drop_negative_pmi) {   # drop negative PMI edges (this keeps the nodes!)
                   network <- delete_edges(network, E(network)[weight > 0])
                 }
                 
                 if (rescale_pmi) {
                   E(network)$weight <- rescale(E(network)$weight, 
                                                to = c(0.001, 1)) # weights must be > 0 for layouting
                 }
                 
                 # add policy field scores from the random walks to graph
                 scores <- tibble(lemma = V(network)$name) %>% 
                   left_join(group %>% 
                               select(lemma, 
                                      !!as.name(classification_measure)) %>% 
                               distinct(),
                             by = join_by(lemma))
                 
                 V(network)$score <- scores %>% 
                   pull(!!as.name(classification_measure))
                 
                 return(network)  
                 }) 
         
         return(networks)
  })




# all policy fields together in one graph (to see overlaps etc)

field_networks <- top_terms %>% 
  imap(\(terms, timeframe)
       {
         network <- terms %>%
           select(!seed_term) %>% 
           rename(weight = !!as.name(classification_measure)) %>% 
           graph_from_data_frame(directed = T)
         
         # add policy field indicator
         network <- set_vertex_attr(graph = network, 
                                    name = "policy_field", 
                                    index = V(network)[name %in% terms$policy_field],
                                    value = V(network)[name %in% terms$policy_field]$name)
         
         return(network)
  })



# Visualize

timeframes <- names(classification_result)

timeframe <- timeframes[34] # specify a timeframe here

for (i in 1:length(policy_networks[[timeframe]])){
  graph <- policy_networks[[timeframe]][[i]] %>% 
    ggraph(layout = "fr") +
    geom_edge_fan(aes(edge_alpha = weight), color = "darkgrey") +
    geom_node_point(aes(size = score)) +
    geom_node_text(aes(label = name, size = score), repel = TRUE) +
    scale_size(range = c(3,7)) +
    labs(title = names(policy_networks[1]),
         subtitle = paste("Policy Field:", 
                          names(policy_networks[[timeframe]][i])),
         edge_alpha = "Normalized PMI")
  
  print(graph)
  
  readline(prompt="Press [enter] to display the next policy field graph.")
  
}

field_networks[[timeframe]] %>% 
  ggraph(layout = "fr") +
  geom_edge_fan(aes(edge_alpha = weight)) +
  geom_node_point(aes(color = policy_field)) +
  geom_node_text(aes(label = name, color = policy_field), repel = TRUE) +
  labs(title = labs(title = names(field_networks[1])),
       edge_alpha = "Normalized PMI")



