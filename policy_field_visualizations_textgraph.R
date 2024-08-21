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
  library(textgraph)
  library(future)
}


plan(multisession, workers = 16)

n_terms <- 10 # number of top terms to be extracted per timeframe and policy field

classification_measure = "ScoreNormMean" # classification measure found in the classification results

timeframes = c("2023-04-02", "2023-04-09", "2023-04-16")

timeframes %>% 
  future_walk(\(timeframe)
              {
                network_twitter <- readRDS(paste0("init_classification/walk_network_data/", 
                                                  timeframe, ".RDS"))
                
                rwr_terms_twitter <- vroom(paste0("init_classification/walk_terms/", 
                                                  timeframe, ".csv"))
                
                twitter_plots <- visualize_rwr(rwr_terms_twitter, 
                                               network_twitter,
                                               group_name = "policy_field",
                                               classification_measure = classification_measure,
                                               n_terms = n_terms,
                                               verbose = F
                )
                
                twitter_plots %>% 
                  iwalk(\(plot, name)
                        ggsave(filename = paste0("twitter_", timeframe, 
                                                 "_", name, ".png"), 
                               plot = plot, 
                               path = "visualizations"))
                
                network_news <- readRDS(paste0("news_classification/walk_network_data/", 
                                               timeframe, ".RDS"))
                
                rwr_terms_news <- vroom(paste0("news_classification/walk_terms/", 
                                               timeframe, ".csv"))
                
                news_plots <- visualize_rwr(rwr_terms_news, 
                                            network_news,
                                            group_name = "policy_field",
                                            classification_measure = classification_measure,
                                            n_terms = n_terms,
                                            verbose = F
                )
                
                news_plots %>% 
                  iwalk(\(plot, name)
                        ggsave(filename = paste0("news_", timeframe, 
                                                 "_", name, ".png"), 
                               plot = plot, 
                               path = "visualizations"))
                
  })
