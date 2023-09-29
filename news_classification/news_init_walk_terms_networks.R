## Initial Classification ##
############################

# Classify Additional Terms #

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
}


source("get_rwr_terms.R")

source("utils_text_processing.R")

dir <- "news_classification"

recalculate_all = FALSE # should walk networks already in the walk_network_data directory be recalulcalated? Setting it to FALSE is helpful if only a number of as-of-yet unclaculated networks should be computed

cat("\n ======= Preparations ======= \n")

time_frame_walks = weeks(12) # length of the time frame for random walks

# read data
news_data <- list.files(file.path(dir, "data"), 
                        pattern = "data_news_", 
                        full.names = T) %>% 
  map(\(file)
      {
        data <- vroom(file = file) %>%
          select(
            # drop unnecessary columns
            where(~ !all(is.na(.x))) &  # drop columns containing only NAs
              !c(
                "_index",
                "_type",
                "_score",
                "_source.nlp_pipeline_applied",     # drop db internal information
                "_source.lang",
                "_source.publication_date",
                "_source.crawled",
                "_source.extracted"
              ) # drop redundant information (estimated date is the only important date)
          ) %>%
          rename_with( ~ str_remove(.x, pattern = "_source.")) %>%  # drop "_source." from variable names
          rename("id" = "_id") # drop "_" from id
        return(data)
  }) %>% rbindlist(fill = TRUE)

# Map
### the whole process will be mapped, and only one token object (timefrime of 12-18 months) processed at a time, to save ressources

list.files(dir, pattern = "tokens_news", full.names = T) %>% # token files
  walk(\(file){
    cat(paste("\n\n ====== **", file, "** ======= \n"))
    
    ## read tokens, NEs only, make week indicator 
    ### (as the combined tokens get very large, we read them seperately and reduce them to NEs on the fly)
    walk_NE <- file %>%
      vroom() %>% 
      filter_tokens(tokens_col = "lemma", 
                    tags = c("NN", "NE"), # Noun words and NEs only
                    #minimum string length, stopwords dictionaries, additional stopwords and lower casing set to default
                    replies = NULL, # not required
                    keep_mentions = NULL, # not required
                    keep_urls = NULL # not required
      ) %>% 
      left_join(news_data %>% select(id, estimated_date), by = join_by(doc_id == id)) %>% # add date indicator
      mutate(week = ceiling_date(as_datetime(estimated_date), # make week indicator (last day of the week)
                                 unit = "week"))
    
    gc()
    
    # List of Timeframes
    ## in contrast to the seed terms, we directly use the NE data for the list of 
    ##  frames, since we need all NEs (not specific to ministries etc.) anyways
    
    cat("\n ======= Make List of Timeframes ======= \n")
    
    dat_list <- map(walk_NE$week %>% unique(), # parallelization overhead is larger than the gain, so we use a simple map() here
                    ~ {
                      walk_NE %>% 
                        filter(week >= (.x - time_frame_walks) & # beginning: set above
                                 week <= .x)          # end: week of interest
                    })
    
    names(dat_list) <- walk_NE$week %>% unique() # name the dataframes in the list
    
    rm(walk_NE)
    
    gc()
    
    ## drop 10% percentile of counts for each time frame
    
    # dat_list <- dat_list %>% 
    #   future_map(\(dat)
    #              dat %>% drop_quantile(.,
    #                            tokens = "lemma",
    #                            quantile = 0.1,
    #                            ignore_case = F, # already transformed to lower case
    #                            group = "tag",
    #                            verbose = F),
    #              .progress = T)
    
    if (!recalculate_all) { # drop timeframes from the list where networks were already calculated 
      dat_list <-
        dat_list[!(names(dat_list) %in% 
                     str_remove(list.files(file.path(dir, "walk_network_data")), 
                                ".RDS"))]
    }
    
    if (length(dat_list) > 0){
      cat("\n ======= Drop 10% Quantiles ======= \n")
      ## we reduce the data as much as we can before committing to the network calculation (rahter than dropping quantiles during the network map)
      for (i in 1:length(dat_list)){ # somehow this is faster and more stable than mapping the function
        
        dat_list[[i]] <-  drop_quantile(dat_list[[i]],
                                        tokens = "lemma",
                                        quantile = 0.1,
                                        ignore_case = F, # already transformed to lower case
                                        group = "tag",
                                        verbose = F)
        
      }
      
      
      # Prepare Network for Random Walks
      
      cat("\n ======= Make Networks for Random Walks ======= \n")
      
      #plan(sequential) # no multisession - seems unstable for certain processes here, such as making the networks [we can rather use a simple iwalk here than changing the plan]
      
      if (!dir.exists(file.path(dir, "walk_network_data"))) {
        dir.create(file.path(dir, "walk_network_data"))
      }
      
      dat_list %>% 
        iwalk(\(dat, name) # saving the results as .RDS to read in later is more stable than keeping them in the environment
              {make_multiplex_objects(dat,
                                      vertex_a = "doc_id",
                                      vertex_b = "lemma",
                                      directed = F,
                                      pmi_weight = T,
                                      keep_igraph_network = F,
                                      keep_multiplex_network = T,
                                      keep_adjacency_matrix = F,
                                      keep_normalized_adjacency_matrix = T) %>% 
                  saveRDS(file = paste0(dir, "/walk_network_data/",
                                        name, ".RDS"))},
              .progress = F)
      
      
      gc()
    } else {
      cat("\nAll timeframes have already been calculated. Set recalculate_all == TRUE to recalculate them.\n")
    }
  })
