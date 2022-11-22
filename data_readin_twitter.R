### Read in Twitter Data ###

library(tidyverse)
library(vroom)


### using vroom to read in csv files is considerably faster than reading RDS files
### however, it needs some additional specification to preserve the original data format

list_cols <- # columns that were originally lists, but needed to be flattened for .csv export
  c(
    "hashtags",
    "ext_urls",
    "ext_urls_expanded",
    "ext_urls_unwound",
    "ext_urls_title",
    "ext_urls_description",
    "context.domain.id",
    "context.domain.name",
    "context.domain.description",
    "context.entity.id",
    "context.entity.name",
    "context.entity.description",
    "mentions_username",
    "mentions_user_id",
    "annotation_probability",
    "annotation_type",            
    "annotation_entity"
  )

twitter_data <-
  vroom(
    file = "data_twitter_2019-2021.csv.tar.gz",
    # Important! specify coltypes to preserve correct IDs
    col_types = list(
      tweet_id = "c",
      conversation_id = "c",
      author_id = "c",
      in_reply_to_user_id = "c",
      user_pinned_tweet_id = "c",
      sourcetweet_id = "c",
      sourcetweet_author_id = "c",
      hashtags = "c"
    )
  ) %>%
  # transform flattened columns into lists again
  mutate(across(.cols = any_of(list_cols), ~ ifelse(
    is.na(.x), NA, str_split(.x, pattern = ", ") %>% as.list()
  )))


### we can add additional variables of interest through our EPINetz list

politicians_list <- readRDS("EPINetz_full_collection_list_update_9.RDs")

twitter_data <- # add the "official name" of an author
  twitter_data %>% left_join(politicians_list %>% distinct(user_id, official_name), # distinct to not produce duplicates (some accounts have multiple roles)
                             by = c("author_id" = "user_id"))

twitter_data < -# add party affiliation
  twitter_data %>% left_join(politicians_list %>% distinct(user_id, party), # distinct to not produce duplicates (some accounts have multiple roles)
            by = c("author_id" = "user_id"))       
            #  note that there are two shared CDU/CSU account in the data that are duplicated in the politicians list still
            #  (since they represent both parties, duplicates should be kept)
            #  this effectively duplicates this accounts tweets in the dataset when the party variable is joined


### we can now filter for e.g. certain parties

cdu <- twitter_data %>% fitler(party == "CDU")



### note that columns like hashtags and mentions are lists nested in the datatrame
twitter_data$hashtags[10]

### in order to filter for certain hashtags, it is most efficient to flatten them, then filter. E.g.:

russia <- twitter_data %>% 
  unnest(cols = "hashtags") %>% # unnest the list. This creates a duplicate tweet for every hashtag in the list
  mutate(hashtags = tolower(hashtags)) %>% # convert hashtags to lower case to ignore different spellings
  filter(hashtags == "russland" | hashtags == "ukraine") %>%  # filter for the hashtag(s) we want
  distinct(tweet_id, .keep_all = T)  # since we introduced duplicates, we need to get rid of them again

russia <- russia %>% select(!hashtags) %>% 
  left_join(twitter_data %>% select(tweet_id, hashtags), by = "tweet_id") # we can restore the original hashtags through a join



#### there might be more efficient solutions for filtering, e.g. with map()

# russia_2 <- twitter_data %>% 
#   mutate(hashtags = tolower(hashtags)) %>% # convert hashtags to lower case to ignore different spellings
#   filter(str_detect(.$hashtags, "russland")) # this is a soft detect, i.e. it also detects e.g. "russlanddeutsche"
# 
# russia_3 <- twitter_data %>%
#   filter(map_lgl(hashtags, ~any(.x == "russland))) # this should work in theory, but doesn't retrieve all results somehow?




