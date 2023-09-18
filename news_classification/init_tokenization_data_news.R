## Get News Data from Erinome EPINetz Server ##
###############################################

## connect to the server via terminal: ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@adrastea.ifi.uni-heidelberg.de
## if Adrastea (for tunneling) is down: ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@wega.ifi.uni-heidelberg.de

library(tidyverse)
library(elastic)


committees <- read_csv("Seed_Accounts/committee_seeds_19-20_2023-04-06.csv", col_types = list(user_id = "c")) # only necessary for min date

#### API call ####

date_range <- tibble(until = ymd("2023-06-22"), from = min(committees$begin)) 

# Setup Elastic Search Connection

user = readline(prompt = "Enter Heidelberg Username: ") # enter credentials in console
password = readline(prompt = "Enter Password: ") # user prompts for username and password, so the password is not visible in the script

conn <- connect(
  path = "",
  port = 9201,
  user = user,
  pwd = password,
  host = "localhost"
)

conn$ping()


# result <- Search(conn, index = "german_news", # available indices: german_news, twitter_v2_tweets, twitter_v2_users
#                  q = q, size = 100) # maximum: 10.000 results per call


# load function for more than 10.000 results
source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R")

q <- paste0("estimate_date:[\"", date_range$from, "T00:00:00Z\" TO \"", date_range$until, "T00:00:00Z\"]")

result <- full_scroll(conn, q = q, index = "german_news")

vroom::vroom_write(result, delim = ",", pipe("pigz > news_classification/data_news_2017-2023.csv.tar.gz"))



