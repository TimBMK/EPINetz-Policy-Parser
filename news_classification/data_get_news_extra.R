## Get News Data from Erinome EPINetz Server ##
###############################################

## additionally get data for the years 2017, 2018, 2022 and 2023

## connect to the server via terminal: ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@adrastea.ifi.uni-heidelberg.de
## if Adrastea (for tunneling) is down: ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@wega.ifi.uni-heidelberg.de

{
  library(tidyverse)
  library(elastic)
  library(data.table)
  library(vroom)
}


# Setup Elastic Search Connection


source("/data/koenigt/Tools-Scripts/Tools & Scripts/elasticsearch_scrolledsearch.R") # scrolled search function

# connect to Heidelberg Database via Tunnel. In Bash, use:
# ssh -L 9201:erinome.ifi.uni-heidelberg.de:9200 USERNAME@adrastea.ifi.uni-heidelberg.de

credentials <- readRDS("/data/koenigt/elastic_credentials.RDS")

conn <- connect(
  path = "",
  port = 9201,
  user = credentials$user,
  pwd = credentials$password,
  host = "localhost"
)

conn$ping() # check connection



# WP 2017 - 2019

q <- "estimated_date:[\"2017-10-24 00:00:00\" TO \"2019-01-01 00:00:00\"]" 

result <- full_scroll(conn, q = q, index = "german_news")

vroom::vroom_write(result, delim = ",", pipe("pigz > news_classification/data/data_news_2017-2018.csv.tar.gz"))


# 2022 - June 2023 (to end of twitter access)

q <- "estimated_date:[\"2022-01-01 00:00:00\" TO \"2023-06-22 00:00:00\"]" 

result <- full_scroll(conn, q = q, index = "german_news")

vroom::vroom_write(result, delim = ",", pipe("pigz > news_classification/data/data_news_2022-2023.csv.tar.gz"))