# Get data from HD Server #


library(tidyverse)
library(elastic)
library(ssh)

user = readline(prompt = "Enter Heidelberg Username: ") # enter credentials in console
password = readline(prompt = "Enter Password: ") # user prompts for username and password, so the password is not visible in the script

# host = "@wega.ifi.uni-heidelberg.de" # wega or adrastea to jump
# 
# jump <- ssh_connect(host = paste0(user, host),
#             passwd = password)
# 
# tunnel <- ssh_tunnel(jump,  port = 9200, target = "erinome.ifi.uni-heidelberg.de:9200") # this command freezes the session?!

conn <- connect(
  path = "",
  port = 9201,
  user = user,
  pwd = password,
  host = "localhost"
)

conn$ping()
