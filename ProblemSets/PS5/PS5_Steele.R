#Problem #3
# Load required packages
library(rvest)
library(dplyr)
library(tidyverse)

# Set URL for webpage to scrape
url <- "https://www.ncaa.com/stats/softball/d1/current/individual/271"

# Scrape table from webpage
table <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(header = TRUE)

# View scraped data
view(table)

#Problem 4
library(devtools)
library(tidyverse)
library(softballR)

pbp<-data.frame(get_espn_pbp(401444869))

view(pbp)

