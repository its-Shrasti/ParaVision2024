library(rvest)
library(dplyr)
library(tidyverse)
html <- read_html("https://en.wikipedia.org/wiki/All-time_Paralympic_Games_medal_table")
tables <- html %>%html_nodes("table") %>%html_table(fill = TRUE)
tab <- tables[[2]]
tab <-  tab[-1,] 
colnames(tab) <-c("Country","No.","Gold","Silver","Bronze","Total")
tab <- tab[1:6]
tab$No.<-as.numeric(tab$No.)
tab$Total<-as.numeric(tab$Total)
rename(tab,Number_of_games_participated=No.)
all_medal_tally <- tab
save(all_medal_tally,file="Paralympics_total_medal_tally.Rdata")
