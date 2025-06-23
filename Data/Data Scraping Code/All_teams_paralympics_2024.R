library(rvest)
library(dplyr)
library(tidyverse)
##List of top20 countries
countries <- c('China','Great_Britain', 'United_States','Brazil','Ukraine','France','Italy',
               'Australia','Netherlands','Germany','Japan','Spain','Thailand','South_Korea','Canada','India'
               ,'Colombia','Turkey','Uzbekistan','Iran')#4&8
##Countries with partipants table at 7th position
countries2 <- c('China','France','Australia','Thailand','Uzbekistan')#4&7
##Germany + Japan + Canada + South_Korea sports vs medals nhin h 
countries3 <- c('Italy','Turkey') #4th and 6th
countries4 <- c('Spain') #3rd and 6th

##Iran , 5th and 8th
all_medal_tally <- list()

for (i in countries) {
  url <- paste0("https://en.wikipedia.org/wiki/", i, "_at_the_2024_Summer_Paralympics")
  html <- read_html(url)
  
  tables <- html %>% html_nodes("table") %>% html_table(fill = TRUE)
  if (i %in% countries2){
    tab1=tables[[4]]
    tab2=tables[[7]]
    all_medal_tally[[i]] <- list(tab1 = tab1, tab2 = tab2)
  }
  else if(i %in% countries3){
    tab1=tables[[4]]
    tab2=tables[[6]]
    all_medal_tally[[i]] <- list(tab1 = tab1, tab2 = tab2)
  }
  else if(i %in% countries4){
    tab1=tables[[3]]
    tab2=tables[[6]]
    all_medal_tally[[i]] <- list(tab1 = tab1, tab2 = tab2)
  }
  else{
  tab1=tables[[4]]
  tab2=tables[[8]]
  all_medal_tally[[i]] <- list(tab1 = tab1, tab2 = tab2)}
}
all_medal_tally_2024 <- all_medal_tally
# Save all medal tally data
save(all_medal_tally_2024, file = "All_teams_para2024.Rdata")

