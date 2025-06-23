library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)
dat = read_html("https://www.worldometers.info/gdp/gdp-by-country/")
table = dat %>% html_table()
tab = table[[1]]
tab <- tab %>%
  mutate(`GDP (abbrev.)` = `GDP (abbrev.)` %>%
           str_sub(2) %>%               # Remove the first character (dollar sign)
           str_extract("^[0-9,.]+") %>% # Extract only the numeric part
           as.numeric())
tab <- tab %>%
  mutate(`GDP (abbrev.)` = case_when(
    row_number() <= 17 ~ str_remove_all(`GDP (abbrev.)`, "\\."),         # Remove dots for the first 17 rows
    TRUE ~ str_extract(`GDP (abbrev.)`, "^[^\\.]+")                      # Keep only the part before the dot for the remaining rows
  ))
tab$`GDP (abbrev.)` <- ifelse(tab$`GDP (abbrev.)` %in% c("224", "214", "201", "192"), 
                              paste0(tab$`GDP (abbrev.)`, "0"), 
                              tab$`GDP (abbrev.)`)
GDP_data = tab[1:167,]
GDP_data$`GDP (abbrev.)`[GDP_data$Country=="Algeria"]<-192
GDP_data = rename(GDP_data, GDP_per_billion=`GDP (abbrev.)`)
GDP_data$GDP_per_billion = as.numeric(GDP_data$GDP_per_billion)
save(GDP_data, file = "GDP.Rdata")

