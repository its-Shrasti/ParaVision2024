library(tidyverse)
dat = read.csv("paris-paralympic-2024-medal-table-3.csv")
medal_table = dat %>% slice(1:80)
save(medal_table, file = "Gender-wise Para 2024.Rdata")
