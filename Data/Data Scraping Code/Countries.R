# loading uncleaned dataset
load("All_teams_para2024.Rdata")
library(tibble)

# function to clean medal tally for each country
data_clean_medals <- function(data)
{ 
  data <- data[-1,]
  vec <- c("Sport", "Gold", "Silver", "Bronze", "Total")
  for(i in 1:5)
  {
    colnames(data)[i] <- vec[i]
  }
 
  for( i in 2:5)
  {
    data[,i] <- as.numeric(data[,i])
  }
  
  return(data)
}

# function to clean participation by gender for each country
data_clean_gender <- function(data)
{
  data <- subset(data, (Men != "—" & Women != "—" & Total != "—"))
  for( i in 2:4)
  {
    data[,i] <- as.numeric(data[,i])
  }
  return(data)
}
#creating medal dataframe for each country
China_medal <- data_clean_medals(all_medal_tally$China$tab1)
Great_Britain_medal <- data_clean_medals(all_medal_tally$Great_Britain$tab1)
US_medal <- data_clean_medals(all_medal_tally$United_States$tab1)
Brazil_medal <- data_clean_medals(all_medal_tally$Brazil$tab1)
Ukraine_medal <- data_clean_medals(all_medal_tally$Ukraine$tab1)
France_medal <- data_clean_medals(all_medal_tally$France$tab1)
Italy_medal <- data_clean_medals(all_medal_tally$Italy$tab1)
Australia_medal <- data_clean_medals(all_medal_tally$Australia$tab1)
Netherlands_medal <- data_clean_medals(all_medal_tally$Netherlands$tab1)
Spain_medal <- data_clean_medals(all_medal_tally$Spain$tab1)
Thailand_medal <- data_clean_medals(all_medal_tally$Thailand$tab1)
India_medal <- data_clean_medals(all_medal_tally$India$tab1)
Colombia_medal <- data_clean_medals(all_medal_tally$Colombia$tab1)
Turkey_medal <- data_clean_medals(all_medal_tally$Turkey$tab1)
Uzbekistan_medal <- data_clean_medals(all_medal_tally$Uzbekistan$tab1)

# creating gender participation dataframe for each country
China_gender <- data_clean_gender(all_medal_tally$China$tab2)
US_gender <- data_clean_gender(all_medal_tally$United_States$tab2)
Brazil_gender <- data_clean_gender(all_medal_tally$Brazil$tab2)
Ukraine_gender <- data_clean_gender(all_medal_tally$Ukraine$tab2)
Italy_gender <- data_clean_gender(all_medal_tally$Italy$tab2)
Australia_gender <- data_clean_gender(all_medal_tally$Australia$tab2)
Spain_gender <- data_clean_gender(all_medal_tally$Spain$tab2)
Thailand_gender <- data_clean_gender(all_medal_tally$Thailand$tab2)
India_gender <- data_clean_gender(all_medal_tally$India$tab2)
Colombia_gender <- data_clean_gender(all_medal_tally$Colombia$tab2)
Turkey_gender <- data_clean_gender(all_medal_tally$Turkey$tab2)
Uzbekistan_gender <- data_clean_gender(all_medal_tally$Uzbekistan$tab2)
Great_Britain_gender <- data_clean_gender(all_medal_tally$Great_Britain$tab2)
# working on France separately

France_gender <- all_medal_tally$France$tab2
France_gender <- France_gender[-17,]
France_gender[17,4]<- sum(as.numeric(France_gender[1:16,4]))
France_gender <- data_clean_gender(France_gender)

# working on Netherlands separatley

Netherlands_gender <- all_medal_tally$Netherlands$tab2
Netherlands_gender[12,2] <- sum(as.numeric(Netherlands_gender[1:11,2]))
Netherlands_gender[12,4] <- sum(as.numeric(Netherlands_gender[1:11,4]))
Netherlands_gender <- data_clean_gender(Netherlands_gender)

# working on Spain separately

Spain_gender <- all_medal_tally$Spain$tab2
Spain_gender[17,4] <-  sum(as.numeric(Spain_gender[1:16,4]))
Spain_gender <- data_clean_gender(Spain_gender)


# function to create a list containing two dataframes

countries_dataset <- tibble(
  "China" = list("df1" = China_medal, "df2" = China_gender),
  "Great_Britain" = list("df1" = Great_Britain_medal, "df2" = Great_Britain_gender),
  "United_States" = list("df1" = US_medal, "df2" = US_gender),
  "Brazil" = list("df1" = Brazil_medal, "df2" = Brazil_gender),
  "Ukraine" = list("df1" = Ukraine_medal, "df2" = Ukraine_gender),
  "France" = list("df1" = France_medal, "df2" = France_gender),
  "Italy" = list("df1" = Italy_medal, "df2" = Italy_gender),
  "Australia" = list("df1" = Australia_medal, "df2" = Australia_gender),
  "Netherlands" = list("df1" = Netherlands_medal, "df2" = Netherlands_gender),
  "Spain" = list("df1" = Spain_medal, "df2" = Spain_gender),
  "Thailand" = list("df1" = Thailand_medal, "df2" = Thailand_gender),
  "India" = list("df1" = India_medal, "df2" = India_gender),
  "Colombia" = list("df1" = Colombia_medal, "df2" = Colombia_gender),
  "Turkey" = list("df1" = Turkey_medal, "df2" = Turkey_gender),
  "Uzbekistan" = list("df1" = Uzbekistan_medal, "df2" = Uzbekistan_gender)
  )

save(countries_dataset, file = "Countries.RData")
