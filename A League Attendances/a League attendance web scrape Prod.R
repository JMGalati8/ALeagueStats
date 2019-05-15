library(tidyverse)
library(rvest)
library(sqldf)

setwd('C:/Users/Jack Galati/Documents/Projects/A League Attendances')
getwd()

#Create the unique URLs
IDS <- as.data.frame(cbind(seq(from = 927942, to = 928076),"17/18"))
IDS <- rbind(IDS, cbind(seq(from = 854325, to = 854459), "16/17"))
IDS <- rbind(IDS, cbind(seq(from = 811609, to = 811743), "15/16"))
IDS <- rbind(IDS, cbind(seq(from = 753788, to = 753922), "14/15")) #If there are errors, it will be this season
IDS <- rbind(IDS, cbind(seq(from = 1002827, to = 1002961), "18/19"))
url_df <- as.data.frame(cbind(IDS, "https://www.a-league.com.au/match/id/"))
url_df$URL <- paste(url_df[, 3], url_df[, 1], sep ='')

#Do a single export
url <- url_df$URL[1]
webpage <- read_html(url)

#Do the first data export
data1 <- html_nodes(webpage, '.match-banner__team-name , .match-banner__start-date--initialized div , .match-banner__arena , .match-banner__league-wrapper .match-banner__league')
head(html_text(data1), 20)
data_teams <- html_text(data1)

#Do the second data export
data2 <- html_nodes(webpage, '.match-banner__bottom-left-content div')
head(html_text(data2), 20)
data_date <- html_text(data2)

#Fix the first data export
df1 <- as.data.frame(t(data_teams))

#Fix the second data export
df2 <- as.data.frame(t(data_date))

#Combine these tw
df3 <- cbind(df1, df2)

#Ok, that worked. Lets loop it now.
#Create the sets you will need
urls <- unique(url_df$URL)
output <- data.frame()
df_export <- NULL
df_error <- NULL

for (ID in urls) {
  temp = url_df$URL[url_df$URL == ID]
  webpage <- read_html(ID)
  
  #First export
  data_ex_1 <- html_nodes(webpage, '.match-banner__team-name , .match-banner__start-date--initialized div , .match-banner__arena , .match-banner__league-wrapper .match-banner__league')
  data_ex_teams <- html_text(data_ex_1)
  
  #Second export
  data_ex_2 <- html_nodes(webpage, '.match-banner__bottom-left-content div')
  data_ex_date <- html_text(data_ex_2)
  
  #Fix the exports
  df_ex_1 <- as.data.frame(t(data_ex_teams))
  df_ex_2 <- as.data.frame(t(data_ex_date))
  
  #combine these
  df_combined <- cbind(df_ex_1, df_ex_2, url_df$V2[url_df$URL == ID])
  
  #Final df
  if(ncol(df_combined) == 13){
    df_export <- rbind(df_export, df_combined)
  }
  else
    df_error <- rbind(df_error, df_combined)
}

#Lets clean this file now
df_test <- df_export
colnames(df_export) <- (c('Team1', 'Team2', 'Indicator1', 'Stadium_Clean1', 'Attendance_Clean1', 'Indicator2', 'Indicator3', 'Stadium_Clean2', 'Date', 'Attendance2', 'Attendance3', 'Attendance_Clean4', 'Season'))
df_raw <- df_export

#Write a raw version to file
write.csv(df_raw, "Raw_Season_All_Attendances.csv")
write.csv(url_df, "URL_DF_OUTPUT.csv")

#Check of the raw version shows that Indicator2 and Attendance2 didn't survive the export. Easy way to start cutting down on columns
df_export$Indicator2 <- NULL
df_export$Attendance2 <- NULL
df_export$Indicator3 <- NULL #Isn't required
df_export$Stadium_Clean2 <- NULL #Isn't required
df_export$Attendance3 <- NULL #Useless
df_export$Attendance_Clean4 <- NULL #Isn't required

#Now lets clean the date and the indicator
df_export$Date_clean <- str_extract(df_export$Date, "\\d+[\\-]\\d+[\\-]\\d+") #Fixing the date
df_export$Round <- str_extract(df_export$Indicator1, "\\d+") #Getting the round number

#We're doing this so I can create a home stadium reference table
write.csv((unique(df_export$Team1)),"Unique_Teams.csv", row.names = FALSE)
write.csv(unique(df_export$Stadium_Clean1),"Unique_Stadiums.csv", row.names = FALSE)

#Checking whether Team 1 is always the home team - Appears to be the case
REF_HOME_STADIUMS <- read.csv("Home_Stadiums.csv")
join_check <- left_join(df_export, REF_HOME_STADIUMS, by = c("Stadium_Clean1" = "Stadium")) 

#Ok - Fix the column names for the final time
df_output <- df_export
colnames(df_output) <- c("Home_Team", 'Away_Team', 'Indicator', 'Stadium', 'Attendance', 
                        'DROP', 'Season', 'Date', 'Round')
df_output$DROP <- NULL #As the name implies, drop it like it hot
df_output$Attendance <- as.numeric(gsub(",","",df_output$Attendance)) #Need to make this a real number
df_output$Home_Team <- gsub("\t","",gsub("\n","",as.character(df_output$Home_Team))) #Converting this into a character
df_output$Away_Team <- gsub("\t","",gsub("\n","",as.character(df_output$Away_Team))) #Converting this into a character

df_output$Home_Team <- case_when(
                        df_output$Home_Team == 'Melbourne City FC'  ~ 'Melbourne City',
                        df_output$Home_Team == 'Central Coast Mariners'  ~ 'Central Coast Mariners',
                        df_output$Home_Team == 'Melbourne Victory'  ~ 'Melbourne Victory',
                        df_output$Home_Team == 'Wellington Phoenix'  ~ 'Wellington Phoenix',
                        df_output$Home_Team == 'Western Sydney Wanderers FC'  ~ 'Western Sydney Wanderers',
                        df_output$Home_Team == 'Brisbane Roar FC'  ~ 'Brisbane Roar',
                        df_output$Home_Team == 'Newcastle Jets'  ~ 'Newcastle Jets',
                        df_output$Home_Team == 'Sydney FC'  ~ 'Sydney FC',
                        df_output$Home_Team == 'Adelaide United'  ~ 'Adelaide United',
                        df_output$Home_Team == 'Perth Glory'  ~ 'Perth Glory',
                        TRUE ~ 'ERROR'
)

df_output$Away_Team <- case_when (
                        df_output$Away_Team == 'Melbourne City FC'  ~ 'Melbourne City',
                        df_output$Away_Team == 'Central Coast Mariners'  ~ 'Central Coast Mariners',
                        df_output$Away_Team == 'Melbourne Victory'  ~ 'Melbourne Victory',
                        df_output$Away_Team == 'Wellington Phoenix'  ~ 'Wellington Phoenix',
                        df_output$Away_Team == 'Western Sydney Wanderers FC'  ~ 'Western Sydney Wanderers',
                        df_output$Away_Team == 'Brisbane Roar FC'  ~ 'Brisbane Roar',
                        df_output$Away_Team == 'Newcastle Jets'  ~ 'Newcastle Jets',
                        df_output$Away_Team == 'Sydney FC'  ~ 'Sydney FC',
                        df_output$Away_Team == 'Adelaide United'  ~ 'Adelaide United',
                        df_output$Away_Team == 'Perth Glory'  ~ 'Perth Glory',
                        TRUE ~ 'ERROR'
)

ggplot(df_output, aes(x = Home_Team, y = Attendance)) +geom_col()

df_output %>%
  subset(Home_Team == 'Central Coast Mariners') %>%
  ggplot(aes(Home_Team, as.numeric(Attendance))) + geom_col()

write.csv(df_output, "Final_Attendance_Output.csv")

unique_teams <- as.data.frame(unique(df_output$Home_Team))


