setwd('C:/Users/Jack Galati/Documents/Projects/A League Attendances')

#These were all obtained from https://www.flashscore.com

Season_19 <- read.csv("Season 19.csv", na.strings = c("", "NA"))
Season_18 <- read.csv("Season 18.csv", na.strings = c("", "NA"))
Season_17 <- read.csv("Season 17.csv", na.strings = c("", "NA"))

Season_19 <- Season_19 %>%
  fill(Round, .direction = "down")

Season_18 <- Season_18 %>%
  fill(Round, .direction = "down")

Season_17 <- Season_17 %>%
  fill(Round, .direction = "down")

Season_19$Season <- "19"
Season_18$Season <- "18"
Season_17$Season <- "17"

All_Seasons <- rbind(Season_19, Season_18, Season_17)

All_Seasons <- na.omit(All_Seasons)

Scores_List <- unlist(strsplit(as.character(All_Seasons$Score), ":"))

All_Seasons <- cbind(All_Seasons, as.data.frame(matrix(Scores_List, ncol = 2, byrow = TRUE)))

All_Seasons <- rename(All_Seasons, Home_Score = V1, Away_Score = V2)

All_Seasons$Score <- NULL

write.csv(All_Seasons,"Season_Results.csv", row.names = FALSE)
