#`Progress Report`: Modeling score differentials

##Loaded data
url <- read_html("http://www.basketball-reference.com/leagues/NBA_2015_games.html")
tbl <- html_table(url)
library(dplyr)
d1 <- tbl_df(as.data.frame(tbl[1]))
d2 <- tbl_df(as.data.frame(tbl[2]))

##Cleaning
d1$Visitor.Neutral <- as.factor(d1$Visitor.Neutral)
d1$Home.Neutral <- as.factor(d1$Home.Neutral)

names(d1) <- c("Date", "ET.Start", "Var.3", "VisitingTeam", "PTS.V", "HomeTeam", "PTS.H", "Var.8", "Notes")
names(d1)
str(d1)

library(dplyr)
home <- d1 %>% filter(HomeTeam == "Boston Celtics")
visiting <- d1 %>% filter(VisitingTeam == "Boston Celtics")

home$score.dif <- home$PTS.H - home$PTS.V #negative values denote celtics' loss
visiting$score.dif <- visiting$PTS.V - visiting$PTS.H

plot(home$score.dif, type = "b", xlab = "Time", ylab = "Score differential", main = "When celtics were the home team")
plot(visiting$score.dif, type = "b", xlab = "Time", ylab = "Score differential", main = "When celtics were the visiting team") ## CLASSIC TIME SERIES 

#Combine the home and visiting by proper time

#Have to convert Date column to date data type
#How to use lubridate?
library(lubridate)


