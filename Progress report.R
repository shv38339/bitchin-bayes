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
home <- home[,-c(2,3,8,9)]
visiting <- visiting[, -c(2,3,8,9)]

dates <- as.Date(home$Date, "%a, %b %d, %Y")
home$Date <- dates

dates2 <- as.Date(visiting$Date, "%a, %b %d, %Y")
visiting$Date <- dates2
View(home)

games <- rbind(home, visiting)
games <- games %>% arrange(Date)
View(games)

#convert VisitingTeam == "Boston Celtics" to 1, 0 otherwise
#convert HomeTeam == "Boston Celtics" to 1, -1 otherwise
#Final result: VisitingTeam + HomeTeam = 1 if Home, 0 otherwise
games$VisitingTeam <- ifelse(games$VisitingTeam == "Boston Celtics", 1, 0)
games$HomeTeam <- ifelse(games$HomeTeam == "Boston Celtics", 1, -1)
games$Home <- games$VisitingTeam + games$HomeTeam
View(games)
games <- games[, -c(2,3,4,5)]
View(games)

plot(games$score.dif, type = "b")
abline(h = 0)

#Further steps: 
## time series model specification
## hierarchy based on teams???


