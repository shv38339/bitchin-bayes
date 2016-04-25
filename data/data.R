# extract data with commands from the library rvest ----
library(rvest)
url <- read_html("http://www.basketball-reference.com/leagues/NBA_2004_games.html")
tbl <- html_table(url)
dat <- tbl_df(as.data.frame(tbl[1]))
url1 <- read_html("http://www.basketball-reference.com/leagues/NBA_2005_games.html")
tbl1 <- html_table(url1)
dat1 <- tbl_df(as.data.frame(tbl1[1]))
url2 <- read_html("http://www.basketball-reference.com/leagues/NBA_2006_games.html")
tbl2 <- html_table(url2)
dat2 <- tbl_df(as.data.frame(tbl2[1]))
url3 <- read_html("http://www.basketball-reference.com/leagues/NBA_2007_games.html")
tbl3 <- html_table(url3)
dat3 <- tbl_df(as.data.frame(tbl3[1]))
url4 <- read_html("http://www.basketball-reference.com/leagues/NBA_2008_games.html")
tbl4 <- html_table(url4)
dat4 <- tbl_df(as.data.frame(tbl4[1]))
url5 <- read_html("http://www.basketball-reference.com/leagues/NBA_2009_games.html")
tbl5 <- html_table(url5)
dat5 <- tbl_df(as.data.frame(tbl5[1]))
url6 <- read_html("http://www.basketball-reference.com/leagues/NBA_2010_games.html")
tbl6 <- html_table(url6)
dat6 <- tbl_df(as.data.frame(tbl6[1]))
url7 <- read_html("http://www.basketball-reference.com/leagues/NBA_2011_games.html")
tbl7 <- html_table(url7)
dat7 <- tbl_df(as.data.frame(tbl7[1]))
url8 <- read_html("http://www.basketball-reference.com/leagues/NBA_2012_games.html")
tbl8 <- html_table(url8)
dat8 <- tbl_df(as.data.frame(tbl8[1]))
url9 <- read_html("http://www.basketball-reference.com/leagues/NBA_2013_games.html")
tbl9 <- html_table(url9)
dat9 <- tbl_df(as.data.frame(tbl9[1]))
url10 <- read_html("http://www.basketball-reference.com/leagues/NBA_2014_games.html")
tbl10 <- html_table(url10)
dat10 <- tbl_df(as.data.frame(tbl10[1]))
url11 <- read_html("http://www.basketball-reference.com/leagues/NBA_2015_games.html")
tbl11 <- html_table(url11)
dat11 <- tbl_df(as.data.frame(tbl11[1]))

# bind data, re-arrange, clean, and create a few columns ----
d1 <- rbind(dat, dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10)
names(d1) <- c("Date", "ET.Start", "Var.3", "VisitingTeam", "PTS.V", "HomeTeam", "PTS.H", "Var.8", "Notes")
d1$h.diff <- d1$PTS.H - d1$PTS.V # home diff
d1$v.diff <- d1$PTS.V - d1$PTS.H # visitor diff - originally unnecessary but when changing to long format
d1$game.id <- seq(1, nrow(d1), 1) # game id to have unique row identifiers

# save to a csv ----
write.csv(d1, "d1.csv")

# add conferences and divisions ----
atl.div <- c("Toronto Raptors", "Boston Celtics", "Brooklyn Nets", "Philadelphia 76ers", "New York Knicks")
cen.div <- c("Cleveland Cavaliers", "Chicago Bulls", "Milwaukee Bucks", "Indiana Pacers", "Detroit Pistons")
se.div <- c("Atlanta Hawks", "Washington Wizards", "Miami Heat", "Charlotte Hornets", "Orlando Magic")
nw.div <- c("Portland Trail Blazers", "Oklahoma City Thunder", "Utah Jazz", "Denver Nuggets")
pac.div <- c("Golden State Warriors", "Los Angeles Clippers", "Phoenix Suns", "Sacramento Kings", "Los Angeles Lakers")
sw.div <- c("Houston Rockets", "San Antonio Spurs", "Memphis Grizzlies", "Dallas Mavericks", "New Orleans Pelicans")
east.conf <- c(atl.div, cen.div, se.div)
west.conf <- c(nw.div, pac.div, sw.div)

d1$vtconf <- ifelse(d1$VisitingTeam %in% east.conf, "East", "West") # visiting team conference
d1$htconf <- "East"
d1$vtdiv <- ifelse(d1$VisitingTeam %in% atl.div, "Atlantic Division", 
ifelse(d1$VisitingTeam %in% cen.div, "Central Division",
ifelse(d1$VisitingTeam %in% se.div, "Southeast Division", 
ifelse(d1$VisitingTeam %in% nw.div, "Northwest Division", 
ifelse(d1$VisitingTeam %in% pac.div, "Pacific Division", "Southwest Division")))))
d1$htdiv <- "Atlantic Division"

d1$htconf <- ifelse(d1$HomeTeam %in% east.conf, "East", "West")
d1$vtconf <- "East"
d1$htdiv <- ifelse(d1$VisitingTeam %in% atl.div, "Atlantic Division", 
ifelse(d1$VisitingTeam %in% cen.div, "Central Division",
ifelse(d1$VisitingTeam %in% se.div, "Southeast Division", 
ifelse(d1$VisitingTeam %in% nw.div, "Northwest Division", 
ifelse(d1$VisitingTeam %in% pac.div, "Pacific Division", "Southwest Division")))))
d1$vtdiv <- "Atlantic Division"

# from wide to long data format ----
d2 <- d1 %>% select(game.id, Date,
                    VisitingTeam, PTS.V, vtconf, vtdiv, v.diff,
                    HomeTeam, PTS.H, htconf, htdiv, h.diff) %>%
  rename(v.team = VisitingTeam, v.pts = PTS.V, v.conf = vtconf, v.div = vtdiv,
         h.team = HomeTeam, h.pts = PTS.H, h.conf = htconf, h.div = htdiv) %>%
  gather(key, value, v.team:h.diff) %>%
  separate(key, c("status", "type")) %>%
  spread(type, value, convert = TRUE) # thank god...
d3 <- select(d2, game.id, Date, team, div, conf, status, pts, diff)