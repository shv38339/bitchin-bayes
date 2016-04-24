# extract data with commands from the library rvest ----
library(rvest)
library(dplyr)
library(tidyr)
url <- read_html("http://www.basketball-reference.com/leagues/NBA_2004_games.html")
tbl <- html_table(url)
dat <- tbl_df(as.data.frame(tbl[1]))
index.v <- which(dat$Visitor.Neutral == "Boston Celtics")
v.test <- dat[index.v, ]
v.test$diff <- v.test$PTS - v.test$PTS.1
index.h <- which(dat$Home.Neutral == "Boston Celtics")
h.test <- dat[index.h, ]
h.test$diff <- h.test$PTS.1 - h.test$PTS
h.test$Team <- h.test$Visitor.Neutral
v.test$Team <- v.test$Home.Neutral
h.test <- h.test[, -c(2,3,4,5,6,7,8,9)]
names(h.test)
v.test <- v.test[, -c(2:9)]
names(v.test)
fin <- rbind(v.test, h.test)
View(fin)

rank_url <- read_html("http://www.basketball-reference.com/leagues/NBA_2003_ratings.html")
rank_tbl <- html_table(rank_url)
rank_dat <- tbl_df(as.data.frame(rank_tbl[1]))
#View(rank_dat)
names(rank_dat) <- rank_dat[1, ]
rank_dat <- rank_dat[-1,1:2]

final <- left_join(fin, rank_dat, by = "Team")
View(final)



url1 <- read_html("http://www.basketball-reference.com/leagues/NBA_2005_games.html")
tbl1 <- html_table(url1)
dat1 <- tbl_df(as.data.frame(tbl1[1]))
index.v1 <- which(dat1$Visitor.Neutral == "Boston Celtics")
v.test1 <- dat1[index.v1, ]
v.test1$diff <- v.test1$PTS - v.test1$PTS.1
index.h1 <- which(dat1$Home.Neutral == "Boston Celtics")
h.test1 <- dat1[index.h1, ]
h.test1$diff <- h.test1$PTS.1 - h.test1$PTS
h.test1$Team <- h.test1$Visitor.Neutral
v.test1$Team <- v.test1$Home.Neutral
h.test1 <- h.test1[, -c(2,3,4,5,6,7,8,9)]
v.test1 <- v.test1[, -c(2:9)]
fin1 <- rbind(v.test1, h.test1)
View(fin1)

rank_url1 <- read_html("http://www.basketball-reference.com/leagues/NBA_2004_ratings.html")
rank_tbl1 <- html_table(rank_url1)
rank_dat1 <- tbl_df(as.data.frame(rank_tbl1[1]))
#View(rank_dat)
names(rank_dat1) <- rank_dat1[1, ]
rank_dat1 <- rank_dat1[-1,1:2]

final1 <- left_join(fin1, rank_dat1, by = "Team")
View(final1)





url2 <- read_html("http://www.basketball-reference.com/leagues/NBA_2006_games.html")
tbl2 <- html_table(url2)
dat2 <- tbl_df(as.data.frame(tbl2[1]))
index.v2 <- which(dat2$Visitor.Neutral == "Boston Celtics")
v.test2 <- dat2[index.v2, ]
v.test2$diff <- v.test2$PTS - v.test2$PTS.1
index.h2 <- which(dat2$Home.Neutral == "Boston Celtics")
h.test2 <- dat2[index.h2, ]
h.test2$diff <- h.test2$PTS.1 - h.test2$PTS
h.test2$Team <- h.test2$Visitor.Neutral
v.test2$Team <- v.test2$Home.Neutral
h.test2 <- h.test2[, -c(2,3,4,5,6,7,8,9)]
v.test2 <- v.test2[, -c(2:9)]
fin2 <- rbind(v.test2, h.test2)
#View(fin2)

rank_url2 <- read_html("http://www.basketball-reference.com/leagues/NBA_2005_ratings.html")
rank_tbl2 <- html_table(rank_url2)
rank_dat2 <- tbl_df(as.data.frame(rank_tbl2[1]))
#View(rank_dat)
names(rank_dat2) <- rank_dat2[1, ]
rank_dat2 <- rank_dat2[-1,1:2]
final2 <- left_join(fin2, rank_dat2, by = "Team")
View(final2)





url3 <- read_html("http://www.basketball-reference.com/leagues/NBA_2007_games.html")
tbl3 <- html_table(url3)
dat3 <- tbl_df(as.data.frame(tbl3[1]))
index.v3 <- which(dat3$Visitor.Neutral == "Boston Celtics")
v.test3 <- dat3[index.v3, ]
v.test3$diff <- v.test3$PTS - v.test3$PTS.1
index.h3 <- which(dat3$Home.Neutral == "Boston Celtics")
h.test3 <- dat3[index.h3, ]
h.test3$diff <- h.test3$PTS.1 - h.test3$PTS
h.test3$Team <- h.test3$Visitor.Neutral
v.test3$Team <- v.test3$Home.Neutral
h.test3 <- h.test3[, -c(2,3,4,5,6,7,8,9)]
v.test3 <- v.test3[, -c(2:9)]
fin3 <- rbind(v.test3, h.test3)
View(fin3)

rank_url3 <- read_html("http://www.basketball-reference.com/leagues/NBA_2006_ratings.html")
rank_tbl3 <- html_table(rank_url3)
rank_dat3 <- tbl_df(as.data.frame(rank_tbl3[1]))
#View(rank_dat)
names(rank_dat3) <- rank_dat3[1, ]
rank_dat3 <- rank_dat3[-1,1:2]
final3 <- left_join(fin3, rank_dat3, by = "Team")
View(final3)




url4 <- read_html("http://www.basketball-reference.com/leagues/NBA_2008_games.html")
tbl4 <- html_table(url4)
dat4 <- tbl_df(as.data.frame(tbl4[1]))
index.v4 <- which(dat4$Visitor.Neutral == "Boston Celtics")
v.test4 <- dat4[index.v4, ]
v.test4$diff <- v.test4$PTS - v.test4$PTS.1
index.h4 <- which(dat4$Home.Neutral == "Boston Celtics")
h.test4 <- dat4[index.h4, ]
h.test4$diff <- h.test4$PTS.1 - h.test4$PTS
h.test4$Team <- h.test4$Visitor.Neutral
v.test4$Team <- v.test4$Home.Neutral
h.test4 <- h.test4[, -c(2,3,4,5,6,7,8,9)]
v.test4 <- v.test4[, -c(2:9)]
fin4 <- rbind(v.test4, h.test4)
View(fin4)

rank_url4 <- read_html("http://www.basketball-reference.com/leagues/NBA_2007_ratings.html")
rank_tbl4 <- html_table(rank_url4)
rank_dat4 <- tbl_df(as.data.frame(rank_tbl4[1]))
#View(rank_dat)
names(rank_dat4) <- rank_dat4[1, ]
rank_dat4 <- rank_dat4[-1,1:2]
final4 <- left_join(fin4, rank_dat4, by = "Team")




url5 <- read_html("http://www.basketball-reference.com/leagues/NBA_2009_games.html")
tbl5 <- html_table(url5)
dat5 <- tbl_df(as.data.frame(tbl5[1]))
index.v5 <- which(dat5$Visitor.Neutral == "Boston Celtics")
v.test5 <- dat5[index.v5, ]
v.test5$diff <- v.test5$PTS - v.test5$PTS.1
index.h5 <- which(dat5$Home.Neutral == "Boston Celtics")
h.test5 <- dat5[index.h5, ]
h.test5$diff <- h.test5$PTS.1 - h.test5$PTS
h.test5$Team <- h.test5$Visitor.Neutral
v.test5$Team <- v.test5$Home.Neutral
h.test5 <- h.test5[, -c(2,3,4,5,6,7,8,9)]
v.test5 <- v.test5[, -c(2:9)]
fin5 <- rbind(v.test5, h.test5)
#View(fin5)

rank_url5 <- read_html("http://www.basketball-reference.com/leagues/NBA_2008_ratings.html")
rank_tbl5 <- html_table(rank_url5)
rank_dat5 <- tbl_df(as.data.frame(rank_tbl5[1]))
#View(rank_dat)
names(rank_dat5) <- rank_dat5[1, ]
rank_dat5 <- rank_dat5[-1,1:2]
final5 <- left_join(fin5, rank_dat5, by = "Team")


url6 <- read_html("http://www.basketball-reference.com/leagues/NBA_2010_games.html")
tbl6 <- html_table(url6)
dat6 <- tbl_df(as.data.frame(tbl6[1]))
index.v6 <- which(dat6$Visitor.Neutral == "Boston Celtics")
v.test6 <- dat6[index.v6, ]
v.test6$diff <- v.test6$PTS - v.test6$PTS.1
index.h6 <- which(dat6$Home.Neutral == "Boston Celtics")
h.test6 <- dat6[index.h6, ]
h.test6$diff <- h.test6$PTS.1 - h.test6$PTS
h.test6$Team <- h.test6$Visitor.Neutral
v.test6$Team <- v.test6$Home.Neutral
h.test6 <- h.test6[, -c(2,3,4,5,6,7,8,9)]
v.test6 <- v.test6[, -c(2:9)]
fin6 <- rbind(v.test6, h.test6)
#View(fin6)

rank_url6 <- read_html("http://www.basketball-reference.com/leagues/NBA_2009_ratings.html")
rank_tbl6 <- html_table(rank_url6)
rank_dat6 <- tbl_df(as.data.frame(rank_tbl6[1]))
#View(rank_dat)
names(rank_dat6) <- rank_dat6[1, ]
rank_dat6 <- rank_dat6[-1,1:2]
final6 <- left_join(fin6, rank_dat6, by = "Team")
View(final6)




url7 <- read_html("http://www.basketball-reference.com/leagues/NBA_2011_games.html")
tbl7 <- html_table(url7)
dat7 <- tbl_df(as.data.frame(tbl7[1]))
index.v7 <- which(dat7$Visitor.Neutral == "Boston Celtics")
v.test7 <- dat7[index.v7, ]
v.test7$diff <- v.test7$PTS - v.test7$PTS.1
index.h7 <- which(dat7$Home.Neutral == "Boston Celtics")
h.test7 <- dat7[index.h7, ]
h.test7$diff <- h.test7$PTS.1 - h.test7$PTS
h.test7$Team <- h.test7$Visitor.Neutral
v.test7$Team <- v.test7$Home.Neutral
h.test7 <- h.test7[, -c(2,3,4,5,6,7,8,9)]
v.test7 <- v.test7[, -c(2:9)]
fin7 <- rbind(v.test7, h.test7)
#View(fin7)

rank_url7 <- read_html("http://www.basketball-reference.com/leagues/NBA_2010_ratings.html")
rank_tbl7 <- html_table(rank_url7)
rank_dat7 <- tbl_df(as.data.frame(rank_tbl7[1]))
#View(rank_dat)
names(rank_dat7) <- rank_dat7[1, ]
rank_dat7 <- rank_dat7[-1,1:2]
final7 <- left_join(fin7, rank_dat7, by = "Team")
View(final7)



url8 <- read_html("http://www.basketball-reference.com/leagues/NBA_2012_games.html")
tbl8 <- html_table(url8)
dat8 <- tbl_df(as.data.frame(tbl8[1]))
index.v8 <- which(dat8$Visitor.Neutral == "Boston Celtics")
v.test8 <- dat8[index.v8, ]
v.test8$diff <- v.test8$PTS - v.test8$PTS.1
index.h8 <- which(dat8$Home.Neutral == "Boston Celtics")
h.test8 <- dat8[index.h8, ]
h.test8$diff <- h.test8$PTS.1 - h.test8$PTS
h.test8$Team <- h.test8$Visitor.Neutral
v.test8$Team <- v.test8$Home.Neutral
h.test8 <- h.test8[, -c(2,3,4,5,6,7,8,9)]
v.test8 <- v.test8[, -c(2:9)]
fin8 <- rbind(v.test8, h.test8)
#View(fin8)

rank_url8 <- read_html("http://www.basketball-reference.com/leagues/NBA_2011_ratings.html")
rank_tbl8 <- html_table(rank_url8)
rank_dat8 <- tbl_df(as.data.frame(rank_tbl8[1]))
#View(rank_dat)
names(rank_dat8) <- rank_dat8[1, ]
rank_dat8 <- rank_dat8[-1,1:2]
final8 <- left_join(fin8, rank_dat8, by = "Team")
View(final8)


url9 <- read_html("http://www.basketball-reference.com/leagues/NBA_2013_games.html")
tbl9 <- html_table(url9)
dat9 <- tbl_df(as.data.frame(tbl9[1]))
index.v9 <- which(dat9$Visitor.Neutral == "Boston Celtics")
v.test9 <- dat9[index.v9, ]
v.test9$diff <- v.test9$PTS - v.test9$PTS.1
index.h9 <- which(dat9$Home.Neutral == "Boston Celtics")
h.test9 <- dat9[index.h9, ]
h.test9$diff <- h.test9$PTS.1 - h.test9$PTS
h.test9$Team <- h.test9$Visitor.Neutral
v.test9$Team <- v.test9$Home.Neutral
h.test9 <- h.test9[, -c(2,3,4,5,6,7,8,9)]
v.test9 <- v.test9[, -c(2:9)]
fin9 <- rbind(v.test9, h.test9)
#View(fin9)

rank_url9 <- read_html("http://www.basketball-reference.com/leagues/NBA_2012_ratings.html")
rank_tbl9 <- html_table(rank_url9)
rank_dat9 <- tbl_df(as.data.frame(rank_tbl9[1]))
#View(rank_dat)
names(rank_dat9) <- rank_dat9[1, ]
rank_dat9 <- rank_dat9[-1,1:2]
final9 <- left_join(fin9, rank_dat9, by = "Team")
#View(final9)


url10 <- read_html("http://www.basketball-reference.com/leagues/NBA_2014_games.html")
tbl10 <- html_table(url10)
dat10 <- tbl_df(as.data.frame(tbl10[1]))
index.v10 <- which(dat10$Visitor.Neutral == "Boston Celtics")
v.test10 <- dat10[index.v10, ]
v.test10$diff <- v.test10$PTS - v.test10$PTS.1
index.h10 <- which(dat10$Home.Neutral == "Boston Celtics")
h.test10 <- dat10[index.h10, ]
h.test10$diff <- h.test10$PTS.1 - h.test10$PTS
h.test10$Team <- h.test10$Visitor.Neutral
v.test10$Team <- v.test10$Home.Neutral
h.test10 <- h.test10[, -c(2,3,4,5,6,7,8,9)]
v.test10 <- v.test10[, -c(2:9)]
fin10 <- rbind(v.test10, h.test10)
#View(fin10)

rank_url10 <- read_html("http://www.basketball-reference.com/leagues/NBA_2013_ratings.html")
rank_tbl10 <- html_table(rank_url10)
rank_dat10 <- tbl_df(as.data.frame(rank_tbl10[1]))
#View(rank_dat)
names(rank_dat10) <- rank_dat10[1, ]
rank_dat10 <- rank_dat10[-1,1:2]
final10 <- left_join(fin10, rank_dat10, by = "Team")
#View(final10)



url11 <- read_html("http://www.basketball-reference.com/leagues/NBA_2015_games.html")
tbl11 <- html_table(url11)
dat11 <- tbl_df(as.data.frame(tbl11[1]))
index.v11 <- which(dat11$Visitor.Neutral == "Boston Celtics")
v.test11 <- dat11[index.v11, ]
v.test11$diff <- v.test11$PTS - v.test11$PTS.1
index.h11 <- which(dat11$Home.Neutral == "Boston Celtics")
h.test11 <- dat11[index.h11, ]
h.test11$diff <- h.test11$PTS.1 - h.test11$PTS
h.test11$Team <- h.test11$Visitor.Neutral
v.test11$Team <- v.test11$Home.Neutral
h.test11 <- h.test11[, -c(2,3,4,5,6,7,8,9)]
v.test11 <- v.test11[, -c(2:9)]
fin11 <- rbind(v.test11, h.test11)
#View(fin11)

rank_url11 <- read_html("http://www.basketball-reference.com/leagues/NBA_2014_ratings.html")
rank_tbl11 <- html_table(rank_url11)
rank_dat11 <- tbl_df(as.data.frame(rank_tbl11[1]))
#View(rank_dat)
names(rank_dat11) <- rank_dat11[1, ]
rank_dat11 <- rank_dat11[-1,1:2]
final11 <- left_join(fin11, rank_dat11, by = "Team")
#View(final11)


url12 <- read_html("http://www.basketball-reference.com/leagues/NBA_2016_games.html")
tbl12 <- html_table(url12)
dat12 <- tbl_df(as.data.frame(tbl12[1]))
index.v12 <- which(dat12$Visitor.Neutral == "Boston Celtics")
v.test12 <- dat12[index.v12, ]
v.test12$diff <- v.test12$PTS - v.test12$PTS.1
index.h12 <- which(dat12$Home.Neutral == "Boston Celtics")
h.test12 <- dat12[index.h12, ]
h.test12$diff <- h.test12$PTS.1 - h.test12$PTS
h.test12$Team <- h.test12$Visitor.Neutral
v.test12$Team <- v.test12$Home.Neutral
h.test12 <- h.test12[, -c(2,3,4,5,6,7,8,9)]
v.test12 <- v.test12[, -c(2:9)]
fin12 <- rbind(v.test12, h.test12)
#View(fin12)

rank_url12 <- read_html("http://www.basketball-reference.com/leagues/NBA_2015_ratings.html")
rank_tbl12 <- html_table(rank_url12)
rank_dat12 <- tbl_df(as.data.frame(rank_tbl12[1]))
#View(rank_dat)
names(rank_dat12) <- rank_dat12[1, ]
rank_dat12 <- rank_dat12[-1,1:2]
final12 <- left_join(fin12, rank_dat12, by = "Team")
#View(final12)


# bind data, re-arrange, clean, and create a few columns ----
ult <- rbind(final, final2, final3, final4, final5, final6, final7, final8, final9, final10, final11, final12)
#View(ult)

# save to a csv ----
write.csv(ult, "ult.csv")

# Doc Rivers Period April 29, 2004 to June 25, 2013
str(ult)
doc <- ult[82:722, ]
brad <- ult[723:951, ]

write.csv(doc, "doc.csv")
write.csv(brad, "brad.csv")
