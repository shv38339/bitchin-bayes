url <- read_html("http://www.basketball-reference.com/leagues/NBA_2015_games.html")
tbl <- html_table(url)
library(dplyr)
d1 <- tbl_df(as.data.frame(tbl[1]))
d2 <- tbl_df(as.data.frame(tbl[2]))

