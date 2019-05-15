library(tidyverse)
library(rvest)

url_pg <- 'https://www.a-league.com.au/match/perth-glory-v-central-coast-mariners-a-league-25-11-2018/1002851'

webpage_pg <- read_html(url_pg)

data_pg <- html_nodes(webpage_pg, '.match-banner__team-name , .match-banner__start-date--initialized div , .match-banner__arena , .match-banner__league-wrapper .match-banner__league')

head(html_text(data_pg), 20)

left_table_pg <- html_text(data_pg)
left_table_pg

data_pg_r <- html_nodes(webpage_pg, '.match-banner__bottom-left-content div')
head(html_text(data_pg_r), 20)
head(html_text(data_sfc), 20)
pg_list <- html_text(data_pg_r)
sfc_list <- html_text(data_sfc)

right_table_pg[1] <- NULL
colnames(right_table_pg)[1] <- 'A'
right_table_pg
rt_pg_wide <- spread(right_table_pg, A)

list(right_table_pg)
test1 <- list( c(a='a',b='b',c='c'), c(a='d',b='e',c='f'))
test1

test2 <- unlist(right_table_pg)
is.list(right_table_pg)
typeof(right_table_pg)
testl <- list(right_table_pg)
unlist(testl)
as.data.frame(testl)
as.data.frame(list(list(pg_list), list(sfc_list)))

testdf2 <- as.data.frame(t(pg_list)) #This is the one!
sfc_df <- as.data.frame(t(sfc_list))

testrb <- rbind(testdf2, sfc_df)
testdf2
