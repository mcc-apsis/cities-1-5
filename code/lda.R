library(RPostgreSQL)
library(scimetrix)
library(dplyr)
library(SnowballC)
library(tm)
library(slam)
library(topicmodels)

source("/home/galm/pg_keys.R")


drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)


q <- 'SELECT * FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id")
  LEFT OUTER JOIN "scoping_wosarticle" ON ("scoping_doc"."UT" = "scoping_wosarticle"."doc_id")
  WHERE "scoping_doc_query"."query_id" = 1053'

alldocs <- data.frame(dbGetQuery(con, q))

alldocs  <- data.frame(alldocs) %>%
  filter(!is.na(content))

alldocs$AB <- iconv(alldocs$content,to="utf-8")
alldocs <- alldocs %>%
  filter(nchar(AB) > 0)

corporate <- function (df, col = "AB") 
{
  ignoreWords <- c("the", "however", "this", "and")
  corpus <- tm::Corpus(tm::VectorSource(df[[col]])) %>%
    tm::tm_map(tm::removePunctuation) %>% tm::tm_map(tm::removeWords, 
                                                     tm::stopwords()) %>% tm::tm_map(tm::removeWords, ignoreWords) %>% 
    tm::tm_map(tm::stemDocument)
  return(corpus)
}

corpus <- corporate(alldocs)

dtm <- makeDTM(corpus,0.95,alldocs$UT,0.05,0)

optimal_k(dtm$dtm,max.k = 60)
ggplot2::ggsave("/home/galm/projects/cities-1-5/plots/optimal_k.png",width=8,height=5)
