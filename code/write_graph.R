rm(list=ls())
library(RPostgreSQL)
library(scimetrix)
library(dplyr)
library(SnowballC)
library(tm)
library(slam)
library(topicmodels)
library(tidyr)
library(igraph)

source("/home/galm/pg_keys.R")


drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)


q <- 'SELECT "tmv_app_topic"."title", T3."title", "tmv_app_topiccorr"."score" FROM "tmv_app_topiccorr" 
  LEFT OUTER JOIN "tmv_app_topic" ON ("tmv_app_topiccorr"."topic_id" = "tmv_app_topic"."id") 
  LEFT OUTER JOIN "tmv_app_topic" T3 ON ("tmv_app_topiccorr"."topiccorr_id" = T3."id") 
  WHERE ("tmv_app_topiccorr"."run_id" = 96)'

cors <- data.frame(dbGetQuery(con, q)) %>%
  spread(title.1,score,fill=0) 

rownames(cors) <- cors$title

cors <- select(cors,-title)

cors[lower.tri(cors, diag = TRUE)] <- 0


g <- as.undirected(graph.adjacency(as.matrix(cors), weighted = TRUE, 
                                   mode = "upper"))

layout1 <- layout.fruchterman.reingold(g, niter = 500)
b1 <- degree(g)
V(g)$label.cex <- b1 * 2/max(b1)
V(g)$size <- b1 * 30/max(b1)
