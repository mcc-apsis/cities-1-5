rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)

library(RPostgreSQL)

source("/home/galm/pg_keys.R")
source("/home/galm/projects/learning-climate-solutions/functions.R")

drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)

## Colorscale 
APscale <- brewer.pal(6,"Spectral")

##############################################################
## Get all the papers and show by OECD cat
q <- paste0('SELECT "scoping_doc"."UT", "scoping_doc"."PY", "scoping_wc"."oecd", "scoping_wc"."oecd_fos_text", "scoping_wc"."text" as "WC" FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
  INNER JOIN "scoping_wc_doc" ON ("scoping_doc"."UT" = "scoping_wc_doc"."doc_id") 
  INNER JOIN "scoping_wc" ON ("scoping_wc_doc"."wc_id" = "scoping_wc"."id")
  LEFT OUTER JOIN "scoping_wosarticle" ON ("scoping_doc"."UT" = "scoping_wosarticle"."doc_id") 
  WHERE "scoping_doc_query"."query_id" = ',1281)

alldocs <- data.frame(dbGetQuery(con, q)) 

alldocs <- filter(alldocs,PY>1985)


