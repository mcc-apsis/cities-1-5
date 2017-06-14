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


q <- 'SELECT "tmv_app_topic"."title", T3."title", "tmv_app_topiccorr"."score", "tmv_app_topiccorr"."ar"
  FROM "tmv_app_topiccorr" 
  LEFT OUTER JOIN "tmv_app_topic" ON ("tmv_app_topiccorr"."topic_id" = "tmv_app_topic"."id") 
  LEFT OUTER JOIN "tmv_app_topic" T3 ON ("tmv_app_topiccorr"."topiccorr_id" = T3."id") 
  WHERE ("tmv_app_topiccorr"."run_id" = 181 
  AND "tmv_app_topiccorr"."score" > 0.025 
  AND "tmv_app_topiccorr"."ar" = -1)'

cors <- data.frame(dbGetQuery(con, q)) %>%
  select(-ar) %>%
  spread(title.1,score,fill=0) 

rownames(cors) <- cors$title

cors <- select(cors,-title)

cors[lower.tri(cors, diag = TRUE)] <- 0


g <- as.undirected(graph.adjacency(as.matrix(cors), weighted = TRUE, 
                                   mode = "upper"))

q <- 'SELECT "tmv_app_topic"."title", "tmv_app_topic"."score" FROM "tmv_app_topic" WHERE "tmv_app_topic"."run_id_id" = 181'

tscores <- data.frame(dbGetQuery(con, q)) %>%
  arrange(title)

layout1 <- layout.fruchterman.reingold(g, niter = 500)
b1 <- tscores$score
#V(g)$label <- V(g)
V(g)$lsize <- b1 * 30/max(b1)

write.graph(g,file="plots/181_025.graphml", format="graphml")


alpha_centrality(g)


centr_eigen(g)$centralization

centralize(alpha_centrality(g))


######################################################################
## by years

df <- data.frame(topic = names(alpha_centrality(g)))

cent_df <- data.frame(ar = c(1,2,3,4,5),centrality=0)

for (ar in seq(1,5)) {
  q <- paste0('SELECT "tmv_app_topic"."title", T3."title", "tmv_app_topiccorr"."score", "tmv_app_topiccorr"."ar"
  FROM "tmv_app_topiccorr" 
LEFT OUTER JOIN "tmv_app_topic" ON ("tmv_app_topiccorr"."topic_id" = "tmv_app_topic"."id") 
LEFT OUTER JOIN "tmv_app_topic" T3 ON ("tmv_app_topiccorr"."topiccorr_id" = T3."id") 
WHERE ("tmv_app_topiccorr"."run_id" = 181 
AND "tmv_app_topiccorr"."score" > 0.0 
AND "tmv_app_topiccorr"."ar" = ',ar,')')
  
  cors <- data.frame(dbGetQuery(con, q)) %>%
    select(-ar) %>%
    spread(title.1,score,fill=0) 
  
  rownames(cors) <- cors$title
  
  cors <- select(cors,-title)
  
  cors[lower.tri(cors, diag = TRUE)] <- 0
  
  gar  <- as.undirected(graph.adjacency(as.matrix(cors), weighted = TRUE, 
                                        mode = "upper"))
  v <- paste0("g_",ar)
  assign(v,gar)
  ac <- alpha_centrality(gar)
  ec <- eigen_centrality(gar)$vector
  acdf <- data.frame(ec,row.names=NULL)
  acdf$topic <- names(ac)
  names(acdf)[1] <- v
  df <- left_join(df,acdf)
 # df[,v] <- ac
  cent_df[cent_df$ar==ar,]$centrality <- centr_eigen(gar)$centralization
  cent_df[cent_df$ar==ar,]$ar <- v
}

eigen_centrality(gar)

cent_df$measure <- "centr_eigen"

df_long <- df %>%
  gather(ar,centrality,-topic)

ggplot() +
  geom_line(
    data=df_long,
    aes(ar,centrality,colour=topic,group=topic)
  ) +
  geom_line(
    data=cent_df,
    aes(ar,centrality,group=measure),
    linetype=2
  ) +
  geom_text(
    data = filter(df_long,ar=="g_5"),
    aes(ar,centrality,label=topic)
  )

ggplot() +
  geom_line(
    data=cent_df,
    aes(ar,centrality,group=measure),
    linetype=2
  )

q <- 'SELECT "tmv_app_doctopic"."doc_id", "tmv_app_topic"."title", 
  "tmv_app_doctopic"."scaled_score", "scoping_doc"."PY" 
  FROM "tmv_app_doctopic" 
  LEFT OUTER JOIN "scoping_doc" 
  ON ("tmv_app_doctopic"."doc_id" = "scoping_doc"."UT") 
  LEFT OUTER JOIN "tmv_app_topic" 
  ON ("tmv_app_doctopic"."topic_id" = "tmv_app_topic"."id") 
  WHERE "tmv_app_doctopic"."run_id" = 181'

gamma <- data.frame(dbGetQuery(con, q))

pys <- sort(unique(gamma$PY))

pydf <- data.frame(topic = unique(gamma$title))

pycent_df <- data.frame(py = pys,centrality=0)

for (py in pys) {

  pygamma <- gamma %>%
    filter(PY==py) %>%
    spread(title,scaled_score,fill=0) %>%
    select(-doc_id,-PY)
  
  cors <- cor(pygamma)
  
  rownames(cors) <- names(pygamma)
  
  cors[lower.tri(cors, diag = TRUE)] <- 0
  
  cors[cors<0.25] = 0
  
  gar  <- as.undirected(graph.adjacency(as.matrix(cors), weighted = TRUE, 
                                        mode = "upper"))
  plot.igraph(gar,main=py)
  v <- paste0("g_",py)
  assign(v,gar)
  ac <- alpha_centrality(gar)
  ec <- eigen_centrality(gar)$vector
  acdf <- data.frame(ec,row.names=NULL)
  acdf$topic <- names(ac)
  names(acdf)[1] <- v
  pydf <- left_join(pydf,acdf)
  # df[,v] <- ac
  pycent_df[pycent_df$py==py,]$centrality <- centr_eigen(gar)$centralization
  pycent_df[pycent_df$py==py,]$py <- v
}



pycent_df$measure <- "centr_eigen"

pydf_long <- pydf %>%
  gather(ar,centrality,-topic)

ggplot() +
  geom_line(
    data=pydf_long,
    aes(ar,centrality,colour=topic,group=topic)
  ) +
  geom_line(
    data=pycent_df,
    aes(ar,centrality,group=measure),
    linetype=2
  ) +
  geom_text(
    data = filter(pydf_long,ar=="g_2017"),
    aes(ar,centrality,label=topic)
  )

ggsave("plots/centrality_time.png",width=12,height=7.5)

############################################################
## Bigger Slices

pysum <- gamma %>%
  group_by(PY) %>%
  summarise(n= n()) %>%
  mutate(t=sum(n),p=round(n/t*100))

pys <- sort(unique(gamma$PY))

decades <- c("A90s","B00s","C10s")

gamma$slice <- cut(gamma$PY,c(0,2000,2010,Inf),labels=decades)

decdf <- data.frame(topic = unique(gamma$title))

deccent_df <- data.frame(decade = decades,centrality=0,stringsAsFactors = F)

for (py in decades) {
  print(py)
  
  pygamma <- gamma %>%
    filter(slice==py) %>%
    spread(title,scaled_score,fill=0) %>%
    select(-doc_id,-PY,-slice)
  
  cors <- cor(pygamma)
  
  rownames(cors) <- names(pygamma)
  
  cors[lower.tri(cors, diag = TRUE)] <- 0
  
  cors[cors<0.025] = 0
  
  gar  <- as.undirected(graph.adjacency(as.matrix(cors), weighted = TRUE, 
                                        mode = "upper"))
  plot.igraph(gar,main=py)
  v <- paste0("g_",py)
  print(v)
  assign(v,gar)
  print(v)
  ac <- alpha_centrality(gar)
  ec <- eigen_centrality(gar)$vector
  acdf <- data.frame(ec,row.names=NULL)
  acdf$topic <- names(ac)
  names(acdf)[1] <- v
  decdf <- left_join(decdf,acdf)
  # df[,v] <- ac
  deccent_df[deccent_df$decade==py,]$centrality <- centr_eigen(gar)$centralization
  deccent_df[deccent_df$decade==py,]$decade <- v
}



deccent_df$measure <- "centr_eigen"

decdf_long <- decdf %>%
  gather(ar,centrality,-topic)

ggplot() +
  geom_line(
    data=decdf_long,
    aes(ar,centrality,colour=topic,group=topic)
  ) +
  geom_line(
    data=deccent_df,
    aes(ar,centrality,group=measure),
    linetype=2
  ) +
  geom_text(
    data = filter(decdf_long,ar=="g_C10s"),
    aes(ar,centrality,label=topic)
  )

ggsave("plots/centrality_time.png",width=12,height=7.5)


plot.igraph(gar)


plot.igraph(gar)


