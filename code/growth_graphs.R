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

alldocs <- filter(alldocs,PY>1985,PY<2017)

pn <- scimetrix::paperNumbers(alldocs,"oecd",graph=T) + scale_x_continuous(breaks = seq(1985,2020,by=5))

ggsave("plots/paper_numbers.png")



########################################################

papers <- alldocs %>%
  group_by(PY) %>%
  summarise(n=n())

#cut papers by assessment period
papers$AP <- cut(papers$PY,
                 c(0,1985,1990.1,1995.1,2001.1,2007.1,2013.1,Inf),
                 c(NA,"AR1","AR2","AR3","AR4","AR5","AR6")
)

apCounts <- papers %>%
  group_by(AP) %>%
  summarise(
    total = formatC(sum(n),format="d", big.mark=',',preserve.width="none"),
    midY = median(PY),
    maxV = max(n),
    n = sum(n)
  ) %>%
  ungroup() %>%
  filter(AP %in% c("AR1","AR2","AR3","AR4","AR5","AR6")) %>%
  mutate(
    total = paste0("[",total,"]")
  )

####################
### Figure 1: Growth
growth <- ggplot() +
  geom_bar( # Bars for each year, colour coded by AP
    data = filter(papers,PY > 1985 & PY < 2017),
    aes(PY,n,fill=AP),
    stat="identity",
    colour="grey22"
  ) +
  geom_text( # Labels
    data=apCounts,
    aes(label=total,x=midY,y=maxV+100),
    hjust = 0.5
  ) +
  labs(x="Year",y="Number of Publications") +
  theme_classic() +
  theme(
    text=element_text(size=12),
    legend.position=c(0.1,0.9),
    legend.justification=c(0,1),
    legend.direction="horizontal",
    panel.grid.major.y=element_line(size=0.2,colour="grey22"),
    panel.border=element_rect(size=0.2,colour="grey22",fill=NA)
  ) +
  scale_fill_manual(values=APscale[1:6],name="Assessment Period") +
  scale_x_continuous(breaks = seq(1985,2015,by=5))

growth
ggsave("plots/total_growth.png")
