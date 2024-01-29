library(tidyverse)
library(codyn)

Dependent <- read_csv('input_data/Dependent_variable.csv')
Independent<- read_csv('input_data/Independent_variable.csv')

Combined <- Dependent %>% 
  left_join(Independent, by = "CSA2020") %>% 
  select(CSA2020, bahigher20, PercBlk, PercWhite, mhhi20, avg_temp, vacancy20) %>% 
  drop_na()

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("r = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "darkgray", ...)
}

# Create the plots
pairs(Combined[,2:6], 
      lower.panel = panel.cor,
      upper.panel = upper.panel,
      diag.panel=panel.hist)

dat<-read.csv('../BaltimoreStreetTreeProject_Large_Data/street_trees_with_neigh_attributes_2023-10-31.csv')

dat_trees<-dat %>% 
  filter(SPP!="unknown tree"&SPP!='Vacant Site'&SPP!='Vacant Potential'&SPP!='Stump'&SPP!='Vacant Site Not Suitable'&SPP!='NA'&SPP!='unknown shrub'&SPP!="Z Add 01"&SPP!=" "&SPP!="Dead")%>%
filter(CONDITION!="Dead"&CONDITION!="Stump"&CONDITION!="Sprout")

test<-dat_trees %>% 
  filter(SPACE_TYPE=="Open/Unrestricted")

subsetCSA<-dat_trees %>% 
  select(CSA2020, SPP, Street, DBH, CONDITION, MT, TREE_HT, LOC_TYPE, CULTIVAR)

nbdat<-subsetCSA %>% 
  group_by(CSA2020, Street, SPP) %>% 
  summarize(n=length(DBH)) %>% 
  mutate(ID=paste(CSA2020, Street, sep="::")) %>% 
  filter(!is.na(CSA2020)) %>% 
  drop_na()

rac<-RAC_difference(nbdat, species.var = "SPP", abundance.var = "n", treatment.var = 'CSA2020', replicate.var = 'ID')


data("pplots")
df <- subset(pplots, year == 2002 & block < 3)
RAC_difference(df = df,
               species.var = "species",
               abundance.var = "relative_cover",
               treatment.var = 'treatment',
               block.var = "block",
               replicate.var = "plot")
