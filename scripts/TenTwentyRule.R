library(tidyverse)

dat<-read.csv('../BaltimoreStreetTreeProject_Large_Data/street_trees_with_neigh_attributes_2023-09-20.csv')

test<-dat %>% 
  filter(CSA2010=="Allendale/Irvington/S. Hilton") %>% 
  filter(CONDITION=="Absent")

dat_trees<-dat %>% 
  filter(SPP!="unknown tree"&SPP!='Vacant Site'&SPP!='Vacant Potential'&SPP!='Stump'&SPP!='Vacant Site Not Suitable'&SPP!='NA'&SPP!='unknown shrub'&SPP!="Z Add 01"&SPP!=" "&SPP!="Dead")#%>%
  filter(CONDITION!="Dead"&CONDITION!="Stump"&CONDITION!="Sprout")

subsetCSA<-dat_trees %>% 
  select(SPP, DBH, CONDITION, MT, TREE_HT, LOC_TYPE, CULTIVAR, CSA2010)

SPP<-subsetCSA %>% 
  select(SPP) %>% 
  unique()

```