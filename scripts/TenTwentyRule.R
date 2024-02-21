library(tidyverse)

dat<-read.csv('../BaltimoreStreetTreeProject_Large_Data/street_trees_with_neigh_attributes_2023-10-31.csv')

dat_trees<-dat %>% 
  filter(SPP!="unknown tree"&SPP!='Vacant Site'&SPP!='Vacant Potential'&SPP!='Stump'&SPP!='Vacant Site Not Suitable'&SPP!='NA'&SPP!='unknown shrub'&SPP!="Z Add 01"&SPP!=" "&SPP!="Dead")%>%
  filter(CONDITION!="Dead"&CONDITION!="Stump"&CONDITION!="Sprout") %>% 
  filter(CSA2020!="Dickeyville/Franklintown"&CSA2020!='Unassigned -- Jail')


subsetCSA<-dat_trees %>% 
  select(SPP, DBH, CSA2020)

SPP<-subsetCSA %>% 
  select(SPP) %>% 
  unique() %>% 
  separate(SPP, into=c("Genus", "Species", 'other'), sep=" ", remove = F)

# write.csv(SPP, 'species.csv', row.names = F)

fam<-read.csv('\\Users/mavolio2/Documents/R projects/BaltimoreStreetTreeProject/input_data/Family.csv')

tentwenty<-subsetCSA %>% 
  left_join(fam)

totaltrees<-length(tentwenty$SPP)

familyOverall<-tentwenty %>% 
  group_by(Family) %>% 
  summarize(n=length(SPP)) %>% 
  mutate(percent=(n/totaltrees)*100)

GeneraOverall<-tentwenty %>% 
  group_by(Genus) %>% 
  summarize(n=length(SPP)) %>% 
  mutate(percent=(n/totaltrees)*100)

speciesoverall<-tentwenty %>% 
  group_by(SPP) %>% 
  summarize(n=length(DBH)) %>% 
  mutate(percent=(n/totaltrees)*100)

#number of tree / nb
nbnumtree<-tentwenty %>% 
  group_by(CSA2020) %>% 
  summarize(numtree=length(SPP))

familyCSA<-tentwenty %>% 
  group_by(Family, CSA2020) %>% 
  summarize(n=length(SPP)) %>% 
  left_join(nbnumtree) %>% 
  mutate(fampercent=(n/numtree)*100) %>% 
  ungroup() %>% 
  mutate(F30=ifelse(fampercent>30, 1, 0),
         F20=ifelse(fampercent>20, 1, 0),
         F15=ifelse(fampercent>15, 1, 0)) %>% 
  filter(F30==1|F15==1|F20==1)

#how many fail F30?
length(unique(subset(familyCSA, F30==1)$CSA2020))
length(unique(subset(familyCSA, F20==1)$CSA2020))
length(unique(subset(familyCSA, F15==1)$CSA2020))
 

GeneraCSA<-tentwenty %>% 
  group_by(Genus, CSA2020) %>% 
  summarize(n=length(SPP)) %>% 
  left_join(nbnumtree) %>% 
  mutate(genpercent=(n/numtree)*100) %>% 
  ungroup() %>% 
  mutate(G20=ifelse(genpercent>20, 1, 0),
         G10=ifelse(genpercent>10, 1, 0)) %>% 
  filter(G20==1|G10==1)

length(unique(subset(GeneraCSA, G20==1)$CSA2020))
length(unique(subset(GeneraCSA, G10==1)$CSA2020))


speciesCSA<-tentwenty %>% 
  group_by(SPP, CSA2020) %>% 
  summarize(n=length(SPP)) %>% 
  left_join(nbnumtree) %>% 
  mutate(spppercent=(n/numtree)*100) %>% 
  ungroup() %>% 
  mutate(S10=ifelse(spppercent>10, 1, 0),
         S5=ifelse(spppercent>5, 1, 0)) %>% 
  filter(S10==1|S5==1)

length(unique(subset(speciesCSA, S10==1)$CSA2020))
length(unique(subset(speciesCSA, S5==1)$CSA2020))
