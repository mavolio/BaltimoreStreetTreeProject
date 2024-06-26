library(tidyverse)

dat<-read.csv('../BaltimoreStreetTreeProject_Large_Data/street_trees_with_neigh_attributes.csv')

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
  mutate(F30=ifelse(fampercent>30, 0, 1),
         F20=ifelse(fampercent>20, 0, 1),
         F15=ifelse(fampercent>15, 0, 1)) %>% 
  filter(F30==0|F15==0|F20==0)

#how many fail F30?
length(unique(subset(familyCSA, F30==0)$CSA2020))
length(unique(subset(familyCSA, F20==0)$CSA2020))
length(unique(subset(familyCSA, F15==0)$CSA2020))
 

GeneraCSA<-tentwenty %>% 
  group_by(Genus, CSA2020) %>% 
  summarize(n=length(SPP)) %>% 
  left_join(nbnumtree) %>% 
  mutate(genpercent=(n/numtree)*100) %>% 
  ungroup() %>% 
  mutate(G20=ifelse(genpercent>20, 0, 1),
         G10=ifelse(genpercent>10, 0, 1)) %>% 
  filter(G20==0|G10==0)

length(unique(subset(GeneraCSA, G20==0)$CSA2020))
length(unique(subset(GeneraCSA, G10==0)$CSA2020))


speciesCSA<-tentwenty %>% 
  group_by(SPP, CSA2020) %>% 
  summarize(n=length(SPP)) %>% 
  left_join(nbnumtree) %>% 
  mutate(spppercent=(n/numtree)*100) %>% 
  ungroup() %>% 
  mutate(S10=ifelse(spppercent>10, 0, 1),
         S5=ifelse(spppercent>5, 0, 1)) %>% 
  filter(S10==1|S5==1)

length(unique(subset(speciesCSA, S10==0)$CSA2020))
length(unique(subset(speciesCSA, S5==0)$CSA2020))

#combining to get map
famMap<-familyCSA %>% 
  filter(F30==0|F20==0) %>% 
  select(CSA2020, F30, F20) %>% 
  unique

genMap<-GeneraCSA %>% 
  filter(G20==0) %>% 
  select(CSA2020, G20) %>% 
  unique

sppMap<-speciesCSA %>% 
  filter(S10==0) %>% 
  select(CSA2020, S10) %>% 
  unique

map<-nbnumtree %>% 
  left_join(famMap) %>% 
  left_join(genMap) %>% 
  left_join(sppMap)

map[is.na(map)] <- 1

map2<-map %>% 
  mutate(tentwentythirty=ifelse(F30==1&G20==1&S10==1, 'Meets10/20/30', 'Not10/20/30'))

#trying to map this
library(mapview)
library(sf)

bmore_nb <- 
  st_read('..//BaltimoreStreetTreeProject_Large_Data//Community_Statistical_Areas_(CSAs)__Reference_Boundaries//Community_Statistical_Areas_(CSAs)__Reference_Boundaries.shp') |> #BNIA community delineations
  st_make_valid() 


bmore_nb |> 
  left_join(map2, by = 'CSA2020') |>
  mapview::mapview(zcol = "tentwentythirty")

         