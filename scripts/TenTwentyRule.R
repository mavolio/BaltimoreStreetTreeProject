library(tidyverse)

dat<-read.csv('../BaltimoreStreetTreeProject_Large_Data/street_trees_with_neigh_attributes_2025-03-14.csv')

dat_trees<-dat %>% 
  filter(CSA2020 != "Dickeyville/Franklintown" & CSA2020 != "Unassigned -- Jail") %>% 
  filter(SPP!="unknown tree"&SPP!='Vacant Site'&SPP!='Vacant Potential'&SPP!='Stump'&SPP!='Vacant Site Not Suitable'&SPP!='NA'&SPP!="Z Add 01"&SPP!=" "&SPP!="Dead")%>%
  filter(CONDITION!="Dead"&CONDITION!="Stump"&CONDITION!="Sprout") %>% 
  mutate(Species=case_when(
    SPP=='Acer spp.' ~ 'unk999'
    ,SPP=='Amelanchier spp.' ~ 'Amelanchier canadensis'
    ,SPP=='Carya spp.' ~ 'unk999'
    ,SPP=='Chamaecyparis spp.' ~ 'unk999'
    ,SPP=='Cornus spp.' ~ 'unk999'
    ,SPP=='Crataegus spp.' ~ 'Crataegus laevigata'
    ,SPP=='Euonymus spp.'~ 'unknown shrub'
    ,SPP=='Ficus spp.' ~ 'unk999'
    ,SPP=='Fraxinus spp.' ~ 'unk999'
    ,SPP=='Hydrangea spp.' ~ 'unknown shrub'
    ,SPP=='Ilex spp.' ~ 'unk999'
    ,SPP=='Ilex x' ~ 'Ilex x attenuata-Fosteri'
    ,SPP=='Juniperus spp.' ~ 'unk999'
    ,SPP=='Lonicera spp.' ~ 'unknown shrub'
    ,SPP=='Magnolia spp.' ~ 'unk999'
    ,SPP=='Magnolia x' ~ 'Magnolia x soulangiana'
    ,SPP=='Malus spp.' ~ 'Malus sylvestris'
    ,SPP=='Photinia spp.' ~ 'unknown shrub'
    ,SPP=='Picea spp.' ~ 'unk999'
    ,SPP=='Populus spp.' ~ 'unk999'
    ,SPP=="Prunus spp." ~ 'AssignPrunus'
    ,SPP=='Quercus spp.' ~ 'unk999'
    ,SPP=='Quercus x' ~ 'unk999'
    ,SPP=='Rhus spp.' ~ 'unk999'
    ,SPP=='Salix spp.' ~ 'unk999'
    ,SPP=='Taxus spp.' ~ 'Taxus baccata'
    ,SPP=='Thuja spp.' ~ 'Thuja occidentalis'
    ,SPP=='Ulmus spp.' ~ 'unk999'
    ,SPP=='Ulmus x' ~ 'Ulmus hybrid'
    ,SPP=='Viburnum spp.' ~ 'unknown shrub'
    ,SPP=='unknown shrub' ~ 'unknown shrub'
    ,.default = SPP ), .after='SPP') %>% 
  filter(Species!='unknown shrub'& Species!='unk999') %>% 
  mutate(Species = ifelse(Species=='AssignPrunus', 
                          sample(c('Prunus serrulata', 'Prunus subhirtella', 'Prunus x yedoensis'), size=dat_trees %>% filter(Species=='AssignPrunus') %>% nrow(), replace=T),Species))

subsetCSA<-dat_trees %>% 
  select(Species, DBH, CSA2020) %>% 
  filter(Species!='unknown shrub'|Species!="unk999"|Species!='unknown tree') %>% 
  rename(SPP=Species)

SPP<-subsetCSA %>% 
  select(SPP) %>% 
  unique() %>% 
  separate(SPP, into=c("Genus", "Species", 'other'), sep=" ", remove = F)

raresp<-subsetCSA %>% 
  group_by(SPP) %>% 
  summarise(n=length(DBH)) %>% 
  filter(n<5)

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
  filter(F30==0) %>% 
  select(CSA2020, F30) %>% 
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

         