library(tidyverse)
library(codyn)
library(vegan)

theme_set(theme_bw(12))


Independent<- read_csv('input_data/Independent_variable.csv')

dat<-read.csv('../BaltimoreStreetTreeProject_Large_Data/street_trees_with_neigh_attributes_2023-10-31.csv')

dat_trees<-dat %>% 
  filter(SPP!="unknown tree"&SPP!='Vacant Site'&SPP!='Vacant Potential'&SPP!='Stump'&SPP!='Vacant Site Not Suitable'&SPP!='NA'&SPP!='unknown shrub'&SPP!="Z Add 01"&SPP!=" "&SPP!="Dead")%>%
filter(CONDITION!="Dead"&CONDITION!="Stump"&CONDITION!="Sprout")

common<-dat_trees %>% 
  group_by(SPP) %>% 
  summarize(n=length(DBH))

subsetCSA<-dat_trees %>% 
  select(CSA2020, SPP, Street, DBH, CONDITION, MT, TREE_HT, LOC_TYPE, CULTIVAR) %>% 
  filter(CSA2020!="Dickeyville/Franklintown"&CSA2020!='Unassigned -- Jail')


####within a neighborhood how similar are street trees on differnt blocks?
#sums number of inds per species per block within a NB
nbdat<-subsetCSA %>% 
  group_by(CSA2020, Street, SPP) %>% 
  summarize(n=length(DBH)) %>% 
  filter(!is.na(CSA2020)) %>% 
  drop_na()

#for loop to generate measures of tree similarity among streets in a NB
nblist<-unique(nbdat$CSA2020)
outrac<-data.frame()

for (i in 1:length(nblist)){
  
  subset<-nbdat %>% 
    filter(CSA2020==nblist[i])
  
  rac<-RAC_difference(subset, species.var = "SPP", abundance.var = "n", replicate.var = 'Street')%>% 
    mutate(CSA2020=nblist[i])
  
  outrac<-outrac %>% 
    bind_rows(rac) 
  
}
##Looking into how within NB similarity relates to indepdent variables
winbeta<-outrac %>% 
  group_by(CSA2020) %>% 
  summarize(spave=mean(species_diff), rave=mean(rank_diff)) %>% 
  left_join(Independent) 

#TRINI DOES STATS HERE

toplot<-winbeta%>% 
  pivot_longer(PercBlk:vacant20, names_to = "ind", values_to = "indval") %>% 
  pivot_longer(spave:rave, names_to = 'measure', values_to = 'measval')



#relationship between diversity and drivers
ggplot(data=toplot, aes(x=indval, y=measval))+
  geom_point()+
  geom_smooth(method='lm', color='black')+
  facet_grid(measure~ind, scales = 'free')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


###between neighborhoods, how similar are communities?
##within a NB how many inds of each speceis, and creates categories for independant variables
nbmean<-subsetCSA %>% 
  group_by(CSA2020, SPP) %>% 
  summarize(n=length(DBH)) %>% 
  left_join(Independent) %>% 
  filter(!is.na(CSA2020)) %>% 
  drop_na() %>% 
  mutate(inc=ifelse(mhhi20<40000, 'low', ifelse(mhhi20>40000&mhhi20<80000, 'mid', 'high')),
         vac=ifelse(vacant20<10, 'low', 'high'),
         temp=ifelse(avg_temp>32, 'hot', 'lesshot'),
         race=ifelse(PercBlk>70, 'PredomBlk', ifelse(PercBlk<70&PercBlk>30, 'Mixed', 'PreDomWhite')),
         ed=ifelse(bahigher20>60, 'HighPEd', 'LessPEd')) %>% 
  pivot_wider(names_from = SPP, values_from = n, values_fill = 0) 

#multivariate analyses of community composiiton
mds<-metaMDS(nbmean[13:275])

nbinfo<-nbmean[1:12]

#plotting the NMDS results
scores<-mds$points %>% 
  bind_cols(nbinfo) 

#which do we want? Temp, and one or two other
ggplot(data=scores, aes(x=MDS1, y=MDS2, color=race))+
  geom_point(size=5)
# ggplot(data=scores, aes(x=MDS1, y=MDS2, color=Wht))+
#   geom_point(size=5)
ggplot(data=scores, aes(x=MDS1, y=MDS2, color=inc))+
  geom_point(size=5)
ggplot(data=scores, aes(x=MDS1, y=MDS2, color=ed))+
  geom_point(size=5)
ggplot(data=scores, aes(x=MDS1, y=MDS2, color=temp))+
  geom_point(size=5)
ggplot(data=scores, aes(x=MDS1, y=MDS2, color=vac))+
  geom_point(size=5)

#doing stats on the multiviarite analyes
#looking at dispersion, use same matrix everytime
dist<-vegdist(nbmean[13:275])

#temp
adonis2(nbmean[13:275]~nbmean$temp)#diff in means
permutest(betadisper(dist,nbmean$temp,type="centroid"))#diff in dispersion

#vacant
adonis2(nbmean[13:275]~nbmean$vac)
permutest(betadisper(dist,nbmean$vac,type="centroid"))

#race
adonis2(nbmean[13:275]~nbmean$race)
permutest(betadisper(dist,nbmean$race,type="centroid"))

#inc
adonis2(nbmean[13:275]~nbmean$inc)
permutest(betadisper(dist,nbmean$inc,type="centroid"))

#ed
adonis2(nbmean[13:275]~nbmean$ed)
permutest(betadisper(dist,nbmean$ed,type="centroid"))
