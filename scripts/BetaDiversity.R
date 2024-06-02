library(tidyverse)
library(codyn)
library(vegan)
library(gtable)
library(grid)
library(gridExtra)

install.packages("cowplot")
library(cowplot)

getwd()
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


####within a neighborhood how similar are street trees on different blocks?
#sums number of individuals per species per block within a NB
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
##Looking into how within NB similarity relates to independent variables
winbeta<-outrac %>% 
  group_by(CSA2020) %>% 
  summarize(spave=mean(species_diff), rave=mean(rank_diff)) %>% 
  left_join(Independent) 

write_csv(winbeta,'input_data/winbeta.csv')

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
##within a NB how many inds of each species, and creates categories for independent variables
nbmean<-subsetCSA %>% 
  group_by(CSA2020, SPP) %>% 
  summarize(n=length(DBH)) %>% 
  left_join(Independent) %>% 
  filter(!is.na(CSA2020)) %>% 
  drop_na() %>% 
  mutate(inc=ifelse(mhhi20<=40000, 'low', ifelse(40000<mhhi20&mhhi20<80000,'middle' ,'high' )),
         vac=ifelse(vacant20<10, 'low', 'high'),
         temp=ifelse(avg_temp>35, '>35', '=<35'),
         race=ifelse(PercBlk>60, 'PredomBlk', ifelse(PercWhite>60, 'PreDomWhite', "drop")),
         ed=ifelse(bahigher20>60, 'HighPEd', 'LessPEd')) %>% 
  pivot_wider(names_from = SPP, values_from = n, values_fill = 0) 

#multivariate analyses of community composition
mds<-metaMDS(nbmean[13:275])

nbinfo<-nbmean[1:12]

#plotting the NMDS results
scores<-mds$points %>% 
  bind_cols(nbinfo) 


#Race
A <- ggplot(data=subset(scores, race !="drop"), aes(x=MDS1, y=MDS2, color=race))+
  geom_point(size=3)+
  stat_ellipse(size=1, aes(color=race))+
  scale_color_manual(values = c("darkslategray3", "darkseagreen"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  labs(color = "Race")

#Income
B <- ggplot(data=scores, aes(x=MDS1, y=MDS2, color=inc))+
  geom_point(size=3)+
  stat_ellipse(size=1, aes(color=inc))+
  scale_color_manual(values = c("cyan3", "blue3", "coral1"), limits = c("low", "middle", "high"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  labs(color = "Income")
  

#Education
C <- ggplot(data=scores, aes(x=MDS1, y=MDS2, color=ed))+
  geom_point(size=3)+
  stat_ellipse(size=1, aes(color=ed))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2") +
  labs(color = "Education")

#Temperature
D <- ggplot(data=scores, aes(x=MDS1, y=MDS2, color=temp))+
  geom_point(size=3) +
  stat_ellipse(size=1, aes(color=temp))+
  scale_color_manual(values = c("orange", "indianred3")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  labs(color = "Temp.")

#Vacancy
E <- ggplot(data=scores, aes(x=MDS1, y=MDS2, color=vac))+
  geom_point(size=3) +
  stat_ellipse(size=1, aes(color=vac))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("NMDS Axis 1")+
  ylab("NMDS Axis 2")+
  labs(color = "Vacancy")

Fig <- plot_grid(A, B, C, D, E , labels = c('A', 'B', 'C', 'D', 'E'), ncol = 2)

ggsave("..\\BaltimoreStreetTreeProject/input_data/FigNMDS.jpg",width=180, height=200, unit="mm", plot=Fig, dpi=300 )

#doing stats on the multivariate analyses
#looking at dispersion, use same matrix every time
dist<-vegdist(nbmean[13:275])

#temp
adonis2(nbmean[13:275]~nbmean$temp)#diff in means
permutest(betadisper(dist,nbmean$temp,type="centroid"))#diff in dispersion

#vacant
adonis2(nbmean[13:275]~nbmean$vac)
permutest(betadisper(dist,nbmean$vac,type="centroid"))

#race
race2 <- nbmean %>% 
  filter(race!= "drop")
dist.r<-vegdist(race2[13:275])
adonis2(race2[13:275]~race2$race)
permutest(betadisper(dist.r,race2$race,type="centroid"))

#inc
adonis2(nbmean[13:275]~nbmean$inc)
permutest(betadisper(dist,nbmean$inc,type="centroid"))

#ed
adonis2(nbmean[13:275]~nbmean$ed)
permutest(betadisper(dist,nbmean$ed,type="centroid"))

hist(Independent$avg_temp)
