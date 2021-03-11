## Descriptive statistics of habitat covariates in 144km2 cells of Bikaner Landscape
# Refer to Pg 19, Section 4.16 of Thar survey report 2017-2018
library(matrixStats)

habitat=read.csv("Data/transect_segment_habitat.csv")

habitat=habitat%>%
  mutate(Human=ifelse(str_detect(habitat$X36_Anthropogenic_Pre,"Human")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Livestock=ifelse(str_detect(habitat$X36_Anthropogenic_Pre,"Livestock")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Machinery=ifelse(str_detect(habitat$X36_Anthropogenic_Pre,"Machinery")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Dog=ifelse(str_detect(habitat$X36_Anthropogenic_Pre,"Dog")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(No_active_disturbance=ifelse(str_detect(habitat$X36_Anthropogenic_Pre,"None")==T,as.numeric(1),as.numeric(0)))%>%
  select(-X36_Anthropogenic_Pre)%>%
  mutate(Farm_hut=ifelse(str_detect(habitat$X37_Infrastructures_5,"Farm hut")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Powerline=ifelse(str_detect(habitat$X37_Infrastructures_5,"Powerline")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Settlement=ifelse(str_detect(habitat$X37_Infrastructures_5,"Settlement")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Water_source=ifelse(str_detect(habitat$X37_Infrastructures_5,"Water Source")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Fence=ifelse(str_detect(habitat$X37_Infrastructures_5,"Fence")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Industrial_uses=ifelse(str_detect(habitat$X37_Infrastructures_5,"Industrial Uses")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Road=ifelse(str_detect(habitat$X37_Infrastructures_5,"Road")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(No_infrastructure=ifelse(str_detect(habitat$X37_Infrastructures_5,"None")==T,as.numeric(1),as.numeric(0)))%>%
  select(-X37_Infrastructures_5)%>%
  mutate(Soil=ifelse(str_detect(habitat$X27_Substrate,"Soil")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Sand=ifelse(str_detect(habitat$X27_Substrate,"Sand")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Gravel=ifelse(str_detect(habitat$X27_Substrate,"Gravel")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Rocky=ifelse(str_detect(habitat$X27_Substrate,"Rocky")==T,as.numeric(1),as.numeric(0)))%>%
  select(-X27_Substrate)%>%
  mutate(Flat=ifelse(str_detect(habitat$X26_Terrain,"Flat")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Sloping=ifelse(str_detect(habitat$X26_Terrain,"Sloping")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Undulating=ifelse(str_detect(habitat$X26_Terrain,"Undulating")==T,as.numeric(1),as.numeric(0)))%>%
  select(-X26_Terrain)
  
active_disturbance=habitat%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Human=sum(Human)/n_distinct(Segment_ID),
            Livestock=sum(Livestock)/n_distinct(Segment_ID),
            Machinery=sum(Machinery)/n_distinct(Segment_ID),
            Dog=sum(Dog)/n_distinct(Segment_ID),
            No_active_disturbance=sum(No_active_disturbance)/n_distinct(Segment_ID))

active_disturbance_summary=data.frame(Mean=colMeans(active_disturbance[sapply(active_disturbance, is.numeric)]),
                                      SE=c(sd(active_disturbance$Human)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$Livestock)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$Machinery)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$Dog)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$No_active_disturbance)/sqrt(nrow(active_disturbance))),
                                      Feature="Active_Disturbance")

infrastructure=habitat%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Farm_hut=sum(Farm_hut)/n_distinct(Segment_ID),
            Road=sum(Road)/n_distinct(Segment_ID),
            Fence=sum(Fence)/n_distinct(Segment_ID),
            Settlement=sum(Settlement)/n_distinct(Segment_ID),
            No_infrastructure=sum(No_infrastructure)/n_distinct(Segment_ID),
            Water_source=sum(Water_source)/n_distinct(Segment_ID),
            Industrial_uses=sum(Industrial_uses)/n_distinct(Segment_ID),
            Powerline=sum(Powerline)/n_distinct(Segment_ID))

infrastructure_summary=data.frame(Mean=colMeans(infrastructure[sapply(infrastructure, is.numeric)]),
                                      SE=c(sd(infrastructure$Farm_hut)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$Road)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$Fence)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$Settlement)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$No_infrastructure)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$Water_source)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$Industrial_uses)/sqrt(nrow(infrastructure)),
                                           sd(infrastructure$Powerline)/sqrt(nrow(infrastructure))),
                                  Feature="Infrastructure")

substrate=habitat%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Soil=sum(Soil)/n_distinct(form_id,Segment_ID),
            Sand=sum(Sand)/n_distinct(form_id,Segment_ID),
            Gravel=sum(Gravel)/n_distinct(form_id,Segment_ID),
            Rocky=sum(Rocky)/n_distinct(form_id,Segment_ID))

substrate_summary=data.frame(Mean=colMeans(substrate[sapply(substrate, is.numeric)]),
                             SE=c(sd(substrate$Soil)/sqrt(nrow(substrate)),
                                  sd(substrate$Sand)/sqrt(nrow(substrate)),
                                  sd(substrate$Gravel)/sqrt(nrow(substrate)),
                                  sd(substrate$Rocky)/sqrt(nrow(substrate))),
                             Feature="Substrate")

Terrain=habitat%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Flat=sum(Flat)/n_distinct(form_id,Segment_ID),
            Sloping=sum(Sloping)/n_distinct(form_id,Segment_ID),
            Undulating=sum(Undulating)/n_distinct(form_id,Segment_ID))

Terrain_summary=data.frame(Mean=colMeans(Terrain[sapply(Terrain, is.numeric)]),
                           SE=c(sd(Terrain$Flat)/sqrt(nrow(substrate)),
                                sd(Terrain$Sloping)/sqrt(nrow(substrate)),
                                sd(Terrain$Undulating)/sqrt(nrow(substrate))),
                           Feature="Terrain")

Summary=rbind(active_disturbance_summary,infrastructure_summary,Terrain_summary,substrate_summary)

write.table(Summary,"clipboard")
