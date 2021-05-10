## Descriptive statistics of habitat covariates in 144km2 cells of Bikaner Landscape
# Refer to Pg 19, Section 4.16 of Thar survey report 2017-2018
library(matrixStats)
library(tidyverse)

habitat_long=read.csv("Master_sheets/habitat_long.csv")

active_disturbance=habitat_long%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Human=sum(Human)/n_distinct(form_id,Segment_ID),
            Livestock=sum(Livestock)/n_distinct(form_id,Segment_ID),
            Machinery=sum(Machinery)/n_distinct(form_id,Segment_ID),
            Dog=sum(Dog)/n_distinct(form_id,Segment_ID),
            No_active_disturbance=sum(No_active_disturbance)/n_distinct(form_id,Segment_ID))

active_disturbance_summary=data.frame(Mean=colMeans(active_disturbance[sapply(active_disturbance, is.numeric)]),
                                      SE=c(sd(active_disturbance$Human)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$Livestock)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$Machinery)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$Dog)/sqrt(nrow(active_disturbance)),
                                           sd(active_disturbance$No_active_disturbance)/sqrt(nrow(active_disturbance))),
                                      Feature="Active_Disturbance")

infrastructure=habitat_long%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Farm_hut=sum(Farm_hut)/n_distinct(form_id,Segment_ID),
            Road=sum(Road)/n_distinct(form_id,Segment_ID),
            Fence=sum(Fence)/n_distinct(form_id,Segment_ID),
            Settlement=sum(Settlement)/n_distinct(form_id,Segment_ID),
            No_infrastructure=sum(No_infrastructure)/n_distinct(form_id,Segment_ID),
            Water_source=sum(Water_source)/n_distinct(form_id,Segment_ID),
            Industrial_uses=sum(Industrial_uses)/n_distinct(form_id,Segment_ID),
            Powerline=sum(Powerline)/n_distinct(form_id,Segment_ID))

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

substrate=habitat_long%>%
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

Terrain=habitat_long%>%
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

Landcover=habitat_long%>%
  filter(is.na(Segment_ID)==F)%>%
  group_by(X7_Cell_ID)%>%
  summarise(Grassland=sum(Grassland)/n_distinct(form_id,Segment_ID),
            Agriculture_active=sum(Agriculture_active)/n_distinct(form_id,Segment_ID),
            Agriculture_inactive=sum(Agriculture_inactive)/n_distinct(form_id,Segment_ID),
            Scrubland=sum(Scrubland)/n_distinct(form_id,Segment_ID))

Landcover_summary=data.frame(Mean=colMeans(Landcover[sapply(Landcover, is.numeric)]),
                             SE=c(sd(Landcover$Grassland)/sqrt(nrow(Landcover)),
                                  sd(Landcover$Agriculture_active)/sqrt(nrow(Landcover)),
                                  sd(Landcover$Agriculture_inactive)/sqrt(nrow(Landcover)),
                                  sd(Landcover$Scrubland)/sqrt(nrow(Landcover))),
                             Feature="Landcover") 

Summary=rbind(active_disturbance_summary,infrastructure_summary,Terrain_summary,substrate_summary,Landcover_summary)%>%
  mutate(Mean=round(Mean,2))%>%
  mutate(SE=round(SE,2))%>%
  mutate(Value=paste(Mean," (",SE,")",sep=""))%>%
  rownames_to_column(var = "Variable")%>%
  relocate(Feature,Variable,Value)%>%
  select(-Mean,-SE)%>%
  group_by(Feature)%>%
  `rownames<-`(NULL)


irrigation=read.csv("Master_sheets/IGNP_length.csv")
transect_summary=read.csv("Master_sheets/Transect_summary.csv")

habitat_summarised=left_join(active_disturbance,infrastructure)%>%
  left_join(Terrain)%>%
  left_join(substrate)%>%
  left_join(Landcover)%>%
  left_join(transect_summary)%>%
  left_join(irrigation)


write.csv(habitat_summarised,"Master_sheets/Habitat_summary_gridwise.csv")
write.csv(Summary,"Master_sheets/Habitat_summary_long.csv")

#write.csv(Terrain,"Processed_data/Terrain.csv")
#write.csv(Landcover,"Processed_data/Landcover.csv")
#write.csv(infrastructure,"Processed_data/infrastructure.csv")
#write.csv(active_disturbance,"Processed_data/active_disturbance.csv")
#write.csv(substrate,"Processed_data/substrate.csv")
