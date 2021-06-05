library(sf)
library(ggspatial)
library(tidyverse)
library(ggrepel)
library(ggmap)
library(RColorBrewer)

transect_summary=read.csv("Master_sheets/Transect_summary.csv")
survey_HQ=st_read("Raw_data/GIS_Layers/Survey_HQ.shp")
survey_HQ=survey_HQ%>%
  mutate(X=st_coordinates(survey_HQ)[,1])%>%
  mutate(Y=st_coordinates(survey_HQ)[,2])
all_grids=st_read("D:/Bikaner_survey/Raw_data/GIS_Layers/Bikaner_survey_all_grids.shp")%>%
  select(-layer,-path)%>%
  rename(X7_Cell_ID=ID)%>%
  filter(X7_Cell_ID%in%transect_summary$X7_Cell_ID)
Ind_adm=st_read("Raw_data/GIS_Layers/India_ShapeFiles/IND_adm2.shp")%>%
  filter(NAME_2%in%c("Bikaner","Jaisalmer","Jodhpur","Churu","Ganganagar","Nagaur","Hanumangarh"))
Bik_outline=Ind_adm%>%
  filter(NAME_2=="Bikaner")

animal_long=read.csv("Master_sheets/animal_master_long.csv")%>%
  arrange(X7_Cell_ID)%>%
  rename(size=X46_Individuals_Count)%>%
  select(-X)

habitat_summarised=read.csv("Master_sheets/Habitat_summary_gridwise.csv")%>%
  mutate(Canal=as.factor(Canal))%>%
  arrange(X7_Cell_ID)%>%
  select(-X)

Chinkara_ER=animal_long%>%
  filter(X45_Species=="Chinkara (Gazella bennettii)")%>%
  full_join(habitat_summarised)%>%
  rename(Effort=transect_length)%>%
  mutate(Area=ifelse(Canal==1,as.numeric(6336),as.numeric(6480)))%>%
  mutate(Region.Label=Canal)%>%
  group_by(X7_Cell_ID)%>%
  summarise(ER=sum(size,na.rm = T)/min(Effort))%>%
  mutate(Species="Chinkara")%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm,inherit.aes = F)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2)+
  geom_sf(aes(fill=ER*100))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  geom_sf(data = survey_HQ,aes(color="red"),size=4,show.legend = "point")+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  fontface = "bold", nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1,  -0.3, 0.2, 0.13))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",hjust = .5,size = 16),
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Encounter Rate \nper 100 sq.km")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())+
  ggtitle("Chinkara Encounter Rate per 100 sq.km")

Dfox_ER=animal_long%>%
  filter(X45_Species=="Desert Fox (Vulpes vulpes pusilla)")%>%
  full_join(habitat_summarised)%>%
  rename(Effort=transect_length)%>%
  mutate(Area=ifelse(Canal==1,as.numeric(6336),as.numeric(6480)))%>%
  mutate(size=ifelse(size>10,as.numeric(1),size))%>%
  mutate(Region.Label=Canal)%>%
  group_by(X7_Cell_ID)%>%
  summarise(ER=sum(size,na.rm = T)/mean(Effort))%>%
  mutate(Species="Desert Fox")%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=ER*100))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  fontface = "bold", nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1,-0.3, 0.2, 0.13))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",hjust = .5,size = 16))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Encounter Rate \nper 100 sq.km")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())+
  ggtitle("Desert Fox Encounter Rate per 100 sq.km")

####### Habitat

Terrain=habitat_summarised%>%
  select(X7_Cell_ID,Flat:Undulating)%>%
  mutate(Undulating=Sloping+Undulating)%>%
  select(-Sloping)%>%
  pivot_longer(cols = Flat:Undulating,values_to = "Mean")%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=Mean))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  fontface = "bold", nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1,-0.3, 0.2, 0.13))+
  ggtitle("Terrain",subtitle = "(Proportion of points with a certain terrain type in 100m radius)")+
  facet_wrap(.~name,ncol=2,nrow=2)+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.50, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,height = unit(0.4,"in"),width = unit(0.3,"in"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",size = 16),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(hjust = 0,face = "bold",size = 13),
        strip.background = element_blank(),text = element_text(size=11))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Proportion \nof points")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())

Landcover=habitat_summarised%>%
  select(X7_Cell_ID,Grassland:Scrubland)%>%
  pivot_longer(cols = Grassland:Scrubland,values_to = "Mean")%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=Mean))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  fontface = "bold", nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1, -0.3, 0.2, 0.13))+
  facet_wrap(.~name,ncol=2,nrow=2)+
  ggtitle("Landcover",subtitle = "Proportion of points with a particular Land-cover type in 100m radius")+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.50, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,height = unit(0.4,"in"),width = unit(0.3,"in"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",size = 16),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(hjust = 0,face = "bold",size = 13),
        strip.background = element_blank(),text = element_text(size=11))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Proportion \nof points")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())

active_disturbance=habitat_summarised%>%
  select(X7_Cell_ID,Human:Dog)%>%
  pivot_longer(cols = Human:Dog,values_to = "Mean")%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=Mean))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  fontface = "bold", nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1,-0.3, 0.2, 0.13))+
  facet_wrap(.~name,ncol=2,nrow=2)+
  ggtitle("Active Disturbance",subtitle = "Proportion of points with a particular active disturbance in 100m radius")+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.50, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,height = unit(0.4,"in"),width = unit(0.3,"in"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",size = 16),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(hjust = 0,face = "bold",size = 13),
        strip.background = element_blank(),text = element_text(size=11))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Proportion \nof points")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())

infrastructure=habitat_summarised%>%
  select(X7_Cell_ID,Road,Fence,Powerline,Water_source)%>%
  pivot_longer(cols = Road:Water_source,values_to = "Mean")%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=Mean))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1, -0.3, 0.2, 0.13))+
  facet_wrap(.~name,ncol=2,nrow=2)+
  ggtitle("Infrastructure",subtitle = "Proportion of points with a particular Infrastructure in 100m radius")+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.50, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,height = unit(0.4,"in"),width = unit(0.3,"in"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",size = 16),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(hjust = 0,face = "bold",size = 13),
        strip.background = element_blank(),text = element_text(size=11))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Proportion \nof points")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())

spprich=read.csv("Processed_data/spprich.csv")%>%
  rename(X7_Cell_ID=X)%>%
  mutate(cv=Mean/SD)%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=Mean))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1, 
                                                              -0.3, 0.2, 0.13))+
  ggtitle("Bird Species Richness",subtitle = "Predicted bird richness at the grid level")+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.50, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,height = unit(0.4,"in"),width = unit(0.3,"in"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",size = 16),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(hjust = 0,face = "bold",size = 13),
        strip.background = element_blank(),text = element_text(size=11))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Species \nrichness")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())

spprich_obsv=point_master%>%
  left_join(read.csv("Raw_data/Bird_traits.csv"))%>%
  group_by(X7_Cell_ID)%>%
  summarise(Overall=length(unique(X14_Species_Name)),
            Generalist=length(unique(X14_Species_Name[Habitat=="Multiple"])),
            Grassland=length(unique(X14_Species_Name[Habitat=="Grassland-Desert"])),
            Woodland=length(unique(X14_Species_Name[Habitat=="Woodland"])))%>%
  pivot_longer(cols = c("Overall","Grassland","Woodland","Generalist"))%>%
  filter(name%in%c("Grassland","Generalist"))%>%
  left_join(all_grids)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(data=Ind_adm)+
  geom_sf(data=Bik_outline,fill="khaki4",alpha=0.2,show.legend = "polygon")+
  geom_sf(aes(fill=value))+
  scale_fill_binned(low="#f7fcb9",high="#31a354")+
  geom_sf(data = survey_HQ,aes(color="red"),size=4)+
  geom_text_repel(data = survey_HQ, aes(x = X, y = Y, label = Name), 
                  nudge_x = c(0.5, 0, -0.2, 0.2), nudge_y = c(-0.1, 
                                                              -0.3, 0.2, 0.13))+
  ggtitle("Bird Species Richness",subtitle = "Observed bird richness at the grid level")+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.50, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering,height = unit(0.4,"in"),width = unit(0.3,"in"))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        legend.position = "bottom",
        legend.title = element_text(face="bold"),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",size = 16),
        panel.spacing = unit(2,"lines"),
        strip.text.x = element_text(hjust = 0,face = "bold",size = 13),
        strip.background = element_blank(),text = element_text(size=11))+
  geom_text(aes(74.4,28.1,label="CHURU"))+
  geom_text(aes(71.65,27.65,label="JAISALMER"))+
  geom_text(aes(73.5,29.18,label="SHRI GANGANAGAR"))+
  geom_text(aes(72.5,27.3,label="JODHPUR"))+
  geom_text(aes(73.8,27.3,label="NAGAUR"))+
  geom_text(aes(71.8,28.8,label="PAKISTAN"),fontface="bold")+
  ylim(27.2,29.2)+
  xlim(71.5,74.5)+
  labs(fill="Species \nrichness")+
  labs(color="Survey HQ")+
  scale_colour_discrete(label=element_blank())+
  facet_wrap(.~name)
