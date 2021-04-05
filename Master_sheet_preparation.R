##### Load packages ####

library(tidyverse)
library(lubridate)
library(chron)

#### Input data ####
form=read.csv("Data/bikaner-survey-csv/form-1__bikaner-survey-datasheet.csv")%>%
  rename(form_id=ï..ec5_uuid)%>%
  select(-created_at,-uploaded_at,-title,-X53_Secondary_Informa)%>%
  mutate(X9_Date_of_survey=dmy(X9_Date_of_survey))

habitat=read.csv("Data/bikaner-survey-csv/branch-2__habitat-sampling.csv")%>%
  rename(form_id=ï..ec5_branch_owner_uuid)%>%
  rename(Segment_ID=X20_Segment_ID)%>%
  rename(HabitatBranch=ec5_branch_uuid)%>%
  mutate(X22_Location_Manual=gsub(",", " ",X22_Location_Manual, fixed=TRUE))%>%
  mutate(X22_Location_Manual=gsub("  ", " ",X22_Location_Manual, fixed=TRUE))%>%
  separate(X22_Location_Manual,c("long_manual","lat_manual"),extra = "drop", fill = "right",sep=" ")%>%
  mutate(Segment_Lat = as.numeric(lat_21_Location),Segment_Long=as.numeric(long_21_Location))%>%
  mutate(Segment_Lat = ifelse(is.na(lat_21_Location)==T, as.numeric(lat_manual), lat_21_Location))%>%
  mutate(Segment_Long = ifelse(is.na(long_21_Location)==T, as.numeric(long_manual), long_21_Location))%>%
  mutate(Lat_segment=ifelse(Segment_Lat<50,Segment_Lat,Segment_Long))%>%
  mutate(Long_segment=ifelse(Segment_Long>50,Segment_Long,Segment_Lat))%>%
  select(-Segment_Lat,-Segment_Long)%>%
  select(-lat_21_Location,-long_21_Location,-UTM_Northing_21_Location,-UTM_Easting_21_Location,-UTM_Zone_21_Location,
         -title,-long_manual,-lat_manual)
habitat=full_join(form,habitat,by="form_id")
habitat=habitat[order(habitat$X7_Cell_ID,habitat$X8_Transect_ID,habitat$Segment_ID),]%>%
  group_by(X7_Cell_ID)%>%
  mutate(New_segment=row_number())

point=read.csv("Data/bikaner-survey-csv/branch-3__point-count.csv")%>%
  rename(form_id=ï..ec5_branch_owner_uuid)%>%
  rename(Segment_ID=X12_Segment_ID)%>%
  rename(PointBranch=ec5_branch_uuid)%>%
  select(-created_at,-uploaded_at,-title)

animal=read.csv("Data/bikaner-survey-csv/branch-1__animal-sighting.csv")%>%
  rename(form_id=ï..ec5_branch_owner_uuid)%>%
  rename(Segment_ID=X41_Segment_ID)%>%
  rename(AnimalBranch=ec5_branch_uuid)%>%
  mutate(X43_Location_Manual=gsub(",", " ",X43_Location_Manual, fixed=TRUE))%>%
  mutate(X43_Location_Manual=gsub("  ", " ",X43_Location_Manual, fixed=TRUE))%>%
  separate(X43_Location_Manual,c("long_manual","lat_manual"),extra = "drop", fill = "right",sep=" ")%>%
  mutate(Animal_Lat = as.character(lat_42_Location),Animal_Long=as.character(long_42_Location))%>%
  mutate(Animal_Lat = ifelse(is.na(lat_42_Location)==T, as.character(lat_manual), lat_42_Location))%>%
  mutate(Animal_Long = ifelse(is.na(long_42_Location)==T, as.character(long_manual), long_42_Location))%>%
  select(-created_at,-uploaded_at,-title,-lat_42_Location,-long_42_Location,-UTM_Northing_42_Location,
         -UTM_Easting_42_Location,-UTM_Zone_42_Location,-long_manual,-lat_manual,-X51_Gender_if_known,-X52_ID_Manual)%>%
  mutate(angle=abs(X48_SightAnimal_Beari-X49_Transect_Bearing))%>%
  mutate(distance=abs(X47_Sighting_Distance*sin(angle*pi/180)))

#### Make animal sighting master sheet ####

habitat_long=habitat%>%
  ungroup()%>%
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
  mutate(Agriculture=ifelse(str_detect(habitat$X25_Landcover_100m,"Agriculture")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Scrubland=ifelse(str_detect(habitat$X25_Landcover_100m,"Scrubland")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Grassland=ifelse(str_detect(habitat$X25_Landcover_100m,"Grassland")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Woodland=ifelse(str_detect(habitat$X25_Landcover_100m,"Woodland")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Fallow=ifelse(str_detect(habitat$X25_Landcover_100m,"Barren/Fallow")==T,as.numeric(1),as.numeric(0)))%>%
  mutate(Agriculture_active=ifelse(Agriculture==1 & X33_Agriculture!=0,as.numeric(1),as.numeric(0)))%>%
  mutate(Agriculture_active=ifelse(str_detect(habitat$X34_3_Dominant_Plants,"Wheat|Chana|Mustard")==T,as.numeric(1),Agriculture_active))%>%
  mutate(Agriculture_active=ifelse(str_detect(habitat$X35_Name_of_species_i,"Wheat|Chana|Mustard")==T,as.numeric(1),Agriculture_active))

point_master=full_join(habitat,point,by = c("form_id","Segment_ID"))%>%
  mutate(Time=as.character(X13_Time))%>%
  mutate(Time = ifelse(X13_Time==""|is.na(X13_Time)==T, as.character(X19_Time_H), X13_Time))%>%
  select(-created_by.y,-X13_Time,-X19_Time_H)

point_master_long=full_join(habitat_long,point,by = c("form_id","Segment_ID"))%>%
  mutate(Time=as.character(X13_Time))%>%
  mutate(Time = ifelse(X13_Time==""|is.na(X13_Time)==T, as.character(X19_Time_H), X13_Time))%>%
  select(-created_by.y,-X13_Time,-X19_Time_H)%>%
  mutate(created_at=ymd_hms(created_at,tz = "Asia/Calcutta"))%>%
  mutate(Time=ifelse(Time=="" & month(created_at)==2,format(created_at,format="%H:%M:%S"),Time))

segment_time=point_master_long%>%
  mutate(Time=chron(times. = Time))%>%
  group_by(X7_Cell_ID,New_segment)%>%
  filter(!is.na(Time))%>%
  summarise(Time=min(Time))

habitat_short=full_join(habitat,segment_time,by=c("X7_Cell_ID","New_segment"))%>%
  select(-X19_Time_H)
habitat_long=full_join(habitat_long,segment_time,by=c("X7_Cell_ID","New_segment"))%>%
  select(-X19_Time_H)

animal_master_short=full_join(habitat,animal,by = c("form_id","Segment_ID"))%>%
  select(-created_by.y)
animal_master_long=full_join(habitat_long,animal,by = c("form_id","Segment_ID"))%>%
  select(-created_by.y)


write.csv(point_master,"Data/point_master_short.csv")
write.csv(point_master_long,"Data/point_master_long.csv")


write.csv(habitat,"Data/habitat_short.csv")
write.csv(habitat_long,"Data/habitat_long.csv")

write.csv(animal_master_short,"Data/animal_master_short.csv")
write.csv(animal_master_long,"Data/animal_master_long.csv")
