##### Load packages ####

library(tidyverse)

#### Input data ####
form=read.csv("Data/bikaner-survey-csv/form-1__bikaner-survey-datasheet.csv")%>%
  rename(form_id=ï..ec5_uuid)%>%
  select(-created_at,-uploaded_at,-title,-X53_Secondary_Informa)

habitat=read.csv("Data/bikaner-survey-csv/branch-2__habitat-sampling.csv")%>%
  rename(form_id=ï..ec5_branch_owner_uuid)%>%
  rename(Segment_ID=X20_Segment_ID)%>%
  rename(HabitatBranch=ec5_branch_uuid)%>%
  mutate(X22_Location_Manual=gsub(",", " ",X22_Location_Manual, fixed=TRUE))%>%
  mutate(X22_Location_Manual=gsub("  ", " ",X22_Location_Manual, fixed=TRUE))%>%
  separate(X22_Location_Manual,c("long_manual","lat_manual"),extra = "drop", fill = "right",sep=" ")%>%
  mutate(Segment_Lat = as.character(lat_21_Location),Segment_Long=as.character(long_21_Location))%>%
  mutate(Segment_Lat = ifelse(is.na(lat_21_Location)==T, as.character(lat_manual), lat_21_Location))%>%
  mutate(Segment_Long = ifelse(is.na(long_21_Location)==T, as.character(long_manual), long_21_Location))%>%
  select(-lat_21_Location,-long_21_Location,-UTM_Northing_21_Location,-UTM_Easting_21_Location,-UTM_Zone_21_Location,
         -created_at,-uploaded_at,-title,-long_manual,-lat_manual)

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
         -UTM_Easting_42_Location,-UTM_Zone_42_Location,-long_manual,-lat_manual,-X51_Gender_if_known,-X52_ID_Manual)

transect_segment=full_join(form,habitat,by="form_id")

#### Make animal sighting master sheet ####

animal_master=full_join(transect_segment,animal,by = c("form_id","Segment_ID"))%>%
  select(-created_by.y)
write.csv(animal_master,"Data/animal_master.csv")

##### Make point count master sheet #####

point_master=full_join(transect_segment,point,by = c("form_id","Segment_ID"))%>%
  mutate(Time=as.character(X13_Time))%>%
  mutate(Time = ifelse(X13_Time==""|X13_Time=="NA", as.character(X19_Time_H), X13_Time))%>%
  select(-created_by.y,-X13_Time,-X19_Time_H)
write.csv(point_master,"Data/point_master.csv")
