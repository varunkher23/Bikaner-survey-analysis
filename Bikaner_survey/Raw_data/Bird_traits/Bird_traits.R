library(readxl)

point=read.csv("Master_sheets/point_master_long.csv")%>%
  select(X14_Species_Name)%>%
  filter(!is.na(X14_Species_Name))%>%
  unique.data.frame()%>%
  separate(X14_Species_Name,c("Common_Name","Scientific_Name"),sep = "[()]",remove = F)
  
HWI=read_xlsx("Raw_data/Bird_traits/HWI.xlsx")%>%
  rename(Scientific_Name=`Species name`)%>%
  select(-Synonym,-Notes,-`Species-ID`,-`Tree name`,`Sample size`)

Funcdat=read.csv("Raw_data/Bird_traits/BirdFuncDat.csv")%>%
  rename(Scientific_Name=Scientific)%>%
  select(Scientific_Name,BodyMass.Value)

bird_traits=left_join(point,HWI)%>%
  left_join(Funcdat)

write.csv(bird_traits,"Raw_data/Bird_traits.csv")
