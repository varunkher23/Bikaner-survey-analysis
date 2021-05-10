library(unmarked)
library(MuMIn)
library(reshape2)

habitat_summarised=read.csv("Master_sheets/Habitat_summary_gridwise.csv")%>%
  mutate(Canal=as.factor(Canal))%>%
  arrange(X7_Cell_ID)

data_raw=read.csv("Master_sheets/animal_master_long.csv")%>%
  select(X7_Cell_ID,New_segment,X45_Species,X46_Individuals_Count,X47_Sighting_Distance,X48_SightAnimal_Beari,
         X49_Transect_Bearing,X50_Landcover)%>%
  arrange(X7_Cell_ID)
data_raw$X46_Individuals_Count[is.na(data_raw$X46_Individuals_Count)]=as.numeric(0)

assign(paste(i,"_data",sep=""),data)

i="Nilgai (Boselaphus tragocamelus)"
data=data_raw%>%
  mutate(X45_Species=ifelse(is.na(X45_Species)==T,"None",X45_Species))%>%
  mutate(Individuals=ifelse(X45_Species==i,as.numeric(X46_Individuals_Count),as.numeric(0)))%>%
  dcast(X7_Cell_ID~New_segment,value.var = "Individuals",fun.aggregate = sum,fill = NA_real_)%>%
  filter(is.na(X7_Cell_ID)==F)
data$sel=ifelse(rowSums(data[,-1],na.rm=T)>1,1,0)

# Pull out occupancy matrix - all sites
y.all <- data[,2:19]
y.all[y.all>0]=1
x.all = habitat_summarised%>%
  select(Canal)
init=data.frame(knownocc=rowSums(y.all,na.rm = T),init=seq(1:nrow(y.all)))%>%
  select(init)

# Make unmarked frame - all sites
rm(umf.all)
umf.all <- unmarkedFrameOccu(y=y.all,siteCovs = x.all)  

# Run occupancy models
## all data  to estimate range-level occupancy
occ_null = occu(~1~Canal, data=umf.all,knownOcc = init$init)
chinkara_psi=backTransform(occ_null, type="state")
chinkara_psi@estimate
predict(occ_null,x.all,type="state",se.fit=T)%>%
  View()


# Pull out occupancy matrix - sites in occupied landscapes
y.rgnr <- data[data$sel==1,2:19]



