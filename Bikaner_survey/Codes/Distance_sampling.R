library(tidyverse)
library(Distance)

habitat_summarised=read.csv("Master_sheets/Habitat_summary_gridwise.csv")%>%
  mutate(Canal=as.factor(Canal))%>%
  arrange(X7_Cell_ID)%>%
  select(-X)

animal_long=read.csv("Master_sheets/animal_master_long.csv")%>%
  arrange(X7_Cell_ID)%>%
  rename(size=X46_Individuals_Count)%>%
  select(-X)

chinkara=animal_long%>%
  filter(X45_Species=="Chinkara (Gazella bennettii)")%>%
  full_join(habitat_summarised)%>%
  rename(Effort=transect_length)%>%
  mutate(Area=ifelse(Canal==1,as.numeric(6336),as.numeric(6480)))%>%
  rename(Sample.Label=X7_Cell_ID)%>%
  mutate(Region.Label=Canal)


Dfox=animal_long%>%
  filter(X45_Species=="Desert Fox (Vulpes vulpes pusilla)")%>%
  full_join(habitat_summarised)%>%
  rename(Effort=transect_length)%>%
  mutate(Area=ifelse(Canal==1,as.numeric(6336),as.numeric(6480)))%>%
  rename(Sample.Label=X7_Cell_ID)%>%
  mutate(size=ifelse(size>4,1,size))%>%
  mutate(Region.Label=Canal)

hist(chinkara$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)
breakpoints <- c(0,90,150,250,330)
##### Chinkara = c(0,90,150,250,330)
#### Desert Fox = c(0,50,100,150,200)
conversion <- convert_units("meter", "kilometer", "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn1 <- ds(chinkara, transect = "line", key="hn", convert.units = conversion,adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
hn2 <- ds(chinkara, transect = "line", key="hn", convert.units = conversion,adjustment = "herm",
          cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
hn3 <- ds(chinkara, transect = "line", key="hn", convert.units = conversion,adjustment = "cos",
          cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
hn4 <- ds(chinkara, transect = "line", key="hn", convert.units = conversion,adjustment = "poly",
          cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
uni1=ds(chinkara, transect = "line", key="unif", convert.units = conversion,adjustment = "cos",
        cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
hr1=ds(chinkara, transect = "line", key="hr", convert.units = conversion,adjustment = NULL,
        cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
hr2=ds(chinkara, transect = "line", key="hr", convert.units = conversion,adjustment = "cos",
        cutpoints = mybreaks,truncation = trunc.list,formula = ~1)

kable(summarize_ds_models(hn1,hn2,hn3,uni1,hr1,hr2),digits=3,sort="AIC",
      caption = "Model selection summary for Chinkara line transect")
best_model=data.frame(AIC(hn1,hn2,hn3,hn4,uni1,hr1,hr2))%>%filter(AIC==min(AIC))%>%rownames()%>%get()
gof_ds(best_model)
plot(best_model)
kable(best_model[["dht"]][["individuals"]][["D"]],caption = "Density of Chinkara using the best fitting detection model")
