library(raster)
library(dismo)
library(tidyverse)
library(maptools)
library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(rJava)  
library(jsonlite)

Bik_outline=st_read("Raw_data/GIS_Layers/India_ShapeFiles/IND_adm2.shp")%>%
  filter(NAME_2=="Bikaner")

files <- list.files(path = "D:/Bikaner_survey/Raw_data/GIS_Layers/Raster_DryZone/Projected_IndSubC/Bioclim", 
                    pattern='adf$', full.names=TRUE,recursive = T )%>%
  file.info()%>%
  filter(size>1000000)%>%
  rownames()

bioclim <- stack(files)%>%
  crop(Bik_outline)
bioclim_sr <- sampleRandom(bioclim, ncell(bioclim))
bioclim_pca=prcomp(bioclim_sr,scale=TRUE, retx=FALSE)
bioclim_pca3=predict(bioclim, bioclim_pca, index=1:2)
tri=raster("D:/Bikaner_survey/Raw_data/GIS_Layers/Raster_DryZone/Projected_WGS1984/tri/w001001.adf" )%>%
  resample(bioclim,method='bilinear')%>%
  crop(bioclim_pca3)
modelEnv=stack(bioclim_pca3,tri)%>%
  mask(Bik_outline)

summary(bioclim_pca)
plot(tri)

animal=read.csv("Master_sheets/animal_master_long.csv")%>%
  filter(!is.na(Lat))%>%
  filter(X45_Species=="Chinkara (Gazella bennettii)")%>%
  dplyr::select(Long,Lat)

animal=read.csv("Master_sheets/habitat_long.csv")%>%
  filter(X38_Spinytailed_Lizar=="Preset")%>%
  dplyr::select(Long_segment,Lat_segment)

# first layer of the RasterStack
plot(bioclim, 1)
# with the points function, "add" is implicit
points(animal, col='blue')

fold <- kfold(animal, k=5)
animaltest <- animal[fold == 1, ]
animaltrain <- animal[fold != 1, ]

animal.maxent <- maxent(modelEnv, animaltrain)
animal.maxent.predict=predict(animal.maxent,modelEnv)%>%
  mask(Bik_outline)
plot(animal.maxent.predict)
plot(animal,add=T)

bg <- randomPoints(modelEnv, 1000) #background "pseudoabsences"

#simplest way to use 'evaluate'
e1 <- evaluate(animal.maxent, p = animal, a=bg, x=modelEnv)
e1
