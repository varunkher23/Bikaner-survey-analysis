The model code below is written program R and uses the R2jags package to run JAGS.
#The data file, 'D', includes columns representing the study site, species (SppCode), number of 
#days during which the species was photographed ???1 time (detections), number of sampling 
#occasions (num.nights), covariate values, and diet classification (Diet)
#Define the group variables
library(tidyverse)
library(purrr)
library(matrixStats)
library(boot)

MSOM_datafile=function(data,return) ### return = data_long or datafile; by= "ocassion" or "date"
{
  data=data
  All_points=data%>%
    select(X7_Cell_ID,New_segment)%>%
    unique.data.frame()
  species=as.character(unique(data$X14_Species_Name[!is.na(data$X14_Species_Name)]))
  if(return=="datafile") {
    
    datafile=data.frame(X7_Cell_ID=NA,X14_Species_Name=NA,detections=NA,num.segments=NA)
    
    for (i in 1:length(species))   {
      sample=data%>%
        filter(X14_Species_Name==species[i])%>%
        select(X7_Cell_ID,New_segment,X15_Individual_Count)%>%
        mutate(X15_Individual_Count=as.numeric(X15_Individual_Count))
      
      data_filtered=full_join(All_points,sample)%>%
        mutate(X15_Individual_Count=ifelse(is.na(X15_Individual_Count)==T, 
                                           as.numeric(0),as.numeric(X15_Individual_Count)))%>%
        mutate(X15_Individual_Count=ifelse(is.na(X15_Individual_Count)==F & X15_Individual_Count>0, 
                                           as.numeric(1),as.numeric(X15_Individual_Count)))%>%
        unique.data.frame()%>%
        arrange(X7_Cell_ID)
      
      data_long=pivot_wider(data = data_filtered,id_cols = "X7_Cell_ID",names_from = "New_segment",
                            values_from  = X15_Individual_Count,
                            values_fill = NA)
      file=data.frame(X7_Cell_ID=data_long$X7_Cell_ID,X14_Species_Name=as.character(species[i]),
                      detections=rowSums(data_long[,c(-1,-2)],na.rm = T),
                      num.segments=rowCounts(as.matrix(data_long[,-1]),value = 0,na.rm=T)+
                        rowCounts(as.matrix(data_long[,c(-1,-2)]),value = 1,na.rm=T))
      
      datafile=rbind(datafile,file)%>%
        filter(!is.na(X14_Species_Name))
      
    }
    return(datafile)
  }
  if(return=="data_long"){
    data_long=array(dim=c(length(species),length(unique(data$X7_Cell_ID)),max(data$New_segment)))
    for(i in 1:length(species)){
      sample=data%>%
        filter(X14_Species_Name==species[i])%>%
        select(X7_Cell_ID,New_segment,X15_Individual_Count)%>%
        mutate(X15_Individual_Count=as.numeric(X15_Individual_Count))
      
      data_filtered=full_join(All_points,sample)%>%
        mutate(X15_Individual_Count=ifelse(is.na(X15_Individual_Count)==T, 
                                           as.numeric(0),as.numeric(X15_Individual_Count)))%>%
        mutate(X15_Individual_Count=ifelse(is.na(X15_Individual_Count)==F & X15_Individual_Count>0, 
                                           as.numeric(1),as.numeric(X15_Individual_Count)))%>%
        unique.data.frame()%>%
        arrange(X7_Cell_ID)
      
      pivot=as.matrix(select(pivot_wider(data = data_filtered,id_cols = "X7_Cell_ID",names_from = "New_segment",
                                         values_from  = X15_Individual_Count,
                                         values_fill = NA),-X7_Cell_ID))%>%
        as.matrix()
      data_long[i,,]=pivot
    }  
    dimnames(data_long)[[2]]=c(unique(data_filtered$X7_Cell_ID))
    dimnames(data_long)[[3]]=c(1:length(unique(data_filtered$New_segment)))
    dimnames(data_long)[[1]]=c(species)
    return(data_long)
  }
}

habitat_long=read.csv("Master_sheets/habitat_long.csv")%>%
  filter(X65_Point_Count_List_=="All Birds recorded")%>%
  select(X7_Cell_ID,New_segment)%>%
  unique.data.frame()%>%
  arrange(X7_Cell_ID,New_segment)%>%
  group_by(X7_Cell_ID)%>%
  mutate(New_segment_bird=row_number())%>%
  ungroup(X7_Cell_ID)

point_master=read.csv("Master_sheets/point_master_long.csv")%>%
  filter(X65_Point_Count_List_=="All Birds recorded")%>%
  right_join(habitat_long)%>%
  mutate(New_segment=New_segment_bird)%>%
  select(-New_segment_bird)%>%
  arrange(X7_Cell_ID,New_segment)

datalong=MSOM_datafile(data = point_master,return = "data_long")

########### Data long ################

bird_traits=read.csv("Raw_data/Bird_traits.csv")%>%
  select(X14_Species_Name,Diet)%>%
  mutate(Diet=ifelse(is.na(Diet)==T,as.character("omnivore"),Diet))%>%
  mutate(Diet=ifelse(Diet=="fruit"|Diet=="seeds"|Diet=="plants",as.character("herbivore"),Diet))%>%
  mutate(Diet=ifelse(Diet=="vertebrates"|Diet=="scav",as.character("carnivore"),Diet))
Species_details=left_join(data.frame(X14_Species_Name=rownames(datalong)),bird_traits)%>%
                mutate(Sppcode=seq(1:dim(datalong)[[1]]))

habitat=read.csv("Master_sheets/habitat_long.csv")%>%
  right_join(habitat_long)%>%
  mutate(New_segment=New_segment_bird)%>%
  select(-New_segment_bird)%>%
  arrange(X7_Cell_ID,New_segment)

sitecovs=read.csv("Master_sheets/Habitat_summary_gridwise.csv")%>%
  select(-X)%>%
  filter(X7_Cell_ID%in%unique(point_master$X7_Cell_ID))%>%
  arrange(X7_Cell_ID)

active_agriculture=as.matrix(select(pivot_wider(data = habitat,id_cols = "X7_Cell_ID",names_from = "New_segment",
                                        values_from  = Agriculture_active,
                                        values_fill = NA),-X7_Cell_ID))
Flat=as.matrix(select(pivot_wider(data = habitat,id_cols = "X7_Cell_ID",names_from = "New_segment",
                                                values_from  = Flat,
                                                values_fill = NA),-X7_Cell_ID))

G <- cbind(as.numeric(Species_details$Diet=="herbivore"),as.numeric(Species_details$Diet=="invertebrates"),
           as.numeric(Species_details$Diet=="carnivore"),as.numeric(Species_details$Diet=="omnivore"))
#Define the covariates for occupancy
X = sitecovs%>%
  arrange(X7_Cell_ID)%>%
  select(Canal)
#Define the group covariates
XG = cbind(X*G[,1],X*G[,2],X*G[,3],X*G[,4])
#Define the covariates for detection
dX=array(dim = c(dim(active_agriculture)[[1]],dim(active_agriculture)[[2]],2),dimnames = list(dimnames(active_agriculture)[[1]],
                                                                              dimnames(active_agriculture)[[2]],
                                                                              c("active_agriculture","Flat")))
dX[,,1]=active_agriculture
dX[,,2] = Flat
nvisits=as.double(rowSums(apply(datalong,c(2,3),min)+1,na.rm = T))

#Load the necessary libraries
library(R2jags); library(reshape2); library(dplyr)
#Define the necessary arguments to run the jags command
#Load all the data including the detection array, number of sampling occasions, individual 
#species sampled, total number of sampled species, and covariate information
data <- list(D = datalong,  Species =Species_details$Sppcode, 
             n = dim(datalong)[[2]], nspp = max(as.numeric(Species_details$Sppcode)),X = X, XG = XG, dX = dX,
             init=apply(datalong, c(1,2), max,na.rm=T),nvisits=nvisits)

#Specify the initial values
inits = function() {list(Z = data$init)}
#Specify the parameters to be monitored
params = c("rho","pbeta","spbeta","sigpbeta","mbeta","sigbeta","sbeta","gbeta",
           "psi.mean","sigma.occ","p.mean","sigma.p","alpha","Z","P","psi","p")

nc = 2
ni = 10000
nb = 2500
nthin = 50#Write the model code to a text file called "AllMammals.txt"
cat(
  "    data {
      nBeta <- dim(X)
      nG <- dim(XG)
      nP <- dim(dX)
    }
    model {
      # Define covariance parameter between detection and mean occupancy
      rho ~ dunif(-1,1)
      var.p <- sigma.p /(1.-pow(rho,2))
      
      #Define prior distributions for occupancy parameters
      alpha.mean <- log(psi.mean) - log(1-psi.mean)
      psi.mean ~ dunif(0,1)
      sigma.occ ~ dunif(0,10)
      tau.occ <- pow(sigma.occ,-2)
      #Define prior distributions for true positive detections
      p.mean ~ dunif(0,1)
      b <- log(p.mean) - log(1-p.mean)
      sigma.p ~ dunif(0,10)
      tau.p <- pow(sigma.p,-2)
      #Define prior distributions for occupancy effects where nbeta is the number of occupancy 
      #covariates in the model, mbeta is the community-level hyper-parameter for each of the nbeta 
      #covariates, tbeta is the amount of variability in each of the community-level hyper-parameters, 
      #and sbeta is the species-specific covariate effects
      for (a in 1:nBeta[2]){
        mbeta[a] ~ dnorm(0,0.01) 
        sigbeta[a] ~ dunif(0,10)
        tbeta[a] <- pow(sigbeta[a],-2) 
        for (i in 1:(nspp)) { 
          sbeta[i,a] ~ dnorm(0,tbeta[a]) 
        }
      }
      #Define prior distributions for nG, the group-level hyper-parameters
      for (a in 1:nG[2]){
      gbeta[a] ~ dnorm(0,0.01)
    }
    #Define prior distributions for detection effects where nP is the number of detection covariates in 
    #the model, pbeta is the community-level hyper-parameter for each of the nP covariates, tpbeta is 
    #the amount of variability in each of the community-level hyper-parameters, and spbeta is the 
    #species-specific covariate effects
    for (a in 1:nP[3]){
      pbeta[a] ~ dnorm(0,0.01)
      sigpbeta[a] ~ dunif(0,10)
      tpbeta[a] <- pow(sigpbeta[a],-2) 
      for (i in 1:(nspp)) { 
        spbeta[i,a] ~ dnorm(0,tpbeta[a]) 
      }
    }
    #Define prior distributions for the occupancy and detection covariates for each species 
    for (i in 1:(nspp)) {
      alpha[i] ~ dnorm(alpha.mean, tau.occ)
      mu.p[i] <- b + (rho*sigma.p /sigma.occ)*(alpha[i] - alpha.mean)
      P[i] ~ dnorm(mu.p[i], var.p)
    }
    
    #Estimate the occupancy probability (latent Z matrix) for each species at each camera station
    for(i in 1:(nspp)){
    for (j in 1:n) {
      logit(psi[i,j]) <- alpha[Species[i]] + inprod(mbeta,X[j,])+ inprod(sbeta[Species[i],],
                                                                       X[j,]) + inprod(gbeta,XG[j,])
      
      Z[i,j] ~ dbern(psi[i,j])
      #Estimate the detection probability for each species at each camera station
      for(k in 1:nvisits[j]){
      logit(p[i,j,k]) <- P[Species[i]] + inprod(pbeta,dX[j,k,]) + inprod(spbeta[Species[i],],
                                                                   dX[j,k,])
      zp[i,j,k] <- p[i,j,k]*Z[i,j]
      D[i,j,k] ~ dbern(zp[i,j,k])
      }
    }
  }
}
", file = "Codes/MSOM.txt")

#Run the model and call the results "output"
output <- jags(data = data, inits = inits, parameters.to.save = params, 
               model.file ="Codes/MSOM.txt", 
               n.chains =nc, n.iter =ni, n.burnin =nb, n.thin =nthin)
output.mcmc=as.mcmc(output)
  

write_rds(output,"D:/WII-Thar_LTEO/Camera_trapping/MSOM/Allmammals_output_thar_obsv.Rdata")

output=readRDS("D:/WII-Thar_LTEO/Camera_trapping/MSOM/Allmammals_output_thar.Rdata")
#See a summary of the parameter estimates
output.sum <- output$BUGSoutput$summary%>%
  as.data.frame()
output_summary=output.sum%>%
  mutate(parameter=as.character(row.names(output.sum)))

occupancy_data=output_summary[contains(match = "alpha",vars = output_summary$parameter),]%>%
  mutate(Sppcode=parse_number(parameter))%>%
  left_join(unique.data.frame(select(Species_details,X14_Species_Name,Sppcode)),by="Sppcode")
occupancy_summary=  occupancy_data%>%
  mutate(mean=inv.logit(occupancy_data$mean))%>%
  mutate(sd=inv.logit(occupancy_data$sd))%>%
  mutate(`2.5%`=inv.logit(occupancy_data$`2.5%`))%>%
  mutate(`50%`=inv.logit(occupancy_data$`50%`))%>%
  mutate(`97.5%`=inv.logit(occupancy_data$`97.5%`))%>%
  select(-`25%`,-`75%`)%>%
  relocate(X14_Species_Name)


######### Datafile ######### 

##IGNORE
datafile=MSOM_datafile(data = sudasiri,by="Occasion",return = "datafile")

D=datafile
Species_details=data.frame(Species=unique(datafile$Species),
                           Group=c("Xeric","Syanthropic","Mesic","Synanthropic","Xeric","Xeric",
                                   "Mesic","Mesic","Mesic","Mesic","Syanthropic","Xeric","Xeric","Xeric"),
                           Sppcode=seq(1:14))
D=left_join(D,Species_details,by="Species")

traps=trap_effort(data = sudasiri)%>%
  mutate(Date=as.Date(Date,format="%Y-%m-%d"))
sitecovs=read.csv("D:/WII-Thar_LTEO/Camera_trapping/REM/Input/cam_fact.csv")%>%
  filter(Grid%in%D$Grid==T)%>%
  select(-X)
sitecovs=traps%>%
  select(Grid)%>%
  unique.data.frame()%>%
  left_join(sitecovs)%>%
  mutate(fac1=ifelse(is.na(fac1)==T,as.numeric(0),fac1))%>%
  mutate(fac2=ifelse(is.na(fac2)==T,as.numeric(0),fac2))%>%
  mutate(fac3=ifelse(is.na(fac3)==T,as.numeric(0),fac3))%>%
  arrange(Grid)

sitecovs=sitecovs[-23,] ### Error in S126
D=left_join(D,sitecovs,by="Grid")

G <- cbind(as.numeric(D$Group=="Mesic"),as.numeric(D$Group=="Synanthropic"))
#Define the covariates for occupancy
X = select(D,fac1:fac3)
#Define the group covariates
XG = cbind(X*G[,1],X*G[,2])
#Define the covariates for detection
dX = select(D,Date_index,Camera_age)
#Load the necessary libraries
library(R2jags); library(reshape2); library(dplyr)
#Define the necessary arguments to run the jags command
#Load all the data including the detection array, number of sampling occasions, individual 
#species sampled, total number of sampled species, and covariate information
data <- list(D = D$detections, N = ceiling(D[,"num.nights"]), Species =D$Sppcode, 
             n = nrow(D), nspp = max(as.numeric(D$Sppcode)),X = X, XG = XG, dX = dX)

cat(
  "data {
  nBeta <- dim(X)
  nG <- dim(XG)
  nP <- dim(dX)
  }
model {
  # Define covariance parameter between detection and mean occupancy
  rho ~ dunif(-1,1)
  var.p <- sigma.p /(1.-pow(rho,2))
  
  #Define prior distributions for occupancy parameters
  alpha.mean <- log(psi.mean) - log(1-psi.mean)
  psi.mean ~ dunif(0,1)
  sigma.occ ~ dunif(0,10)
  tau.occ <- pow(sigma.occ,-2)
  #Define prior distributions for true positive detections
  p.mean ~ dunif(0,1)
  b <- log(p.mean) - log(1-p.mean)
  sigma.p ~ dunif(0,10)
  tau.p <- pow(sigma.p,-2)
  #Define prior distributions for occupancy effects where nbeta is the number of occupancy 
  #covariates in the model, mbeta is the community-level hyper-parameter for each of the nbeta 
  #covariates, tbeta is the amount of variability in each of the community-level hyper-parameters, 
  #and sbeta is the species-specific covariate effects
  for (a in 1:nBeta[2]){
    mbeta[a] ~ dnorm(0,0.01) 
    sigbeta[a] ~ dunif(0,10)
    tbeta[a] <- pow(sigbeta[a],-2) 
    for (i in 1:(nspp)) { 
      sbeta[i,a] ~ dnorm(0,tbeta[a]) 
    }
  }
  #Define prior distributions for nG, the group-level hyper-parameters
  for (a in 1:nG[2]){
    gbeta[a] ~ dnorm(0,0.01)
  }
  #Define prior distributions for detection effects where nP is the number of detection covariates in 
  #the model, pbeta is the community-level hyper-parameter for each of the nP covariates, tpbeta is 
  #the amount of variability in each of the community-level hyper-parameters, and spbeta is the 
  #species-specific covariate effects
  for (a in 1:nP[2]){
    pbeta[a] ~ dnorm(0,0.01)
    sigpbeta[a] ~ dunif(0,10)
    tpbeta[a] <- pow(sigpbeta[a],-2) 
    for (i in 1:(nspp)) { 
      spbeta[i,a] ~ dnorm(0,tpbeta[a]) 
    }
  }
  #Define prior distributions for the occupancy and detection covariates for each species 
  for (i in 1:(nspp)) {
    alpha[i] ~ dnorm(alpha.mean, tau.occ)
    mu.p[i] <- b + (rho*sigma.p /sigma.occ)*(alpha[i] - alpha.mean)
    P[i] ~ dnorm(mu.p[i], var.p)
  }
  
  #Estimate the occupancy probability (latent Z matrix) for each species at each camera station
  for (j in 1:n) {
    logit(psi[j]) <- alpha[Species[j]] + inprod(mbeta,X[j,])+ inprod(sbeta[Species[j],],
                                                                     X[j,]) + inprod(gbeta,XG[j,])
    #Estimate the detection probability for each species at each camera station
    logit(p[j]) <- P[Species[j]] + inprod(pbeta,dX[j,]) + inprod(spbeta[Species[j],],
                                                                 dX[j,]) 
    Z[j] ~ dbern(psi[j])
    zp[j] <- p[j]*Z[j]
    D[j] ~ dbin(zp[j], N[j])
  }
}",file = "D:/WII-Thar_LTEO/Camera_trapping/MSOM/AllMammals_thar_site.txt")



###### TO ESTIMATE GROUP-LEVEL HYPER-PARAMETERS: #####
#Define the occupancy covariate effects where mbeta is the community-level hyper-parameter, 
#gbeta is the group-level hyper-parameter, and sbeta is the species-specific parameter
mbeta <- output$BUGSoutput$sims.list$mbeta
gbeta <- output$BUGSoutput$sims.list$gbeta
sbeta <- output$BUGSoutput$sims.list$sbeta

#Define the covariates and the groups
covs <- colnames(X)
sizes <- c("Overall","herbivore","invertebrates","carnivore","omnivore")
#Create a data frame where the number of rows is equal to the number of covariates * the 
#number of groups
group <- data.frame(expand.grid(covs,sizes), matrix(NA,length(covs)*length(sizes),4))
colnames(group) <- c("Factor","Group","Mean","SD","LCI","UCI")
#Create a loop estimating the reference group values
for (a in 1:length(covs)){
  group[a,3:6] <- c(mean(mbeta[,a]),sd(mbeta[,a]),quantile(mbeta[,a],
                                                           c(0.025,0.975)))
}
#Create a second loop estimating the other group values
for (a in 1:length(covs)){
  for (b in 1:(length(sizes)-1)){
    sims <- mbeta[,a] + gbeta[,((b-1)*dim(X)[2]+a)]
    group[(dim(X)[2]*(b)+a),3:6] <- c(mean(sims),sd(sims),quantile(sims,
                                                                   c(0.025,0.975)))
  }
}

#Export the results as a table
write.table(x=group,file="Results/group.csv",sep=",")

#TO ESTIMATE SPECIES-SPECIFIC COVARIATE VALUES:#Begin by defining the species
spec <- select(Species_details,X14_Species_Name,Diet)%>%
  unique.data.frame()
#Define the group levels where 1 = Carnivore, 2 = herbivore, and 3 = omnivore
spec$Diet <- factor(spec$Diet)
gg <- as.numeric(spec$Diet)
#Define the occupancy covariates and groups
covs <- colnames(X)
sizes <- c("herbivore","invertebrates","carnivore","omnivore")
#Create a data frame where the number of rows is equal to the number of covariates * the 
#number of species
species <- data.frame(expand.grid(covs,spec$X14_Species_Name), 
                      matrix(NA,length(covs)*length(spec$X14_Species_Name),4))
colnames(species) <- c("Covariate","Species","Mean","SD","LCI","UCI")
#Re-define gbeta
gbeta <- output$BUGSoutput$sims.list$gbeta #Original
gbeta <- cbind(gbeta, matrix(0,nrow(gbeta),length(covs))) #New
#Create a loop that will estimate species-specific values for each of the covariates
for (a in 1:length(covs)){
  for (b in 1:length(spec$X14_Species_Name)){
    sims <- mbeta[,a] + gbeta[,((gg[b]-1)*+a)] + sbeta[,b,a]
    species[(dim(X)[2]*(b-1)+a),3:6] <- c(mean(sims), sd(sims),
                                          quantile(sims,c(0.025,0.975)))
  }
}

#Export the results as a table
write.csv(x=species,file="D:/WII-Thar_LTEO/Camera_trapping/MSOM/Trial/Results/species.csv",sep=",")

output=readRDS("D:/WII-Thar_LTEO/Camera_trapping/MSOM/Allmammals_output_thar_obsv.Rdata")
#TO ESTIMATE SPECIES RICHNESS FOR EACH SITE:
#Begin by increasing memory to avoid errors
memory.limit(size=10000)
#Define the z matrix
spec <- select(Species_details,X14_Species_Name,Diet)%>%
  unique.data.frame()
z = output$BUGSoutput$sims.list$Z

#Sort the data frame based on species, study site, and diet category
dz=data.frame(Species=spec$X14_Species_Name[1],Diet=spec$Diet[1],Station=colnames(datalong),t(z[,1,]))
for (i in 2:dim(z)[2]) {
  a=z[,i,]
  Z_all=data.frame(Species=spec$X14_Species_Name[i],Diet=spec$Diet[i],Station=colnames(datalong),t(a))
  dz=rbind(dz,Z_all)
}
dz <- dz%>%
  mutate(ID=1:nrow(dz))
#Load reshape2 library
library(reshape2)
#Melt the data frame for easy casting
m.dz <- melt(dz,id.vars = c("Species","Station","Diet","ID") )
#Aggregate the data by summing the values in the z matrix for each camera station during each 
#iteration 
z.all <- acast(m.dz, Station ~ variable, fun.aggregate = sum) ### Add up 1/0 values for each species per iteration
#Use the aggregated values to create probability distributions and estimate mean, sd, and 95% 
#credible interval values for camera-station specific species richness
z.all <- t(apply(z.all,1,function(x) c(mean(x),sd(x),quantile(x,c(0.025,0.975))))); 
colnames(z.all) = c("Mean","SD","LCI","UCI")
#Export estimates of species richness as a table
write.csv(x=z.all,file="Processed_data/spprich.csv",sep=",")

#To estimate group richness for each site:
#Aggregate the data by summing the values in the z matrix for each camera station during each 
#iteration 
z.all <- acast(m.dz, Station ~ variable, fun.aggregate = sum) ### Add up 1/0 values for each species per iteration
#Use the aggregated values to create probability distributions and estimate mean, sd, and 95% 
#credible interval values for camera-station specific species richness
z.all <- t(apply(z.all,1,function(x) c(mean(x),sd(x),quantile(x,c(0.025,0.975))))); 
colnames(z.all) = c("Mean","SD","LCI","UCI")
#Export estimates of group richness as a table
write.table(x=z.group,file="Results/grouprich.csv",sep=",")

left_join(data.frame(Sppcode=seq(1:16),psi.mean=inv.logit(output$BUGSoutput$mean$alpha),
                     psi.sd=inv.logit(output$BUGSoutput$sd$alpha)),
          data.frame(unique.data.frame(select(D,Species,Sppcode))),by="Sppcode")

naive_occu=pivot_wider(data = D,id_cols = Species,names_from = Station,values_from = detections,values_fn = sum)
