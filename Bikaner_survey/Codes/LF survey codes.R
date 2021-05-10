getwd()
library(unmarked)
library(MuMIn)
data <- read.csv("lf_occ_18.csv")
names(data)
data$Lndscp <- as.factor(data$Lndscp)
data$Rgn <- as.factor(data$Rgn)
summary(data)
# checking for multicollinearity
habdat <- data[,44:53]
cor <- cor(habdat)
corr <- ifelse(cor>0.4, round(cor,2),0)
corr
# Pull out occupancy matrix - all sites
y.all <- data[,2:38]
x.all <- data[,39:53]
# Make unmarked frame - all sites
umf.all <- unmarkedFrameOccu(y=y.all, siteCovs=data.frame(x.all))
head(umf.all)

# Pull out occupancy matrix - sites in occupied landscapes
y.rgnr <- data[data$sel==1,2:38]
x.rgnr <- data[data$sel==1,39:53]

# Make unmarked frame - sites in occupied landscapes
umf.rgnr <- unmarkedFrameOccu(y=y.rgnr, siteCovs=data.frame(x.rgnr))
head(umf.rgnr)

# Pull out occupancy matrix - sites in occupied regions
drgnl <- read.csv("lf_occ_18.csv")
names(drgnl)
drgnl$Lndscp <- as.factor(drgnl$Lndscp)
drgnl$Rgn <- as.factor(drgnl$Rgn)
summary(drgnl)
y.rgnl <- drgnl[,2:38]
x.rgnl <- drgnl[,39:53]
# Make unmarked frame - sites in occupied landscapes
umf.rgnl <- unmarkedFrameOccu(y=y.rgnl,
                              siteCovs=data.frame(x.rgnl))
head(umf.rgnl)

# Run occupancy models
## all data  to estimate range-level occupancy
occ_null = occu(~1 ~1, data=umf.all)
occ_RN = occuRN(~obsNum ~1, K=36, data=umf.all)
# Compare basic models
list(AIC(occ_null, occ_RN))
backTransform(occ_null, type="state")

## modeling detection probability with occupancy modeled on regions
odet_null = occu(~1 ~1, data=umf.all)
odet_null.rgn = occu(~1 ~Rgn, data=umf.all)
odet_RN.rgn = occuRN(~1 ~Rgn, K=72, data=umf.all)
odet_drgn.rgn = occu(~Rgn ~Rgn, data=umf.all)
odet_dcova.rgn = occu(~Wthr+Wndspd ~Rgn, data=umf.all)
odet_dcovb.rgn = occu(~Wthr ~Rgn, data=umf.all)
odet_drgncova.rgn = occu(~Rgn+Wthr+Wndspd ~Rgn, data=umf.all)
odet_drgncovb.rgn = occu(~Rgn+Wthr ~Rgn, data=umf.all)

# model selection and inference
odet_sel = model.sel(odet_null,odet_null.rgn, odet_RN.rgn,odet_drgn.rgn,
                       odet_dcova.rgn,odet_dcovb.rgn,odet_drgncova.rgn, odet_drgncovb.rgn)
write.csv(odet_sel,"detection_models.csv")
odet_sel
summary(odet_RN.rgn)  
summary(odet_null.rgn) # selected as the best model


# modeling occupancy on habitat covariates with best detection model
occ1 = occu(~1 ~1, data=umf.all) 
occ2 = occu(~1 ~Rgn, data=umf.all) 
occ3 = occu(~1 ~ Grsl, data=umf.all)
occ4 = occu(~1 ~ Rgn*Grsl, data=umf.all)
occ5 = occu(~1 ~ Rgn*Grsl+Psv_dstb, data=umf.all)
occ6 = occu(~1 ~ Rgn*Grsl+Psv_dstb+Act_dstb, data=umf.all)
occ7 = occu(~1 ~ Rgn*Grsl+Arth, data=umf.all)
occ8 = occu(~1 ~ Rgn*Grsl+Grdvg_ht*Grdvg_cov, data=umf.all)
occ9 = occu(~1 ~ Rgn*Grsl+Arth+Psv_dstb+Act_dstb, data=umf.all)
occ10 = occu(~1 ~ Rgn*Grsl+Grdvg_ht*Grdvg_cov+Arth+Act_dstb+Psv_dstb, data=umf.all)
occ11 = occu(~1 ~ Rgn+Grsl+Grdvg_ht*Grdvg_cov+Arth+Act_dstb+Psv_dstb, data=umf.all)
occ.modsel = model.sel(occ1,occ2,occ3,occ4,occ5,occ6,occ7,occ8,occ9,occ10,occ11)
occ.modsel
write.csv(occ.modsel,"occupancy_models1.csv")

summary(occ2) # use for regional occupancy
summary(occ5) # use for response curves

# Generate regional occupancy estimates
Rgn = c("Ajm","Dcn","Guj","MP","Rajr")
newdat1 = data.frame(Rgn)
occrgn_prd = predict(occ2 , newdata=newdat1, type="state", se.fit=TRUE)
detrgn_prd = predict(occ2 , newdata=newdat1, type="det", se.fit=TRUE)
occ_est_rgn = data.frame(newdat1,occrgn_prd, detrgn_prd)
occ_est_rgn
write.csv(occ_est_rgn,"regional_occupancy.csv")

# Generate response curves
Grsl = rep(seq(0,1,0.01),5)
Rgn = rep(c("Ajm","Dcn","Guj","MP","Rajr"), each=101)
Psv_dstb = rep(mean(data$Psv_dst),101*5)
newdat2 = data.frame(Rgn,Grsl,Psv_dstb)
head(newdat2)
occmod_pred = predict(occ5, newdata=newdat2, type="state", se.fit=TRUE)
occmod_est = data.frame(newdat2,occmod_pred)
head(occmod_est)
write.csv(occmod_est,"occupancy_prediction.csv")

data("minke")
# Density estimated in Distance sampling
install.packages("Distance")
library("Distance")

dist = read.csv("LF_dist_2018.csv")
lf_hn = ds(dist)
lf_hn1 = ds(dist, truncation = 0.6)
lf_hr = ds(dist, key = "hr")
lf_hr1 = ds(dist, key = "hr", truncation = 0.6)
lf_uni = ds(dist, key = "unif") # error in fitting

list(AIC(lf_hn1,lf_hr1))
gof_ds(lf_hn1, plot = TRUE)
gof_ds(lf_hr1, plot = TRUE)
plot(lf_hn1)
plot(lf_hr1)

summary(lf_hn1)
summary(lf_hr1)



# Calculate geometric mean & 95% CI of population abundance
# run for range-level
psi = 0.096
psi.se = 0.023
sites = 860


D = 0.20
Ns = D*29
N.se = Ns*0.34
# start iterations
iter=1000
N = as.data.frame(matrix(0,ncol=4,nrow=iter))
colnames(N) = c("occ","Ns", "Abun", "lnAbun")
for(n in 1:iter){
  N[n,1] = rnorm(1, psi, psi.se)*sites
  N[n,2] = rnorm(1, Ns, N.se)
  N[n,3] = N[n,1]*N[n,2]
  N[n,4] = log(N[n,3]+1)}
arith = mean(N$Abun)
geo = mean(N$lnAbun)
arith
geo

# Computing lf male abundance estimates
mean = exp(geo)-1
tlc = quantile(N$lnAbun, probs = 0.05)
tuc = quantile(N$lnAbun, probs = 0.95) 
lc = exp(tlc)-1
uc = exp(tuc)-1
est = c(mean, lc, uc)
est

# Regional abundances
occ_est_rgn
summary(lf_hn1)
# Gujarat
Gpsi = 0.044
Gpsi.se = 0.021
Gsites = 196


GD = 0.72
GNs = D*29
GN.se = GNs*0.45
# start iterations
iter=100
GN = as.data.frame(matrix(0,ncol=4,nrow=iter))
colnames(N) = c("occ","Ns", "Abun", "lnAbun")
for(n in 1:iter){
  GN[n,1] = rnorm(1, Gpsi, Gpsi.se)*Gsites
  GN[n,2] = rnorm(1, GNs, GN.se)
  GN[n,3] = GN[n,1]*GN[n,2]
  GN[n,4] = log(GN[n,3]+1)}
Garith = mean(GN$Abun)
Ggeo = mean(GN$lnAbun)
Garith
Ggeo

# Computing lf male abundance estimates
Gmean = exp(Ggeo)-1
Gtlc = quantile(GN$lnAbun, probs = 0.05)
Gtuc = quantile(GN$lnAbun, probs = 0.95) 
Glc = exp(Gtlc)-1
Guc = exp(Gtuc)-1
Gest = c(Gmean, Glc, Guc)
Gest
