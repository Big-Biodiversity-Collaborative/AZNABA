# growth rate model, hierarchical: sites within species
# loop has a fork for 1) species which occur at just a single site, and 
# 2) species that occur at multiple sites. Code is very wet for clarity.
library(rjags)
library(coda)
library(HDInterval)

dat<-read.csv("fourthAllSp.csv",header = T)
dim(dat)  # 82243    12
head(dat)
names(dat)
str(dat)

length(unique(dat$sp)) #315 spp.
species<-unique(dat$sp)
# data prep
# add filtering here
# filter: drop any sites per species with < 10 years of observations/counts
dat<-dat[which(dat$yearsPerSitePerSp>9),]
dim(dat) # 56347    12
length(unique(dat$sp)) #272 spp.
species<-unique(dat$sp)

# centering years across all data here 
yearsZ<-(dat$year - mean(dat$year))  # 2001.654
dat<-cbind(dat,yearsZ)

# set up results collection:
yearSpeciesMu<-data.frame()
effortSpeciesMu<-data.frame()
dateSpeciesMu<-data.frame()
previousSpeciesMu<-data.frame()
yearSpeciesTau<-data.frame()
effortSpeciesTau<-data.frame()
dateSpeciesTau<-data.frame()
previousSpeciesTau<-data.frame()
speciesNames<-data.frame()
listPPC<-list()
speciesDiag<-data.frame()
intmuSpeciesMU<-data.frame()
yearSpeciesMu80<-data.frame()
interzSites<-data.frame()
# site-level results
yearSites<-data.frame()
effortSites<-data.frame()
dateSites<-data.frame()
previousSites<-data.frame()

# BIG LOOP begins here
# loop for pulling out each species for analysis:
sp_dat<-data.frame()
for (j in 1:length(species)){
sp_dat<-droplevels(dat[which(dat$sp==species[j]),])
sites<-unique(sp_dat$site)
workingDat<-data.frame()
for (i in 1:length(sites)){
tmp<-droplevels(sp_dat[which(sp_dat$site==sites[i]),])
tmp<-tmp[-is.na(tmp$r), ]
tmp<-cbind(tmp,scale(tmp$tM),scale(tmp$hours),scale(tmp$date))
workingDat<-rbind(workingDat,tmp[,-c(1:2,4:5,7:12)]) 
}
names(workingDat)<-c("site","growth","stdYear","stdPrevious","stdHours","stdDate") # this is re-arranged from previous version and stdYear is now transformed across all the data
head(workingDat)
datB<-workingDat # just to condense the name
head(datB)

Nsite<-length(unique(datB$site))
########### loop for species with only 1 site
if (Nsite < 2){
basicData<-list(growth=datB$growth, year=datB$stdYear,effort=datB$stdHours,
                date=datB$stdDate,previous=datB$stdPrevious,N=length(datB$stdYear))
                
model<-"model{
	# poisson likelihood for counts/abundance, ln is link
	for(i in 1:N){
	      growth[i] ~ dnorm(mu[i],tau)
	      # link is identity
	      mu[i] <- int + beta1 * year[i] + beta2 * effort[i] + beta3 * date[i] + beta4 * previous[i]

        ## posterior predictive check stuff
        #### generate ynew for posterior pedictive check
        ynew[i] ~ dnorm(mu[i],tau)
        ##### examine residuals
        resid[i]<- growth[i] - ynew[i] 
	}

  tau ~ dgamma(0.1,0.1) # prior on precision

      beta1 ~ dnorm(0, 0.0001)  # watch these priors
      beta2 ~ dnorm(0, 0.0001)
      beta3 ~ dnorm(0, 0.0001)
      beta4 ~ dnorm(0, 0.0001)   
      int ~ dnorm(0, 0.0001)  
}
"


basicModel<-jags.model(textConnection(model),data=basicData,n.chains=2)  #compile


samples<-jags.samples(model=basicModel,
                      variable.names=c("beta1","beta2","beta3","beta4","mu","tau","int","ynew","resid"),
                                        n.adapt=1000,n.iter=15000)
# str(samples)


##################################
#######Diagnostics and Results ###
####################################

# site level - diagnostics then parameter estimates
# get median HDIs probPos ProbNeg and write is all to csv for big geographical analysis...

beta1ESS<-numeric()
beta1ESS[1]<-effectiveSize(samples$beta1[,,1]) 
beta1ESS[2]<-effectiveSize(samples$beta1[,,2]) 
write.csv(beta1ESS,file = paste(species[j],"beta1ESSsummary.csv"),row.names = F,quote = F)

###############
# think about the autoburnin argument here - default = T... because your parameter estimates use the whole chain...
beta1GR<-numeric()
beta1GR<-gelman.diag(mcmc.list(as.mcmc(samples$beta1[,,1]),as.mcmc(samples$beta1[,,2])),autoburnin = F)[[1]][1]
write.csv(beta1GR,file = paste(species[j],"beta1GRrange.csv"),row.names = F,quote = F)

###############
beta2ESS<-numeric()
beta2ESS[1]<-effectiveSize(samples$beta2[i,,1]) 
beta2ESS[2]<-effectiveSize(samples$beta2[i,,2]) 
write.csv(beta2ESS,file = paste(species[j],"beta2ESSsummary.csv"),row.names = F,quote = F)

###############
beta2GR<-numeric()
beta2GR<-gelman.diag(mcmc.list(as.mcmc(samples$beta2[,,1]),as.mcmc(samples$beta2[,,2])))[[1]][1]
write.csv(beta2GR,file = paste(species[j],"beta2GRrange.csv"),row.names = F,quote = F)

###############
beta3ESS<-numeric()
beta3ESS[1]<-effectiveSize(samples$beta3[,,1]) 
beta3ESS[2]<-effectiveSize(samples$beta3[,,2]) 
write.csv(beta3ESS,file = paste(species[j],"beta3ESSsummary.csv"),row.names = F,quote = F)

####################
beta3GR<-numeric()
beta3GR<-gelman.diag(mcmc.list(as.mcmc(samples$beta3[,,1]),as.mcmc(samples$beta3[,,2])))[[1]][1]
write.csv(beta3GR,file = paste(species[j],"beta3GRrange.csv"),row.names = F,quote = F)

##############  site level parameter estimates
# collect site parameters to be incorporated in Species list (for sp with only 1 site)
beta1sp<-numeric()
# beta1[1]<-paste(species[j])
# # beta1[1]<-paste(sites)
beta1sp[1]<-round(median(samples$beta1[,,]),3)
beta1sp[2]<-round(hdi(samples$beta1[,,],credMass = 0.95)[2],3) #upper
beta1sp[3]<-round(hdi(samples$beta1[,,],credMass = 0.95)[1],3) # lower
beta1sp[4]<-round((length(which(samples$beta1[,,]>0)))/length(samples$beta1[,,]),3)
beta1sp[5]<-round((length(which(samples$beta1[,,]<0)))/length(samples$beta1[,,]),3)

# year HDI 80
beta180sp<-numeric()
beta180sp[1]<-round(median(samples$beta1[,,]),3)
beta180sp[2]<-round(hdi(samples$beta1[,,],credMass = 0.80)[2],3) #upper
beta180sp[3]<-round(hdi(samples$beta1[,,],credMass = 0.80)[1],3) # lower
beta180sp[4]<-round((length(which(samples$beta1[,,]>0)))/length(samples$beta1[,,]),3)
beta180sp[5]<-round((length(which(samples$beta1[,,]<0)))/length(samples$beta1[,,]),3)

beta2sp<-numeric()
beta2sp[1]<-round(median(samples$beta2[,,]),3)
beta2sp[2]<-round(hdi(samples$beta2[,,],credMass = 0.95)[2],3) #upper
beta2sp[3]<-round(hdi(samples$beta2[,,],credMass = 0.95)[1],3) # lower
beta2sp[4]<-round((length(which(samples$beta2[,,]>0)))/length(samples$beta2[i,,]),3)
beta2sp[5]<-round((length(which(samples$beta2[,,]<0)))/length(samples$beta2[i,,]),3)

beta3sp<-numeric()
beta3sp[1]<-round(median(samples$beta3[,,]),3)
beta3sp[2]<-round(hdi(samples$beta3[,,],credMass = 0.95)[2],3) #upper
beta3sp[3]<-round(hdi(samples$beta3[,,],credMass = 0.95)[1],3) # lower
beta3sp[4]<-round((length(which(samples$beta3[,,]>0)))/length(samples$beta3[i,,]),3)
beta3sp[5]<-round((length(which(samples$beta3[,,]<0)))/length(samples$beta3[i,,]),3)

beta4sp<-numeric()
beta4sp[1]<-round(median(samples$beta4[,,]),3)
beta4sp[2]<-round(hdi(samples$beta4[,,],credMass = 0.95)[2],3) #upper
beta4sp[3]<-round(hdi(samples$beta4[,,],credMass = 0.95)[1],3) # lower
beta4sp[4]<-round((length(which(samples$beta4[,,]>0)))/length(samples$beta4[i,,]),3)
beta4sp[5]<-round((length(which(samples$beta4[,,]<0)))/length(samples$beta4[i,,]),3)

# site-level intercepts
interzsp<-numeric()
interzsp[1]<-round(median(samples$int[,,]),3)
interzsp[2]<-round(hdi(samples$int[,,],credMass = 0.95)[2],3) #upper
interzsp[3]<-round(hdi(samples$int[,,],credMass = 0.95)[1],3) # lower
interzsp[4]<-round((length(which(samples$int[,,]>0)))/length(samples$int[i,,]),3)
interzsp[5]<-round((length(which(samples$int[,,]<0)))/length(samples$int[i,,]),3)

##############  site level parameter estimates continued
# collect site parameters to be incorporated in site list (for sp with only 1 site)
beta1site<-matrix(NA,nrow = Nsite,ncol = 7)
beta1site[1]<-paste(species[j])
beta1site[2]<-paste(sites)
beta1site[3]<-round(median(samples$beta1[,,]),3)
beta1site[4]<-round(hdi(samples$beta1[,,],credMass = 0.95)[2],3) #upper
beta1site[5]<-round(hdi(samples$beta1[,,],credMass = 0.95)[1],3) # lower
beta1site[6]<-round((length(which(samples$beta1[,,]>0)))/length(samples$beta1[,,]),3)
beta1site[7]<-round((length(which(samples$beta1[,,]<0)))/length(samples$beta1[,,]),3)

beta2site<-matrix(NA,nrow = Nsite,ncol = 7)
beta2site[1]<-paste(species[j])
beta2site[2]<-paste(sites)
beta2site[3]<-round(median(samples$beta2[,,]),3)
beta2site[4]<-round(hdi(samples$beta2[,,],credMass = 0.95)[2],3) #upper
beta2site[5]<-round(hdi(samples$beta2[,,],credMass = 0.95)[1],3) # lower
beta2site[6]<-round((length(which(samples$beta2[,,]>0)))/length(samples$beta2[i,,]),3)
beta2site[7]<-round((length(which(samples$beta2[,,]<0)))/length(samples$beta2[i,,]),3)

beta3site<-matrix(NA,nrow = Nsite,ncol = 7)
beta3site[1]<-paste(species[j])
beta3site[2]<-paste(sites)
beta3site[3]<-round(median(samples$beta3[,,]),3)
beta3site[4]<-round(hdi(samples$beta3[,,],credMass = 0.95)[2],3) #upper
beta3site[5]<-round(hdi(samples$beta3[,,],credMass = 0.95)[1],3) # lower
beta3site[6]<-round((length(which(samples$beta3[,,]>0)))/length(samples$beta3[i,,]),3)
beta3site[7]<-round((length(which(samples$beta3[,,]<0)))/length(samples$beta3[i,,]),3)

beta4site<-matrix(NA,nrow = Nsite,ncol = 7)
beta4site[1]<-paste(species[j])
beta4site[2]<-paste(sites)
beta4site[3]<-round(median(samples$beta4[,,]),3)
beta4site[4]<-round(hdi(samples$beta4[,,],credMass = 0.95)[2],3) #upper
beta4site[5]<-round(hdi(samples$beta4[,,],credMass = 0.95)[1],3) # lower
beta4site[6]<-round((length(which(samples$beta4[,,]>0)))/length(samples$beta4[i,,]),3)
beta4site[7]<-round((length(which(samples$beta4[,,]<0)))/length(samples$beta4[i,,]),3)

# site-level intercepts
interzsite<-matrix(NA,nrow = Nsite,ncol = 7)
interzsite[1]<-paste(species[j])
interzsite[2]<-paste(sites)
interzsite[3]<-round(median(samples$int[,,]),3)
interzsite[4]<-round(hdi(samples$int[,,],credMass = 0.95)[2],3) #upper
interzsite[5]<-round(hdi(samples$int[,,],credMass = 0.95)[1],3) # lower
interzsite[6]<-round((length(which(samples$int[,,]>0)))/length(samples$int[i,,]),3)
interzsite[7]<-round((length(which(samples$int[,,]<0)))/length(samples$int[i,,]),3)

########## PPC
PPCynew<-data.frame(NA,nrow = Nsite,ncol = 3)
for(i in 1:length(datB$growth)){
  PPCynew[i,]<-quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
}
PPC<-cbind(datB$growth,PPCynew)
colnames(PPC)<-c("growth","ynew","0.025","0.975")
write.csv(PPC,file = paste(species[j],"PPC.csv"))

# collecting results
# species level
yearSpeciesMu<-rbind(yearSpeciesMu,beta1sp)
effortSpeciesMu<-rbind(effortSpeciesMu,beta2sp)
dateSpeciesMu<-rbind(dateSpeciesMu,beta3sp)
previousSpeciesMu<-rbind(previousSpeciesMu,beta4sp)
# yearSpeciesTau<-rbind(yearSpeciesTau,yearSpTau)
# effortSpeciesTau<-rbind(effortSpeciesTau,effortSpTau)
# dateSpeciesTau<-rbind(dateSpeciesTau,dateSpTau)
# previousSpeciesTau<-rbind(previousSpeciesTau,previousSpTau)
intmuSpeciesMU<-rbind(intmuSpeciesMU,interzsp)
yearSpeciesMu80<-rbind(yearSpeciesMu80,beta180sp)

# site level
yearSites<-rbind(yearSites,beta1site)
effortSites<-rbind(effortSites,beta2site)
dateSites<-rbind(dateSites,beta3site)
previousSites<-rbind(previousSites,beta4site)
interzSites<-rbind(interzSites,interzsite)

listPPC[[j]]<-PPC
}
############### begin loop for species with >1 site
else{
  basicData<-list(growth=datB$growth, site=datB$site,year=datB$stdYear,effort=datB$stdHours,
                  date=datB$stdDate,previous=datB$stdPrevious,N=length(datB$stdYear),Nsite=Nsite)
  
  model<-"model{
  # poisson likelihood for counts/abundance, ln is link
  for(i in 1:N){
  growth[i] ~ dnorm(mu[i],tau)
  # link is identity
  mu[i] <- int[site[i]] + beta1[site[i]] * year[i] + beta2[site[i]] * effort[i] + beta3[site[i]] * date[i] + beta4[site[i]] * previous[i]
  
  ## posterior predictive check stuff
  #### generate ynew for posterior pedictive check
  ynew[i] ~ dnorm(mu[i],tau)
  ##### examine residuals
  resid[i]<- growth[i] - ynew[i] 
  }
  
  tau ~ dgamma(0.1,0.1) # prior on precision
  
  # random effect (hierarchical) coefficients  sites - t1 = site mean and precision (sites nested within the species)
  for(j in 1:Nsite){
  beta1[j] ~ dnorm(s1mu, s1tau)
  beta2[j] ~ dnorm(s2mu, s2tau)
  beta3[j] ~ dnorm(s3mu, s3tau)
  beta4[j] ~ dnorm(s4mu, s4tau)   # or make the intercepts non-hierarchical (who cares?)
  int[j] ~ dnorm(intmu,inttau)  
  }
  
  # random effect (hierarchical) coefficients for the species (conditional priors) 	
  s1mu ~ dnorm(0, 0.0001)
  s1tau ~ dgamma(0.1,0.1)
  s2mu ~ dnorm(0, 0.0001)
  s2tau ~ dgamma(0.1,0.1)
  s3mu ~ dnorm(0, 0.0001)
  s3tau ~ dgamma(0.1,0.1)
  s4mu ~ dnorm(0, 0.0001)
  s4tau ~ dgamma(0.1,0.1)
  intmu ~ dnorm(0,0.001)
  inttau ~ dgamma(0.1,0.1)    
}
"


basicModel<-jags.model(textConnection(model),data=basicData,n.chains=2)  #compile


samples<-jags.samples(model=basicModel,
                      variable.names=c("beta1","beta2","beta3","beta4","s1mu","s1tau","s2mu","s2tau",
                                       "s3mu","s3tau","s4mu","s4tau","mu","tau","intmu","inttau","ynew","resid"),
                      n.adapt=1000,n.iter=15000)
# str(samples)


##################################
#######Diagnostics and Results ###
####################################

# site level - diagnostics then parameter estimates

beta1ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta1ESS[i,1]<-effectiveSize(samples$beta1[i,,1]) 
  beta1ESS[i,2]<-effectiveSize(samples$beta1[i,,2]) 
}
colnames(beta1ESS)<-c("chain1ESS","chain2ESS")
write.csv(summary(beta1ESS),file = paste(species[j],"beta1ESSsummary.csv"),row.names = F,quote = F)

###############
beta1GR<-numeric()
for (i in 1:Nsite){
  beta1GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta1[i,,1]),as.mcmc(samples$beta1[i,,2])),autoburnin = F)[[1]][1]
}
write.csv(range(beta1GR),file = paste(species[j],"beta1GRrange.csv"),row.names = F,quote = F)

###############
beta2ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta2ESS[i,1]<-effectiveSize(samples$beta2[i,,1]) 
  beta2ESS[i,2]<-effectiveSize(samples$beta2[i,,2]) 
}
colnames(beta2ESS)<-c("chain1ESS","chain2ESS")
write.csv(summary(beta2ESS),file = paste(species[j],"beta2ESSsummary.csv"),row.names = F,quote = F)

###############
beta2GR<-numeric()
for (i in 1:Nsite){
  beta2GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta2[i,,1]),as.mcmc(samples$beta2[i,,2])))[[1]][1]
}
write.csv(range(beta2GR),file = paste(species[j],"beta2GRrange.csv"),row.names = F,quote = F)

###############
beta3ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta3ESS[i,1]<-effectiveSize(samples$beta3[i,,1]) 
  beta3ESS[i,2]<-effectiveSize(samples$beta3[i,,2]) 
}
colnames(beta3ESS)<-c("chain1ESS","chain2ESS")
write.csv(summary(beta3ESS),file = paste(species[j],"beta3ESSsummary.csv"),row.names = F,quote = F)

####################
beta3GR<-numeric()
for (i in 1:Nsite){
  beta3GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta3[i,,1]),as.mcmc(samples$beta3[i,,2])))[[1]][1]
}
write.csv(range(beta3GR),file = paste(species[j],"beta3GRrange.csv"),row.names = F,quote = F)

###############
beta4ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta4ESS[i,1]<-effectiveSize(samples$beta4[i,,1]) 
  beta3ESS[i,2]<-effectiveSize(samples$beta4[i,,2]) 
}
colnames(beta4ESS)<-c("chain1ESS","chain2ESS")
write.csv(summary(beta4ESS),file = paste(species[j],"beta4ESSsummary.csv"),row.names = F,quote = F)

####################
beta4GR<-numeric()
for (i in 1:Nsite){
  beta4GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta4[i,,1]),as.mcmc(samples$beta4[i,,2])))[[1]][1]
}
write.csv(range(beta4GR),file = paste(species[j],"beta4GRrange.csv"),row.names = F,quote = F)

##############  site level parameter estimates
beta1<-matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta1[i,1]<-paste(species[j])
  beta1[i,2]<-paste(sites[i])
  beta1[i,3]<-round(median(samples$beta1[i,,]),3)
  beta1[i,4]<-round(hdi(samples$beta1[i,,],credMass = 0.95)[2],3) #upper
  beta1[i,5]<-round(hdi(samples$beta1[i,,],credMass = 0.95)[1],3) # lower
  beta1[i,6]<-round((length(which(samples$beta1[i,,]>0)))/length(samples$beta1[i,,]),3)
  beta1[i,7]<-round((length(which(samples$beta1[i,,]<0)))/length(samples$beta1[i,,]),3)
  # beta1[i,]<-round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}

beta2<-matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta2[i,1]<-paste(species[j])
  beta2[i,2]<-paste(sites[i])
  beta2[i,3]<-round(median(samples$beta2[i,,]),3)
  beta2[i,4]<-round(hdi(samples$beta2[i,,],credMass = 0.95)[2],3) #upper
  beta2[i,5]<-round(hdi(samples$beta2[i,,],credMass = 0.95)[1],3) # lower
  beta2[i,6]<-round((length(which(samples$beta2[i,,]>0)))/length(samples$beta2[i,,]),3)
  beta2[i,7]<-round((length(which(samples$beta2[i,,]<0)))/length(samples$beta2[i,,]),3)
  # beta1[i,]<-round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}


beta3<-matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta3[i,1]<-paste(species[j])
  beta3[i,2]<-paste(sites[i])
  beta3[i,3]<-round(median(samples$beta3[i,,]),3)
  beta3[i,4]<-round(hdi(samples$beta3[i,,],credMass = 0.95)[2],3) #upper
  beta3[i,5]<-round(hdi(samples$beta3[i,,],credMass = 0.95)[1],3) # lower
  beta3[i,6]<-round((length(which(samples$beta3[i,,]>0)))/length(samples$beta3[i,,]),3)
  beta3[i,7]<-round((length(which(samples$beta3[i,,]<0)))/length(samples$beta3[i,,]),3)
  # beta1[i,]<-round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}

beta4<-matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta4[i,1]<-paste(species[j])
  beta4[i,2]<-paste(sites[i])
  beta4[i,3]<-round(median(samples$beta4[i,,]),3)
  beta4[i,4]<-round(hdi(samples$beta4[i,,],credMass = 0.95)[2],3) #upper
  beta4[i,5]<-round(hdi(samples$beta4[i,,],credMass = 0.95)[1],3) # lower
  beta4[i,6]<-round((length(which(samples$beta4[i,,]>0)))/length(samples$beta4[i,,]),3)
  beta4[i,7]<-round((length(which(samples$beta4[i,,]<0)))/length(samples$beta4[i,,]),3)
  # beta1[i,]<-round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}

# species level
# diagnostics first
#  ESS and GR 
s1muESS<-sum(effectiveSize(samples$s1mu[,,]))
s2muESS<-sum(effectiveSize(samples$s2mu[,,]))
s3muESS<-sum(effectiveSize(samples$s3mu[,,]))
s4muESS<-sum(effectiveSize(samples$s4mu[,,]))
s1tauESS<-sum(effectiveSize(samples$s1tau[,,]))
s2tauESS<-sum(effectiveSize(samples$s2tau[,,]))
s3tauESS<-sum(effectiveSize(samples$s3tau[,,]))
s4tauESS<-sum(effectiveSize(samples$s4tau[,,]))

s1muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s1mu[,,1]),as.mcmc(samples$s1mu[,,2])))[[1]][1],3)
s2muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s2mu[,,1]),as.mcmc(samples$s2mu[,,2])))[[1]][1],3)
s3muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s3mu[,,1]),as.mcmc(samples$s3mu[,,2])))[[1]][1],3)
s4muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s4mu[,,1]),as.mcmc(samples$s4mu[,,2])))[[1]][1],3)
s1tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s1tau[,,1]),as.mcmc(samples$s1tau[,,2])))[[1]][1],3)
s2tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s2tau[,,1]),as.mcmc(samples$s2tau[,,2])))[[1]][1],3)
s3tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s3tau[,,1]),as.mcmc(samples$s3tau[,,2])))[[1]][1],3)
s4tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s4tau[,,1]),as.mcmc(samples$s4tau[,,2])))[[1]][1],3)

year_diag<-c(s1muESS,s1muGR,s1tauESS,s1tauGR)
effort_diag<-c(s2muESS,s2muGR,s2tauESS,s2tauGR)
date_diag<-c(s3muESS,s3muGR,s3tauESS,s3tauGR)
previous_diag<-c(s4muESS,s4muGR,s4tauESS,s4tauGR)
spDiag<-c(year_diag,effort_diag,date_diag,previous_diag)

# parameter estimates:
# species level
# year
yearUp<-round(hdi(samples$s1mu,credMass = 0.95)[1],3) #lower
yearLow<-round(hdi(samples$s1mu,credMass = 0.95)[2],3) #upper
yearMedian<-round(median(samples$s1mu[,,]),3)
yearProbPos<-round((length(which(samples$s1mu[,,]>0)))/length(samples$s1mu[,,]),3)
yearProbNeg<-round((length(which(samples$s1mu[,,]<0)))/length(samples$s1mu[,,]),3)
yearSp<-c(yearMedian,yearUp,yearLow,yearProbPos,yearProbNeg)

# effort
effortUp<-round(hdi(samples$s2mu,credMass = 0.95)[1],3) #lower
effortLow<-round(hdi(samples$s2mu,credMass = 0.95)[2],3) #upper
effortMedian<-round(median(samples$s2mu[,,]),3)
effortProbPos<-round((length(which(samples$s2mu[,,]>0)))/length(samples$s2mu[,,]),3)
effortProbNeg<-round((length(which(samples$s2mu[,,]<0)))/length(samples$s2mu[,,]),3)
effortSp<-c(effortMedian,effortUp,effortLow,effortProbPos,effortProbNeg)

# date
dateUp<-round(hdi(samples$s3mu,credMass = 0.95)[1],3) #lower
dateLow<-round(hdi(samples$s3mu,credMass = 0.95)[2],3) #upper
dateMedian<-round(median(samples$s3mu[,,]),3)
dateProbPos<-round((length(which(samples$s3mu[,,]>0)))/length(samples$s3mu[,,]),3)
dateProbNeg<-round((length(which(samples$s3mu[,,]<0)))/length(samples$s3mu[,,]),3)
dateSp<-c(dateMedian,dateUp,dateLow,dateProbPos,dateProbNeg)

# previous
previousUp<-round(hdi(samples$s4mu,credMass = 0.95)[1],3) #lower
previousLow<-round(hdi(samples$s4mu,credMass = 0.95)[2],3) #upper
previousMedian<-round(median(samples$s4mu[,,]),3)
previousProbPos<-round((length(which(samples$s4mu[,,]>0)))/length(samples$s4mu[,,]),3)
previousProbNeg<-round((length(which(samples$s4mu[,,]<0)))/length(samples$s4mu[,,]),3)
previousSp<-c(previousMedian,previousUp,previousLow,previousProbPos,previousProbNeg)

# next: capture precision
yearUpTau<-round(hdi(samples$s1tau,credMass = 0.95)[1],3) #lower
yearLowTau<-round(hdi(samples$s1tau,credMass = 0.95)[2],3) #upper
yearMedianTau<-round(quantile(samples$s1tau[,,],probs=c(0.5)),3)
yearProbPosTau<-round((length(which(samples$s1tau[,,]>0)))/length(samples$s1tau[,,]),3)
yearProbNegTau<-round((length(which(samples$s1tau[,,]<0)))/length(samples$s1tau[,,]),3)
yearSpTau<-c(yearMedianTau,yearUpTau,yearLowTau,yearProbPosTau,yearProbNegTau)

# effort
effortUpTau<-round(hdi(samples$s2tau,credMass = 0.95)[1],3) #lower
effortLowTau<-round(hdi(samples$s2tau,credMass = 0.95)[2],3) #upper
effortMedianTau<-round(quantile(samples$s2tau[,,],probs=c(0.5)),3)
effortProbPosTau<-round((length(which(samples$s2tau[,,]>0)))/length(samples$s2tau[,,]),3)
effortProbNegTau<-round((length(which(samples$s2tau[,,]<0)))/length(samples$s2tau[,,]),3)
effortSpTau<-c(effortMedianTau,effortUpTau,effortLowTau,effortProbPosTau,effortProbNegTau)

# date
dateUpTau<-round(hdi(samples$s3tau,credMass = 0.95)[1],3) #lower
dateLowTau<-round(hdi(samples$s3tau,credMass = 0.95)[2],3) #upper
dateMedianTau<-round(quantile(samples$s3tau[,,],probs=c(0.5)),3)
dateProbPosTau<-round((length(which(samples$s3tau[,,]>0)))/length(samples$s3tau[,,]),3)
dateProbNegTau<-round((length(which(samples$s3tau[,,]<0)))/length(samples$s3tau[,,]),3)
dateSpTau<-c(dateMedianTau,dateUpTau,dateLowTau,dateProbPosTau,dateProbNegTau)

# previous
previousUpTau<-round(hdi(samples$s4tau,credMass = 0.95)[1],3) #lower
previousLowTau<-round(hdi(samples$s4tau,credMass = 0.95)[2],3) #upper
previousMedianTau<-round(quantile(samples$s4tau[,,],probs=c(0.5)),3)
previousProbPosTau<-round((length(which(samples$s4tau[,,]>0)))/length(samples$s4tau[,,]),3)
previousProbNegTau<-round((length(which(samples$s4tau[,,]<0)))/length(samples$s4tau[,,]),3)
previousSpTau<-c(previousMedianTau,previousUpTau,previousLowTau,previousProbPosTau,previousProbNegTau)




########## PPC
PPCynew<-data.frame(NA,nrow = Nsite,ncol = 3)
for(i in 1:length(datB$growth)){
  PPCynew[i,]<-quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
}
PPC<-cbind(datB$growth,PPCynew)
colnames(PPC)<-c("growth","ynew","0.025","0.975")
write.csv(PPC,file = paste(species[j],"PPC.csv"))

# collecting results
# species level
yearSpeciesMu<-rbind(yearSpeciesMu,yearSp)
effortSpeciesMu<-rbind(effortSpeciesMu,effortSp)
dateSpeciesMu<-rbind(dateSpeciesMu,dateSp)
previousSpeciesMu<-rbind(previousSpeciesMu,previousSp)
yearSpeciesTau<-rbind(yearSpeciesTau,yearSpTau)
effortSpeciesTau<-rbind(effortSpeciesTau,effortSpTau)
dateSpeciesTau<-rbind(dateSpeciesTau,dateSpTau)
previousSpeciesTau<-rbind(previousSpeciesTau,previousSpTau)

# species level diagnostics
speciesDiag<-rbind(speciesDiag,spDiag)


# HERE
# site level
yearSites<-rbind(yearSites,beta1)
effortSites<-rbind(effortSites,beta2)
dateSites<-rbind(dateSites,beta3)
previousSites<-rbind(previousSites,beta4)

listPPC[[j]]<-PPC

}
}
# species level
colnames(yearSpeciesMu)<-c("median","low","up","probPos","probNeg")
colnames(effortSpeciesMu)<-c("median","low","up","probPos","probNeg")
colnames(dateSpeciesMu)<-c("median","low","up","probPos","probNeg")
colnames(previousSpeciesMu)<-c("median","low","up","probPos","probNeg")
colnames(yearSpeciesTau)<-c("median","low","up","probPos","probNeg")
colnames(effortSpeciesTau)<-c("median","low","up","probPos","probNeg")
colnames(dateSpeciesTau)<-c("median","low","up","probPos","probNeg")
colnames(previousSpeciesTau)<-c("median","low","up","probPos","probNeg")
colnames(speciesDiag)<-c("yearmuESS","yearmuGR","yeartauESS","yeartauGR",
                         "effortmuESS","effortmuGR","efforttauESS","efforttauGR",
                         "datemuESS","datemuGR","datetauESS","datetauGR","previousmuESS","previousmuGR","previoustauESS","previoustauGR")

yearSpeciesMu<-cbind(species,yearSpeciesMu)
effortSpeciesMu<-cbind(species,effortSpeciesMu)
dateSpeciesMu<-cbind(species,dateSpeciesMu)
previousSpeciesMu<-cbind(species,previousSpeciesMu)
yearSpeciesTau<-cbind(species,yearSpeciesTau)
effortSpeciesTau<-cbind(species,effortSpeciesTau)
dateSpeciesTau<-cbind(species,dateSpeciesTau)
previousSpeciesTau<-cbind(species,previousSpeciesTau)
speciesDiag<-cbind(species,speciesDiag)

write.csv(yearSpeciesMu,"yearSpMu_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(effortSpeciesMu,"effortSpMu_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(dateSpeciesMu,"dateSpMu_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(previousSpeciesMu,"previousSpMu_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(yearSpeciesTau,"yearSpTau_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(effortSpeciesTau,"effortSpTau_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(dateSpeciesTau,"dateSpTau_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(previousSpeciesTau,"previousSpTau_MIN10_zYear.csv",row.names = F,quote = F)
write.csv(speciesDiag,"speciesDiag_MIN10_zYear.csv",row.names = F,quote = F)

# site level
colnames(yearSites)<-c("species","site","median","up","low","probPos","probNeg")
colnames(effortSites)<-c("species","site","median","up","low","probPos","probNeg")
colnames(dateSites)<-c("species","site","median","up","low","probPos","probNeg")
colnames(previousSites)<-c("species","site","median","up","low","probPos","probNeg")

write.csv(yearSites,"yearSites_MIN10_GrowthRate_zYear.csv",row.names = F,quote = F)
write.csv(effortSites,"effortSites_MIN10_GrowthRate_zYear.csv",row.names = F,quote = F)
write.csv(dateSites,"dateSites_MIN10_GrowthRate_zYear.csv",row.names = F,quote = F)
write.csv(previousSites,"previousSites_MIN10_GrowthRate_zYear.csv",row.names = F,quote = F)
save.image("sitesMIN10_GrowthRate_zYear.RData")
