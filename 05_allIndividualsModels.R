

## This is the code that takes the NABA data, pools species and does the "all individuals" Poisson model.
## Options in here to run with different subsets of individuals.
##
## Original file was called:
## 4thCountsScript_totalInds_GLOBAL_constructionV2_var10.R


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
sites<-droplevels(unique(dat$site)) #114

# add filtering here
# filter 1: drop any sites per species with < 10 years of observations/counts
dat<-dat[which(dat$yearsPerSitePerSp>9),]
dim(dat) # 56956    12
length(unique(dat$sp)) #272 spp.
species<-unique(dat$sp)
sites<-droplevels(unique(dat$site))
length(sites) # 72
head(dat)

## count length of unique site-year combinations
temp84 <- dat
temp84$siteYear <- paste(temp84$site,temp84$year, sep="_")
head(temp84)
length(unique(temp84$siteYear))


# filter 10 species with the highest variance in counts - remove top 10
var<-read.csv("varianceSpecies.csv")
var[order(-var$var),]                                                
mostVariable<-c(169,262,144,211,237,193,88,141,39,245)
length(mostVariable) # 10

newSpecies<-species[-mostVariable]
length(newSpecies) # 262
newDat<-data.frame()
for (l in 1:length(newSpecies)){
  tmp<-dat[which(dat$sp == newSpecies[l]),]
  newDat<-rbind(newDat,tmp)
}
dim(newDat) # 54618    12
length(unique(newDat$sp)) #262 spp.
species<-unique(newDat$sp)
sites<-unique(newDat$site)
length(sites) #72 
dat<-newDat # pass it back for rest of the script

yearsZ<-(dat$year - mean(dat$year))  # 2001.659 (slight diff)
dat<-cbind(dat,yearsZ)
head(dat)
dim(dat)



########################
########################
## pick a subset of species (or all species) based on abundance.
########################
########################


## generate a count by species (across all years and sites)
tally <- aggregate(dat$count ~ dat$sp, FUN="sum")
colnames(tally) <- c("sp","tot")
head(tally)
tally$log <- log(tally$tot)
tally <- tally[order(tally$tot,decreasing=T),]
head(tally)
tally$order <- 1:dim(tally)[1]

## Have a look at rank abundance plots just for fun.
bigTot <- sum(tally$tot)
tally$per <- tally$tot / bigTot
plot(tally$per ~ tally$order)
plot(tally$log ~ tally$order)
dim(tally)


#########
#########
## pick which group (only execute one of the following three lines)
#########
#########
pick <- droplevels(tally[1:262,1]);length(pick)  #all
#pick <- droplevels(tally[1:50,1]);length(pick)   #common
#pick <- droplevels(tally[51:262,1]); length(pick)  #others


temp <- droplevels(dat[dat$sp %in% pick,])                
dim(temp)
head(temp)
length(levels(temp$sp))
dat <- temp

head(dat)
length(levels(dat$site))


## gotta reset these vectors
species<-unique(dat$sp)
sites<-unique(dat$site)


########################
########################
########################



### prep for bayes model


# set up results collection:
global_yearMu<-data.frame()
global_effortMu<-data.frame()
global_dateMu<-data.frame()
global_yearTau<-data.frame()
global_effortTau<-data.frame()
global_dateTau<-data.frame()
globalDiagstics<-data.frame()

yearSpeciesMu<-data.frame()
effortSpeciesMu<-data.frame()
dateSpeciesMu<-data.frame()
yearSpeciesTau<-data.frame()
effortSpeciesTau<-data.frame()
dateSpeciesTau<-data.frame()
speciesNames<-data.frame()
listPPC<-list()
speciesDiag<-data.frame()

# site-level results
yearSites<-data.frame()
effortSites<-data.frame()
dateSites<-data.frame()


######
###### QUESTION: what is the scaling of each species that's happening below?
######


# LOOP to build full data for Global model: loop through and scale each sp then rbind it all back together
site_dat<-data.frame()
datB<-data.frame()
for (j in 1:length(sites)){
  site_dat<-droplevels(dat[which(dat$site==sites[j]),])
  years<-unique(site_dat$year)
  workingDat<-data.frame()
  siteYearCount<-matrix(NA,ncol = 5)
  for (i in 1:length(years)){
    tmp<-droplevels(site_dat[which(site_dat$year==years[i]),])
    siteYearCount<-c(years[i],sum(tmp$count),unique(tmp$hours),unique(tmp$date),unique(tmp$yearsZ))
    workingDat<-rbind(workingDat,siteYearCount)
    names(workingDat)<-c("year","count","effort","date","stdYear")  # "stdYEar = centered across all
  }
  workingDat<-cbind(workingDat,scale(workingDat$effort),scale(workingDat$date))
  siteName<-rep(paste(sites[j]),length(years))
  workingDat<-cbind(siteName,workingDat)
  # colnames(workingDat)<-c("site","year","count","effort","date")
  # workingDat<-cbind(workingDat,scale(as.numeric(workingDat$year)),scale(as.numeric(workingDat$effort)),scale(as.numeric(workingDat$date)))
  names(workingDat)<-c("site","year","count","effort","date","stdYear","stdHours","stdDate")
  datB<-rbind(datB,workingDat) 
}
dim(datB) #  1481    8  sites are in the order of the original data
head(datB)
length(levels(datB$site))





###################
###### CHECKING (abundance sums)
head(dat)
crap <- aggregate(dat$count ~ dat$site + dat$year, FUN = "sum")
head(crap[order(crap[,1]),])
## compare
head(datB)
## all good
###################





Nsite<-length(unique(datB$site)) # 72

basicData<-list(counts=datB$count, site=datB$site, year=datB$stdYear,effort=datB$stdHours,
                date=datB$stdDate,N=length(datB$stdYear),Nsite=Nsite)

model<-"model{
	# poisson likelihood for counts/abundance, ln is link
	for(i in 1:N){
		counts[i] ~ dpois(lambda[i])
		
		# link is ln for a poisson model
		lambda[i] <- exp(int[site[i]]  + beta1[site[i]] * year[i] + beta2[site[i]] * effort[i] + beta3[site[i]] * date[i])

	## posterior predictive check
	#### generate ynew for posterior pedictive check
	ynew[i] ~ dpois(lambda[i])

	##### examine residuals
	resid[i]<- counts[i] - lambda[i] 
	}

	# random effect (hierarchical) coefficients for sites (sites nested within the total)
	for(j in 1:Nsite){	
		beta1[j] ~ dnorm(s1mu, s1tau)
		beta2[j] ~ dnorm(s2mu, s2tau)
		beta3[j] ~ dnorm(s3mu, s3tau)
		int[j] ~ dnorm(0, 0.0001)  # sites have independent intercepts, not hierarchical
	}

	# hyperpriors on mean and precision for the global-level
	s1mu ~ dnorm(0, 0.0001)
	s1tau  ~ dgamma(0.1,0.1)
	s2mu ~ dnorm(0, 0.0001)
	s2tau  ~ dgamma(0.1,0.1)
	s3mu ~ dnorm(0, 0.0001)
	s3tau  ~ dgamma(0.1,0.1)
}
"


basicModel<-jags.model(textConnection(model),data=basicData,n.chains=2)  #compile


samples<-jags.samples(model=basicModel,
                      variable.names=c("beta1","beta2","beta3","lambda","int","ynew","resid",
                                       "s1mu","s1tau","s2mu","s2tau","s3mu","s3tau"),
                      n.adapt=1000,n.iter=15000)
# str(samples)






##################################
#######Diagnostics and Results ###
####################################

# site level - diagnostics then parameter estimates
# HERE: get median HDIs probPos ProbNeg 

beta1ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta1ESS[i,1]<-effectiveSize(samples$beta1[i,,1]) 
  beta1ESS[i,2]<-effectiveSize(samples$beta1[i,,2]) 
}
colnames(beta1ESS)<-c("chain1ESS","chain2ESS")
head(beta1ESS)
#write.csv(summary(beta1ESS),file = "beta1ESSsummary.csv",row.names = F,quote = F)



###############
beta1GR<-numeric()
for (i in 1:Nsite){
  beta1GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta1[i,,1]),as.mcmc(samples$beta1[i,,2])),autoburnin = F)[[1]][1]
}
head(beta1GR)
#write.csv(range(beta1GR),file = "beta1GRrange.csv",row.names = F,quote = F)



###############
beta2ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta2ESS[i,1]<-effectiveSize(samples$beta2[i,,1]) 
  beta2ESS[i,2]<-effectiveSize(samples$beta2[i,,2]) 
}
colnames(beta2ESS)<-c("chain1ESS","chain2ESS")
head(beta2ESS)
#write.csv(summary(beta2ESS),file = "beta2ESSsummary.csv",row.names = F,quote = F)


###############
beta2GR<-numeric()
for (i in 1:Nsite){
  beta2GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta2[i,,1]),as.mcmc(samples$beta2[i,,2])))[[1]][1]
}
head(beta2GR)
#write.csv(range(beta2GR),file = "beta2GRrange.csv",row.names = F,quote = F)



###############
beta3ESS<-matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains
for (i in 1:Nsite){
  beta3ESS[i,1]<-effectiveSize(samples$beta3[i,,1]) 
  beta3ESS[i,2]<-effectiveSize(samples$beta3[i,,2]) 
}
colnames(beta3ESS)<-c("chain1ESS","chain2ESS")
head(beta3ESS)
#write.csv(summary(beta3ESS),file = "beta3ESSsummary.csv",row.names = F,quote = F)



####################
beta3GR<-numeric()
for (i in 1:Nsite){
  beta3GR[i]<-gelman.diag(mcmc.list(as.mcmc(samples$beta3[i,,1]),as.mcmc(samples$beta3[i,,2])))[[1]][1]
}
#write.csv(range(beta3GR),file = "beta3GRrange.csv",row.names = F,quote = F)



##############  site level parameter estimates
beta1<-matrix(NA,nrow = Nsite,ncol = 6)  # site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta1[i,1]<-paste(sites[i])
  beta1[i,2]<-round(median(samples$beta1[i,,]),3)
  beta1[i,3]<-round(hdi(samples$beta1[i,,],credMass = 0.95)[2],3) #upper
  beta1[i,4]<-round(hdi(samples$beta1[i,,],credMass = 0.95)[1],3) # lower
  beta1[i,5]<-round((length(which(samples$beta1[i,,]>0)))/length(samples$beta1[i,,]),3)
  beta1[i,6]<-round((length(which(samples$beta1[i,,]<0)))/length(samples$beta1[i,,]),3)
}

beta2<-matrix(NA,nrow = Nsite,ncol = 6)  #  site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta2[i,1]<-paste(sites[i])
  beta2[i,2]<-round(median(samples$beta2[i,,]),3)
  beta2[i,3]<-round(hdi(samples$beta2[i,,],credMass = 0.95)[2],3) #upper
  beta2[i,4]<-round(hdi(samples$beta2[i,,],credMass = 0.95)[1],3) # lower
  beta2[i,5]<-round((length(which(samples$beta2[i,,]>0)))/length(samples$beta2[i,,]),3)
  beta2[i,6]<-round((length(which(samples$beta2[i,,]<0)))/length(samples$beta2[i,,]),3)
}


beta3<-matrix(NA,nrow = Nsite,ncol = 6)  #  site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta3[i,1]<-paste(sites[i])
  beta3[i,2]<-round(median(samples$beta3[i,,]),3)
  beta3[i,3]<-round(hdi(samples$beta3[i,,],credMass = 0.95)[2],3) #upper
  beta3[i,4]<-round(hdi(samples$beta3[i,,],credMass = 0.95)[1],3) # lower
  beta3[i,5]<-round((length(which(samples$beta3[i,,]>0)))/length(samples$beta3[i,,]),3)
  beta3[i,6]<-round((length(which(samples$beta3[i,,]<0)))/length(samples$beta3[i,,]),3)
}

# global level
# diagnostics first
#  ESS and GR 
s1muESS<-sum(effectiveSize(samples$s1mu[,,]))
s2muESS<-sum(effectiveSize(samples$s2mu[,,]))
s3muESS<-sum(effectiveSize(samples$s3mu[,,]))
s1tauESS<-sum(effectiveSize(samples$s1tau[,,]))
s2tauESS<-sum(effectiveSize(samples$s2tau[,,]))
s3tauESS<-sum(effectiveSize(samples$s3tau[,,]))

s1muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s1mu[,,1]),as.mcmc(samples$s1mu[,,2])))[[1]][1],3)
s2muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s2mu[,,1]),as.mcmc(samples$s2mu[,,2])))[[1]][1],3)
s3muGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s3mu[,,1]),as.mcmc(samples$s3mu[,,2])))[[1]][1],3)
s1tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s1tau[,,1]),as.mcmc(samples$s1tau[,,2])))[[1]][1],3)
s2tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s2tau[,,1]),as.mcmc(samples$s2tau[,,2])))[[1]][1],3)
s3tauGR<-round(gelman.diag(mcmc.list(as.mcmc(samples$s3tau[,,1]),as.mcmc(samples$s3tau[,,2])))[[1]][1],3)

year_diag<-c(s1muESS,s1muGR,s1tauESS,s1tauGR)
effort_diag<-c(s2muESS,s2muGR,s2tauESS,s2tauGR)
date_diag<-c(s3muESS,s3muGR,s3tauESS,s3tauGR)
global_Diag<-c(year_diag,effort_diag,date_diag)



# parameter estimates:
# global level
# year
yearUp<-round(hdi(samples$s1mu,credMass = 0.95)[1],3) #lower
yearLow<-round(hdi(samples$s1mu,credMass = 0.95)[2],3) #upper
yearMedian<-round(median(samples$s1mu[,,]),3)
yearProbPos<-round((length(which(samples$s1mu[,,]>0)))/length(samples$s1mu[,,]),3)
yearProbNeg<-round((length(which(samples$s1mu[,,]<0)))/length(samples$s1mu[,,]),3)
global_yearMu<-c(yearMedian,yearUp,yearLow,yearProbPos,yearProbNeg)
#-0.018 -0.040  0.006  0.067  0.933

# effort
effortUp<-round(hdi(samples$s2mu,credMass = 0.95)[1],3) #lower
effortLow<-round(hdi(samples$s2mu,credMass = 0.95)[2],3) #upper
effortMedian<-round(median(samples$s2mu[,,]),3)
effortProbPos<-round((length(which(samples$s2mu[,,]>0)))/length(samples$s2mu[,,]),3)
effortProbNeg<-round((length(which(samples$s2mu[,,]<0)))/length(samples$s2mu[,,]),3)
global_effortMu<-c(effortMedian,effortUp,effortLow,effortProbPos,effortProbNeg)

# date
dateUp<-round(hdi(samples$s3mu,credMass = 0.95)[1],3) #lower
dateLow<-round(hdi(samples$s3mu,credMass = 0.95)[2],3) #upper
dateMedian<-round(median(samples$s3mu[,,]),3)
dateProbPos<-round((length(which(samples$s3mu[,,]>0)))/length(samples$s3mu[,,]),3)
dateProbNeg<-round((length(which(samples$s3mu[,,]<0)))/length(samples$s3mu[,,]),3)
global_dateMu<-c(dateMedian,dateUp,dateLow,dateProbPos,dateProbNeg)

# next: capture precision
yearUpTau<-round(hdi(samples$s1tau,credMass = 0.95)[1],3) #lower
yearLowTau<-round(hdi(samples$s1tau,credMass = 0.95)[2],3) #upper
yearMedianTau<-round(quantile(samples$s1tau[,,],probs=c(0.5)),3)
yearProbPosTau<-round((length(which(samples$s1tau[,,]>0)))/length(samples$s1tau[,,]),3)
yearProbNegTau<-round((length(which(samples$s1tau[,,]<0)))/length(samples$s1tau[,,]),3)
global_yearTau<-c(yearMedianTau,yearUpTau,yearLowTau,yearProbPosTau,yearProbNegTau)

# effort
effortUpTau<-round(hdi(samples$s2tau,credMass = 0.95)[1],3) #lower
effortLowTau<-round(hdi(samples$s2tau,credMass = 0.95)[2],3) #upper
effortMedianTau<-round(quantile(samples$s2tau[,,],probs=c(0.5)),3)
effortProbPosTau<-round((length(which(samples$s2tau[,,]>0)))/length(samples$s2tau[,,]),3)
effortProbNegTau<-round((length(which(samples$s2tau[,,]<0)))/length(samples$s2tau[,,]),3)
global_effortTau<-c(effortMedianTau,effortUpTau,effortLowTau,effortProbPosTau,effortProbNegTau)

# date
dateUpTau<-round(hdi(samples$s3tau,credMass = 0.95)[1],3) #lower
dateLowTau<-round(hdi(samples$s3tau,credMass = 0.95)[2],3) #upper
dateMedianTau<-round(quantile(samples$s3tau[,,],probs=c(0.5)),3)
dateProbPosTau<-round((length(which(samples$s3tau[,,]>0)))/length(samples$s3tau[,,]),3)
dateProbNegTau<-round((length(which(samples$s3tau[,,]<0)))/length(samples$s3tau[,,]),3)
global_dateTau<-c(dateMedianTau,dateUpTau,dateLowTau,dateProbPosTau,dateProbNegTau)




########## PPC
PPCynew<-data.frame(NA,nrow = Nsite,ncol = 3)
for(i in 1:length(datB$count)){
  PPCynew[i,]<-quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
}
PPC<-cbind(datB$site,datB$year,datB$count,PPCynew)
colnames(PPC)<-c("site","year","count","ynew","0.025","0.975")
head(PPC)
plot(PPC$ynew ~ PPC$count)
cor(PPC$ynew,PPC$count)
#write.csv(PPC,"PPC_var10.csv")


# site level
yearSites<-rbind(yearSites,beta1)
effortSites<-rbind(effortSites,beta2)
dateSites<-rbind(dateSites,beta3)


# listPPC[[j]]<-PPC

# global level
# colnames(global_yearMu)<-c("median","low","up","probPos","probNeg")
# colnames(global_effortMu)<-c("median","low","up","probPos","probNeg")
# colnames(global_dateMu)<-c("median","low","up","probPos","probNeg")
# colnames(global_yearTau)<-c("median","low","up","probPos","probNeg")
# colnames(global_effortTau)<-c("median","low","up","probPos","probNeg")
# colnames(global_dateTau)<-c("median","low","up","probPos","probNeg")
# colnames(globalDiagnostics)<-c("yearmuESS","yearmuGR","yeartauESS","yeartauGR",
#                          "effortmuESS","effortmuGR","efforttauESS","efforttauGR",
#                          "datemuESS","datemuGR","datetauESS","datetauGR")
# 





#################
################# CHOOSE output, one of the three blocks below 
#################  (depending on which of the groups of species were run [all, abundant, or others])
#################  note that much is commented out here, but one can uncomment to save different parts of the output
#################



## run this batch to write "all" files

#write.csv(global_yearMu,"global_yearMu_Min10_var10_all_zYear.csv",row.names = F,quote = F)
#write.csv(global_effortMu,"global_effortMu_MIN10_all_zYear.csv",row.names = F,quote = F)
#write.csv(global_dateMu,"global_dateMu_MIN10_var10_all_zYear.csv",row.names = F,quote = F)
#write.csv(global_yearTau,"global_yearTau_MIN10_var10_all_zYear.csv",row.names = F,quote = F)
#write.csv(global_effortTau,"global_effortTau_MIN10_all_var10_zYear.csv",row.names = F,quote = F)
#write.csv(global_dateTau,"global_dateTau_MIN10_var10_all_zYear.csv",row.names = F,quote = F)
# write.csv(globalDiagnostic,"globalDiagnostic_MIN10.csv",row.names = F,quote = F)

# site level
colnames(yearSites)<-c("site","median","up","low","probPos","probNeg")
#colnames(effortSites)<-c("site","median","up","low","probPos","probNeg")
#colnames(dateSites)<-c("site","median","up","low","probPos","probNeg")

write.csv(yearSites,"yearSites_MIN10_totalInds_global_var10_all_zYear.csv",row.names = F,quote = F)
#write.csv(effortSites,"effortSites_MIN10_totalInds_global_var10_all_zYear.csv",row.names = F,quote = F)
#write.csv(dateSites,"dateSites_MIN10_totalInds_global_var10_all_zYear.csv",row.names = F,quote = F)





## run this batch to write "common" files

#write.csv(global_yearMu,"global_yearMu_Min10_var10_common_zYear.csv",row.names = F,quote = F)
#write.csv(global_effortMu,"global_effortMu_MIN10_common_zYear.csv",row.names = F,quote = F)
#write.csv(global_dateMu,"global_dateMu_MIN10_var10_common_zYear.csv",row.names = F,quote = F)
#write.csv(global_yearTau,"global_yearTau_MIN10_var10_common_zYear.csv",row.names = F,quote = F)
#write.csv(global_effortTau,"global_effortTau_MIN10_common_var10_zYear.csv",row.names = F,quote = F)
#write.csv(global_dateTau,"global_dateTau_MIN10_var10_common_zYear.csv",row.names = F,quote = F)
# write.csv(globalDiagnostic,"globalDiagnostic_MIN10.csv",row.names = F,quote = F)

# site level
colnames(yearSites)<-c("site","median","up","low","probPos","probNeg")
#colnames(effortSites)<-c("site","median","up","low","probPos","probNeg")
#colnames(dateSites)<-c("site","median","up","low","probPos","probNeg")

write.csv(yearSites,"yearSites_MIN10_totalInds_global_var10_common_zYear.csv",row.names = F,quote = F)
#write.csv(effortSites,"effortSites_MIN10_totalInds_global_var10_common_zYear.csv",row.names = F,quote = F)
#write.csv(dateSites,"dateSites_MIN10_totalInds_global_var10_common_zYear.csv",row.names = F,quote = F)





## run this batch to write "other" files

#write.csv(global_yearMu,"global_yearMu_Min10_var10_other_zYear.csv",row.names = F,quote = F)
#write.csv(global_effortMu,"global_effortMu_MIN10_other_zYear.csv",row.names = F,quote = F)
#write.csv(global_dateMu,"global_dateMu_MIN10_var10_other_zYear.csv",row.names = F,quote = F)
#write.csv(global_yearTau,"global_yearTau_MIN10_var10_other_zYear.csv",row.names = F,quote = F)
#write.csv(global_effortTau,"global_effortTau_MIN10_other_var10_zYear.csv",row.names = F,quote = F)
#write.csv(global_dateTau,"global_dateTau_MIN10_var10_other_zYear.csv",row.names = F,quote = F)
# write.csv(globalDiagnostic,"globalDiagnostic_MIN10.csv",row.names = F,quote = F)

# site level
colnames(yearSites)<-c("site","median","up","low","probPos","probNeg")
#colnames(effortSites)<-c("site","median","up","low","probPos","probNeg")
#colnames(dateSites)<-c("site","median","up","low","probPos","probNeg")

write.csv(yearSites,"yearSites_MIN10_totalInds_global_var10_other_zYear.csv",row.names = F,quote = F)
#write.csv(effortSites,"effortSites_MIN10_totalInds_global_var10_other_zYear.csv",row.names = F,quote = F)
#write.csv(dateSites,"dateSites_MIN10_totalInds_global_var10_other_zYear.csv",row.names = F,quote = F)




