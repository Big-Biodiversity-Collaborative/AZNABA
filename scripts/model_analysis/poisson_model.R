# POISSON MODEL FOR ABUNDANCE DATA



# ABOUT ---

# 4th of July species-level parameters are estimated in hierarchical 
# models for each species

# Code is very wet for clarity



# LOAD LIBRARIES ---
library(rjags)
library(coda)
library(HDInterval)



# LOAD DATA ---
dat <- read.csv("data/Forister_data/fourthAllSp.csv", header = T)



# EXAMINE DATA ---

# View dimensions of data
dim(dat)      

# 82243 observations, 12 variables

# View first few values
head(dat)     

# View column headers
names(dat)    

# View data types
str(dat)      

# Find number of unique species
length(unique(dat$sp)) 

# 315 species

# Store species names in object
species <- unique(dat$sp)



# PREP DATA ---

# Drop any sites per species with < 10 years of observations/counts
dat <- dat[which(dat$yearsPerSitePerSp>9),]

# View dimensions of data
dim(dat) 

# 56956 observations, 12 variables

# View number of unique species
length(unique(dat$sp)) 

# 272 species

# Store species names in object
species <- unique(dat$sp)

# Centering years across all data here 
yearsZ <- (dat$year - mean(dat$year))  # 2001.654
dat <- cbind(dat,yearsZ)



# SET UP DATA FRAMES TO COLLECT RESULTS ---

# Data frames for species-level results
yearSpeciesMu <- data.frame()
effortSpeciesMu <- data.frame()
dateSpeciesMu <- data.frame()
yearSpeciesTau <- data.frame()
effortSpeciesTau <- data.frame()
dateSpeciesTau <- data.frame()
speciesNames <- data.frame()
listPPC <- list()
speciesDiag <- data.frame()
intmuSpeciesMU <- data.frame()
inttauSpeciesTau <- data.frame()
yearSpeciesMu80<-data.frame()

# Data frames for site-level results
yearSites <- data.frame()
effortSites <- data.frame()
dateSites <- data.frame()
interzSites <- data.frame()
MCMCyears <- array()
MCMC_siteYears <- list()

sp_dat <- data.frame()



############## CREATE LOOP FOR PULLING EACH SPECIES OUT FOR ANALYSIS ##############

for (j in 1:length(species)){
  
  sp_dat <- droplevels(dat[which(dat$sp==species[j]),])
  sites <- unique(sp_dat$site)
  workingDat <- data.frame()
  
  for (i in 1:length(sites)){
    tmp <- droplevels(sp_dat[which(sp_dat$site==sites[i]),])
    tmp <- cbind(tmp,scale(tmp$hours),scale(tmp$date))
    workingDat <- rbind(workingDat,tmp[,-c(1:2,4,6:12)])
  }
  
  names(workingDat) <- c("site","count","stdYear","stdHours","stdDate") # Still calling it "stdYear" here but it is across all data
  head(workingDat)
  datB <- workingDat # Just to condense the name
  head(datB)
  Nsite <- length(unique(datB$site))
  
  
  
  ############## (IF) LOOP FOR SPECIES WITH ONLY 1 SITE ##############
  
  if (Nsite < 2){
    basicData <- list(counts=datB$count, year=datB$stdYear,effort=datB$stdHours,
                      date=datB$stdDate,N=length(datB$stdYear))
    
    model <- "model{
  # poisson likelihood for counts/abundance, ln is link
  for(i in 1:N){
  counts[i] ~ dpois(lambda[i])
  # link is ln for a poisson model
  lambda[i] <- exp(int + beta1 * year[i] + beta2 * effort[i] + beta3 * date[i])
  
  ## posterior predictive check stuff
  #### generate ynew for posterior pedictive check
  ynew[i] ~ dpois(lambda[i])
  ##### examine residuals
  resid[i]<- counts[i] - lambda[i] 
  }

  beta1 ~ dnorm(0, 0.0001)
  beta2 ~ dnorm(0, 0.0001)
  beta3 ~ dnorm(0, 0.0001)
  int ~ dnorm(0, 0.0001)  
}
"
  
  basicModel <- jags.model(textConnection(model),
                           data = basicData,
                           n.chains = 2)  # Compile
  
  samples <- jags.samples(model = basicModel,
                          variable.names = c("beta1","beta2","beta3","lambda","int","ynew","resid"),
                          n.adapt = 1000,
                          n.iter = 1000)
  
  
  
  # ADDING CAPTURE STEPS FOR Matt 23vi20 ---
  
  MCMCyears <- cbind(MCMCyears,as.numeric(samples$beta1)) # Same as species-level for those species with just one site
  
  # Decided to do this as list
  tmp_siteYears <- numeric()
  
  for(k in 1:length(sites)){
    tmp_siteYears <- cbind(tmp_siteYears,as.numeric(samples$beta1[k,,]))
  }
  
  colnames(tmp_siteYears) <- sites
  
  # MCMC_siteYears <- cbind(MCMC_siteYears,tmp_siteYears)
  
  MCMC_siteYears[[j]] <- tmp_siteYears
  
  
  
  # DIAGNOSTICS AND RESULTS ---
  
  # Site level - diagnostics then parameter estimates
  
  beta1ESS<-numeric()
  beta1ESS[1] <- effectiveSize(samples$beta1[,,1]) 
  beta1ESS[2] <- effectiveSize(samples$beta1[,,2]) 
  
  write.csv(beta1ESS,
            file = paste(species[j], "beta1ESSsummary.csv"),
            row.names = F,
            quote = F)
  
  ###############
  beta1GR <- numeric()
  beta1GR <- gelman.diag(mcmc.list(as.mcmc(samples$beta1[,,1]),as.mcmc(samples$beta1[,,2])),autoburnin = F)[[1]][1]
  write.csv(beta1GR,file = paste(species[j],"beta1GRrange.csv"),row.names = F,quote = F)
  
  ###############
  beta2ESS <- numeric()
  beta2ESS[1] <- effectiveSize(samples$beta2[i,,1]) 
  beta2ESS[2] <- effectiveSize(samples$beta2[i,,2]) 
  write.csv(beta2ESS,file = paste(species[j],"beta2ESSsummary.csv"),row.names = F,quote = F)
  
  ###############
  beta2GR <- numeric()
  beta2GR <- gelman.diag(mcmc.list(as.mcmc(samples$beta2[,,1]),as.mcmc(samples$beta2[,,2])))[[1]][1]
  write.csv(beta2GR,file = paste(species[j],"beta2GRrange.csv"),row.names = F,quote = F)
  
  ###############
  beta3ESS <- numeric()
  beta3ESS[1] <- effectiveSize(samples$beta3[,,1]) 
  beta3ESS[2] <- effectiveSize(samples$beta3[,,2]) 
  write.csv(beta3ESS,file = paste(species[j],"beta3ESSsummary.csv"),row.names = F,quote = F)
  
  ###############
  beta3GR <- numeric()
  beta3GR <- gelman.diag(mcmc.list(as.mcmc(samples$beta3[,,1]),as.mcmc(samples$beta3[,,2])))[[1]][1]
  write.csv(beta3GR,file = paste(species[j],"beta3GRrange.csv"),row.names = F,quote = F)
  
  
  
  # SITE-LEVEL PARAMETER ESTIMATES ---
  
  # Collect site parameters to be incorporated in Species list (for species with only 1 site)
  
  beta1sp <- numeric()
  # beta1[1] <- paste(species[j])
  # beta1[1] <- paste(sites)
  beta1sp[1] <- round(median(samples$beta1[,,]),3)
  beta1sp[2] <- round(hdi(samples$beta1[,,],credMass = 0.95)[2],3) # Upper
  beta1sp[3] <- round(hdi(samples$beta1[,,],credMass = 0.95)[1],3) # Lower
  beta1sp[4] <- round((length(which(samples$beta1[,,]>0)))/length(samples$beta1[,,]),3)
  beta1sp[5] <- round((length(which(samples$beta1[,,]<0)))/length(samples$beta1[,,]),3)
  
  # Year HDI 80
  
  beta180sp <- numeric()
  # beta180[1] <- paste(species[j])
  # beta180[2] <- paste(sites)
  beta180sp[1] <- round(median(samples$beta1[,,]),3)
  beta180sp[2] <- round(hdi(samples$beta1[,,],credMass = 0.80)[2],3) # Upper
  beta180sp[3] <- round(hdi(samples$beta1[,,],credMass = 0.80)[1],3) # Lower
  beta180sp[4] <- round((length(which(samples$beta1[,,]>0)))/length(samples$beta1[,,]),3)
  beta180sp[5] <- round((length(which(samples$beta1[,,]<0)))/length(samples$beta1[,,]),3)
  
  ###############
  beta2sp <- numeric()
  # beta2[1] <- paste(species[j])
  # beta2[2] <- paste(sites)
  beta2sp[1] <- round(median(samples$beta2[,,]),3)
  beta2sp[2] <- round(hdi(samples$beta2[,,],credMass = 0.95)[2],3) # Upper
  beta2sp[3] <- round(hdi(samples$beta2[,,],credMass = 0.95)[1],3) # Lower
  beta2sp[4] <- round((length(which(samples$beta2[,,]>0)))/length(samples$beta2[i,,]),3)
  beta2sp[5] <- round((length(which(samples$beta2[,,]<0)))/length(samples$beta2[i,,]),3)
  
  ###############
  beta3sp <- numeric()
  # beta3[1] <- paste(species[j])
  # beta3[2] <- paste(sites)
  beta3sp[1] <- round(median(samples$beta3[,,]),3)
  beta3sp[2] <- round(hdi(samples$beta3[,,],credMass = 0.95)[2],3) # Upper
  beta3sp[3] <- round(hdi(samples$beta3[,,],credMass = 0.95)[1],3) # Lower
  beta3sp[4] <- round((length(which(samples$beta3[,,]>0)))/length(samples$beta3[i,,]),3)
  beta3sp[5] <- round((length(which(samples$beta3[,,]<0)))/length(samples$beta3[i,,]),3)
  
  
  
  # SITE-LEVEL INTERCEPTS ---
  
  interzsp <- numeric()
  # interz[1] <- paste(species[j])
  # interz[2] <- paste(sites)
  interzsp[1] <- round(median(samples$int[,,]),3)
  interzsp[2] <- round(hdi(samples$int[,,],credMass = 0.95)[2],3) # Upper
  interzsp[3] <- round(hdi(samples$int[,,],credMass = 0.95)[1],3) # Lower
  interzsp[4] <- round((length(which(samples$int[,,]>0)))/length(samples$int[i,,]),3)
  interzsp[5] <- round((length(which(samples$int[,,]<0)))/length(samples$int[i,,]),3)
  
  
  
  # SITE-LEVEL PARAMETER ESTIMATES CONTINUED ---
  
  # Collect site parameters to be incorporated in site list (for species with only 1 site)
  
  beta1site <- matrix(NA,nrow = Nsite,ncol = 7)
  beta1site[1] <- paste(species[j])
  beta1site[2] <- paste(sites)
  beta1site[3] <- round(median(samples$beta1[,,]),3)
  beta1site[4] <- round(hdi(samples$beta1[,,],credMass = 0.95)[2],3) # Upper
  beta1site[5] <- round(hdi(samples$beta1[,,],credMass = 0.95)[1],3) # Lower
  beta1site[6] <- round((length(which(samples$beta1[,,]>0)))/length(samples$beta1[,,]),3)
  beta1site[7] <- round((length(which(samples$beta1[,,]<0)))/length(samples$beta1[,,]),3)
  
  beta2site <- matrix(NA,nrow = Nsite,ncol = 7)
  beta2site[1] <- paste(species[j])
  beta2site[2] <- paste(sites)
  beta2site[3] <- round(median(samples$beta2[,,]),3)
  beta2site[4] <- round(hdi(samples$beta2[,,],credMass = 0.95)[2],3) # Upper
  beta2site[5] <- round(hdi(samples$beta2[,,],credMass = 0.95)[1],3) # Lower
  beta2site[6] <- round((length(which(samples$beta2[,,]>0)))/length(samples$beta2[i,,]),3)
  beta2site[7] <- round((length(which(samples$beta2[,,]<0)))/length(samples$beta2[i,,]),3)
  
  beta3site <- matrix(NA,nrow = Nsite,ncol = 7)
  beta3site[1] <- paste(species[j])
  beta3site[2] <- paste(sites)
  beta3site[3] <- round(median(samples$beta3[,,]),3)
  beta3site[4] <- round(hdi(samples$beta3[,,],credMass = 0.95)[2],3) # Upper
  beta3site[5] <- round(hdi(samples$beta3[,,],credMass = 0.95)[1],3) # Lower
  beta3site[6] <- round((length(which(samples$beta3[,,]>0)))/length(samples$beta3[i,,]),3)
  beta3site[7] <- round((length(which(samples$beta3[,,]<0)))/length(samples$beta3[i,,]),3)
  
  
  
  # SITE-LEVEL INTERCEPTS ---
  interzsite <- matrix(NA,nrow = Nsite,ncol = 7)
  interzsite[1] <- paste(species[j])
  interzsite[2] <- paste(sites)
  interzsite[3] <- round(median(samples$int[,,]),3)
  interzsite[4] <- round(hdi(samples$int[,,],credMass = 0.95)[2],3) # Upper
  interzsite[5] <- round(hdi(samples$int[,,],credMass = 0.95)[1],3) # Lower
  interzsite[6] <- round((length(which(samples$int[,,]>0)))/length(samples$int[i,,]),3)
  interzsite[7] <- round((length(which(samples$int[,,]<0)))/length(samples$int[i,,]),3)
  
  
  
  # PPC ---
  
  PPCynew <- data.frame(NA,nrow = Nsite,ncol = 3)
  
  for(i in 1:length(datB$count)){
    PPCynew[i,] <- quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
  }
  
  PPC <- cbind(datB$count,PPCynew)
  
  colnames(PPC) <- c("count","ynew","0.025","0.975")
  
  write.csv(PPC,file = paste(species[j],"PPC.csv"))
  
  
  
  # COLLECTING RESULTS ---
  
  # SPECIES-LEVEL
  yearSpeciesMu <- rbind(yearSpeciesMu,beta1sp)
  effortSpeciesMu <- rbind(effortSpeciesMu,beta2sp)
  dateSpeciesMu <- rbind(dateSpeciesMu,beta3sp)
  # yearSpeciesTau <- rbind(yearSpeciesTau,yearSpTau)
  # effortSpeciesTau <- rbind(effortSpeciesTau,effortSpTau)
  # dateSpeciesTau <- rbind(dateSpeciesTau,dateSpTau)
  intmuSpeciesMU <- rbind(intmuSpeciesMU,interzsp)
  # inttauSpeciesTau <- rbind(inttauSpeciesTau,inttauSpecies)
  yearSpeciesMu80<-rbind(yearSpeciesMu80,beta180sp)
  
  # SITE-LEVEL
  length(beta1site)
  dim(yearSites)
  yearSites<-rbind(yearSites,beta1site)
  effortSites<-rbind(effortSites,beta2site)
  dateSites<-rbind(dateSites,beta3site)
  interzSites<-rbind(interzSites,interzsite)
  
  listPPC[[j]]<-PPC
  
  } # END OF IF LOOP (SPECIES HAS >1 SITE)



############## (ELSE) LOOP FOR SPECIES WITH LESS THAN ONE SITE ##############

else{
  
  basicData <- list(counts = datB$count, 
                    site = datB$site,
                    year = datB$stdYear,
                    effort = datB$stdHours,
                    date = datB$stdDate,
                    N = length(datB$stdYear),
                    Nsite = Nsite)
  
  model <- "model{
	# poisson likelihood for counts/abundance, ln is link
	for(i in 1:N){
	      counts[i] ~ dpois(lambda[i])
	      # link is ln for a poisson model
	      lambda[i] <- exp(int[site[i]] + beta1[site[i]] * year[i] + beta2[site[i]] * effort[i] + beta3[site[i]] * date[i])

        ## posterior predictive check stuff
        #### generate ynew for posterior pedictive check
        ynew[i] ~ dpois(lambda[i])
        ##### examine residuals
        resid[i]<- counts[i] - lambda[i] 
}
	# random effect (hierarchical) coefficients  sites - t1 = site mean and precision (sites nested within the species)
	for(j in 1:Nsite){	
      beta1[j] ~ dnorm(s1mu, s1tau)
      beta2[j] ~ dnorm(s2mu, s2tau)
      beta3[j] ~ dnorm(s3mu, s3tau)
      int[j] ~ dnorm(intmu,inttau)  
}

	# random effect (hierarchical) coefficients for the species (conditional priors) 	
	     	s1mu ~ dnorm(0, 0.0001)
	     	s1tau ~ dgamma(0.1,0.1)
	     	s2mu ~ dnorm(0, 0.0001)
	     	s2tau ~ dgamma(0.1,0.1)
        s3mu ~ dnorm(0, 0.0001)
	     	s3tau ~ dgamma(0.1,0.1)
        intmu ~ dnorm(0,0.001)
	     	inttau ~ dgamma(0.1,0.1)    
}
"

basicModel<-jags.model(textConnection(model),data=basicData,n.chains=2)  # Compile

samples<-jags.samples(model=basicModel,
                      variable.names=c("beta1","beta2","beta3","s1mu","s1tau","s2mu","s2tau",
                                       "s3mu","s3tau","lambda","int","intmu","inttau","ynew","resid"),
                      n.adapt=1000,n.iter=1000)



# ADDING CAPTURE OF MCMC STEPS FOR Matt 23vi20 ---

tmpMCMC <- as.data.frame(as.numeric(samples$s1mu))
names(tmpMCMC) <- species[j]
MCMCyears <- cbind(MCMCyears,tmpMCMC)

# Decided to do this as a list
tmp_siteYears <- numeric()

for(k in 1:length(sites)){
  tmp_siteYears <- cbind(tmp_siteYears,as.numeric(samples$beta1[k,,]))
}

tmp_siteYears <- as.data.frame(tmp_siteYears)

names(tmp_siteYears) <- sites

# MCMC_siteYears <- cbind(MCMC_siteYears,tmp_siteYears)

MCMC_siteYears[[j]] <- tmp_siteYears



# DIAGNOSTICS AND RESULTS ---

# Site level - diagnostics then parameter estimates

beta1ESS <- matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains

for (i in 1:Nsite){
  beta1ESS[i,1] <- effectiveSize(samples$beta1[i,,1]) 
  beta1ESS[i,2] <- effectiveSize(samples$beta1[i,,2]) 
}

colnames(beta1ESS) <- c("chain1ESS","chain2ESS")

write.csv(summary(beta1ESS),file = paste(species[j],"beta1ESSsummary.csv"),row.names = F,quote = F)

###############
beta1GR <- numeric()

for (i in 1:Nsite){
  beta1GR[i] <- gelman.diag(mcmc.list(as.mcmc(samples$beta1[i,,1]),as.mcmc(samples$beta1[i,,2])),autoburnin = F)[[1]][1]
}

write.csv(range(beta1GR),file = paste(species[j],"beta1GRrange.csv"),row.names = F,quote = F)

###############
beta2ESS <- matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains

for (i in 1:Nsite){
  beta2ESS[i,1] <- effectiveSize(samples$beta2[i,,1]) 
  beta2ESS[i,2] <- effectiveSize(samples$beta2[i,,2]) 
}

colnames(beta2ESS) <- c("chain1ESS","chain2ESS")

write.csv(summary(beta2ESS),file = paste(species[j],"beta2ESSsummary.csv"),row.names = F,quote = F)

###############
beta2GR <- numeric()

for (i in 1:Nsite){
  beta2GR[i] <- gelman.diag(mcmc.list(as.mcmc(samples$beta2[i,,1]),as.mcmc(samples$beta2[i,,2])))[[1]][1]
}

write.csv(range(beta2GR),file = paste(species[j],"beta2GRrange.csv"),row.names = F,quote = F)

###############
beta3ESS <- matrix(NA,nrow = Nsite,ncol = 2) # collect ESS for each site and for both chains

for (i in 1:Nsite){
  beta3ESS[i,1] <- effectiveSize(samples$beta3[i,,1]) 
  beta3ESS[i,2] <- effectiveSize(samples$beta3[i,,2]) 
}

colnames(beta3ESS) <- c("chain1ESS","chain2ESS")

write.csv(summary(beta3ESS),file = paste(species[j],"beta3ESSsummary.csv"),row.names = F,quote = F)

###############
beta3GR <- numeric()

for (i in 1:Nsite){
  beta3GR[i] <- gelman.diag(mcmc.list(as.mcmc(samples$beta3[i,,1]),as.mcmc(samples$beta3[i,,2])))[[1]][1]
}

write.csv(range(beta3GR),file = paste(species[j],"beta3GRrange.csv"),row.names = F,quote = F)



# SITE-LEVEL PARAMETER ESTIMATES ---

beta1 <- matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta1[i,1] <- paste(species[j])
  beta1[i,2] <- paste(sites[i])
  beta1[i,3] <- round(median(samples$beta1[i,,]),3)
  beta1[i,4] <- round(hdi(samples$beta1[i,,],credMass = 0.95)[2],3) # Upper
  beta1[i,5] <- round(hdi(samples$beta1[i,,],credMass = 0.95)[1],3) # Lower
  beta1[i,6] <- round((length(which(samples$beta1[i,,]>0)))/length(samples$beta1[i,,]),3)
  beta1[i,7] <- round((length(which(samples$beta1[i,,]<0)))/length(samples$beta1[i,,]),3)
  # beta1[i,] <- round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}

beta2 <- matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta2[i,1] <- paste(species[j])
  beta2[i,2] <- paste(sites[i])
  beta2[i,3] <- round(median(samples$beta2[i,,]),3)
  beta2[i,4] <- round(hdi(samples$beta2[i,,],credMass = 0.95)[2],3) # Upper
  beta2[i,5] <- round(hdi(samples$beta2[i,,],credMass = 0.95)[1],3) # Lower
  beta2[i,6] <- round((length(which(samples$beta2[i,,]>0)))/length(samples$beta2[i,,]),3)
  beta2[i,7] <- round((length(which(samples$beta2[i,,]<0)))/length(samples$beta2[i,,]),3)
  # beta1[i,] <- round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}


beta3 <- matrix(NA,nrow = Nsite,ncol = 7)  # sp, site, median upper, lower, probPos, probNeg
for(i in 1:Nsite){
  beta3[i,1] <- paste(species[j])
  beta3[i,2] <- paste(sites[i])
  beta3[i,3] <- round(median(samples$beta3[i,,]),3)
  beta3[i,4] <- round(hdi(samples$beta3[i,,],credMass = 0.95)[2],3) # Upper
  beta3[i,5] <- round(hdi(samples$beta3[i,,],credMass = 0.95)[1],3) # Lower
  beta3[i,6] <- round((length(which(samples$beta3[i,,]>0)))/length(samples$beta3[i,,]),3)
  beta3[i,7] <- round((length(which(samples$beta3[i,,]<0)))/length(samples$beta3[i,,]),3)
  # beta1[i,] <- round(quantile(samples$beta1[i,,],probs=c(0.5,0.025,0.975)),3)
}



# SITE-LEVEL INTERCEPTS ---

interz <- matrix(NA,nrow = Nsite,ncol = 7)
for(i in 1:Nsite){
  interz[i,1] <- paste(species[j])
  interz[i,2] <- paste(sites[i])
  interz[i,3] <- round(median(samples$int[i,,]),3)
  interz[i,4] <- round(hdi(samples$int[i,,],credMass = 0.95)[2],3) # Upper
  interz[i,5] <- round(hdi(samples$int[i,,],credMass = 0.95)[1],3) # Lower
  interz[i,6] <- round((length(which(samples$int[i,,]>0)))/length(samples$int[i,,]),3)
  interz[i,7] <- round((length(which(samples$int[i,,]<0)))/length(samples$int[i,,]),3)
}



# SPECIES-LEVEL ---

# FIRST: DIAGNOSTICS
# ESS and GR - Maybe just get sum across both chains - out of 30K

s1muESS <- sum(effectiveSize(samples$s1mu[,,]))
s2muESS <- sum(effectiveSize(samples$s2mu[,,]))
s3muESS <- sum(effectiveSize(samples$s3mu[,,]))
s1tauESS <- sum(effectiveSize(samples$s1tau[,,]))
s2tauESS <- sum(effectiveSize(samples$s2tau[,,]))
s3tauESS <- sum(effectiveSize(samples$s3tau[,,]))

s1muGR <- round(gelman.diag(mcmc.list(as.mcmc(samples$s1mu[,,1]),
                                      as.mcmc(samples$s1mu[,,2])))[[1]][1],3)
s2muGR <- round(gelman.diag(mcmc.list(as.mcmc(samples$s2mu[,,1]),
                                      as.mcmc(samples$s2mu[,,2])))[[1]][1],3)
s3muGR <- round(gelman.diag(mcmc.list(as.mcmc(samples$s3mu[,,1]),
                                      as.mcmc(samples$s3mu[,,2])))[[1]][1],3)
s1tauGR <- round(gelman.diag(mcmc.list(as.mcmc(samples$s1tau[,,1]),
                                       as.mcmc(samples$s1tau[,,2])))[[1]][1],3)
s2tauGR <- round(gelman.diag(mcmc.list(as.mcmc(samples$s2tau[,,1]),
                                       as.mcmc(samples$s2tau[,,2])))[[1]][1],3)
s3tauGR <- round(gelman.diag(mcmc.list(as.mcmc(samples$s3tau[,,1]),
                                       as.mcmc(samples$s3tau[,,2])))[[1]][1],3)

year_diag <- c(s1muESS,s1muGR,s1tauESS,s1tauGR)

effort_diag <- c(s2muESS,s2muGR,s2tauESS,s2tauGR)

date_diag <- c(s3muESS,s3muGR,s3tauESS,s3tauGR)

spDiag <- c(year_diag,effort_diag,date_diag)



# PARAMETER ESTIMATES ---

# SPECIES-LEVEL

# Year
yearUp <- round(hdi(samples$s1mu,credMass = 0.95)[1],3)         # Lower
yearLow <- round(hdi(samples$s1mu,credMass = 0.95)[2],3)        # Upper
yearMedian <- round(median(samples$s1mu[,,]),3)
yearProbPos <- round((length(which(samples$s1mu[,,]>0)))/length(samples$s1mu[,,]),3)
yearProbNeg <- round((length(which(samples$s1mu[,,]<0)))/length(samples$s1mu[,,]),3)
yearSp <- c(yearMedian,yearUp,yearLow,yearProbPos,yearProbNeg)

# Year - changing to 80% HDIs
yearUp80 <- round(hdi(samples$s1mu,credMass = 0.8)[1],3)        # Lower
yearLow80 <- round(hdi(samples$s1mu,credMass = 0.8)[2],3)       # Upper
yearMedian80 <- round(median(samples$s1mu[,,]),3)
yearProbPos80 <- round((length(which(samples$s1mu[,,]>0)))/length(samples$s1mu[,,]),3)
yearProbNeg80 <- round((length(which(samples$s1mu[,,]<0)))/length(samples$s1mu[,,]),3)
yearSp80 <- c(yearMedian80,yearUp80,yearLow80,yearProbPos80,yearProbNeg80)

# s1mu <- round(quantile(samples$s1mu[,,],probs=c(0.5,0.025,0.975)),3)
# s2mu <- round(quantile(samples$s2mu[,,],probs=c(0.5,0.025,0.975)),3)
# s3mu <- round(quantile(samples$s3mu[,,],probs=c(0.5,0.025,0.975)),3)
# outSpeciesMU <- rbind(s1mu,s2mu,s3mu)
# rownames(outSpeciesMU) <- c("year","effort","date")
# write.csv(outSpeciesMU,file=paste(species[j],"_speciesBetas.csv"),quote = F)

# Effort
effortUp <- round(hdi(samples$s2mu,credMass = 0.95)[1],3)       # Lower
effortLow <- round(hdi(samples$s2mu,credMass = 0.95)[2],3)      # Upper
effortMedian <- round(median(samples$s2mu[,,]),3)
effortProbPos <- round((length(which(samples$s2mu[,,]>0)))/length(samples$s2mu[,,]),3)
effortProbNeg <- round((length(which(samples$s2mu[,,]<0)))/length(samples$s2mu[,,]),3)
effortSp <- c(effortMedian,effortUp,effortLow,effortProbPos,effortProbNeg)

# Date
dateUp <- round(hdi(samples$s3mu,credMass = 0.95)[1],3)         # Lower
dateLow <- round(hdi(samples$s3mu,credMass = 0.95)[2],3)        # Upper
dateMedian <- round(median(samples$s3mu[,,]),3)
dateProbPos <- round((length(which(samples$s3mu[,,]>0)))/length(samples$s3mu[,,]),3)
dateProbNeg <- round((length(which(samples$s3mu[,,]<0)))/length(samples$s3mu[,,]),3)
dateSp <- c(dateMedian,dateUp,dateLow,dateProbPos,dateProbNeg)



# CAPTURE PRECISION ---

# Year
yearUpTau <- round(hdi(samples$s1tau,credMass = 0.95)[1],3)       # Lower
yearLowTau <- round(hdi(samples$s1tau,credMass = 0.95)[2],3)      # Upper
yearMedianTau <- round(quantile(samples$s1tau[,,],probs=c(0.5)),3)
yearProbPosTau <- round((length(which(samples$s1tau[,,]>0)))/length(samples$s1tau[,,]),3)
yearProbNegTau <- round((length(which(samples$s1tau[,,]<0)))/length(samples$s1tau[,,]),3)
yearSpTau <- c(yearMedianTau,yearUpTau,yearLowTau,yearProbPosTau,yearProbNegTau)

# Effort
effortUpTau <- round(hdi(samples$s2tau,credMass = 0.95)[1],3)     # Lower
effortLowTau <- round(hdi(samples$s2tau,credMass = 0.95)[2],3)    # Upper
effortMedianTau <- round(quantile(samples$s2tau[,,],probs=c(0.5)),3)
effortProbPosTau <- round((length(which(samples$s2tau[,,]>0)))/length(samples$s2tau[,,]),3)
effortProbNegTau <- round((length(which(samples$s2tau[,,]<0)))/length(samples$s2tau[,,]),3)
effortSpTau <- c(effortMedianTau,effortUpTau,effortLowTau,effortProbPosTau,effortProbNegTau)

# Date
dateUpTau <- round(hdi(samples$s3tau,credMass = 0.95)[1],3)         # Lower
dateLowTau <- round(hdi(samples$s3tau,credMass = 0.95)[2],3)        # Upper
dateMedianTau <- round(quantile(samples$s3tau[,,],probs=c(0.5)),3)
dateProbPosTau <- round((length(which(samples$s3tau[,,]>0)))/length(samples$s3tau[,,]),3)
dateProbNegTau <- round((length(which(samples$s3tau[,,]<0)))/length(samples$s3tau[,,]),3)
dateSpTau <- c(dateMedianTau,dateUpTau,dateLowTau,dateProbPosTau,dateProbNegTau)

# Intercept
intmuUp <- round(hdi(samples$intmu,credMass = 0.95)[1],3)           # Lower
intmuLow <- round(hdi(samples$intmu,credMass = 0.95)[2],3)          # Upper
intmuMedian <- round(quantile(samples$intmu[,,],probs=c(0.5)),3)
intmuProbPos <- round((length(which(samples$intmu[,,]>0)))/length(samples$intmu[,,]),3)
intmuProbNeg <- round((length(which(samples$intmu[,,]<0)))/length(samples$intmu[,,]),3)
intmuSpecies <- c(intmuMedian,intmuUp,intmuLow,intmuProbPos,intmuProbNeg)

inttauUp <- round(hdi(samples$inttau,credMass = 0.95)[1],3)         # Lower
inttauLow <- round(hdi(samples$inttau,credMass = 0.95)[2],3)        # Upper
inttauMedian <- round(quantile(samples$inttau[,,],probs=c(0.5)),3)
inttauProbPos <- round((length(which(samples$inttau[,,]>0)))/length(samples$inttau[,,]),3)
inttauProbNeg <- round((length(which(samples$inttau[,,]<0)))/length(samples$inttau[,,]),3)
inttauSpecies <- c(inttauMedian,inttauUp,inttauLow,inttauProbPos,inttauProbNeg)

# s1tau <- round(quantile(samples$s1tau[,,],probs=c(0.5,0.025,0.975)),3)
# s2tau <- round(quantile(samples$s2tau[,,],probs=c(0.5,0.025,0.975)),3)
# s3tau <- round(quantile(samples$s3tau[,,],probs=c(0.5,0.025,0.975)),3)
# 
# outSpeciesTAU <- rbind(s1tau,s2tau,s3tau)
# rownames(outSpeciesTAU) <- c("year","effort","date")
# write.csv(outSpeciesTAU,file=paste(species[j],"_speciesTaus.csv"),quote = F)



# PPC ---
PPCynew <- data.frame(NA,nrow = Nsite,ncol = 3)

for(i in 1:length(datB$count)){
  PPCynew[i,] <- quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
}

PPC <- cbind(datB$count,PPCynew)

colnames(PPC) <- c("count","ynew","0.025","0.975")

write.csv(PPC,file = paste(species[j],"PPC.csv"))



# COLLECTING RESULTS ---

# SPECIES-LEVEL

yearSpeciesMu <- rbind(yearSpeciesMu,yearSp)
effortSpeciesMu <- rbind(effortSpeciesMu,effortSp)
dateSpeciesMu <- rbind(dateSpeciesMu,dateSp)
yearSpeciesTau <- rbind(yearSpeciesTau,yearSpTau)
effortSpeciesTau <- rbind(effortSpeciesTau,effortSpTau)
dateSpeciesTau <- rbind(dateSpeciesTau,dateSpTau)
intmuSpeciesMU <- rbind(intmuSpeciesMU,intmuSpecies)
inttauSpeciesTau <- rbind(inttauSpeciesTau,inttauSpecies)
yearSpeciesMu80 <- rbind(yearSpeciesMu80,yearSp80)

# SPECIES-LEVEL DIAGNOSTICS
speciesDiag<-rbind(speciesDiag,spDiag)

# SITE-LEVEL
yearSites<-rbind(yearSites,beta1)
effortSites<-rbind(effortSites,beta2)
dateSites<-rbind(dateSites,beta3)
interzSites<-rbind(interzSites,interz)

listPPC[[j]]<-PPC

} # END OF ELSE (SPECIES WITH <1 SITE)

} # END OF FULL LOOP



# CLEAN AND SAVE DATA ---

# SPECIES-LEVEL

# Rename columns
colnames(yearSpeciesMu) <- c("median","up","low","probPos","probNeg")
colnames(effortSpeciesMu) <- c("median","up","low","probPos","probNeg")
colnames(dateSpeciesMu) <- c("median","up","low","probPos","probNeg")
colnames(yearSpeciesTau) <- c("median","up","low","probPos","probNeg")
colnames(effortSpeciesTau) <- c("median","up","low","probPos","probNeg")
colnames(dateSpeciesTau) <- c("median","up","low","probPos","probNeg")
colnames(speciesDiag) <- c("yearmuESS","yearmuGR","yeartauESS","yeartauGR",
                           "effortmuESS","effortmuGR","efforttauESS","efforttauGR",
                           "datemuESS","datemuGR","datetauESS","datetauGR")
colnames(intmuSpeciesMU) <- c("median","up","low","probPos","probNeg")
colnames(inttauSpeciesTau) <- c("median","up","low","probPos","probNeg")
colnames(yearSpeciesMu80) <- c("median","up","low","probPos","probNeg")

# Bind species to results
yearSpeciesMu <- cbind(species,yearSpeciesMu)
effortSpeciesMu <- cbind(species,effortSpeciesMu)
dateSpeciesMu <- cbind(species,dateSpeciesMu)
yearSpeciesTau <- cbind(species,yearSpeciesTau)
effortSpeciesTau <- cbind(species,effortSpeciesTau)
dateSpeciesTau <- cbind(species,dateSpeciesTau)
speciesDiag <- cbind(species,speciesDiag)
intmuSpeciesMU <- cbind(species,intmuSpeciesMU)
inttauSpeciesTau <- cbind(species,inttauSpeciesTau)
yearSpeciesMu80 <- cbind(species,yearSpeciesMu80)

# Save files
write.csv(yearSpeciesMu,"yearSpMu_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(effortSpeciesMu,"effortSpMu_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(dateSpeciesMu,"dateSpMu_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(yearSpeciesTau,"yearSpTau_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(effortSpeciesTau,"effortSpTau_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(dateSpeciesTau,"dateSpTau_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(speciesDiag,"speciesDiag_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(intmuSpeciesMU,"interceptsMu_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(inttauSpeciesTau,"interceptsTau_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(yearSpeciesMu80,"yearSpMu_MIN8_zYear_HDI80.csv",row.names = F,quote = F)

# SITE-LEVEL

# Rename columns
colnames(yearSites) <- c("species","site","median","up","low","probPos","probNeg")
colnames(effortSites) <- c("species","site","median","up","low","probPos","probNeg")
colnames(dateSites) <- c("species","site","median","up","low","probPos","probNeg")
colnames(interzSites) <- c("species","site","median","up","low","probPos","probNeg")

# Save files
write.csv(yearSites,"yearSites_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(effortSites,"effortSites_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(dateSites,"dateSites_MIN8_zYear.csv",row.names = F,quote = F)
write.csv(interzSites,"interceptsSites_MIN8_zYear.csv",row.names = F,quote = F)

# Save image
save.image("sitesMIN10_zYearMCMC.RData")

# Save other files
MCMCyears <- MCMCyears[,-1]
names(MCMCyears) <- species
save(MCMCyears,file = "MCMCyears.RData")
names(MCMC_siteYears) <- species
save(MCMC_siteYears,file = "MCMC_siteYears.RData")




