# ABOUT ---

# Here we run Bayesian multiple regressions
  # to explore spatial variation in rates of change from the Poisson
  # models (specifically: location-specific year coefficients for 
  # the "all individual" models, not separately by species).
  # The strategy is a combination of ridge and regular regressions
  # to establish a model for the results generated with all individuals,
  # and then that same model is tested with the results from the most 
  # common species and the others.

# Also note that in the full analyses reported in the manuscript,
  # the model below (the final model, not the initial ridge models) was
  # repeated 1000 times across samples of the posterior from the previous
  # hierarchical Bayesian model that generated the location-specific point 
  # estimates.  The results (from that full resampled model are essentially
  # identical to the results based on the point-estimate approach, which
  # is reproduced here for simplicity.)



# LOAD LIBRARIES --
library(car)
library(rjags)
library(psych)
library(adespatial)
library(spdep)
library(adegraphics)
library(vegan)
library(HDInterval)



# LOAD DATA ---
load("allFourthForAnalyses.Rdata")



# EXAMINE DATA ---
str(fourth)
names(fourth)

# Coefficients to be predicted

temp <- read.csv( "yearSites_MIN10_totalInds_global_var10_all_zYear.csv" )
temp$probChange <- apply(temp[,c(5:6)],1,FUN=max) 
colnames(temp)[1] <- "sites"
head(temp)
abun <- temp



# PREPARE VARIABLES --

sites <- fourth$sites
head(sites)

# Add an average annual temp
sites$temp_ann <- (sites$tmax_ann + sites$tmin_ann) / 2

# Transformations (not all of these variables will be used)
sites$urb16_25mi <- log(sites$urb16_25mi + 0.0001)
sites$crop19_25mi <- log(sites$crop19_25mi+0.0001)
sites$ppt_fal <- log(sites$ppt_fal)
sites$ppt_spr <- log(sites$ppt_spr)
sites$ppt_sum <- log(sites$ppt_sum)
sites$ppt_win <- log(sites$ppt_win)
sites$ppt_ann <- log(sites$ppt_ann)

new <- merge(abun, sites,by="sites")
head(new)



# GENERATE COVARIATES FOR SPATIAL AUTOCORRELATION --

head(new)
latlongs <- new[,c(8:9)]
head(latlongs)

# mems
mems <- dbmem(latlongs, thresh=NULL, MEM.autocor="positive")
saveMEMs <- data.frame(mems)
head(saveMEMs)
dim(saveMEMs)



# START OF SCREENING STEPS --
  
# First just spatial covariates (keeping ones above 0.75 prob.)

X <- data.frame(saveMEMs)  
Y <- new$median
regData <- list(y=Y, x=X,N=length(Y),Nb=ncol(X))

# Ridge
model<-"model{
	for(i in 1:N){
		y[i] ~ dnorm(mu[i],tau)
		mu[i] <- b0 + inprod(x[i,], b)   
	}
	
	## priors on intercept & precision
	tau ~ dgamma(0.1,0.1)
	b0 ~ dnorm(0,0.00001)
	
	## L2 (ridge) = normal prior
	for(j in 1:Nb){
		b[j] ~ dnorm(0,precision) 
	}
	precision ~ dgamma(0.1,0.1)  #ridge
}
"
regModel<-jags.model(textConnection(model),data= regData,n.chains=2) 

samples<-jags.samples(model= regModel,variable.names=c("b0","b"),n.iter=100000)

str(samples)

# Summarize results
numVar <- dim(X)[2]
coefsBs <- NA
coefsUp <- NA
coefsLow <- NA
probNeg <- NA
probPos <- NA

for(i in 1:numVar){
  
	coefsBs[i] <- median(samples$b[i,,])

	coefsUp[i] <- hdi(samples$b[i,,],0.95)[2,1]
	coefsLow[i] <- hdi(samples$b[i,,],0.95)[1,1]
	
	probNeg[i] <- length(samples$b[i,,][samples$b[i,,]<0]) / length(samples$b[i,,])
	probPos[i] <- length(samples$b[i,,][samples$b[i,,]>0]) / length(samples$b[i,,])

}

res <- data.frame(colnames(X),coefsBs, coefsLow, coefsUp,probNeg,probPos)

colnames(res)[1] <- "var"

res

# Sort by probability of effect
res$prob <- apply(res[,c(5:6)],1,max)
res2 <- res[order(res$prob,decreasing=T),];res2

# Top mems: 2, 4, 12, 13, 5



# RIDGE AGAIN ON CLIMATE VARIABLES --

# (also including top mems from previous step)

X <- new[,c(47:50,52:55,57:60)]  
head(X)

# Scale and add mems
X <- scale(X)
head(X)
X <- data.frame(X,saveMEMs[,c(2,4,12,13,5)])

data.frame(colnames(X))
Y <- new$median
regData <- list(y=Y, x=X,N=length(Y),Nb=ncol(X))

# Ridge
model <- "model{
	for(i in 1:N){
		y[i] ~ dnorm(mu[i],tau)
		mu[i] <- b0 + inprod(x[i,], b)   
	}
	
	## priors on intercept & precision
	tau ~ dgamma(0.1,0.1)
	b0 ~ dnorm(0,0.00001)
	
	## L2 (ridge) = normal prior
	for(j in 1:Nb){
		b[j] ~ dnorm(0,precision) 
	}
	precision ~ dgamma(0.1,0.1)  #ridge
}
"
regModel<-jags.model(textConnection(model),data= regData,n.chains=2) 

samples<-jags.samples(model= regModel,variable.names=c("b0","b"),n.iter=100000)

str(samples)

# Summarize results

numVar <- dim(X)[2]
coefsBs <- NA
coefsUp <- NA
coefsLow <- NA
probNeg <- NA
probPos <- NA

for(i in 1:numVar){
  
	coefsBs[i] <- median(samples$b[i,,])

	coefsUp[i] <- hdi(samples$b[i,,],0.95)[2,1]
	coefsLow[i] <- hdi(samples$b[i,,],0.95)[1,1]
	
	probNeg[i] <- length(samples$b[i,,][samples$b[i,,]<0]) / length(samples$b[i,,])
	probPos[i] <- length(samples$b[i,,][samples$b[i,,]>0]) / length(samples$b[i,,])

}

res <- data.frame(colnames(X),coefsBs, coefsLow, coefsUp,probNeg,probPos)

colnames(res)[1] <- "var"

res

# Sort by probability of effect
res$prob <- apply(res[,c(5:6)],1,max)
res2 <- res[order(res$prob,decreasing=T),];res2

# Winners: tmax_sum_slope, tmax_fal_slope, ppt_fal_slope, ppt_sum_slope



# --- FINAL NON-RIDGE MODEL ---

# Now the top climate change variables go into a final, 
  # (non-ridge) model that includes the top MEMs as well as the static climate 
  # descriptions (average temps and annual ppt).

X <- new[,c(12,21,43,61,54,52,47,49)] # urb and crop static, climate annual, filtered seasonal
head(X)

# Scale and add mems
X <- scale(X)
head(X)
X <- data.frame(X,saveMEMs[,c(2,4,12,13,5)])

data.frame(colnames(X))
Y <- new$median
regData <- list(y=Y, x=X,N=length(Y),Nb=ncol(X))

# Model (not ridge this time)
model <- "model{
	for(i in 1:N){

		y[i] ~ dnorm(mu[i],tau)
		mu[i] <- b0 + inprod(x[i,], b)   

        ## posterior predictive check stuff
        #### generate ynew for posterior pedictive check
        ynew[i] ~ dnorm(mu[i],tau)
        ##### examine residuals
        resid[i]<- y[i] - ynew[i] 
	}
	
	## priors on intercept & precision
	tau ~ dgamma(0.1,0.1)
	b0 ~ dnorm(0,0.00001)
	
	for(j in 1:Nb){
		b[j] ~ dnorm(0,0.0001) 
	}
}
"
regModel<-jags.model(textConnection(model),data= regData,n.chains=2) 

samples<-jags.samples(model= regModel,variable.names=c("b0","b","ynew","resid"),n.iter=100000)

str(samples)

# Summarize results

numVar <- dim(X)[2]
coefsBs <- NA
coefsUp <- NA
coefsLow <- NA
probNeg <- NA
probPos <- NA

for(i in 1:numVar){
  
	coefsBs[i] <- median(samples$b[i,,])

	coefsUp[i] <- hdi(samples$b[i,,],0.95)[2,1]
	coefsLow[i] <- hdi(samples$b[i,,],0.95)[1,1]
	
	probNeg[i] <- length(samples$b[i,,][samples$b[i,,]<0]) / length(samples$b[i,,])
	probPos[i] <- length(samples$b[i,,][samples$b[i,,]>0]) / length(samples$b[i,,])

}

hist(coefsBs)
res <- data.frame(colnames(X),coefsBs, coefsLow, coefsUp,probNeg,probPos)
colnames(res)[1] <- "var"
res

# Sort by probability of effect
res$prob <- apply(res[,c(5:6)],1,max)
res2 <- res[order(res$prob,decreasing=T),];res2



# DIAGNOSTICS --

# Choose one variable
ID1 <- 6
gelman.diag(mcmc.list(as.mcmc(samples$b[ID1,,1]),as.mcmc(samples$b[ID1,,2]))) 
plot(samples$b[ID1,,1])
effectiveSize(samples$b[ID1,,1]) 

# Posterior predictive correlation
PPCynew <- data.frame(NA,nrow = length(Y),ncol = 3)

for(i in 1:length(Y)){
  PPCynew[i,]<-quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
}

PPC <- cbind(Y,PPCynew)
colnames(PPC)<-c("Y","ynew","0.025","0.975")
head(PPC)
plot(PPC$Y ~ PPC$ynew)
cor(PPC$Y,PPC$ynew)
cor(PPC$Y,PPC$ynew)^2

# Save posterior samples for plotting in fig 3
str(samples)
head(X)

post <- matrix(NA,nrow=10000,ncol=8)

for(i in 1:8){
	post[1: 10000,i] <- sample(samples$b[i,,], 10000,replace=F)
}

colnames(post) <- names(X)[1:8]
head(post);dim(post)

write.csv(post,"allPostSamples.csv")
write.csv(post,"commonPostSamples.csv")
write.csv(post,"otherPostSamples.csv")
# write.csv(post,"rarePostSamples.csv")



# --- RUN SAME MODEL WITH MOST COMMON SPECIES ---

# LOAD DATA --
load("allFourthForAnalyses.Rdata")
str(fourth)
names(fourth)

# Common
temp <- read.csv( "yearSites_MIN10_totalInds_global_var10_common_zYear.csv" )
temp$probChange <- apply(temp[,c(5:6)],1,FUN=max) 
colnames(temp)[1] <- "sites"
head(temp)
abun <- temp


# PREP PREDICTORS --
sites <- fourth$sites
head(sites)
dim(sites)

# Add an average annual temp
sites$temp_ann <- (sites$tmax_ann + sites$tmin_ann) / 2

# Transformations
sites$urb16_25mi <- log(sites$urb16_25mi + 0.0001)
sites$crop19_25mi <- log(sites$crop19_25mi+0.0001)
sites$ppt_fal <- log(sites$ppt_fal)
sites$ppt_spr <- log(sites$ppt_spr)
sites$ppt_sum <- log(sites$ppt_sum)
sites$ppt_win <- log(sites$ppt_win)
sites$ppt_ann <- log(sites$ppt_ann)

new <- merge(abun, sites,by="sites")
head(new)

# Generate mems

head(new)
latlongs <- new[,c(8:9)]
head(latlongs)

mems <- dbmem(latlongs, thresh=NULL, MEM.autocor="positive")
saveMEMs <- data.frame(mems)

X <- new[,c(12,21,43,61,54,52,47,49)] 
head(X)

# Scale and add mems
X <- scale(X)
X <- data.frame(X,saveMEMs[,c(2,4,12,13,5)])
head(X)

data.frame(colnames(X))
Y <- new$median
regData <- list(y=Y, x=X,N=length(Y),Nb=ncol(X))


# MODEL --
model <- "model{
	for(i in 1:N){

		y[i] ~ dnorm(mu[i],tau)
		mu[i] <- b0 + inprod(x[i,], b)   

        ## posterior predictive check stuff
        #### generate ynew for posterior pedictive check
        ynew[i] ~ dnorm(mu[i],tau)
        ##### examine residuals
        resid[i]<- y[i] - ynew[i] 
	}
	
	## priors on intercept & precision
	tau ~ dgamma(0.1,0.1)
	b0 ~ dnorm(0,0.00001)
	
	for(j in 1:Nb){

		b[j] ~ dnorm(0,0.0001) 
	}
}
"
regModel<-jags.model(textConnection(model),data= regData,n.chains=2) 
samples<-jags.samples(model= regModel,variable.names=c("b0","b","ynew","resid"),n.iter=100000)
str(samples)


# SUMMARIZE --
numVar <- dim(X)[2]
coefsBs <- NA
coefsUp <- NA
coefsLow <- NA
probNeg <- NA
probPos <- NA

for(i in 1:numVar){
  
	coefsBs[i] <- median(samples$b[i,,])

	coefsUp[i] <- hdi(samples$b[i,,],0.95)[2,1]
	coefsLow[i] <- hdi(samples$b[i,,],0.95)[1,1]

	probNeg[i] <- length(samples$b[i,,][samples$b[i,,]<0]) / length(samples$b[i,,])
	probPos[i] <- length(samples$b[i,,][samples$b[i,,]>0]) / length(samples$b[i,,])

}

hist(coefsBs)
res <- data.frame(colnames(X),coefsBs, coefsLow, coefsUp,probNeg,probPos)
colnames(res)[1] <- "var"
res

# Sort by probability of effect
res$prob <- apply(res[,c(5:6)],1,max)
res2 <- res[order(res$prob,decreasing=T),];res2


# SAVE POSTERIOR SAMPLES FOR PLOTTING --

post <- matrix(NA,nrow=10000,ncol=8)

for(i in 1:8){
	post[1: 10000,i] <- sample(samples$b[i,,], 10000,replace=F)
}

colnames(post) <- names(X)[1:8]
head(post);dim(post)

write.csv(post,"commonPostSamples.csv")



# --- RUN SAME MODEL FOR EVERYONE ELSE ---

# LOAD DATA --
load("allFourthForAnalyses.Rdata")
str(fourth)
names(fourth)

# Other
temp <- read.csv( "yearSites_MIN10_totalInds_global_var10_other_zYear.csv" )

head(temp)
temp$probChange <- apply(temp[,c(5:6)],1,FUN=max) 
colnames(temp)[1] <- "sites"
abun <- temp


# PREP PREDICTORS --
sites <- fourth$sites
head(sites)
dim(sites)

# Add an average annual temp
sites$temp_ann <- (sites$tmax_ann + sites$tmin_ann) / 2

# Transformations

sites$urb16_25mi <- log(sites$urb16_25mi + 0.0001)
sites$crop19_25mi <- log(sites$crop19_25mi+0.0001)
sites$ppt_fal <- log(sites$ppt_fal)
sites$ppt_spr <- log(sites$ppt_spr)
sites$ppt_sum <- log(sites$ppt_sum)
sites$ppt_win <- log(sites$ppt_win)
sites$ppt_ann <- log(sites$ppt_ann)

new <- merge(abun, sites,by="sites")
head(new)

# Generate mems

head(new)
latlongs <- new[,c(8:9)]
head(latlongs)

mems <- dbmem(latlongs, thresh=NULL, MEM.autocor="positive")
saveMEMs <- data.frame(mems)

X <- new[,c(12,21,43,61,54,52,47,49)] 
head(X)

# Scale and add mems
X <- scale(X)
X <- data.frame(X,saveMEMs[,c(2,4,12,13,5)])
head(X)

data.frame(colnames(X))
Y <- new$median
regData <- list(y=Y, x=X,N=length(Y),Nb=ncol(X))


# MODEL --
model <- "model{
	for(i in 1:N){

		y[i] ~ dnorm(mu[i],tau)
		mu[i] <- b0 + inprod(x[i,], b)   

        ## posterior predictive check stuff
        #### generate ynew for posterior pedictive check
        ynew[i] ~ dnorm(mu[i],tau)
        ##### examine residuals
        resid[i]<- y[i] - ynew[i] 
	}
	
	## priors on intercept & precision
	tau ~ dgamma(0.1,0.1)
	b0 ~ dnorm(0,0.00001)
	
	for(j in 1:Nb){

		b[j] ~ dnorm(0,0.0001) 
	}
}
"
regModel<-jags.model(textConnection(model),data= regData,n.chains=2) 
samples<-jags.samples(model= regModel,variable.names=c("b0","b","ynew","resid"),n.iter=100000)
str(samples)


# SUMMARIZE --
numVar <- dim(X)[2]
coefsBs <- NA
coefsUp <- NA
coefsLow <- NA
probNeg <- NA
probPos <- NA

for(i in 1:numVar){
  
	coefsBs[i] <- median(samples$b[i,,])

	coefsUp[i] <- hdi(samples$b[i,,],0.95)[2,1]
	coefsLow[i] <- hdi(samples$b[i,,],0.95)[1,1]

	probNeg[i] <- length(samples$b[i,,][samples$b[i,,]<0]) / length(samples$b[i,,])
	probPos[i] <- length(samples$b[i,,][samples$b[i,,]>0]) / length(samples$b[i,,])

}

hist(coefsBs)
res <- data.frame(colnames(X),coefsBs, coefsLow, coefsUp,probNeg,probPos)
colnames(res)[1] <- "var"
res

# Sort by probability of effect
res$prob <- apply(res[,c(5:6)],1,max)
res2 <- res[order(res$prob,decreasing=T),];res2


# SAVE POSTERIOR SAMPLES FOR PLOTTING --
str(samples)
head(X)

post <- matrix(NA,nrow=10000,ncol=8)

for(i in 1:8){
	post[1: 10000,i] <- sample(samples$b[i,,], 10000,replace=F)
}

colnames(post) <- names(X)[1:8]
head(post);dim(post)

write.csv(post,"otherPostSamples.csv")




