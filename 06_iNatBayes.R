library(rjags)
library(coda)
library(HDInterval)



################
## This code runs the Bayesian linear models for iNaturalist data.
################

dat <- read.csv("effort_scaled_counts_ALL.csv")
levels(dat$species)
head(dat);dim(dat)


## Add Icaricia acmon, which had been left out of an early version of data because of how data was stored.
IcAc <- read.csv("iNatUpdate.csv")
head(IcAc)
dat <- rbind(dat,IcAc)


## Only keep years from 2005 on.
dat <- dat[dat$year > 2004,]


## Change the name of that scaled response column.
colnames(dat)[5] <- "scaleCounts"
head(dat)


#### Only keep species with at least 10 years of data.

## first make a tally
tally <- aggregate(dat$counts ~ dat$species, FUN="length", data=dat)
colnames(tally) <- c("species","years")
head(tally)

## merge and drop
new <- merge(dat,tally,by="species")
head(new);dim(new)
new2 <- droplevels(new[new$years > 9,]) 
dat <- new2
dim(dat);head(dat)
length(levels(dat$species))

## put in alphabetical and (within species) year order (Icaricia acmon comes out last, out of order, because it was added to the factor list after the other species)
dat <- dat[order(dat$species,dat$year),]



## Center the year column for use in regressions.
## (Note that it's not a full z transform even though that's what the column gets called.)
dat$zYear <- dat$year -  mean(dat$year)
head(dat)



############
############
## Loop through species and run a simple Bayesian linear regression each time;
## saving elements that include year coefficients and values for posterior predictive checks.
############
############


head(dat)
species <- levels(dat$species);length(species)

medYear <- NA
probNeg <- NA
probPos <- NA
gelRub <- NA
up <- NA
low <- NA
predCorr <- NA
for(j in 1:length(species)){
	
	sub <- droplevels(dat[dat$species == species[j],])
	basicData<-list(counts= logit(sub$scaleCounts), year= sub$zYear,N=length(sub$zYear))
                 
	model<-"model{
		for(i in 1:N){
	    	  counts[i] ~ dnorm(mu[i], tau)
	      		mu[i] <- int + beta1*year[i]		
	
		## posterior predictive
		ynew[i] ~ dnorm(mu[i], tau)
		resid[i] <- counts[i] - mu[i]
		
		}
		tau ~ dgamma(0.1,0.1)
      	int ~ dnorm(0, 0.0001)  
      	beta1 ~ dnorm(0, 0.0001) #year
      	
		}"

	basicModel<-jags.model(textConnection(model),data=basicData,n.chains=2)  #compile
	samples<-jags.samples(model=basicModel,variable.names=c("beta1","ynew"), n.adapt=1000,n.iter=15000)

	gelRub[j] <- gelman.diag(mcmc.list(as.mcmc(samples$beta1[,,1]),as.mcmc(samples$beta1[,,2])),autoburnin = F)[[1]][1]
	medYear[j] <- round(quantile(samples$beta1[,,],probs=c(0.5,0.025,0.975)),6)[1]	
	posterior <- samples$beta1[,,]
	probPos[j] <- length(posterior[posterior > 0]) / length(posterior)
	probNeg[j] <- length(posterior[posterior < 0]) / length(posterior)
	
	up[j] <- hdi(samples$beta1[,,],0.95)[2,1]
	low[j] <- hdi(samples$beta1[,,],0.95)[1,1]
	
	ynewci<-apply(samples$ynew,1,quantile,probs=c(0.5,0.025,0.975))
	predCorr[j] <- cor(basicData$counts,ynewci[1,])	
}

res <- data.frame(species,medYear,probNeg,probPos,gelRub,up,low,predCorr)
head(res)
res[order(res$medYear),]

## Look at how many convergence diagnostics are greater than a smidge above 1.
res$gelRub[res$gelRub > 1.001]

write.csv(res,"iNat200slopes2005.csv",row.names=F)








