# ABOUT --

# Model to predict species-level rates of change 



# LOAD LIBRARIES --
library(car)
library(rjags)
library(HDInterval)



# LOAD DATA --
load("allFourthForAnalyses.Rdata")
str(fourth)
names(fourth)



# LOAD GROWTH RATES --
a1 <- merge(fourth$growthRate,fourth$rangesV2,by="sp")
a1$probChange <- apply(a1[,c(5:6)],1,FUN=max) 
head(a1)

# Merge with other data (predictors)
head(a1);dim(a1)
a1 <- merge(a1,fourth$hosts,by="sp")
a1 <- merge(a1,fourth$body,by="sp")
a1 <- merge(a1,fourth$base,by="sp")
a1 <- merge(a1,fourth$voltElev,by="sp")
head(a1);dim(a1)

# Exclude one outlier with an extreme growth rate value
hist(a1$median)
a1 <- a1[a1$median < 0.14,]

# Change broods to just two categories. [prob do this]
table(a1$broods)
a1$broods[a1$broods == 1] <- 0
a1$broods[a1$broods == 2] <- 1
a1$broods[a1$broods == 3] <- 1
head(a1)

# Transformations
a1$range <- log(a1$range)  
a1$numFam <- log(a1$numFam)
a1$size <- log(a1$size)
a1$baseAbun <- log(a1$baseAbun)



# BAYES --

head(a1);data.frame(names(a1))

a2 <- a1[,c(7,17,19,20,22)]  

a2 <- scale(a2)
head(a2)
a3 <- data.frame(a1$median,a2,a1$broods)
head(a3)
colnames(a3)[1] <- "median"
colnames(a3)[7] <- "broods"
head(a3);dim(a3)
a4 <- na.omit(a3)
head(a4);dim(a4)
# pairs.panels(a4)

Y <- a4$median
X <- a4[,-1]
head(X)
# cor(X)

regData <- list(y=Y, range=X$range, numFam=X$numFam, size=X$size, baseAbun=X$baseAbun, avgElev=X$avgElev,  broods=X$broods, N=length(Y))

model <- "model{

	for(i in 1:N){
	
		y[i] ~ dnorm(mu[i],tau)
	      mu[i] <- b0 + beta1*range[i] + beta2*numFam[i] + beta3*size[i] + beta4*baseAbun[i] + beta5*avgElev[i] + beta6*broods[i]

       ## posterior predictive check stuff
        #### generate ynew for posterior pedictive check
        ynew[i] ~ dnorm(mu[i],tau)
        ##### examine residuals
        resid[i]<- y[i] - ynew[i] 
	}
 
	## priors on intercept & precision
	tau ~ dgamma(0.1,0.1)

      b0 ~ dnorm(0, 0.0001) 
      beta1 ~ dnorm(0, 0.0001)  # range
      beta2 ~ dnorm(0, 0.0001)  # fams
      beta3 ~ dnorm(0, 0.0001)  # size
      beta4 ~ dnorm(0, 0.0001)  # abun
      beta5 ~ dnorm(0, 0.0001)  # elev
      beta6 ~ dnorm(0, 0.0001)  # broods
	
}
"

regModel<-jags.model(textConnection(model),data= regData,n.chains=2) 
samples<-jags.samples(model= regModel,variable.names=c("b0","beta1","beta2","beta3","beta4","beta5","beta6","ynew","resid"),n.iter=100000)
str(samples)

effectiveSize(samples$beta1[,,1]) 
gelman.diag(mcmc.list(as.mcmc(samples$beta1[,,1]),as.mcmc(samples$beta1[,,2]))) 
effectiveSize(samples$beta2[,,1]) 
gelman.diag(mcmc.list(as.mcmc(samples$beta2[,,1]),as.mcmc(samples$beta2[,,2]))) 
effectiveSize(samples$beta3[,,1]) 
gelman.diag(mcmc.list(as.mcmc(samples$beta3[,,1]),as.mcmc(samples$beta3[,,2]))) 
effectiveSize(samples$beta4[,,1]) 
gelman.diag(mcmc.list(as.mcmc(samples$beta4[,,1]),as.mcmc(samples$beta4[,,2]))) 
effectiveSize(samples$beta5[,,1]) 
gelman.diag(mcmc.list(as.mcmc(samples$beta5[,,1]),as.mcmc(samples$beta5[,,2]))) 
effectiveSize(samples$beta6[,,1]) 
gelman.diag(mcmc.list(as.mcmc(samples$beta6[,,1]),as.mcmc(samples$beta6[,,2]))) 

PPCynew<-data.frame(NA,nrow = length(Y),ncol = 3)
for(i in 1:length(Y)){
  PPCynew[i,]<-quantile(samples$ynew[i,,],probs=c(0.5,0.025,0.975))[1:3]
}
PPC<-cbind(Y,PPCynew)
colnames(PPC)<-c("Y","ynew","0.025","0.975")
head(PPC)
plot(PPC$Y ~ PPC$ynew)
cor(PPC$Y,PPC$ynew)
cor(PPC$Y,PPC$ynew)^2



# SUMMARIZE RESULTS FOR EACH PREDICTOR --

#####
vec <- samples$beta1[,,]
plot(density(vec));abline(v=0)
median <- median(vec)
low <- hdi(vec,0.95)[1,1]
up <- hdi(vec,0.95)[2,1]
temp1 <- length(vec[vec<0]) / length(vec)
temp2 <- length(vec[vec>0]) / length(vec)
prob <- max(c(temp1,temp2))
range <- data.frame(median,low,up,prob)
range

#####
vec <- samples$beta2[,,]
plot(density(vec));abline(v=0)
median <- median(vec)
low <- hdi(vec,0.95)[1,1]
up <- hdi(vec,0.95)[2,1]
temp1 <- length(vec[vec<0]) / length(vec)
temp2 <- length(vec[vec>0]) / length(vec)
prob <- max(c(temp1,temp2))
fams <- data.frame(median,low,up,prob)
fams

#####
vec <- samples$beta3[,,]
plot(density(vec));abline(v=0)
median <- median(vec)
low <- hdi(vec,0.95)[1,1]
up <- hdi(vec,0.95)[2,1]
temp1 <- length(vec[vec<0]) / length(vec)
temp2 <- length(vec[vec>0]) / length(vec)
prob <- max(c(temp1,temp2))
size <- data.frame(median,low,up,prob)
size

#####
vec <- samples$beta4[,,]
plot(density(vec));abline(v=0)
median <- median(vec)
low <- hdi(vec,0.95)[1,1]
up <- hdi(vec,0.95)[2,1]
temp1 <- length(vec[vec<0]) / length(vec)
temp2 <- length(vec[vec>0]) / length(vec)
prob <- max(c(temp1,temp2))
abun <- data.frame(median,low,up,prob)
abun

#####
vec <- samples$beta5[,,]
plot(density(vec));abline(v=0)
median <- median(vec)
low <- hdi(vec,0.95)[1,1]
up <- hdi(vec,0.95)[2,1]
temp1 <- length(vec[vec<0]) / length(vec)
temp2 <- length(vec[vec>0]) / length(vec)
prob <- max(c(temp1,temp2))
elev <- data.frame(median,low,up,prob)
elev

#####
vec <- samples$beta6[,,]
plot(density(vec));abline(v=0)
median <- median(vec)
low <- hdi(vec,0.95)[1,1]
up <- hdi(vec,0.95)[2,1]
temp1 <- length(vec[vec<0]) / length(vec)
temp2 <- length(vec[vec>0]) / length(vec)
prob <- max(c(temp1,temp2))
broods <- data.frame(median,low,up,prob)
broods

#####
resAll <- rbind(range,fams,size,abun,elev,broods)
vars <- c("range","fams","size","abun","elev","broods")
resAll <- data.frame(vars,resAll)
resAll[order(resAll$prob,decreasing=T),]




