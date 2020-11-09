library(lubridate)
library(maps)




###############
###############
## Step 2 of 2 of main NABA data prep.
###############
###############




###########
#### Bring in output of the previous data prep (step 1 of 1).

dat <- read.csv("fourthData.csv")[,-1]
head(dat)




########
## Only keep species x site combinations with 5 more years.

cutRep <- 4

## make sp x site label
dat$spSite <- paste(dat$sp,dat$site,sep="_")
head(dat);dim(dat)

## count num observations (years) for each sp by site combo
tally <- aggregate(count ~ sp + site, data=dat, FUN="length")
head(tally)

## implement cutoff
tally2 <- tally[tally$count > cutRep,]
tally2$spSite <- paste(tally2$sp,tally2$site,sep="_")
head(tally2)

dat <- droplevels(dat[dat$spSite %in% tally2$spSite,])
dim(dat)
head(dat)

## resort
dat <- dat[order(dat[,1],dat[,2],dat[,3]),]






########
## Create a key of how many sites species are seen at.

## count number of observations by name and site
head(dat);dim(dat)
tally4 <- aggregate(count ~ sp + site, FUN="length", data=dat)
## count number of sites per sp
tally5 <- aggregate(count ~ sp, FUN="length",data=tally4)
colnames(tally5)[2] <- "presence"
head(tally5)
presenceKey <- tally5








##############
## interpolate zeros


## first make visit key (which years had data at which sites and visit covariates)

head(dat)
dat$key <- paste(dat$site,dat$year,dat$date,dat$hours,sep="_")
key2 <- unique(dat$key)
key2
site <- do.call(rbind,strsplit(as.character(key2),"_"))[,1]
year <- do.call(rbind,strsplit(as.character(key2),"_"))[,2]
date <- do.call(rbind,strsplit(as.character(key2),"_"))[,3]
hours <- do.call(rbind,strsplit(as.character(key2),"_"))[,4]
visitKey <- data.frame(site,year,date,hours)
head(visitKey)
visitKey <- visitKey[order(visitKey[,1],visitKey[,2]),]


##### all sp zero interpolation

sps <- levels(dat$sp)

spV <- NA
yearsV <- NA
countV <- NA
siteV <- NA
hoursV <- NA
dateV <- NA
growthRateV <- NA
tMinusV <- NA
for(i in 1:length(sps)){
	
	currentSp <- sps[i]
	subSp <- droplevels(dat[dat$sp== currentSp,])
	
	sites <- levels(subSp$site)
	
	#failing on i=2
	
	for(j in 1:length(sites)){
		
		sub1 <- droplevels(subSp[subSp$site == sites[j],])
		sub2 <- sub1[,c(3,8)]

		keySub <- droplevels(visitKey[visitKey$site == sites[j],])

		sub4 <- merge(keySub,sub2,by="year",all=T)
		sub4[is.na(sub4)] <- 0

		spV <- append(spV,rep(currentSp,length(sub4$year)))
		yearsV <- append(yearsV, as.numeric(as.character(sub4$year)))
		countV <- append(countV, as.numeric(as.character(sub4$count)))
		siteV <- append(siteV, as.character(sub4$site))
		hoursV <- append(hoursV, as.numeric(as.character(sub4$hours)))
		dateV <- append(dateV, as.numeric(as.character(sub4$date)))	
		
		C1 <- sub4$count
		C1[C1==0] <- 1
		L1 <- length(C1)
		t <- C1[-1]
		tMinus <- C1[-L1]
		r <- log(t/tMinus)
		growthRateV <- append(growthRateV,c(NA,r))
		tMinusV <- append(tMinusV,c(NA,log(tMinus)))
		
		print(j)
	}
	print(i)
}

res <- data.frame( spV, siteV, yearsV, dateV, countV, hoursV,growthRateV,tMinusV)[-1,]
colnames(res) <- c("sp","site","year","date","count","hours","r","tMinus")
head(res,50)
dim(res)






#############
## make a key of num of years per site

head(res)
tally1 <- res
tally1$label <- paste(tally1$site,tally1$year,sep="_")
head(tally1)
tally2 <- unique(tally1$label)

site <- do.call(rbind,strsplit(as.character(tally2),"_"))[,1]
year <- do.call(rbind,strsplit(as.character(tally2),"_"))[,2]
tally3 <- data.frame(site,year)
head(tally3)
siteKey <- aggregate(year ~ site, FUN=length, data=tally3)
head(siteKey)





##############
## add keys back to main table

head(presenceKey)
head(siteKey)
head(res);dim(res)
final <- merge(res,presenceKey,by="sp")
head(final);dim(final)
final2 <- merge(final,siteKey,by="site")
head(final2);dim(final2)
final3 <- final2[,c(2,1,3,5,7,8,6,4,9,10)]
head(final3)
colnames(final3) <- c("sp","site","year","count","r","tM","hours","date","sitesPerSp","yearsPerSite")
final4 <- final3[order(final3[,1],final3[,2],final3[,3]),]
head(final4)





## add a column for sp x site x year
temp <- final4
head(temp)
temp$PA <- temp$count
temp$PA[temp$PA > 0] <- 1
temp2 <- aggregate(PA ~ sp + site, data=temp, FUN="sum")
head(temp2)
temp2$label <- paste(temp2$sp,temp2$site,sep="_")

final4$label <- paste(final4$sp,final4$site,sep="_")
final4 <- merge(final4,temp2,by="label")
head(final4,50)
final4 <- final4[,-c(12:13)]
colnames(final4)[2] <- "sp"
colnames(final4)[3] <- "site"
colnames(final4)[12] <- "yearsPerSitePerSp"


write.csv(final4,"fourthAllSp.csv",row.names=F)





