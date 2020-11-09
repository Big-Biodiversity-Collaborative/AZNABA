
source("gsrFunction.R")


#############################################


#### load main result files
load("allFourthForAnalyses.Rdata")
names(fourth)


#############################################
## load raw data for plotting

fourthRaw <- read.csv("fourthAllSp.csv")
names(fourthRaw)


###############
art <- read.csv("paThrough2018.csv")[,-1]
head(art)
str(art)
art$species <- as.factor(art$species)



###############
inat <- read.csv("effort_scaled_counts_ALL.csv")
head(inat)

key <- read.csv("iNatToFourthKey.csv")
head(key)
inat$sp <- gsr(inat$species,as.vector(key$sp),as.vector(key$newNamForiNat))
head(inat)





########################################################
########################################################
########################################################
## time series plots
########################################################
########################################################
########################################################


quartz(width=7,height=4)



bug1 <- "Vanessa annabella"
bug2 <- "Satyrium californica"
bug3 <- "Lycaena xanthoides"
bug4 <- "Polites sabuleti"





nabaCol <- "orange"
artCol <- "light blue"
inatCol <- "green"

#lasPick <- 2
#yearOff <- -0.5  #-0.9 if lasPick = 1


par(mfrow=c(2,2))

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


#yMin <- -1.75
#yMax <- 2.5


topY1 <- 0.57
topY2 <- 0.87


################################################
################################################

par(mar=c(0,0,0,0))
par(oma=c(1,2,1,0.2))

#par(fig=c(0.05,0.35,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(0.01,0.19, topY1, topY2),new=T)  #fig goes: x1,x2,y1,y2 


bug <- bug1


############# 4th (plotting with average zs)

dat <- fourthRaw
head(dat)

dat2 <- dat[dat$sp == as.character(bug),]
dat3 <- droplevels(dat2[dat2$yearsPerSitePerSp >9,])
head(dat3)

year <- NA
z4th <- NA
sites <- levels(dat3$site)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	
	res <- glm(sub$count ~ sub$date + sub$hours,family="poisson")
	year <- append(year,sub$year)
	z4th <- append(z4th,scale(resid(res)))		
}
new <- data.frame(year,z4th)[-1,]
new2 <- aggregate(new$z4th ~ new$year, FUN="mean")


plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)

axis(1,labels=F,tck=-0.045,at=c(1980,1990,2000,2010))
shift1 <- 3.2
text(x=c(1980+shift1,1990+shift1,2000+shift1,2010+shift1), -0.95, labels = c("1980","1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)


mtext("Scaled pop. index",2,line=1.5,cex=0.7)
mtext(bug,3,font=3,cex=0.7,line=0.5,at=min(new2[,1]),adj=0)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=nabaCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")






################ Art

par(mar=c(0,0,0,0))

#par(fig=c(0.41,0.60,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(0.225,0.365, topY1, topY2),new=T)  #fig goes: x1,x2,y1,y2 


dat <- art
head(dat)
str(dat)
levels(dat$species)

dat2 <- droplevels(dat[dat$species == as.character(bug),])
head(dat2)


dat2 <- dat2[dat2$year>1987,]


## make tally
temp <- aggregate(dat2$pa ~ dat2$site_name + dat2$year, FUN="sum")
colnames(temp) <- c("site","year","paTally")
temp$paTally[temp$paTally > 0] <- 1
head(temp)
temp2 <- aggregate(temp$paTally ~ temp$site, FUN="sum")
colnames(temp2) <- c("site_name","tally")
head(temp2)
new <- merge(dat2,temp2,by="site_name")
head(new)


dat3 <- droplevels(new[new$tally >9,])
head(dat3)


year <- NA
zArt <- NA
sites <- levels(dat3$site_name)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	

	sub2 <- aggregate(sub$pa ~ sub$year, FUN="sum")
	colnames(sub2) <- c("year","pa")
	head(sub2)

	sub3 <- aggregate(sub$pa ~ sub$year, FUN="length")
	colnames(sub3) <- c("year","visits")
	head(sub3)
	
	sub2$FDP <- sub2$pa / sub3$visits
	sub2$zFDP <- scale(sub2$FDP)

	year <- append(year,sub2$year)
	zArt <- append(zArt,sub2$zFDP)		
}
new <- data.frame(year,zArt)[-1,]
new2 <- aggregate(new$zArt ~ new$year, FUN="mean")
plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white" )
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.05,at=c(1990,2000,2010))
shift1 <- 3.2
text(x=c(1990+shift1,2000+shift1,2010+shift1), -1.75, labels = c("1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)


#mtext("Nor. CA",2,line=2.5)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=artCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")





############### iNat

par(mar=c(0,0,0,0))

par(fig=c(0.39,0.485, topY1, topY2),new=T)  #fig goes: x1,x2,y1,y2 


dat <- inat
head(dat)
dat$year

dat2 <- droplevels(dat[dat$year > 1999,])

dat2 <- droplevels(dat2[dat2$sp == as.character(bug),])
head(dat2)

dat2$zCounts <- scale(log(dat2$effort_scaled_counts))

plot(dat2$zCounts ~ dat2$year,xlab="year",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(dat2$year)-1,max(dat2$year)+1), ylim=c((min(dat2$zCounts)*1.1),max(dat2$zCounts)*1.1) ,col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)

axis(1,labels=F,tck=-0.08,at=c(2005,2015))
shift1 <- 2.8
text(x=c(2005+shift1,2015+shift1), -2.25, labels = c("2005","2015"), srt = -45, pos = 1, xpd = NA,cex=0.8)

lines(dat2$year,dat2$zCounts,col="gray55",lwd=0.75)
points(dat2$year,dat2$zCounts,pch=19,col=inatCol,cex=0.75)
points(dat2$year,dat2$zCounts,pch=1,col="gray25",cex=1)
#lines(smooth.spline(dat2$year,dat2$zCounts,df=2),col="gray35")




#########################################################################################

offset <- 0.515

bug <- bug2



################################################
################################################

#par(mar=c(0,1,0,0))
#par(oma=c(1,1,1,1))

#par(fig=c(0.05,0.35,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(offset +0.01, offset +0.19, topY1, topY2),new=T)  #fig goes: x1,x2,y1,y2 





############# 4th (plotting with average zs)

dat <- fourthRaw
head(dat)

dat2 <- dat[dat$sp == as.character(bug),]
dat3 <- droplevels(dat2[dat2$yearsPerSitePerSp >9,])
head(dat3)

year <- NA
z4th <- NA
sites <- levels(dat3$site)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	
	res <- glm(sub$count ~ sub$date + sub$hours,family="poisson")
	year <- append(year,sub$year)
	z4th <- append(z4th,scale(resid(res)))		
}
new <- data.frame(year,z4th)[-1,]
new2 <- aggregate(new$z4th ~ new$year, FUN="mean")


plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.045,at=c(1980,1990,2000,2010))
shift1 <- 3.2
text(x=c(1980+shift1,1990+shift1,2000+shift1,2010+shift1), -1.05, labels = c("1980","1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)
#mtext("Scaled pop. index",2,line=1.5,cex=0.7)
mtext(bug,3,font=3,cex=0.7,line=0.5,at=min(new2[,1]),adj=0)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=nabaCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")






################ Art

par(mar=c(0,0,0,0))

#par(fig=c(0.41,0.60,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(offset +0.225,offset +0.365, topY1, topY2),new=T)  #fig goes: x1,x2,y1,y2 


dat <- art
head(dat)
str(dat)
levels(dat$species)

dat2 <- droplevels(dat[dat$species == as.character(bug),])
head(dat2)


dat2 <- dat2[dat2$year>1987,]


## make tally
temp <- aggregate(dat2$pa ~ dat2$site_name + dat2$year, FUN="sum")
colnames(temp) <- c("site","year","paTally")
temp$paTally[temp$paTally > 0] <- 1
head(temp)
temp2 <- aggregate(temp$paTally ~ temp$site, FUN="sum")
colnames(temp2) <- c("site_name","tally")
head(temp2)
new <- merge(dat2,temp2,by="site_name")
head(new)


dat3 <- droplevels(new[new$tally >9,])
head(dat3)


year <- NA
zArt <- NA
sites <- levels(dat3$site_name)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	

	sub2 <- aggregate(sub$pa ~ sub$year, FUN="sum")
	colnames(sub2) <- c("year","pa")
	head(sub2)

	sub3 <- aggregate(sub$pa ~ sub$year, FUN="length")
	colnames(sub3) <- c("year","visits")
	head(sub3)
	
	sub2$FDP <- sub2$pa / sub3$visits
	sub2$zFDP <- scale(sub2$FDP)

	year <- append(year,sub2$year)
	zArt <- append(zArt,sub2$zFDP)		
}
new <- data.frame(year,zArt)[-1,]
new2 <- aggregate(new$zArt ~ new$year, FUN="mean")
plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white" )
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.05,at=c(1990,2000,2010))
shift1 <- 3.5
text(x=c(1990+shift1,2000+shift1,2010+shift1), par("usr")[3] - .125, labels = c("1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)


#mtext("Nor. CA",2,line=2.5)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=artCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")





############### iNat

par(mar=c(0,0,0,0))

par(fig=c(offset +0.39,offset +0.485, topY1, topY2),new=T)  #fig goes: x1,x2,y1,y2 


dat <- inat
head(dat)
dat$year

dat2 <- droplevels(dat[dat$year > 1999,])

dat2 <- droplevels(dat2[dat2$sp == as.character(bug),])
head(dat2)

dat2$zCounts <- scale(log(dat2$effort_scaled_counts))

plot(dat2$zCounts ~ dat2$year,xlab="year",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(dat2$year)-1,max(dat2$year)+1), ylim=c((min(dat2$zCounts)*1.1),max(dat2$zCounts)*1.1) ,col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.08,at=c(2005,2015))
shift1 <- 2.4
text(x=c(2005+shift1,2015+shift1), par("usr")[3] - .29, labels = c("2005","2015"), srt = -45, pos = 1, xpd = NA,cex=0.8)


lines(dat2$year,dat2$zCounts,col="gray55",lwd=0.75)
points(dat2$year,dat2$zCounts,pch=19,col=inatCol,cex=0.75)
points(dat2$year,dat2$zCounts,pch=1,col="gray25",cex=1)
#lines(smooth.spline(dat2$year,dat2$zCounts,df=2),col="gray35")




#####################################################################################


bug <- bug3



################################################
################################################

par(mar=c(0,0,0,0))
par(oma=c(1,2,1,0.2))

#par(fig=c(0.05,0.35,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(0.01,0.19,0.1,0.4),new=T)  #fig goes: x1,x2,y1,y2 





############# 4th (plotting with average zs)

dat <- fourthRaw
head(dat)

dat2 <- dat[dat$sp == as.character(bug),]
dat3 <- droplevels(dat2[dat2$yearsPerSitePerSp >9,])
head(dat3)

year <- NA
z4th <- NA
sites <- levels(dat3$site)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	
	res <- glm(sub$count ~ sub$date + sub$hours,family="poisson")
	year <- append(year,sub$year)
	z4th <- append(z4th,scale(resid(res)))		
}
new <- data.frame(year,z4th)[-1,]
new2 <- aggregate(new$z4th ~ new$year, FUN="mean")


plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.05,at=c(1980,1990,2000,2010))
shift1 <- 3.2
text(x=c(1980+shift1,1990+shift1,2000+shift1,2010+shift1), -1.2, labels = c("1980","1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)
mtext("Scaled pop. index",2,line=1.5,cex=0.7)
mtext(bug,3,font=3,cex=0.7,line=0.5,at=min(new2[,1]),adj=0)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=nabaCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")






################ Art

par(mar=c(0,0,0,0))

#par(fig=c(0.41,0.60,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(0.225,0.365,0.1,0.4),new=T)  #fig goes: x1,x2,y1,y2 


dat <- art
head(dat)
str(dat)
levels(dat$species)

dat2 <- droplevels(dat[dat$species == as.character(bug),])
head(dat2)


dat2 <- dat2[dat2$year>1987,]


## make tally
temp <- aggregate(dat2$pa ~ dat2$site_name + dat2$year, FUN="sum")
colnames(temp) <- c("site","year","paTally")
temp$paTally[temp$paTally > 0] <- 1
head(temp)
temp2 <- aggregate(temp$paTally ~ temp$site, FUN="sum")
colnames(temp2) <- c("site_name","tally")
head(temp2)
new <- merge(dat2,temp2,by="site_name")
head(new)


dat3 <- droplevels(new[new$tally >9,])
head(dat3)


year <- NA
zArt <- NA
sites <- levels(dat3$site_name)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	

	sub2 <- aggregate(sub$pa ~ sub$year, FUN="sum")
	colnames(sub2) <- c("year","pa")
	head(sub2)

	sub3 <- aggregate(sub$pa ~ sub$year, FUN="length")
	colnames(sub3) <- c("year","visits")
	head(sub3)
	
	sub2$FDP <- sub2$pa / sub3$visits
	sub2$zFDP <- scale(sub2$FDP)

	year <- append(year,sub2$year)
	zArt <- append(zArt,sub2$zFDP)		
}
new <- data.frame(year,zArt)[-1,]
new2 <- aggregate(new$zArt ~ new$year, FUN="mean")
plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white" )
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.05,at=c(1990,2000,2010))
shift1 <- 3.2
text(x=c(1990+shift1,2000+shift1,2010+shift1), par("usr")[3] - .21, labels = c("1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)


#mtext("Nor. CA",2,line=2.5)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=artCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")





############### iNat

par(mar=c(0,0,0,0))

par(fig=c(0.39,0.485,0.1,0.4),new=T)  #fig goes: x1,x2,y1,y2 


dat <- inat
head(dat)
dat$year

dat2 <- droplevels(dat[dat$year > 1999,])

dat2 <- droplevels(dat2[dat2$sp == as.character(bug),])
head(dat2)

dat2$zCounts <- scale(log(dat2$effort_scaled_counts))

plot(dat2$zCounts ~ dat2$year,xlab="year",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(dat2$year)-1,max(dat2$year)+1), ylim=c((min(dat2$zCounts)*1.1),max(dat2$zCounts)*1.1) ,col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.08,at=c(2005,2015))
shift1 <- 2.6
text(x=c(2005+shift1,2015+shift1), par("usr")[3] - .28, labels = c("2005","2015"), srt = -45, pos = 1, xpd = NA,cex=0.8)


lines(dat2$year,dat2$zCounts,col="gray55",lwd=0.75)
points(dat2$year,dat2$zCounts,pch=19,col=inatCol,cex=0.75)
points(dat2$year,dat2$zCounts,pch=1,col="gray25",cex=1)
#lines(smooth.spline(dat2$year,dat2$zCounts,df=2),col="gray35")







#########################################################################################

offset <- 0.515

bug <- bug4




################################################
################################################

#par(mar=c(0,1,0,0))
#par(oma=c(1,1,1,1))

#par(fig=c(0.05,0.35,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(offset +0.01, offset +0.19,0.1,0.4),new=T)  #fig goes: x1,x2,y1,y2 





############# 4th (plotting with average zs)

dat <- fourthRaw
head(dat)

dat2 <- dat[dat$sp == as.character(bug),]
dat3 <- droplevels(dat2[dat2$yearsPerSitePerSp >9,])
head(dat3)

year <- NA
z4th <- NA
sites <- levels(dat3$site)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	
	res <- glm(sub$count ~ sub$date + sub$hours,family="poisson")
	year <- append(year,sub$year)
	z4th <- append(z4th,scale(resid(res)))		
}
new <- data.frame(year,z4th)[-1,]
new2 <- aggregate(new$z4th ~ new$year, FUN="mean")


plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.05,at=c(1980,1990,2000,2010))
shift1 <- 3
text(x=c(1980+shift1,1990+shift1,2000+shift1,2010+shift1), -1.5, labels = c("1980","1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)
#mtext("Scaled pop. index",2,line=1.5,cex=0.7)
mtext(bug,3,font=3,cex=0.7,line=0.5,at=min(new2[,1]),adj=0)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=nabaCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")






################ Art

par(mar=c(0,0,0,0))

#par(fig=c(0.41,0.60,0.6,0.98),new=T)  #fig goes: x1,x2,y1,y2 
par(fig=c(offset +0.225,offset +0.365,0.1,0.4),new=T)  #fig goes: x1,x2,y1,y2 


dat <- art
head(dat)
str(dat)
levels(dat$species)

dat2 <- droplevels(dat[dat$species == as.character(bug),])
head(dat2)


dat2 <- dat2[dat2$year>1987,]


## make tally
temp <- aggregate(dat2$pa ~ dat2$site_name + dat2$year, FUN="sum")
colnames(temp) <- c("site","year","paTally")
temp$paTally[temp$paTally > 0] <- 1
head(temp)
temp2 <- aggregate(temp$paTally ~ temp$site, FUN="sum")
colnames(temp2) <- c("site_name","tally")
head(temp2)
new <- merge(dat2,temp2,by="site_name")
head(new)


dat3 <- droplevels(new[new$tally >9,])
head(dat3)


year <- NA
zArt <- NA
sites <- levels(dat3$site_name)
for(i in 1:length(sites)){
	sub <- dat3[dat3$site == sites[i],]	

	sub2 <- aggregate(sub$pa ~ sub$year, FUN="sum")
	colnames(sub2) <- c("year","pa")
	head(sub2)

	sub3 <- aggregate(sub$pa ~ sub$year, FUN="length")
	colnames(sub3) <- c("year","visits")
	head(sub3)
	
	sub2$FDP <- sub2$pa / sub3$visits
	sub2$zFDP <- scale(sub2$FDP)

	year <- append(year,sub2$year)
	zArt <- append(zArt,sub2$zFDP)		
}
new <- data.frame(year,zArt)[-1,]
new2 <- aggregate(new$zArt ~ new$year, FUN="mean")
plot(new2[,2] ~ new2[,1],xlab="",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(new2[,1])-1,max(new2[,1])+1), ylim=c((min(new2[,2])*1.1),max(new2[,2])*1.1),col="white" )
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.05,at=c(1990,2000,2010))
shift1 <- 3
text(x=c(1990+shift1,2000+shift1,2010+shift1), par("usr")[3] - .25, labels = c("1990","2000","2010"), srt = -45, pos = 1, xpd = NA,cex=0.8)


#mtext("Nor. CA",2,line=2.5)
lines(new2[,1],new2[,2],col="gray55",lwd=0.75)
points(new2[,1],new2[,2],pch=19,col=artCol,cex=0.75)
points(new2[,1],new2[,2],pch=1,col="gray25",cex=1)
#lines(smooth.spline(new2[,1],new2[,2],df=2),col="gray35")





############### iNat

par(mar=c(0,0,0,0))

par(fig=c(offset +0.39,offset +0.485,0.1,0.4),new=T)  #fig goes: x1,x2,y1,y2 


dat <- inat
head(dat)
dat$year

dat2 <- droplevels(dat[dat$year > 1999,])

dat2 <- droplevels(dat2[dat2$sp == as.character(bug),])
head(dat2)

dat2$zCounts <- scale(log(dat2$effort_scaled_counts))

plot(dat2$zCounts ~ dat2$year,xlab="year",yaxt="n",xaxt="n",cex=0.75,xlim=c(min(dat2$year)-1,max(dat2$year)+1), ylim=c((min(dat2$zCounts)*1.1),max(dat2$zCounts)*1.1) ,col="white")
axis(2,labels=F,tck=-0.05);axis(2,lwd=0,line=-0.6,las=2,cex.axis=0.6)
axis(1,labels=F,tck=-0.08,at=c(2005,2015))
shift1 <- 2.25
text(x=c(2005+shift1,2015+shift1), par("usr")[3] - .39, labels = c("2005","2015"), srt = -45, pos = 1, xpd = NA,cex=0.8)


lines(dat2$year,dat2$zCounts,col="gray55",lwd=0.75)
points(dat2$year,dat2$zCounts,pch=19,col=inatCol,cex=0.75)
points(dat2$year,dat2$zCounts,pch=1,col="gray25",cex=1)
#lines(smooth.spline(dat2$year,dat2$zCounts,df=2),col="gray35")



#####################


v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


text(2.75,91,"J",cex=1.1,font=2)
text(54.75,91,"K",cex=1.1,font=2)
text(2.75,45,"L",cex=1.1,font=2)
text(54.75,45,"M",cex=1.1,font=2)








