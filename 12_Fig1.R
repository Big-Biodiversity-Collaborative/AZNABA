library(maps)
library(car)
library(plotrix)





########## FIG 1



quartz(width=6,height=7.75)


par(mfrow=c(3,1))
par(oma=c(10,10,0,20))

pal <- colorRampPalette(c("red","orange","yellow","green","blue"))(15)
breaks <- c(250,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000)




##########################################################
##########################################################

## NorCal map



## site and color vectors
sites <- c("Suisun Marsh","Gates Canyon","West Sacramento","North Sacramento","Rancho Cordova","Washington","Lang Crossing","Donner Pass","Castle Peak", "Sierra Valley")

elevs <- c(0.5,395,9,8,18,1025,1600,2100,2587,1500)


cols <- rep(NA,10)
cols[which(elevs <= breaks[1])] <- pal[1]
cols[which(elevs > breaks[1] & elevs < breaks[2] )]<- pal[2]
cols[which(elevs >= breaks[2] & elevs < breaks[3] )]<- pal[3]
cols[which(elevs >= breaks[3] & elevs < breaks[4] )]<- pal[4]
cols[which(elevs >= breaks[4] & elevs < breaks[5] )]<- pal[5]
cols[which(elevs >= breaks[5] & elevs < breaks[6])]<- pal[6]
cols[which(elevs >= breaks[6] & elevs < breaks[7])]<- pal[7]
cols[which(elevs >= breaks[7] & elevs < breaks[8])]<- pal[8]
cols[which(elevs >= breaks[8] & elevs < breaks[9])]<- pal[9]
cols[which(elevs >= breaks[9] & elevs < breaks[10])]<- pal[10]
cols[which(elevs >= breaks[10] & elevs < breaks[11])]<- pal[11]
cols[which(elevs >= breaks[11] & elevs < breaks[12])]<- pal[12]
cols[which(elevs >= breaks[12] & elevs < breaks[13])]<- pal[13]
cols[which(elevs >= breaks[13] & elevs < breaks[14])]<- pal[14]
cols[which(elevs >= breaks[14] & elevs < breaks[15])]<- pal[15]


x1 <- -123.3; x2 <- -119.4; y1 <- 37; y2 <- 40.3

par(mar=c(3,1.5,3,2.5))
plot(seq(x1,x2,length=10),seq(y1,y2,length=10),type="n",axes=F,xlab="",ylab="")
gray1 <- "gray98"
map("state","California", xlim=c(x1,x2),ylim=c(y1,y2),fill=T,col=gray1,add=TRUE)
map("state","Nevada", xlim=c(x1,x2),ylim=c(y1,y2),fill=T,col=gray1, add=TRUE)
box(col="gray30")
axis(1,labels=F)
axis(1,tick=F,labels=c(-123,-122,-121,-120),at=c(-123,-122,-121,-120),cex.axis=0.75,line=-0.5)
axis(4,labels=F)
axis(4,tick=F,at=c(37,38,39,40),cex.axis=0.8,line=-0.45)

arrows(-121,37.25,-119.5,37.25,length=0.03,angle=90,code=3,lwd=1.5,lty=1)
text(-120.28,37.48,"150km",cex=0.95)
arrows(-123,39.5,-123,40.2,code=2,length=0.08,lwd=0.8)
text(-122.7,39.65,"N")

sitePoints <- read.csv("siteData.csv",header=T)
head(sitePoints)
siz <- 2
points(sitePoints$Longitude[1],sitePoints$Latitude[1],pch=21,bg=cols[1],col="black",lwd=0.75,cex=siz) #sm
points(sitePoints$Longitude[5],sitePoints$Latitude[5],pch=21,bg=cols[2],col="black",lwd=0.75,cex=siz) #gc
points(sitePoints$Longitude[3],sitePoints$Latitude[3],pch=21,bg=cols[3],col="black",lwd=0.75,cex=siz) #ws
points(sitePoints$Longitude[2]+0.08,sitePoints$Latitude[2],pch=21,bg=cols[4],col="black",lwd=0.75,cex=siz) #ns
points(sitePoints$Longitude[4]+0.1,sitePoints$Latitude[4]+0.05,pch=21,bg=cols[5],col="black",lwd=0.75,cex=siz) #rc
points(sitePoints$Longitude[6]-0.08,sitePoints$Latitude[6]-0.07,pch=21,bg=cols[6],col="black",lwd=0.75,cex=siz) #wa
points(sitePoints$Longitude[8],sitePoints$Latitude[8],pch=21,bg=cols[7],col="black",lwd=0.75,cex=siz) #lc
points(sitePoints$Longitude[9]-0.08,sitePoints$Latitude[9],pch=21,bg=cols[8],col="black",lwd=0.75,cex=siz) #dp
points(sitePoints$Longitude[10]+0.06,sitePoints$Latitude[10],pch=21,bg=cols[9],col="black",lwd=0.75,cex=siz) #cp
points(sitePoints$Longitude[7],sitePoints$Latitude[7],pch=21,bg=cols[10],col="black",lwd=0.75,cex=siz) #sv



##########################################################
##########################################################
par(mar=c(0,0,0,0))

## NABA map


map("state",region=c("california","nevada","utah","oregon","idaho","wyoming",
	"colorado","montana","washington","arizona", "new mexico"),interior=T,fill=T,col="gray99",mar=c(1,0.5,1,1))

dat <- read.csv("fourth_elevs.csv")
head(dat)

abun2 <- dat[order(dat$elevation),]

abun2$cols <- NA
abun2$cols[which(abun2$elevation <= breaks[1])] <- pal[1]
abun2$cols[which(abun2$elevation > breaks[1] & abun2$elevation < breaks[2] )]<- pal[2]
abun2$cols[which(abun2$elevation >= breaks[2] & abun2$elevation < breaks[3] )]<- pal[3]
abun2$cols[which(abun2$elevation >= breaks[3] & abun2$elevation < breaks[4] )]<- pal[4]
abun2$cols[which(abun2$elevation >= breaks[4] & abun2$elevation < breaks[5] )]<- pal[5]
abun2$cols[which(abun2$elevation >= breaks[5] & abun2$elevation < breaks[6])]<- pal[6]
abun2$cols[which(abun2$elevation >= breaks[6] & abun2$elevation < breaks[7])]<- pal[7]
abun2$cols[which(abun2$elevation >= breaks[7] & abun2$elevation < breaks[8])]<- pal[8]
abun2$cols[which(abun2$elevation >= breaks[8] & abun2$elevation < breaks[9])]<- pal[9]
abun2$cols[which(abun2$elevation >= breaks[9] & abun2$elevation < breaks[10])]<- pal[10]
abun2$cols[which(abun2$elevation >= breaks[10] & abun2$elevation < breaks[11])]<- pal[11]
abun2$cols[which(abun2$elevation >= breaks[11] & abun2$elevation < breaks[12])]<- pal[12]
abun2$cols[which(abun2$elevation >= breaks[12] & abun2$elevation < breaks[13])]<- pal[13]
abun2$cols[which(abun2$elevation >= breaks[13] & abun2$elevation < breaks[14])]<- pal[14]
abun2$cols[which(abun2$elevation >= breaks[14] & abun2$elevation < breaks[15])]<- pal[15]

points(abun2$long, abun2$lat,pch=1,col="gray20",cex=1.2)
points(abun2$long, abun2$lat,pch=19, col=abun2$cols)




##########################################################
##########################################################


## iNat map

map("state",region=c("california","nevada","utah","oregon","idaho","wyoming",
	"colorado","montana","washington","arizona", "new mexico"),interior=T,fill=T,col="gray99",mar=c(1,0.5,1,1))

dat <- read.csv("inat_elevs.csv")
head(dat);dim(dat)

#### gotta filter those suckers to be 2005 and after
## bring in record dates 
tempDat <- read.csv("butterfly_occ_export.csv")
head(tempDat);dim(tempDat)
dat$year <- as.numeric(do.call(rbind,strsplit(as.character(tempDat$eventDat),"-"))[,1])
head(dat)
dat <- dat[dat$year >2004,]
dim(dat)
dat <- dat[,-4]


abun2 <- dat[order(dat$elev),]

abun2$cols <- NA
abun2$cols[which(abun2$elev <= breaks[1])] <- pal[1]
abun2$cols[which(abun2$elev > breaks[1] & abun2$elev < breaks[2] )]<- pal[2]
abun2$cols[which(abun2$elev >= breaks[2] & abun2$elev < breaks[3] )]<- pal[3]
abun2$cols[which(abun2$elev >= breaks[3] & abun2$elev < breaks[4] )]<- pal[4]
abun2$cols[which(abun2$elev >= breaks[4] & abun2$elev < breaks[5] )]<- pal[5]
abun2$cols[which(abun2$elev >= breaks[5] & abun2$elev < breaks[6])]<- pal[6]
abun2$cols[which(abun2$elev >= breaks[6] & abun2$elev < breaks[7])]<- pal[7]
abun2$cols[which(abun2$elev >= breaks[7] & abun2$elev < breaks[8])]<- pal[8]
abun2$cols[which(abun2$elev >= breaks[8] & abun2$elev < breaks[9])]<- pal[9]
abun2$cols[which(abun2$elev >= breaks[9] & abun2$elev < breaks[10])]<- pal[10]
abun2$cols[which(abun2$elev >= breaks[10] & abun2$elev < breaks[11])]<- pal[11]
abun2$cols[which(abun2$elev >= breaks[11] & abun2$elev < breaks[12])]<- pal[12]
abun2$cols[which(abun2$elev >= breaks[12] & abun2$elev < breaks[13])]<- pal[13]
abun2$cols[which(abun2$elev >= breaks[13] & abun2$elev < breaks[14])]<- pal[14]
abun2$cols[which(abun2$elev >= breaks[14] & abun2$elev < breaks[15])]<- pal[15]


head(abun2)
points(abun2$lon, abun2$lat, col=abun2$cols)






##########################################################
##########################################################
## NorCal histogram


load("allFourthForAnalyses.Rdata")

#par(fig=c(0.355,0.65,0.15,0.94),new=T,mar=c(0,0,0,0),oma=c(0,0,0,0)) #x1,x2,y1,y2 

v <- c(0.65,0.9,0.79,0.94) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))


xmin <- -0.35
xmax <- 0.35
red <- "gray60"
blue <- "gray98"


h <- hist(fourth$art$medianArt,breaks=15,plot=FALSE)
where <- which(h$breaks==0)
cols <- data.frame(h$breaks,c(rep(red,where-1),rep(blue,length(h$breaks)-(where-1))))
colnames(cols)[2] <- "cols"
plot(h,col=as.character(cols$cols),density=90,xlim=c(xmin,xmax), main="",las=2,xaxt="n");axis(1)
t.test(fourth$art$medianArt,mu=0)
mtext("Change (Binomial year coef.)",1,cex=0.65,line=2)
mtext("Species",2,cex=0.75,line=2.3)






##########################################################
##########################################################
## NABA histogram

v <- c(0.65,0.9,0.515,0.67) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))


h <- hist(fourth$pois4th$median,breaks=20,plot=FALSE)
where <- which(h$breaks==0)
cols <- data.frame(h$breaks,c(rep(red,where-1),rep(blue,length(h$breaks)-(where-1))))
colnames(cols)[2] <- "cols"
plot(h,col=as.character(cols$cols),density=90,xlim=c(xmin,xmax),main="",las=2,xaxt="n");axis(1)
t.test(fourth$pois4th$median,mu=0)
mtext("Change (Poisson year coef.)",1,cex=0.65,line=2)
mtext("Species",2,cex=0.75,line=2.3)





##########################################################
##########################################################
## iNat histogram



v <- c(0.65,0.9,0.25,0.40) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))


#h <- hist(fourth$inat$medianNat,breaks=10,plot=FALSE)
h <- hist(fourth$inat2005$medianNat,breaks=20,plot=FALSE)
#h <- hist(fourth$inat2008$medianNat,breaks=10,plot=FALSE)
where <- which(h$breaks==0)
cols <- data.frame(h$breaks,c(rep(red,where-1),rep(blue,length(h$breaks)-(where-1))))
colnames(cols)[2] <- "cols"
plot(h,col=as.character(cols$cols),density=90,xlim=c(xmin,xmax),main="")
t.test(fourth$inat$medianNat,mu=0)
mtext("Change (Guassian year coef.)",1,cex=0.65,line=2)
mtext("Species",2,cex=0.75,line=2.3)








##########################################################
##########################################################
## legend



v <- c(0.33,0.53,0.0362,0.26) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(0:16,0:16, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',ylim=c(0,2))

pal <- colorRampPalette(c("red","orange","yellow","green","blue"))(15)
breaks <- c(250,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000)

elevs <- breaks-100


cols <- rep(NA,length(elevs))
cols[which(elevs <= breaks[1])] <- pal[1]
cols[which(elevs > breaks[1] & elevs < breaks[2] )]<- pal[2]
cols[which(elevs >= breaks[2] & elevs < breaks[3] )]<- pal[3]
cols[which(elevs >= breaks[3] & elevs < breaks[4] )]<- pal[4]
cols[which(elevs >= breaks[4] & elevs < breaks[5] )]<- pal[5]
cols[which(elevs >= breaks[5] & elevs < breaks[6])]<- pal[6]
cols[which(elevs >= breaks[6] & elevs < breaks[7])]<- pal[7]
cols[which(elevs >= breaks[7] & elevs < breaks[8])]<- pal[8]
cols[which(elevs >= breaks[8] & elevs < breaks[9])]<- pal[9]
cols[which(elevs >= breaks[9] & elevs < breaks[10])]<- pal[10]
cols[which(elevs >= breaks[10] & elevs < breaks[11])]<- pal[11]
cols[which(elevs >= breaks[11] & elevs < breaks[12])]<- pal[12]
cols[which(elevs >= breaks[12] & elevs < breaks[13])]<- pal[13]
cols[which(elevs >= breaks[13] & elevs < breaks[14])]<- pal[14]
cols[which(elevs >= breaks[14] & elevs < breaks[15])]<- pal[15]
cols

#points(1:15,y=rep(1,15),col=cols,pch=15,cex=2)
points(1:15,y=rep(1.1,15),col=cols,pch=15,cex=2)
#points(1:15,y=rep(0.8,15),col=cols,pch=15,cex=2)




##########################################################
##########################################################
## labels


#################
## manual venn (see vennDiagram.R for numbers)

v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))

plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

cirText <- 1


text(23,100,"A",cex=cirText+0.25,font=2)
text(29,99.5,"Shapiro",cex=cirText+0.15)

text(23,73.5,"B",cex=cirText+0.25,font=2)
text(29.5,73.5,"NABA",cex=cirText+0.15)

text(23,44,"C",cex=cirText+0.25,font=2)
text(28.5,44,"iNat",cex=cirText+0.15)

text(60,98,"D",cex=cirText+0.25,font=2)
#text(69,96.75,"Shapiro",cex=cirText+0.15)

text(60,70,"E",cex=cirText+0.25,font=2)
#text(69,69,"NABA",cex=cirText+0.15)

text(60,41,"F",cex=cirText+0.25,font=2)
#text(68,51.5,"iNat",cex=cirText+0.15)

#text(59,29.5,"G",cex=cirText+0.5,font=2)

#text(71,21.5,"H",cex=cirText+0.5,font=2)


breaks <- c(250,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000)
text(34,12,"0m")
text(43,12,"2000m")
text(53,12,"4000m")


##########################################################
##########################################################


