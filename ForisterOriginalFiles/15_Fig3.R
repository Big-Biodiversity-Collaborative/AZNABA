




####################################################
##### fig 3
####################################################




quartz(width=6,height=6.35)
par(mfrow=c(4,2),mar=c(2,0,2,3),oma=c(3,1,1,1))
layout(matrix(c(1,2,3,1,4,5,1,6,7,1,8,9),4,3,byrow=T))


pal <- colorRampPalette(c("red","orange","yellow","green","blue"))(15)
breaks <- c(250,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,4000)





load("allFourthForAnalyses.Rdata")
abun <- fourth$abun
head(abun);dim(abun)


sites <- fourth$sites
head(sites);dim(sites)


new <- merge(abun, sites,by="sites")
head(new)

new2 <- new[,c(1:9,30)]
head(new2)
new3 <- new2[order(new2$lat,decreasing=T),]
head(new3)
new3$order <- c(length(new3$lat):1)


abun2 <- new3[order(new3$elevation),]

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
head(abun2)


plot(exp(abun2$median), abun2$order, pch=1, col="black",cex=1.5,xlim=c(0.77, 1.175),yaxt="n")
linCol <- "gray70"
abline(v=1,lty=2,col="black")
abline(v=0.9,lty=2,col=linCol)
abline(v=0.8,lty=2,col=linCol)
abline(v=1.1,lty=2,col=linCol)

arrows(exp(abun2$low),abun2$order,exp(abun2$up),abun2$order,length=0)
points(exp(abun2$median), abun2$order, pch=19, col=abun2$cols)
mtext("Change in butterflies observed\n(exp. Poisson year coef.)",1,cex=0.75,line=3.2)






####################################################
####################################################

par(mar=c(2,1,2,2))



all1 <- read.csv("allPostSamples.csv")[,-1]
head(all1);dim(all1)

com1 <- read.csv("commonPostSamples.csv")[,-1]
head(com1);dim(com1)

other1 <- read.csv("otherPostSamples.csv")[,-1]
head(other1);dim(other1)




textSize <- 0.8
textLine <- 2
otherColor <- "black"
commonColor <- "black"
allColor <- "gray80"
bigText <- 1.2
smallText <- 1
yBig <- 1
xBig <- 0.16
xSmall <- 0.88
ySmall <- 1.025
ySmall2 <- 0.805
textCol <- "gray30"
textCol2 <- "gray20"





v1 <- density(all1$temp_ann)
v2 <- density(com1$temp_ann)
v3 <- density(other1$temp_ann)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Annual temperature",1,line= textLine,cex= textSize)
mtext("Density",2,cex=textSize-0.05,line = textLine-0.08)


p1V <- all1$temp_ann
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$temp_ann
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$temp_ann
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)







v1 <- density(all1$ppt_ann)
v2 <- density(com1$ppt_ann)
v3 <- density(other1$ppt_ann)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Annual precipitation",1,line= textLine,cex= textSize)


p1V <- all1$ppt_ann
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$ppt_ann
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$ppt_ann
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)






v1 <- density(all1$tmax_fal_slope)
v2 <- density(com1$tmax_fal_slope)
v3 <- density(other1$tmax_fal_slope)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Change in max. fall temp.",1,line= textLine,cex= textSize)
mtext("Density",2,cex=textSize-0.05,line = textLine-0.08)



p1V <- all1$tmax_fal_slope
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$tmax_fal_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$tmax_fal_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)




v1 <- density(all1$tmax_sum_slope)
v2 <- density(com1$tmax_sum_slope)
v3 <- density(other1$tmax_sum_slope)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Change in max. summ. temp.",1,line= textLine,cex= textSize)


p1V <- all1$tmax_sum_slope
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$tmax_sum_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$tmax_sum_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)






v1 <- density(all1$ppt_fal_slope)
v2 <- density(com1$ppt_fal_slope)
v3 <- density(other1$ppt_fal_slope)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Change in fall precipitation",1,line= textLine,cex= textSize)
mtext("Density",2,cex=textSize-0.05,line = textLine-0.08)



p1V <- all1$ppt_fal_slope
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$ppt_fal_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$ppt_fal_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)







v1 <- density(all1$ppt_sum_slope)
v2 <- density(com1$ppt_sum_slope)
v3 <- density(other1$ppt_sum_slope)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F,at=c(0,5,10,15,20,25)); 
axis(2,labels=T,lwd=0,line=-0.25,las=2,at=c(0,5,10,15,20,25))
abline(v=0,col="gray30")
mtext("Change in summ. precip.",1,line= textLine,cex= textSize)



p1V <- all1$ppt_sum_slope
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$ppt_sum_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$ppt_sum_slope
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)



v1 <- density(all1$urb16_25mi)
v2 <- density(com1$urb16_25mi)
v3 <- density(other1$urb16_25mi)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Urban",1,line= textLine,cex= textSize)
mtext("Density",2,cex=textSize-0.05,line = textLine-0.08)

p1V <- all1$urb16_25mi
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$urb16_25mi
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$urb16_25mi
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)





v1 <- density(all1$crop19_25mi)
v2 <- density(com1$crop19_25mi)
v3 <- density(other1$crop19_25mi)
#xBounds <- c((1.2*min(v1$x)),(1.2*max(v1$x)))
xBounds <- c(-max(c(abs(min(v1$x)),abs(max(v1$x)))),max(c(abs(min(v1$x)),abs(max(v1$x)))))
plot(v1,col= allColor,lwd=1,main="",xlim=xBounds,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,(1.1*max(v1$y))))
polygon(v1,col=allColor,border= "gray50")
lines(v2,col= commonColor,lwd=1.5)
lines(v3,lwd=1.5,col= otherColor,lty=3)
axis(1,labels=F); axis(1,labels=T,lwd=0,line=-0.25)
axis(2,labels=F); axis(2,labels=T,lwd=0,line=-0.25,las=2)
abline(v=0,col="gray30")
mtext("Crops",1,line= textLine,cex= textSize)


p1V <- all1$crop19_25mi
xPos <- (xBig * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.12*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (yBig*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol,cex= bigText,font=2)

p1V <- com1$crop19_25mi
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lwd=1.5)

p1V <- other1$crop19_25mi
xPos <- (xSmall * (xBounds[2] - xBounds[1])) + min(xBounds)
#xPos <- (0.93*(max(v1$x) - min(v1$x))) + min(v1$x)
yPos <- (ySmall2*(max(v1$y) - min(v1$y))) + min(v1$y)
p1 <- round(max(length(p1V[p1V <0])/length(p1V),length(p1V[p1V>0])/length(p1V)),2)
sign1 <- if(median(p1V) > 0){print("(+)")}else{print("(-)")}
text(paste(p1,sign1,sep=""),x=xPos,y=yPos,col= textCol2,cex= smallText)
lines(x=c((xPos*0.65),(xPos*1.3)),y=(c((0.91*yPos),(0.91*yPos))),lty=3,lwd=1.5)




##########################################################
##########################################################
## labels


v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


text(1,99,"A",cex=1.5,font=2)
text(30,99,"B",cex=1.5,font=2)
text(67,99,"C",cex=1.5,font=2)
text(30,74,"D",cex=1.5,font=2)
text(67,74,"E",cex=1.5,font=2)
text(30,49.5,"F",cex=1.5,font=2)
text(67,49.5,"G",cex=1.5,font=2)
text(30,25,"H",cex=1.5,font=2)
text(67,25,"I",cex=1.5,font=2)



#########################################
#########################################

