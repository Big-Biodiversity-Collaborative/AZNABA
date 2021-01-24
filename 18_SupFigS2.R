library(reshape2)




####################################################
## supp fig S2
####################################################




########################

load("allFourthForAnalyses.Rdata")
abun <- fourth$abun
head(abun);dim(abun)
colnames(abun)[1] <- "sites"

sites <- fourth$sites
head(sites);dim(sites)

new <- merge(abun, sites,by="sites")
head(new)

########################


quartz(width=3,height=6)

par(mfrow=c(3,1),mar=c(3,3.5,2.5,3),oma=c(2,1,1,1))



head(sites)
data.frame(names(sites))
sub <- sites[,c(51:54,46:49,41:44),] #all
sub2 <- stack(sub)
colnames(sub2) <- c("slopes","var")
head(sub2)
dim(sub2)

t1 <- aggregate(sub2$slopes ~ sub2$var, FUN="mean")
colnames(t1) <- c("var","mean")
t1$sd <- aggregate(sub2$slopes ~ sub2$var, FUN="sd")[,2]
t1$se <- t1$sd / sqrt(200)
t1


##########

t2 <- t1[1:4,]
t2$order <- c(4,2,3,1)
t2
plot(t2$mean ~ t2$order,ylim=c(-0.0001,0.07),xlim=c(0.5,4.5),xaxt="n",pch=c(15,16,17,18),cex=c(1.5,1.5,1.5,2),las=2,ylab="")
arrows(t2$order,t2$mean-t2$sd,t2$order,t2$mean+t2$sd,length=0,lwd=1.5)
mtext("Rate of change",2,line=3.2,cex=0.9)
mtext("Minimum temperatures",3,line=0.4)
axis(1,at=c(1:4),labels=F)
axis(1,at=c(1:4),labels=c("Win.","Spr.","Sum.","Fall"),cex.axis=1)
grid(ny=6,nx=0,col="gray70")
#mtext("Seasons",1,line=2.1,cex=0.85)


#########

t1
t2 <- t1[5:8,]
t2$order <- c(4,2,3,1)
t2
plot(t2$mean ~ t2$order,ylim=c(-0.002,0.045),xlim=c(0.5,4.5),xaxt="n",pch=c(15,16,17,18),cex=c(1.5,1.5,1.5,2),las=2,ylab="")
arrows(t2$order,t2$mean-t2$sd,t2$order,t2$mean+t2$sd,length=0,lwd=1.5)
mtext("Rate of change",2,line=3.2,cex=0.9)
mtext("Maximum temperatures",3,line=0.4)
axis(1,at=c(1:4),labels=F)
axis(1,at=c(1:4),labels=c("Win.","Spr.","Sum.","Fall"),cex.axis=1)
grid(ny=6,nx=0,col="gray70")
#mtext("Seasons",1,line=2.1,cex=0.85)


#########

t1
t2 <- t1[9:12,]
t2$order <- c(4,2,3,1)
t2
plot(t2$mean ~ t2$order,ylim=c(-0.6,0.4),xlim=c(0.5,4.5),xaxt="n",pch=c(15,16,17,18),cex=c(1.5,1.5,1.5,2),las=2,ylab="")
arrows(t2$order,t2$mean-t2$sd,t2$order,t2$mean+t2$sd,length=0,lwd=1.5)
mtext("Rate of change",2,line=2.9,cex=0.9)
mtext("Precipitation",3,line=0.4)
axis(1,at=c(1:4),labels=F)
axis(1,at=c(1:4),labels=c("Win.","Spr.","Sum.","Fall"),cex.axis=1)
grid(ny=6,nx=0,col="gray70")
mtext("Seasons",1,line=2.8,cex=0.95)





###########################

v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


text(7,98.5,"A",cex=1.8,font=2)
text(7,65,"B",cex=1.8,font=2)
text(7,32,"C",cex=1.8,font=2)






