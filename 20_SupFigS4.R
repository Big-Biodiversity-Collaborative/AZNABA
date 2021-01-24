library(reshape2)




####################################################
## supp fig S4
####################################################


########################

load("allFourthForAnalyses.Rdata")
abun <- fourth$abun
head(abun);dim(abun)

sites <- fourth$sites
head(sites);dim(sites)

new <- merge(abun, sites,by="sites")
head(new)
dim(new)

########################

## thinking about rates of change

#hist(new$tmax_fal_slope)
sd(new$tmax_fal_slope) #0.014


########################


quartz(width=6,height=6)

par(mar=c(2,2,1,1),oma=c(4,3,5,5))

####################################################
####################################################

head(new)
new$diff <- new$tmax_fal_slope - new$tmax_sum_slope
plot(exp(new$median) ~ new$diff,xlim=c(-0.02075,0.04225),ylim=c(0.78,1.17),xaxt="n",las=2,ylab="",xlab="",pch=19,col="white")
axis(1,cex.axis=0.9)
abline(v=0,lty=2,col="gray30")
abline(h=1,lty=2,col="gray30")


res <- lm(exp(new$median) ~ new$diff)
summary(res)
#x <- seq(min(new$diff),max(new$diff),by=0.001)
#y <- coef(res)[1] + coef(res)[2] * x
#x <- seq(-0.03,0.05,by=0.001)
new <- new[order(new$diff),]
prd <- predict(res, newdata= data.frame(x=new$diff),interval="confidence")
polygon(c(new$diff,rev(new$diff)), c(prd[,2],rev(prd[,3])), col="gray90",border=F)


points(new$diff, exp(new$median), col="gray80", pch=19,cex=1.2)
points(new$diff, exp(new$median), col="black",cex=1.2)

abline(lm(exp(new$median) ~ new$diff),lty=1,lwd=1)


mtext("Rate of change in butterfly density",2,line=2.8,cex=1.1)
mtext("Difference between fall and summer warming",1,line=2.6,cex=1.1)
#slope: -1.197033 



###########################

v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


text(86,59,"Butterflies\nincreasing",cex=0.9,adj=0,col="gray30")
text(86,50,"Butterflies\ndecreasing",cex=0.9,adj=0,col="gray30")
arrows(84.75,53,84.75,44,length= 0.08,lwd= 1.3,col="gray30")
arrows(84.75,56,84.75,65,length= 0.08,lwd= 1.3,col="gray30")

text(40,89.25,"Greater\nfall warming",cex=0.9,adj=0,col="gray30")
arrows(40,84.75,54,84.75,length= 0.08,lwd=1.3,col="gray30")
text(36,89.25,"Greater\nsummer warming",cex=0.9,adj=1,col="gray30")
arrows(36,84.75,22,84.75,length=0.08,lwd= 1.3,col="gray30")





