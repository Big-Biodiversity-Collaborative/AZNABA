


###########################################
## supp fig s2
###########################################



quartz(width=8,height=5)

par(mfcol=c(2,3))
par(mar=c(2.5,4,2,2))
par(oma=c(3,1,0,0))





all2 <- read.csv("resampled.csv")[,-1]
head(all2)
all2$median <- exp(all2$median)
all3 <- all2
head(all3)
all3 <- all3[all3$reps != 2,]
dim(all3)


## point estimate

new1 <- aggregate(all3$median ~ all3$reps,FUN="median")
new1
func1 <- function(x,q){
	quantile(x,probs=q)
}
new2 <- aggregate(all3$median ~ all3$reps,FUN=function(z) func1(z,0.25))
new2
new3 <- aggregate(all3$median ~ all3$reps,FUN=function(z) func1(z,0.75))
new3
new4 <- data.frame(new1[,1],new1[,2],new2[,2],new3[,2])
colnames(new4) <- c("reps","point","low","up")
new4

plot(all3$median[-601] ~ jitter(all3$reps[-601],0.75),col="gray70",xlab="Locations sampled",ylab="",xlim=c(8,74),ylim=c(0.925,1.035),las=2,xaxt="n")
axis(1)
abline(h=1,lty=2,col="gray30")
points(new4$reps,new4$point,pch=19,col="red")
arrows(new4$reps,new4$up,new4$reps,new4$low,length=0.075,angle=90,code=3,col="red",lwd=1.5)

mtext("Locations",1,line=2.3,cex=0.8)
mtext("exp(Poisson coefficient)",2,line=3.2,cex=0.8)

#lines(smooth.spline(new4$reps,new4$point,df=3),col="red")





## prob
head(all3)

new1 <- aggregate(all3$prob ~ all3$reps,FUN="median")
new1
func1 <- function(x,q){
	quantile(x,probs=q)
}
new2 <- aggregate(all3$prob ~ all3$reps,FUN=function(z) func1(z,0.25))
new2
new3 <- aggregate(all3$prob ~ all3$reps,FUN=function(z) func1(z,0.75))
new3
new4 <- data.frame(new1[,1],new1[,2],new2[,2],new3[,2])
colnames(new4) <- c("reps","point","low","up")
new4

plot(all3$prob[-601] ~ jitter(all3$reps[-601],0.75),col="gray70",xlab="Locations sampled",ylab="",xlim=c(8,74),las=2,xaxt="n")
axis(1)
#abline(h=new4[8,2],lty=2,col="gray30")
abline(h=0.9,lty=2,col="gray30")
points(new4$reps,new4$point,pch=19,col="red")
arrows(new4$reps,new4$up,new4$reps,new4$low,length=0.075,angle=90,code=3,col="red",lwd=1.5)
#lines(smooth.spline(new4$reps,new4$point,df=3),col="red")

mtext("Locations",1,line=2.3,cex=0.8)
mtext("Mass of posterior",2,line=2.9,cex=0.8)








#########################################################################################





all2 <- read.csv("resampled_Common.csv")[,-1]
head(all2)
all2$median <- exp(all2$median)
all3 <- all2
head(all3)
all3 <- all3[all3$reps != 2,]
dim(all3)


## point estimate

new1 <- aggregate(all3$median ~ all3$reps,FUN="median")
new1
func1 <- function(x,q){
	quantile(x,probs=q)
}
new2 <- aggregate(all3$median ~ all3$reps,FUN=function(z) func1(z,0.25))
new2
new3 <- aggregate(all3$median ~ all3$reps,FUN=function(z) func1(z,0.75))
new3
new4 <- data.frame(new1[,1],new1[,2],new2[,2],new3[,2])
colnames(new4) <- c("reps","point","low","up")
new4

plot(all3$median[-601] ~ jitter(all3$reps[-601],0.75),col="gray70",xlab="Locations sampled",ylab="",xlim=c(8,74),ylim=c(0.925,1.035),las=2,xaxt="n")
axis(1)
abline(h=1,lty=2,col="gray30")
points(new4$reps,new4$point,pch=19,col="red")
arrows(new4$reps,new4$up,new4$reps,new4$low,length=0.075,angle=90,code=3,col="red",lwd=1.5)

mtext("Locations",1,line=2.3,cex=0.8)
mtext("exp(Poisson coefficient)",2,line=3.2,cex=0.8)

#lines(smooth.spline(new4$reps,new4$point,df=3),col="red")





## prob
head(all3)

new1 <- aggregate(all3$prob ~ all3$reps,FUN="median")
new1
func1 <- function(x,q){
	quantile(x,probs=q)
}
new2 <- aggregate(all3$prob ~ all3$reps,FUN=function(z) func1(z,0.25))
new2
new3 <- aggregate(all3$prob ~ all3$reps,FUN=function(z) func1(z,0.75))
new3
new4 <- data.frame(new1[,1],new1[,2],new2[,2],new3[,2])
colnames(new4) <- c("reps","point","low","up")
new4

plot(all3$prob[-601] ~ jitter(all3$reps[-601],0.75),col="gray70",xlab="Locations sampled",ylab="",xlim=c(8,74),las=2,xaxt="n")
axis(1)
#abline(h=new4[8,2],lty=2,col="gray30")
abline(h=0.9,lty=2,col="gray30")
points(new4$reps,new4$point,pch=19,col="red")
arrows(new4$reps,new4$up,new4$reps,new4$low,length=0.075,angle=90,code=3,col="red",lwd=1.5)
#lines(smooth.spline(new4$reps,new4$point,df=3),col="red")

mtext("Locations",1,line=2.3,cex=0.8)
mtext("Mass of posterior",2,line=2.9,cex=0.8)









#########################################################################################

head(all)
all2 <- read.csv("resampled_other.csv")[,-1]
head(all2)
all2$median <- exp(all2$median)
all3 <- all2
head(all3)
all3 <- all3[all3$reps != 2,]
dim(all3)


## point estimate

new1 <- aggregate(all3$median ~ all3$reps,FUN="median")
new1
func1 <- function(x,q){
	quantile(x,probs=q)
}
new2 <- aggregate(all3$median ~ all3$reps,FUN=function(z) func1(z,0.25))
new2
new3 <- aggregate(all3$median ~ all3$reps,FUN=function(z) func1(z,0.75))
new3
new4 <- data.frame(new1[,1],new1[,2],new2[,2],new3[,2])
colnames(new4) <- c("reps","point","low","up")
new4

plot(all3$median[-601] ~ jitter(all3$reps[-601],0.75),col="gray70",xlab="Locations sampled",ylab="",xlim=c(8,74),ylim=c(0.925,1.035),las=2,xaxt="n")
axis(1)
abline(h=1,lty=2,col="gray30")
points(new4$reps,new4$point,pch=19,col="red")
arrows(new4$reps,new4$up,new4$reps,new4$low,length=0.075,angle=90,code=3,col="red",lwd=1.5)

mtext("Locations",1,line=2.3,cex=0.8)
mtext("exp(Poisson coefficient)",2,line=3.2,cex=0.8)

#lines(smooth.spline(new4$reps,new4$point,df=3),col="red")





## prob
head(all3)

new1 <- aggregate(all3$prob ~ all3$reps,FUN="median")
new1
func1 <- function(x,q){
	quantile(x,probs=q)
}
new2 <- aggregate(all3$prob ~ all3$reps,FUN=function(z) func1(z,0.25))
new2
new3 <- aggregate(all3$prob ~ all3$reps,FUN=function(z) func1(z,0.75))
new3
new4 <- data.frame(new1[,1],new1[,2],new2[,2],new3[,2])
colnames(new4) <- c("reps","point","low","up")
new4

plot(all3$prob[-601] ~ jitter(all3$reps[-601],0.75),col="gray70",xlab="Locations sampled",ylab="",xlim=c(8,74),las=2,xaxt="n")
axis(1)
#abline(h=new4[8,2],lty=2,col="gray30")
abline(h=0.9,lty=2,col="gray30")
points(new4$reps,new4$point,pch=19,col="red")
arrows(new4$reps,new4$up,new4$reps,new4$low,length=0.075,angle=90,code=3,col="red",lwd=1.5)
#lines(smooth.spline(new4$reps,new4$point,df=3),col="red")

mtext("Locations",1,line=2.3,cex=0.8)
mtext("Mass of posterior",2,line=2.9,cex=0.8)









###########################

v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')


text(0,99,"A",cex=1.5,font=2)
text(0,51,"B",cex=1.5,font=2)

text(34.5,99,"C",cex=1.5,font=2)
text(34.5,51,"D",cex=1.5,font=2)

text(69.5,99,"E",cex=1.5,font=2)
text(69.5,51,"F",cex=1.5,font=2)
