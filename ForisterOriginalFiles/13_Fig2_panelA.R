library(plotrix)
library(VennDiagram)



################################################################
## caterpillar plot (panel A from fig 2)
################################################################




quartz(width=5,height=6.7)
par(mar=c(0,0,0,0),oma=c(1,1,1,1))



## This is the list of top 50 species by probability of having a negative trend in any combination of the two datasets.
names <- c("Vanessa cardui","Vanessa annabella","Junonia coenia","Pieris rapae","Colias eurytheme","Adelpha bredowii","Nymphalis antiopa","Coenonympha tullia" ,"Satyrium californica","Plebejus icarioides","Lycaena arota","Speyeria callippe","Danaus plexippus","Strymon melinus","Hylephila phyleus","Erynnis propertius","Hesperia comma","Anthocharis sara","Everes amyntula","Glaucopsyche lygdamus","Lycaena xanthoides","Euphydryas editha","Atalopedes campestris","Cercyonis oetus","Plebejus saepiolus","Euchloe ausonides","Papilio zelicaon","Speyeria coronis","Pholisora catullus","Lycaena helloides","Satyrium sylvinus","Nymphalis milberti" ,"Callophrys eryphon","Colias eurydice","Polites sabuleti","Cercyonis sthenele","Colias philodice","Vanessa virginiensis","Lerodea eufala","Lycaena editha"  ,"Lycaena gorgon","Lycaena rubidus","Satyrium behrii","Polygonia satyrus","Satyrium tetra","Pontia occidentalis","Atlides halesus","Ochlodes agricola" ,"Satyrium saepium","Glaucopsyche piasus"  )



#### load main result files
load("allFourthForAnalyses.Rdata")
names(fourth)


#### tally of pops per species
temp44 <- data.frame(names)
colnames(temp44) <- "sp"
head(temp44)

popLevel <- fourth$growPops
head(popLevel)
tally45 <- aggregate(popLevel$sites ~ popLevel$sp, FUN=length)
colnames(tally45) <- c("sp","tally")
head(tally45)
tally46 <- merge(temp44,tally45,by="sp")
head(tally46);dim(tally46)

## exclude 1-pop names
length(names)
tally47 <- droplevels(tally46[tally46$tally > 1,])
names <- tally47[,1]
length(names)




#### plot

spLevel <- fourth$growthRate
popLevel <- fourth$growPops


sub1 <- droplevels(spLevel[spLevel$sp %in% names,])
sub1$median <- exp(sub1$median)
sub2 <- sub1[order(sub1$median),]
sub2$order <- c(length(sub2$sp):1)
head(sub2)
dim(sub2)
sub2[order(sub2$sp),]
sub2[order(sub2$probNeg),]






maxX <- 1.15
minX <- 0.85

par(mar=c(2,9,0,1),oma=c(1,0,0.25,0))
#plot(sub2$median,sub2$order,col="white",xlim=c(minX,maxX),xlab="",xaxt="n",yaxt="n",ylab="")
plot(sub2$median,sub2$order,col="white",xlim=c(minX,maxX),xlab="",xaxt="n",yaxt="n",ylab="",ylim=c((2.1),(max(sub2$order)-1.1)))
axis(1,labels=F)
axis(1,labels=T,lwd=0,cex.axis=0.75,line=-0.5)
abline(v=1,lty=2)
points(sub2$median,sub2$order,pch=18,cex=0.6)

for(i in 1:length(names)){
	
	subPop <- popLevel[popLevel$sp == as.character(names[i]),]
	littlePoints <- exp(subPop$growthMedian)
	where <- sub2[sub2$sp==names[i],7]
	lines(x=c(min(littlePoints),max(littlePoints)),y=c(where,where),col="gray80")
	points(littlePoints,rep(where,length(littlePoints)),col="white",pch=18,cex=0.55)
	points(littlePoints,rep(where,length(littlePoints)),col="gray40",pch=5,cex=0.55)
	
}
points(sub2$median,sub2$order,pch=18,cex=1.4)
#axis(2,at=sub2$order,labels=sub2$sp,las=2,cex.axis=0.75,font=4)
axis(2,at=sub2$order,labels=sub2$sp,las=2,cex.axis=0.65,font=3)
mtext("Population growth rate",1,line=1.5,cex=0.8)





################################################################
################################################################






