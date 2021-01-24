library(car)




####################################################
## supp fig S2
####################################################



## slopes for caterpillar plot

slopes <- read.csv("~/Dropbox/IfItsAThreeWay/4thCounts_RESULTS/totalIndsModel_Var10_version2_centerAllYears_GLOBAL/yearSites_MIN10_totalInds_global_var10_zYear.csv")
head(slopes)
head(slopes[order(slopes$site),])
dim(slopes)
str(slopes)
sites <- levels(slopes$site)


## get raw data and remove the top 10 variable sp.

bugs <- read.csv("~/Dropbox/ProjectsAndPapers/RegionalDeclines/fourthAllSp.csv")
head(bugs)
sort(levels(bugs$site))


## remove variable sp
temp <- read.csv("~/Dropbox/IfItsAThreeWay/4thCounts_RESULTS/codeForMatt/totalInds_Hierarchical/varianceSpecies.csv")
temp <- temp[order(temp$var,decreasing=T),]
head(temp)
removers <- temp$species[1:10]
bugs$sp <- as.character(bugs$sp)
bugs <- bugs[bugs$sp != as.character(removers[1]),]
bugs <- bugs[bugs$sp != as.character(removers[2]),]
bugs <- bugs[bugs$sp != as.character(removers[3]),]
bugs <- bugs[bugs$sp != as.character(removers[4]),]
bugs <- bugs[bugs$sp != as.character(removers[5]),]
bugs <- bugs[bugs$sp != as.character(removers[6]),]
bugs <- bugs[bugs$sp != as.character(removers[7]),]
bugs <- bugs[bugs$sp != as.character(removers[8]),]
bugs <- bugs[bugs$sp != as.character(removers[9]),]
bugs <- bugs[bugs$sp != as.character(removers[10]),]
str(bugs)
dim(bugs)
sort(unique(bugs$sp))



## get counts by year and site
bugs2 <- aggregate(bugs$count ~ bugs$site + bugs$year, FUN="sum")
colnames(bugs2) <- c("site","year","count")
bugs2$label <- paste(bugs2$site,bugs2$year,sep="_")
head(bugs2)
bugs2 <- bugs2[,c(3:4)]



## get effort by year and site
new <- paste(bugs$site,bugs$year,sep="_")
new <- paste(new,bugs$hours,sep="_")
new <- unique(new)
length(new)
site <- do.call(rbind,strsplit(as.character(new),"_"))[,1]
year <- as.numeric(do.call(rbind,strsplit(as.character(new),"_"))[,2])
hours <- as.numeric(do.call(rbind,strsplit(as.character(new),"_"))[,3])
eff <- data.frame(site,year,hours)
eff$label <- paste(eff$site,eff$year,sep="_")
head(eff)



## get date of census by year and site
head(bugs)
new <- paste(bugs$site,bugs$year,sep="_")
new <- paste(new,bugs$date,sep="_")
new <- unique(new)
length(new)
site <- do.call(rbind,strsplit(as.character(new),"_"))[,1]
year <- as.numeric(do.call(rbind,strsplit(as.character(new),"_"))[,2])
date <- as.numeric(do.call(rbind,strsplit(as.character(new),"_"))[,3])
date <- data.frame(site,year,date)
date$label <- paste(eff$site,eff$year,sep="_")
head(date)
date <- date[,c(3:4)]



### put them together
bugs3 <- merge(bugs2,eff,by="label")
head(bugs3)
bugs4 <- merge(bugs3,date,by="label")
head(bugs4)




##### correct by sites for date and effort

head(bugs4)

## chuck the crazy V cardui year
#bugs4 <- bugs4[bugs4$label != "BorregoSpringsCA_2005",]

sites
site <- NA
year <- NA
adjCount <- NA
zCount <- NA
for(i in 1:72){
	sub1 <- bugs4[bugs4$site == sites[i],]
	res <- lm(count ~ hours + date, data=sub1)
	adjCount <- append(adjCount, (mean(sub1$count) + res$residual))
	site <- append(site,as.character(sub1$site))
	year <- append(year,sub1$year)
	zCount <- append(zCount,scale(predict(res,type="response")))
}
adjCount[adjCount < 0] <- 1
corrected <- data.frame(site,year,adjCount,zCount)[-1,]
head(corrected)





###########################################
## graph 

quartz(width=6,height=6.5)
par(oma=c(3,1,2,2))
par(mar=c(1,2,0.5,1))

layout(matrix(c(1,2,3,
				1,4,5,
				1,6,7,
				1,8,9,
				1,10,11),nrow=5,byrow=T))
#layout.show(n=11)

head(slopes)
slopes2 <- slopes[order(slopes$median),]
slopes2$order <- c(72:1)
head(slopes2)

## pick sites to plot
picks <- c(2, 20, 40, 57, 71)


plot(exp(slopes2$median),slopes2$order,yaxt="n",ylab="",xlab="",xlim=c(0.785,1.16),cex=0.7,pch=1,col="gray50")
abline(v=1,lty=2)
arrows(exp(slopes2$low),slopes2$order,exp(slopes2$up),slopes2$order,length=0,col="gray50")

slopesSub <- slopes2[slopes2$order %in% picks,]
points(exp(slopesSub$median),slopesSub$order,pch=19,col="red",cex=0.9)
arrows((0.02 + exp(slopesSub$low)),(0.15+ slopesSub$order),(0.065 + exp(slopesSub$low)),(1.2 + slopesSub$order),code=1,length=0.075,col="red")



linCol <- "gray70"
abline(v=1,lty=2,col="black")
abline(v=0.9,lty=2,col=linCol)
abline(v=0.8,lty=2,col=linCol)
abline(v=1.1,lty=2,col=linCol)




names <- droplevels(slopes2[slopes2$order %in% picks,])
names


par(mar=c(1.75,3,1,1))
for(i in 1:5){
	sub <- corrected[corrected$site == as.character(names[i,1]),]

	plot(sub$adjCount ~ sub$year,type="b",ylim=c(0.9*(min(sub$adjCount)),1.1*max(sub$adjCount)),col="black")
	mtext("Butterflies",2,cex=0.75,line=2.2)
	points(sub$year,sub$adjCount,pch=1,col="red")
	res <- lm(sub$adjCount ~ sub$year)
	abline(res,lty=2,col="gray40")
	acf(res$residuals)
	mtext("Correlation",2,cex=0.75,line=2.1)

	#plot(log(sub$adjCount) ~ sub$year,type="b",ylim=c(0.9*(log(min(sub$adjCount))),1.1*(log(max(sub$adjCount)))))
	#abline(lm(log(sub$adjCount) ~ sub$year),lty=2)
}



v <- c(0,1,0,1) #x1,x2,y1,y2 
par(fig=v,new=T,mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(1:100,1:100, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

fontCol <- "gray50"

text(28.25,96.75,"A",cex=1.25,font=2,col=fontCol)

text(62,96,"B",cex=1.25,font=2,col=fontCol)
text(62,76.5,"D",cex=1.25,font=2,col=fontCol)
text(62,57,"F",cex=1.25,font=2,col=fontCol)
text(62,38,"H",cex=1.25,font=2,col=fontCol)
text(62,19,"J",cex=1.25,font=2,col=fontCol)

text(95.25,96,"C",cex=1.25,font=2,col=fontCol)
text(95.25,76.5,"E",cex=1.25,font=2,col=fontCol)
text(95.25,57,"G",cex=1.25,font=2,col=fontCol)
text(95.25,38,"I",cex=1.25,font=2,col=fontCol)
text(95.25,19,"K",cex=1.25,font=2,col=fontCol)

text(17.5,0,"exp(Poisson year coef.)")
text(51.5,1,"Year",cex=1.25)
text(84,1,"Lag (temporal autocorrelation)",cex=1)





