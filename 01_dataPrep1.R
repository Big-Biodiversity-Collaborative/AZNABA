



library(lubridate)
source("gsrFunction.R")



###############
###############
## Step 1 of 2 for NABA data prep.
###############
###############





###########
#### Put two raw data files into one object.

dat1 <- read.csv("NABA1.csv")
colnames(dat1)[2] <- "Date"
dat1 <- dat1[,-4]
head(dat1)

dat2 <- read.csv("NABA2.csv")
head(dat2)

dat <- rbind(dat1,dat2)
head(dat)
dim(dat)





###########
#### Convert dates into useful formats.

dat$ordDate <- yday(as.Date(dat$Date,"%m/%d/%y"))
dat$year <- year(as.Date(dat$Date,"%m/%d/%y"))
dat$month <- month(as.Date(dat$Date,"%m/%d/%y"))
head(dat)
str(dat)




###########
#### Remove rows with missing years.

sel <- which(is.na(dat$year))
dat <- droplevels(dat[-sel,])
dim(dat)



#########
## Remove rows where only a generic name is given, and other errors.

dat <- droplevels(dat[dat$ScientificName != "",])
dat <- droplevels(dat[dat$ScientificName != "Agriades",])
dat <- droplevels(dat[dat$ScientificName != "Amblyscirtes",])
dat <- droplevels(dat[dat$ScientificName != "Anaea",])
dat <- droplevels(dat[dat$ScientificName != "Anthocharis",])
dat <- droplevels(dat[dat$ScientificName != "Asterocampa",])
dat <- droplevels(dat[dat$ScientificName != "Boloria",])
dat <- droplevels(dat[dat$ScientificName != "Butterfly",])
dat <- droplevels(dat[dat$ScientificName != "Calephelis",])
dat <- droplevels(dat[dat$ScientificName != "Callophrys",])
dat <- droplevels(dat[dat$ScientificName != "Celastrina",])
dat <- droplevels(dat[dat$ScientificName != "Cercyonis",])
dat <- droplevels(dat[dat$ScientificName != "Chlosyne",])
dat <- droplevels(dat[dat$ScientificName != "Ceononympha",])
dat <- droplevels(dat[dat$ScientificName != "Coliadinae",])
dat <- droplevels(dat[dat$ScientificName != "Colias",])
dat <- droplevels(dat[dat$ScientificName != "Danaus",])
dat <- droplevels(dat[dat$ScientificName != "Erynnis",])
dat <- droplevels(dat[dat$ScientificName != "Euphilotes",])
dat <- droplevels(dat[dat$ScientificName != "Euphydryas",])
dat <- droplevels(dat[dat$ScientificName != "Eurema",])
dat <- droplevels(dat[dat$ScientificName != "Everes",])
dat <- droplevels(dat[dat$ScientificName != "Heliconiinae",])
dat <- droplevels(dat[dat$ScientificName != "Hemiargus",])
dat <- droplevels(dat[dat$ScientificName != "Hesperia",])
dat <- droplevels(dat[dat$ScientificName != "Hesperiidae",])
dat <- droplevels(dat[dat$ScientificName != "Hesperiinae",])
dat <- droplevels(dat[dat$ScientificName != "Limenitidinae",])
dat <- droplevels(dat[dat$ScientificName != "Limenitis",])
dat <- droplevels(dat[dat$ScientificName != "Lycaena",])
dat <- droplevels(dat[dat$ScientificName != "Lycaenidae",])
dat <- droplevels(dat[dat$ScientificName != "Lycaeides",])
dat <- droplevels(dat[dat$ScientificName != "Megathymus",])
dat <- droplevels(dat[dat$ScientificName != "Nymphalidae",])
dat <- droplevels(dat[dat$ScientificName != "Nymphalinae",])
dat <- droplevels(dat[dat$ScientificName != "Nymphalis",])
dat <- droplevels(dat[dat$ScientificName != "Ochlodes",])
dat <- droplevels(dat[dat$ScientificName != "Oeneis",])
dat <- droplevels(dat[dat$ScientificName != "Papilio",])
dat <- droplevels(dat[dat$ScientificName != "Papilionidae",])
dat <- droplevels(dat[dat$ScientificName != "Papilioninae",])
dat <- droplevels(dat[dat$ScientificName != "Parnassius",])
dat <- droplevels(dat[dat$ScientificName != "Phyciodes",])
dat <- droplevels(dat[dat$ScientificName != "Pieridae",])
dat <- droplevels(dat[dat$ScientificName != "Pierinae",])
dat <- droplevels(dat[dat$ScientificName != "Pieris",])
dat <- droplevels(dat[dat$ScientificName != "Pholisora",])
dat <- droplevels(dat[dat$ScientificName != "Plebejus",])
dat <- droplevels(dat[dat$ScientificName != "Polites",])
dat <- droplevels(dat[dat$ScientificName != "Polygonia",])
dat <- droplevels(dat[dat$ScientificName != "Polyommatinae",])
dat <- droplevels(dat[dat$ScientificName != "Pontia",])
dat <- droplevels(dat[dat$ScientificName != "Pyrginae",])
dat <- droplevels(dat[dat$ScientificName != "Pyrgus",])
dat <- droplevels(dat[dat$ScientificName != "Satyrinae",])
dat <- droplevels(dat[dat$ScientificName != "Satyrium",])
dat <- droplevels(dat[dat$ScientificName != "Speyeria",])
dat <- droplevels(dat[dat$ScientificName != "Strymon",])
dat <- droplevels(dat[dat$ScientificName != "Theclinae",])
dat <- droplevels(dat[dat$ScientificName != "Thorbes",])
dat <- droplevels(dat[dat$ScientificName != "Thorybes",])
dat <- droplevels(dat[dat$ScientificName != "Vanessa",])
dat <- droplevels(dat[dat$ScientificName != "Agriades",])
dat <- droplevels(dat[dat$ScientificName != "Agriades",])
dim(dat)





##########
## Changing trinomials into binomials and fixing a few spelling errors (listed in the fourthKey file).

key <- read.csv("fourthKey.csv")
head(key)

dat$newNames <- gsr(dat$ScientificName,as.vector(key$old),as.vector(key$new))
head(dat)






##########
## Find a very few instances where the same species was reported twice on the same day (i.e. counted twice);
## combine those records and then make sure one is removed.

check <- aggregate(NumSeen ~ ScientificName + CountName + Date, data=dat, FUN=length)
sel <- which(check$NumSeen>1)
check2 <- check[sel,]

ditch <- NA
for(i in 1:dim(check2)[1]){
	sub1 <- dat[dat$CountName == check2[i,2],]
	sub2 <- sub1[sub1$Date == check2[i,3],]
	sub3 <- sub2[sub2$ScientificName == check2[i,1],]
	## replace the second with the sum of both
	find <- which(rownames(dat) == rownames(sub3)[2])
	dat[find,9] <- sum(sub3[1:2,9])
	## remember which ones to ditch (the first)
	ditch[i] <- which(rownames(dat) == rownames(sub3)[1])
}
dim(dat)
dat <- dat[-ditch,]





##########
## Find sites with multiple visits per year and keep the visit closest to the 4th of July.

dat$label <- paste(dat$year,dat$CountName,sep="_")

temp <- aggregate(ordDate ~ year + CountName, data=dat, FUN=function(x){length(unique(x))})
head(temp)
temp2 <- temp[temp$ordDate>1,]
head(temp2)
temp2$label <- paste(temp2$year,temp2$CountName,sep="_")

temp3 <- dat[dat$label %in% temp2$label,]
head(temp3)
temp3 <- temp3[order(temp3[,1],temp3[,13],temp3[,12]),]
temp3$label <- paste(temp3$ordDate,temp3$label,sep="_")
keyTemp <- unique(temp3$label)
keyTemp

ord <- do.call(rbind,strsplit(as.character(keyTemp),"_"))[,1]
year <- do.call(rbind,strsplit(as.character(keyTemp),"_"))[,2]
site <- do.call(rbind,strsplit(as.character(keyTemp),"_"))[,3]
keyTemp2 <- data.frame(ord,year,site)
head(keyTemp2)
keyTemp2$diff <- 185 - as.numeric(as.character(keyTemp2$ord))
keyTemp2$label <- paste(keyTemp2$year,keyTemp2$site,sep="_")

cases <- unique(keyTemp2$label)
flag <- NA
for(i in 1:length(cases)){
	sub <- keyTemp2[keyTemp2$label == cases[i],]
	sub$diff2 <- abs(sub$diff)
	minN <- min(sub$diff2)
	sub$diff2[which(sub$diff2 == minN)] <- 1
	sub$diff2[sub$diff2>1] <- 0
	flag <- append(flag, sub$diff2)
}
keyTemp2$flag <- flag[-1]
head(keyTemp2)
keyTemp2$newLabel <- paste(keyTemp2$ord,keyTemp2$label,sep="_")
keyTemp3 <- keyTemp2[keyTemp2$flag != 1,]


head(dat)
dim(dat)
dat$newLabel <- paste(dat$ordDate, dat$label, sep="_")
sel <- which(dat$newLabel %in% keyTemp3$newLabel)
dat <- dat[-sel,]





##########
## Prep for export.
head(dat)

mainTable <- dat[,c(15,1,13,2:5,9,11,12)]
head(mainTable)
colnames(mainTable) <- c("sp","site","year","fullDate","state","lat","long","count","hours","date")
mainTable <- mainTable[order(mainTable[,1],mainTable[,2],mainTable[,3]),]



write.csv(mainTable,"fourthData.csv")







