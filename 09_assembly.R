

###############
###############
## This code pulls in various data files and assembles (and saves) them as one R object for use in analyses
###############
###############


## Load a simple search and replace function.
source("gsrFunction.R")




###########
#### Results from "all individuals" model (NABA) with year coefficients for each site.

## note here that this originally pulled from a different folder (from Sid's model), but now taking output
## from the modified version that also outputs the common and other subsets. 

abun <- read.csv("yearSites_MIN10_totalInds_global_var10_all_zYear.csv")


## Collapse probability-of-positive effect and probability-of-negative effect into one probability-of-effect column.
abun$probChange <- apply(abun[,c(5:6)],1,FUN=max) 
colnames(abun)[1] <- "sites"
head(abun)




###########
#### Results from species-specific Poisson models (NABA), with species-level year coefficients.

pois4th <- read.csv("yearSpMu_MIN10_zYear.csv")


colnames(pois4th)[1] <- "sp"
pois4th$probChange <- apply(pois4th[,c(5:6)],1,FUN=max) 
head(pois4th)



###########
#### Same as previous, but with site-level estimates (within species).

pois4thSites <- read.csv("yearSites_MIN10_zYear.csv")


colnames(pois4thSites)[1] <- "sp"
colnames(pois4thSites)[2] <- "sites"
pois4thSites$probChange <- apply(pois4thSites[,c(6:7)],1,FUN=max) 
head(pois4thSites)




###########
#### Population growth rate model (NABA), with species-specific year coefficients.

growth <- read.csv("r_yearSpMu_MIN10_zYear.csv")


colnames(growth)[1] <- "sp"
head(growth)



###########
#### Same as previous, but with population-level year estimates.

growthPops <- read.csv("yearSites_MIN10_GrowthRate_zYear.csv")


colnames(growthPops) <- c("sp","sites","growthMedian","growUP","grownLow","growPos","growNeg")
head(growthPops)





###########
#### Results from analyses of Shapiro data.

art <- read.csv("trends2019_no_trinomials(half_score)_allyears.csv")


## Fix labels for species that only appear at one site.
tally1 <- aggregate(art$med ~ art$genus_species,FUN="length")
tally2 <- tally1[tally1[,2] == 1,]
colnames(tally2)[1] <- c("sp")
art[art$genus_species %in% tally2$sp,2] <- "overall"
art <- droplevels(art[art$site_name == "overall",])

## Get taxonomy key and implement name changes (for consistency with NABA).
key <- read.csv("ArtToFourthKey.csv")
art$species <- gsr(art$genus_species,as.vector(key$sp),as.vector(key$newNameForArt))
head(art)

## Reorganize and save a subset of columns.
art2 <- art[,c(11,3,5,4,9:10)] 
colnames(art2) <- c("sp","medianArt","upArt","lowArt","probPosArt","probNegArt")
art2$probChangeArt <- apply(art2[,c(5:6)],1,FUN=max) 
head(art2)





###########
#### iNaturalist data (starting with the year 2005).

nat <- read.csv("iNat200slopes2005.csv")

## Get taxonomic key and implement name changes for consistency with NABA.
key <- read.csv("iNatToFourthKey.csv")
nat$sp <- gsr(nat$species,as.vector(key$sp),as.vector(key$newNamForiNat))

## Reorganize and save a subset of columns.
nat4 <- nat[,c(9,2,6:7,4,3)] 
colnames(nat4) <- c("sp","medianNat","upNat","lowNat","probPosNat","probNegNat")
nat4$probChangeNat <- apply(nat4[,c(5:6)],1,FUN=max) 
head(nat4)





###########
#### Land use data, lat/long coordinates, and elevation.

forElev <- read.csv("fourthPredictors.csv")
colnames(forElev)[1] <- "sites"
forElev <- forElev[,c(1,10)]
head(forElev)

preds <- read.csv("landUse.csv")
colnames(preds)[1] <- "sites"
head(preds)






preds <- merge(preds,forElev, by="sites")
head(preds)

## Manually fix three elevations that were errors.
preds[preds$sites == "GuadalupeCanyonAZNM",24] <- 1353
preds[preds$sites == "PalosVerdesCA",24] <- 0
preds[preds$sites == "VashonIslandWA",24] <- 98




###########
#### Climate data (static and rates of change).

clim <- read.csv("site_climates(1970).csv")
clim <- clim[,-c(1:2)]
colnames(clim)[1] <- "sites"
clim2 <- merge(preds,clim,by="sites")
head(clim2);dim(clim2)

change <- read.csv("site_trends(1970).csv")
colnames(change)[1] <- "sites"
head(change)

change2 <- merge(clim2,change,by="sites")
head(change2)

coords <- change2





###########
#### Average abundance calculations (from NABA data).


base <- read.csv("fourthAllSp.csv")
base$modAbun <- base$count / base$hours
base2 <- aggregate(base$modAbun ~ base$sp, FUN="mean")
head(base2)
colnames(base2) <- c("sp","baseAbun")




###########
#### Body size.

body <- read.csv("wingspans.csv")
body$name <- paste(body$genus,body$species,sep=" ")
body$size <- (body$lower + body$upper) / 2
head(body)

## Get Taxonomic key and implement name changes.
key <- read.csv("wingspanToNABAKey.csv")
body$sp <- gsr(body$name,as.vector(key$sp),as.vector(key$newNameForOpler))
head(body)

## Reorganize and save a subset of columns.
body2 <- body[,c(9,8)] 
colnames(body2) <- c("sp","size")
head(body2)
dim(body2)




###########
#### Georgraphic ranges

rangesDens <- read.csv("range_confs_density.csv")
head(rangesDens)

## Add range for Icaricia acmon that was a late addition.
IcAc <- read.csv("icaricia_acmon_range.csv")
head(IcAc)
rangesDens <- rbind(rangesDens,IcAc)

## Taxonomic corrections (to match NABA)
key <- read.csv("iNatToFourthKey.csv")
head(key)
rangesDens$sp <- gsr(rangesDens$name,as.vector(key$sp),as.vector(key$newNamForiNat))
head(rangesDens)

## Reorganize and save a subset of columns.
rangesDens2 <- rangesDens[,c(9,2:8)] 
colnames(rangesDens2)[1:2] <- c("sp","range")
head(rangesDens2)





###########
#### Host range

hosts <- read.csv("FourthJulyHostRanges.csv")
colnames(hosts)[1] <- "sp"
head(hosts)



###########
## Voltinism and average elevations

volt <- read.csv("broods.csv")
head(volt)

temp <- read.csv("yearSites_MIN10_zYear.csv")
colnames(temp)[1] <- "sp"
colnames(temp)[2] <- "sites"
temp <- temp[,c(1:2)]
head(temp)

temp2 <- preds[,c(1,24)]
temp3 <- merge(temp,temp2,by="sites")

temp4 <- aggregate(temp3$elevation ~ temp3$sp, FUN="mean")
colnames(temp4) <- c("sp","avgElev")
head(temp4)

voltElev <- merge(volt,temp4,by="sp")
head(voltElev)




########
################## Put them all together and save as one R object.
########


fourth <- list(abun= abun, pois4th= pois4th, pois4thSites= pois4thSites, 
							growthRate= growth, growPops= growthPops,  
							art= art2, inat2005= nat4, sites= coords, rangesV2= rangesDens2, 
							hosts= hosts, body= body2, base= base2, voltElev= voltElev)
save(fourth, file="allFourthForAnalyses.Rdata")






