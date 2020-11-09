#Chris Halsch
#September 29 2020
#Bayesian trends analysis

library(tidyverse)
library(jagsUI)



#Import model data
#Columns are:
# site_name = Site name
# genus_species = butterfly genus and species
# Year = year (z transformed)
# presence = number of visits a butterfly was seen
# visits = total number of visits to a site that year

dat <- data.table::fread("Shapiro_data.csv")
## Note that species are labelled numerically here; the identity of species is not important for the analyses that generate the histogram in Fig. 1.

###############################################
############ Bayesian trend models ############
###############################################

#The first model runs if a butterfly is only at one site
#The second model runs if a butterfly is found at more than one site
#and estimates a trend across sites

##Model for PA trends one site
mod_PA_one <- "model{
        # binomial likelihood
		# and logit link function for glm

		for(i in 1:N) {
		pres[i] ~ dbin(p[i], visits[i])
		logit(p[i]) <- int + beta * year[i]
		}     

		# uninformative hyperpriors
		beta ~ dnorm(0, 1)
		int ~ dnorm(0, 1)
		}	
		"
writeLines(mod_PA_one, con="model_by_species_PA_one.txt")


#Model for PA trends across sites
mod_PA_mult <- "model{
        # binomial likelihood
		# and logit link function for glm
		for(i in 1:N) {
		pres[i] ~ dbin(p[i], visits[i])
		logit(p[i]) <- int[sites[i]] + beta[sites[i]] * year[i]
		}     

		#Coef for species nested in sites
		for (j in 1:Nsite) {
		beta[j] ~ dnorm(hbmu, hbsig)
		int[j] ~ dnorm(himu, hisig)   
		}	

		# uninformative hyperpriors
		hbmu ~ dnorm(0, 1)
		hbsig ~ dgamma(0.1,0.1)
		himu ~ dnorm(0, 1)
		hisig ~ dgamma(0.1,0.1)
		}	
		"
#Writes model
writeLines(mod_PA_mult, con="model_by_species_PA_mult.txt")



##############################################
################# RUN MODELS #################
##############################################

#This loops across all species and saves the median of the posterior, 95% CI, rhat, and neff
species <- unique(dat$genus_species)
datalist <-  list()
counter = 0
for (i in 1:length(species)) {
  temp_dat <- dat %>% filter(genus_species == species[i])
  
  form1 <- temp_dat %>% 
    group_by(genus_species, site_name, Year) %>% 
    summarise(times.seen = sum(pres)) %>% 
    filter(times.seen >= 1) %>% 
    group_by(genus_species, site_name) %>% 
    summarise(times.seen = length(times.seen)) %>% 
    filter(times.seen >= 10)
  
  temp_dat <- temp_dat %>%
    filter(site_name %in% form1$site_name)
  
  site_key <- data.frame(site_name = temp_dat$site_name,
                         index = as.numeric(factor(temp_dat$site_name)))
  site_key <-  site_key %>% distinct()
  
  moddat <- list(pres = temp_dat$pres, 
                 visits = temp_dat$visits,
                 #year = scale(temp_dat$Year)[,1],
                 year = temp_dat$Year,
                 sites = as.numeric(factor(temp_dat$site_name)),
                 N=length(temp_dat$site_name), 
                 Nsite = length(unique(temp_dat$site_name)))
  
  if (moddat$Nsite == 0) {}
  
  if (moddat$Nsite == 1) {
    counter = counter + 1
    mod1 <- jagsUI::jags(data = moddat,
                         n.adapt = 10000,
                         n.burnin = 1000,
                         n.iter = 100000,
                         n.chains = 2,
                         modules = "glm",
                         model.file = "model_by_species_PA_one.txt",
                         parameters.to.save = c("beta", "int"),
                         verbose = FALSE)
    
    summ_dat <- data.frame(genus_species = species[i],
                           site_name = site_key$site_name,
                           med = mod1$q50$beta,
                           lc = mod1$q2.5$beta,
                           uc = mod1$q97.5$beta,
                           prec = NA,
                           Rhat = mod1$Rhat$beta,
                           n.eff = mod1$n.eff$beta,
                           pinc = length(mod1$sims.list$beta[mod1$sims.list$beta > 0]) /198000,
                           pdec = length(mod1$sims.list$beta[mod1$sims.list$beta < 0]) /198000)
    
  }
  
  if (moddat$Nsite > 1) {
    counter = counter + 1
    
    #Calls the code and compiles it
    mod1 <- jagsUI::jags(data = moddat,
                         n.adapt = 10000,
                         n.burnin = 1000,
                         n.iter = 100000,
                         n.chains = 2,
                         modules = "glm",
                         model.file = "model_by_species_PA_mult.txt",
                         parameters.to.save = c("beta", "hbmu", "himu", "hbsig"),
                         verbose = FALSE)
    
    probsinc <- c()
    probsdec <- c()
    for (j in 1:length(unique(temp_dat$site_name))) {
      probsinc[j] <-  length(mod1$sims.list$beta[,j][mod1$sims.list$beta[,j] > 0]) /198000
      probsdec[j] <-  length(mod1$sims.list$beta[,j][mod1$sims.list$beta[,j] < 0]) /198000
    }
    summ_sites <- data.frame(genus_species = species[i], 
                             site_name = site_key$site_name, 
                             med = mod1$q50$beta,
                             lc = mod1$q2.5$beta,
                             uc = mod1$q97.5$beta,
                             prec = NA,
                             Rhat = mod1$Rhat$beta,
                             n.eff = mod1$n.eff$beta,
                             pinc = probsinc,
                             pdec = probsdec)
    
    summ_overall <- data.frame(genus_species = species[i],
                               site_name = "overall",
                               med = mod1$q50$hbmu,
                               lc = mod1$q2.5$hbmu,
                               uc = mod1$q97.5$hbmu,
                               prec = mod1$mean$hbsig,
                               Rhat = mod1$Rhat$hbmu,
                               n.eff = mod1$n.eff$hbmu,
                               pinc = length(mod1$sims.list$hbmu[mod1$sims.list$hbmu > 0]) /198000,
                               pdec = length(mod1$sims.list$hbmu[mod1$sims.list$hbmu < 0]) /198000)
    summ_dat <- rbind(summ_sites, summ_overall)
  }
  
  datalist[[counter]] <- summ_dat
  print(i)
}

#Final results
fin_PA <- data.table::rbindlist(datalist)

#Export model results
#write.csv(fin_PA, "trends2019_no_trinomials(half_score).csv", row.names=F)

