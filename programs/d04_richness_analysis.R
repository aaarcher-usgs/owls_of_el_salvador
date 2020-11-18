#' # Analyzing Species Richness for each Route and Year
#' 
#' Description: This program uses a Bayesian species richness model and data 
#' augmentation to analyze
#' species richness of each route and year. 
#' 
#' 
#' 
#' ### Preamble
#' 
#' Load libraries
#+ libraries, message = F, warning = F
library(knitr) # documentation-related
library(ezknitr) # documentation-related
library(devtools) # documentation-related

# analysis-related
library(R2jags)

#' Clear environment and set seed
#' 
#' *Note: for reproducibility, it is important to include these. Clearing the
#' environment ensures that you have specified all pertinent files that need
#' to be loaded, and setting the seed ensures that your analysis is 
#' repeatable*
remove(list = ls())
set.seed(1770562)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' 
#' Processed Owl Data
load(file = "data/processed_data/augmented_jags_ys.Rdata")


#' Processed Support data
load(file = "data/processed_data/augmented_jags_ks.Rdata")

#' Access tables, in Rdata format
#' 
load(file = "data/processed_data/tables_global.Rdata")

#' Supplemental JAGS table
#' 
load(file = "data/processed_data/data_jags_global.Rdata")


#' _____________________________________________________________________________
#' ## Define variables
#' 
#' Observations
n.route <- length(unique(data.jags$Route_ID)) # hh
route.names <- unique(data.jags$Route_ID)

n.year <- length(min(data.jags$year):max(data.jags$year)) # tt
year.names <- min(data.jags$year):max(data.jags$year)

n.survey <- max(data.jags$order) # ii
n.station <- 10 # jj
n.broadcast <- 2 # kk


#' **Augmenting species number**
#' 
#' There were 10 observed owls (if we pool all three unknown observations)
(species.names <- unique(tab.owls$Owl_Species_ID))
n.species <- length(species.names)

#' We should augment with M >> n, where n = 10
#' 
#' Owls of El Salvador lists 13 total species recorded in El Salvador (Table 11.1, p401)
#' 
n.aug <- 5 #number of species that may have been present but were undetected
n.species.aug <- n.species + n.aug




#' _____________________________________________________________________________
#' ## Read model
#' 
source(file = "programs/b_richness_model_global.R")


#' _____________________________________________________________________________
#' ## Compile data for Mottd model
#' 
richness.jags.data <- list(
  ks.prebroad = ks.prebroad,
  ks.pacific = ks.pacific,
  ks.mottled = ks.mottled,
  ks.crested = ks.crested,
  ks.bw = ks.bw,
  ks.spectacled = ks.spectacled,
  ks.whiskered = ks.whiskered,
  ks.gbarred = ks.gbarred,
  ks.stygian = ks.stygian,
  ks.ghorned = ks.ghorned,
  y = ys.aug,
  n.route = n.route,
  n.year = n.year,
  n.survey = n.survey,
  n.station = n.station,
  n.broadcast = n.broadcast,
  n.species.aug = n.species.aug
)

#' Initial values have 1s for each obs of z
z.init <- array(NA, 
                dim = c(n.route, n.year, n.species.aug, n.survey))
w.init <- array(NA,
                dim = c(n.route, n.species.aug))

for(hh in 1:n.route){
  for(tt in 1:n.year){
    for(ss in 1:n.species.aug){

      
      for(ii in 1:n.survey){
        if(sum(is.na(ys.aug[hh,tt,ss,ii,,])==FALSE)==0){ # no survey conducted here
          z.init[hh,tt,ss,ii] <- 0
        }else {
          z.init[hh,tt,ss,ii] <- max(ys.aug[hh,tt,ss,ii,,], na.rm = T)
        } 
      }
    }
  }
}
for(hh in 1:n.route){
    for(ss in 1:n.species){
      w.init[hh,ss] <- max(z.init[hh,,ss,], na.rm = T)
    }
    for(ss in (n.species+1):n.species.aug){
      w.init[hh,ss] <- 0
    }
}

#' Check initials (should be summary of ys)
#'
#' 
#' Owl names correspond with 1:10 rows (z) or 1:10 cols (w)
species.names
#' 
w.init
#' 
z.init[4,3,,] # rows are different species, M2, 2005
z.init[3,9,,] # rows are different species, M1, 2011
z.init[5,6,,] # rows are different species, N1, 2008


#' ## Run models
#' 
#' Richness, 
#+ jags, eval = FALSE
richness.jagsout <- jags(data = richness.jags.data, 
                      inits = function(){list(z = z.init, w = w.init)}, 
                      parameters.to.save = c("mu.psi",
                                             "rho.psi",
                                             "omega",
                                             "spp.occ",
                                             "Nsmall",
                                             "richness",
                                             "richness.RtYr",
                                             "beta.prebroad",
                                             "beta.pacific",
                                             "beta.mottled",
                                             "beta.crested",
                                             "beta.bw",
                                             "beta.spectacled",
                                             "beta.whiskered",
                                             "beta.gbarred",
                                             "beta.stygian",
                                             "beta.ghorned",
                                             "species.present"), 
                      model.file = model.richness, 
                      n.chains = 3,
                      n.iter = 20000, #10000
                      n.burnin = 2000, #1000
                      n.thin = 2)


#' _____________________________________________________________________________
#' ## Post-process Psi = Probability of occupancy
#' 
#' Survey list
(route.names <- c("EI1", "EI2", "M1", "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)
#' ### Calculate median, 95% quantile and 5% quantile of psi posteriors by route
#' and year
#' 
#' For all analysis, remove years and routes that were not surveyed
(exclude.byrow <- c(3,4,9,15,19:24,27,33,40,44,56,63,64))
all.rows <- 1:(length(route.index)*length(year.index))
(include.byrow <- all.rows[!all.rows %in% exclude.byrow])
#' 

#' Richness
#' 
richness.post <- MCMCsummary(richness.jagsout, 
                             params = "richness", 
                             Rhat = TRUE,
                             n.eff = TRUE,
                             probs = c(0.05, 0.5, 0.95))
richness.post$Year <- rep(year.names, each = length(route.index))
richness.post$Route <- 
  rep(c("EI1", "EI2", "M1", "M2", "N1", "N2"), length(year.index))
richness.post <- richness.post[include.byrow,]



#' Populate Region
#' 
richness.post$Region[richness.post$Route == "EI1" |
                       richness.post$Route == "EI2"] <- "El Imposible"
richness.post$Region[richness.post$Route == "N1" |
                       richness.post$Route == "N2"] <- "Nancuchiname"
richness.post$Region[richness.post$Route == "M1" |
                       richness.post$Route == "M2"] <- "Montecristo"
colnames(richness.post)
colnames(richness.post) <- c("Richness.mean", "Richness.sd", "Richness.LL05", 
                             "Richness.median",
                             "Richness.UL95", "Richness.Rhat", "Richness.neff", 
                             "Year", "Route", "Region")

#' Omega Post
#' 
omega.post <- MCMCsummary(richness.jagsout, 
                          params = "omega", 
                          Rhat = TRUE,
                          n.eff = TRUE,
                          probs = c(0.05, 0.5, 0.95))


#' _____________________________________________________________________________
#' ## Calculate number of species detected in each year/route
#' 
#' Create a table to determine what species were detected in what routes/years
species.accounts <- as.data.frame(matrix(
  NA, nrow = nrow(richness.post), ncol = n.species
))
colnames(species.accounts) <- species.names
species.accounts$Year <- richness.post$Year
species.accounts$Route <- richness.post$Route
species.accounts$Region <- richness.post$Region

#' 
#' 
richness.post$richness.detected <- NA
for(rr in 1:nrow(richness.post)){
  temp.route <- richness.post$Route[rr]
  temp.year <- richness.post$Year[rr]
  temp.surveys <- tab.survey$Survey_ID[tab.survey$Route_ID == temp.route &
                                         tab.survey$year == temp.year]
  temp.stations <- tab.stations$Stations_ID[tab.stations$Survey_ID %in% temp.surveys]
  temp.owls.data <- tab.owls[tab.owls$Stations_ID %in% temp.stations,]
  if(nrow(temp.owls.data)>0){
    temp.n.species.detected <- length(unique(temp.owls.data$Owl_Species_ID))
    richness.post$richness.detected[rr] <- temp.n.species.detected
    for(ss in 1:temp.n.species.detected){
      temp.species <- unique(temp.owls.data$Owl_Species_ID)[ss]
      species.accounts[rr,temp.species] <- 1
    }
  }else{
    richness.post$richness.detected[rr] <- 0
  }
}



#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Probability of detection was a function of broadcast species
#' 
#' 
p.det.post.richness <- MCMCsummary(richness.jagsout, 
                                   params = grep("^beta", 
                                                 richness.jagsout$parameters.to.save, 
                                                 value = T), 
                                   Rhat = TRUE,
                                   n.eff = TRUE,
                                   probs = c(0.05, 0.5, 0.95))
p.det.post.richness$broadcast.param <- grep("^beta", 
                                            richness.jagsout$parameters.to.save, 
                                            value = T)
p.det.post.richness



#' Merge data together and change column names
#' 
colnames(p.det.post.richness)
colnames(p.det.post.richness) <- c("mean", "sd", "LL05", "median", "UL95", "Rhat",
                                   "n.eff", "broadcast.param")

#' Transform to probability scale
#' 
p.det.post.richness$LL05.plogis <- plogis(p.det.post.richness$LL05)
p.det.post.richness$median.plogis <- plogis(p.det.post.richness$median)
p.det.post.richness$UL95.plogis <- plogis(p.det.post.richness$UL95)

#' Save broadcast species as a factor, ordered by confidence
#' 
p.det.post.richness$Broadcast <- factor(p.det.post.richness$broadcast.param,
                                        levels = c("beta.prebroad", "beta.mottled",
                                                   "beta.pacific", "beta.crested",
                                                   "beta.bw", "beta.spectacled",
                                                   "beta.whiskered", "beta.gbarred",
                                                   "beta.stygian", "beta.ghorned"),
                                        labels = c("Pre-broadcast", "Mottled",
                                                   "Pacific", "Crested", "Black and White",
                                                   "Spectacled", "Whiskered", "Guat Barred",
                                                   "Stygian", "Great Horned"))

#' _____________________________________________________________________________
#' ## Save files
#' 
#' Jagsout Mottd
#+ savejags, eval = F
#save(richness.post, file = "data/output_data/richness_jagsout_posteriors.Rdata")
#' Psi Posteriors by year and route
save(richness.post, file = "data/output_data/richness_psi_posteriors_RtYr.Rdata")


#' Probability of detection by broadcast species and species of analysis
#' 
save(p.det.post.richness, file = "data/output_data/richness_p_detection_posteriors.Rdata")

#' Omega Posteriors
#' 
save(omega.post, file = "data/output_data/richness_omega_posteriors.Rdata")



#' Species Accounts
#' 
save(species.accounts, file = "data/output_data/richness_species_accounts.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/d04_richness_analysis.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 