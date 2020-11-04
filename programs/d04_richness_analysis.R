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
n.aug <- 4 #number of species that may have been present but were undetected
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
                dim = c(n.route, n.year, n.species.aug))

for(hh in 1:n.route){
  for(tt in 1:n.year){
    for(ss in 1:n.species.aug){
      w.init[hh,tt,ss] <- ifelse(ss <= n.species, yes = 1, no = 0)
      
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
#' Check initials (should be summary of ys)
#' 
#' Owl names correspond with 1:10 rows
species.names
#' 
z.init[4,3,,] # rows are different species, M2, 2005
z.init[3,9,,] # rows are different species, M1, 2011
z.init[5,6,,] # rows are different species, N1, 2008


#' ## Run models
#' 
#' Mottd
richness.jagsout <- jags(data = richness.jags.data, 
                      inits = function(){list(z = z.init, w = w.init)}, 
                      parameters.to.save = c("mu.psi",
                                             "rho.psi",
                                             "omega",
                                             "beta.prebroad",
                                             "beta.pacific",
                                             "beta.mottled",
                                             "beta.crested",
                                             "beta.bw",
                                             "beta.spectacled",
                                             "beta.whiskered",
                                             "beta.gbarred",
                                             "beta.stygian",
                                             "beta.ghorned"), 
                      model.file = model.richness, 
                      n.chains = 3,
                      n.iter = 10000, #10000
                      n.burnin = 1000, #1000
                      n.thin = 10)




#' _____________________________________________________________________________
#' ## Save files
#' 
#' Jagsout Mottd
save(mottd.jagsout, file = "data/output_data/mottd_jagsout.Rdata")



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/d02_occupancy_analysis_mottd.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 