#' # Analyzing Occupancy for each Route and Year
#' 
#' Description: This program uses a Bayesian occupancy model to analyze
#' occupancy of each route and year for each species of owl. 
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
#' Ferpy Owl Ys
load(file = "data/processed_data/ferpy_jags_ys.Rdata")

#' Ferpy Owl Ks
load(file = "data/processed_data/ferpy_jags_ks.Rdata")

#' Global JAGS data
load(file = "data/processed_data/data_jags_global.Rdata")

#' _____________________________________________________________________________
#' ## Define variables
#' 
#' **FerPy was not technically surveyed for at M1, remove from analysis**
#' 
#' Observations
n.route <- length(unique(data.jags$Route_ID[data.jags$Route_ID != "M1"])) # hh
(route.names <- unique(data.jags$Route_ID[data.jags$Route_ID != "M1"])) #exclude M1

n.year <- length(min(data.jags$year):max(data.jags$year)) # tt
year.names <- min(data.jags$year):max(data.jags$year)

n.survey <- max(data.jags$order) # ii
n.station <- 10 # jj
n.broadcast <- 2 # kk







#' _____________________________________________________________________________
#' ## Read model
#' 
source(file = "programs/b_occupancy_model_global.R")


#' _____________________________________________________________________________
#' ## Compile data for Ferpy model
#' 
ferpy.jag.data <- list(
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
  y = ferpy.ys,
  n.route = n.route,
  n.year = n.year,
  n.survey = n.survey,
  n.station = n.station,
  n.broadcast = n.broadcast
)

#' Initial values have 1s for each obs of z
z.init <- array(as.numeric(1), 
                dim = c(n.route, n.year, n.survey, n.station))

#' ## Run models
#' 
#' Ferpy
ferpy.jagsout <- jags(data = ferpy.jag.data, 
                      inits = function(){list(z = z.init)}, 
                      parameters.to.save = c("psi",
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
                      model.file = model.occ, 
                      n.chains = 3,
                      n.iter = 10000,
                      n.burnin = 1000,
                      n.thin = 1)




#' _____________________________________________________________________________
#' ## Save files
#' 
#' Jagsout Ferpy
save(ferpy.jagsout, file = "data/output_data/ferpy_jagsout.Rdata")



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/d01_occupancy_analysis_ferpy.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 