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
#' ### Route Table
load(file = "data/processed_data/mottd_jags_input.Rdata")


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




#' Create a lookup table to link the route.year.survey dataset of Ys with a numerical index 1:198

lookup.hhttii.names <- names(mottd.ys)
lookup.hhttii.numb <- 1:length(lookup.hhttii.names)
lookup.hhttii.array <- array(NA, dim = c(n.route, n.year, n.survey))

for(ii in 1:n.survey){
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      
      temp.record <- 
        lookup.hhttii.numb[lookup.hhttii.names == paste0(route.names[hh],".",year.names[tt],".",ii)]
      
      lookup.hhttii.array[hh,tt,ii] <- temp.record
      
    }
  }
}
lookup.hhttii.array


#' Turn ys and ks into arrays
ys.array <- array(as.numeric(unlist(mottd.ys)), dim = c(10, 2, length(lookup.hhttii.names)))
ks.array <- array(unlist(ks), dim = c(10, 2, length(lookup.hhttii.names)))
ks.array.index <- array(as.numeric(unlist(ks.index.numb)), dim = c(10, 2, length(lookup.hhttii.names)))

#' Demonstrate that in Route 1, year 1, and survey 1, 
#' Ys pull up stations*broadcast presence/absence data and
#' Ks pull up stations*broadcast covariate data
#' 
ys.array[,,lookup.hhttii.array[1,1,1]]
ks.array[,,lookup.hhttii.array[1,1,1]]
ks.array.index[,,lookup.hhttii.array[1,1,1]]


#' All levels of k
#' 
k.names <- unique(as.character(ks.array[,2,]))
ks.levels <- c(0, 1:length(k.names)) # 0 if pre-broadcast, 1:9 if post-broadcast


#' _____________________________________________________________________________
#' ## Write model
#' 
model.occ <- function(){
  # Priors
  #alpha.p ~  dt(0, pow(2.5, -2), 1)
  for(kk in 1:length(ks.levels)){
    beta.p[kk] ~ dt(0, pow(2.5, -2), 1)
  }
 
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      psi[hh,tt] ~ dunif(0,1)
    }
  }
  

  
  
  # Likelihood
  for(hh in 1:n.route){ # 6 routes
    for(tt in 1:n.year){ # all years
      # Occupancy by route and year
      z[hh,tt] ~ dbern(psi[hh,tt]) 
      
      # Detection by route, year, survey station, and broadcast
      for(ii in 1:n.survey){ # 1 to 3 surveys per year
        for(jj in 1:10){ # 10 stations per route
          for(kk in 1:n.broadcast){ # before or after broadcast
            # observations by route, year, survey, station, pre/post broadcast
            y[jj,kk,lookup.hhttii.array[hh,tt,ii]] ~ 
              dbern(eff.p[ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]+1])
            
            # effective probability of detection based on presence
            eff.p[ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]+1] <- 
              z[hh,tt]*p[ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]+1]
            
            logit(p[ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]+1]) <-
              beta.p[ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]+1]*
              ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]
            
            
            
            # p based on broadcast species
            # logit.p[jj,kk,lookup.hhttii.array[hh,tt,ii]] <- 
            #   beta.p[ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]+1]* #slope (10 levels, means)
            #   ks.array.index[jj,kk,lookup.hhttii.array[hh,tt,ii]]    #covariate (categorical)
            # p[jj,kk,lookup.hhttii.array[hh,tt,ii]] <- exp(logit.p[jj,kk,lookup.hhttii.array[hh,tt,ii]])/
            #   (1+exp(p[jj,kk,lookup.hhttii.array[hh,tt,ii]]))
            

            
          }
        }
      }
    }
  }
}


#' _____________________________________________________________________________
#' ## Compile data for model
#' 
mottd.jag.data <- list(
  ks.levels = ks.levels, # 0 if pre-broadcast, 1-9 if after broadcast (0:9)
  ks.array = ks.array, # Look up for k1 character
  ks.array.index = ks.array.index, #array, 0 if pre-broadcast, 1-9 if after broadcast
  y = ys.array,
  n.route = n.route,
  n.year = n.year,
  n.survey = n.survey,
  lookup.hhttii.array = lookup.hhttii.array,
  n.station = n.station,
  n.broadcast = n.broadcast
)

#' ## Run model
#' 
jagsout <- jags(data = mottd.jag.data, 
                #inits = , 
                parameters.to.save = c("alpha.p","beta.p"), 
                model.file = model.occ, 
                n.chains = 3,
                n.iter = 1000,
                n.burnin = 100,
                n.thin = 1)


#' _____________________________________________________________________________
#' ## Save files
#' 



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/a_process_data.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 