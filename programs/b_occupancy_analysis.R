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

n.year <- length(unique(data.jags$year)) # tt
year.names <- unique(data.jags$year)

n.survey <- max(data.jags$order) # ii
n.station <- n.route*10 # jj
n.broadcast <- 2 # kk
ks.levels <- 0:9 # all levels possible of k

#' Look up the name of the y-matrix for each possible route/year/survey
lookup.hhttii <- NULL
for(ii in 1:3){
  lookup.hhttii[[ii]] <- matrix(NA, nrow = n.route, ncol = n.year)
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      lookup.hhttii[[ii]][hh,tt] <- paste0(route.names[hh],".",year.names[tt],".",ii)
    }
  }
}
names.mottd.ys <- names(mottd.ys)
index.mottd.ys <- 1:length(names.mottd.ys)
lookup.hhttii.array <- array(NA, dim = c(n.route,n.year,n.survey))
for(ii in 1:3){
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      if(length(index.mottd.ys[names.mottd.ys == lookup.hhttii[[ii]][hh,tt]]) == 0){
        lookup.hhttii.array[hh,tt,ii] <- NA
      }else{
        lookup.hhttii.array[hh,tt,ii] <- 
          index.mottd.ys[names.mottd.ys == lookup.hhttii[[ii]][hh,tt]]
      }
    }
  }
}

#' Turn ys and ks into arrays
ys.array <- array(as.numeric(unlist(mottd.ys)), dim = c(10, 2, length(mottd.ys)))
ks.array <- array(as.numeric(unlist(ks)), dim = c(10,2,6))




#' _____________________________________________________________________________
#' ## Write model
#' 
model.occ <- function(){
  # Priors
  alpha.p ~ dnorm(0,10)
  beta.p ~ dnorm(0,10)
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      psi[hh,tt] ~ dunif(0,1)
    }
  }
  
  for(jj in 1:10){
    for(kk in 1:2){
      eff.p[jj,kk] ~ dunif(0,1)
    }
  }
  
  
  # Likelihood
  for(hh in 1:n.route){ # 6 routes
    for(tt in 1:n.year){ # all years
      # Occupancy by route and year
      z[hh,tt] ~ dbern(psi[hh,tt]) 
      
      for(ii in 1:surveys.lookup[hh,tt]){ # 1 to 3 surveys per year
        for(jj in 1:10){ # 10 stations per route
          for(kk in 1:n.broadcast){ # before or after broadcast
            # observations by route, year, survey, station, pre/post broadcast
            y[jj,kk,lookup.hhttii.array[hh,tt,ii]] ~ dbin(eff.p[jj,kk],1)
            
            
            
            
            # # p varies by station broadcast species
            # eff.p[jj,kk,hh,tt] <- 
            #   z[hh,tt]*p[jj,kk,hh,tt]
            # 
            # logit.p[jj,kk,hh,tt] <- 
            #   alpha.p + beta.p*k[ks1[jj,kk,hh]]
            # 
            # p[jj,kk,hh,tt] <- 
            #   exp(logit.p[jj,kk,hh,tt])/
            #   (1+exp(logit.p[jj,kk,hh,tt]))
            
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
  k = ks.levels, # 0 if pre-broadcast, 1-9 if after broadcast (0:9)
  #k1 = ks.levels+1, # Index for p (1:10)
  #ks = ks.array, # Look up for k value
  ks1 = ks.array + 1, # Look up for k1 value
  y = ys.array,
  n.route = n.route,
  #route.names = route.names,
  n.year = n.year,
  #year.names = year.names,
  #n.survey = n.survey,
  surveys.lookup = mottd.surveys.lookup,
  lookup.hhttii.array = lookup.hhttii.array,
  #n.station = n.station,
  n.broadcast = n.broadcast
)

#' ## Run model
#' 
jagsout <- jags(data = mottd.jag.data, 
                #inits = , 
                parameters.to.save = c("alpha.p","beta.p","z"), 
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