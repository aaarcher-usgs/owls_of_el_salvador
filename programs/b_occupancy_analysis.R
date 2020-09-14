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

#' Convert ks into a series of 10 model matrices
#' 
#' For example means parameterization, w/ 1s for Pacific screech owl in one matrix, etc
#' 
ks.pacific.list <- rapply(ks, function(x) ifelse(x == "Pacific Screech Owl", 1, 0), how = "replace")
ks.pacific <- array(as.numeric(unlist(ks.pacific.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.mottled.list <- rapply(ks, function(x) ifelse(x == "Mottled Owl", 1, 0), how = "replace")
ks.mottled <- array(as.numeric(unlist(ks.mottled.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.crested.list <- rapply(ks, function(x) ifelse(x == "Crested Owl", 1, 0), how = "replace")
ks.crested <- array(as.numeric(unlist(ks.crested.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.bw.list <- rapply(ks, function(x) ifelse(x == "Black and White Owl", 1, 0), how = "replace")
ks.bw <- array(as.numeric(unlist(ks.bw.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.spectacled.list <- rapply(ks, function(x) ifelse(x == "Spectacled Owl", 1, 0), how = "replace")
ks.spectacled <- array(as.numeric(unlist(ks.spectacled.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.whiskered.list <- rapply(ks, function(x) ifelse(x == "Whiskered", 1, 0), how = "replace")
ks.whiskered <- array(as.numeric(unlist(ks.whiskered.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.gbarred.list <- rapply(ks, function(x) ifelse(x == "Guat. Barred Owl", 1, 0), how = "replace")
ks.gbarred <- array(as.numeric(unlist(ks.gbarred.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.stygian.list <- rapply(ks, function(x) ifelse(x == "Stygian Owl", 1, 0), how = "replace")
ks.stygian <- array(as.numeric(unlist(ks.stygian.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.ghorned.list <- rapply(ks, function(x) ifelse(x == "Great Horned Owl", 1, 0), how = "replace")
ks.ghorned <- array(as.numeric(unlist(ks.ghorned.list)), dim = c(10, 2, length(lookup.hhttii.names)))



#' All levels of k
#' 
k.names <- unique(as.character(ks.array[,2,]))
ks.levels <- c(0, 1:length(k.names)) # 0 if pre-broadcast, 1:9 if post-broadcast


#' _____________________________________________________________________________
#' ## Write model
#' 
model.occ <- function(){
  # Priors
  beta.pacific ~  dt(0, pow(2.5, -2), 1)
  beta.mottled ~ dt(0, pow(2.5, -2), 1)
  beta.crested ~ dt(0, pow(2.5, -2), 1)
  beta.bw ~ dt(0, pow(2.5, -2), 1)
  beta.spectacled ~ dt(0, pow(2.5, -2), 1)
  beta.whiskered ~ dt(0, pow(2.5, -2), 1)
  beta.gbarred ~ dt(0, pow(2.5, -2), 1)
  beta.stygian ~ dt(0, pow(2.5, -2), 1)
  beta.ghorned ~ dt(0, pow(2.5, -2), 1)
  
  
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      psi[hh,tt] ~ dunif(0,1)
      for(ii in 1:n.survey){ # 1 to 3 surveys per year
        for(jj in 1:10){ # 10 stations per route
          for(kk in 1:n.broadcast){ # before or after broadcast
            
            
            p[hh,tt,ii,jj,kk] <- exp(logit.p[hh,tt,ii,jj,kk])/(1+exp(logit.p[hh,tt,ii,jj,kk]))
            
            logit.p[hh,tt,ii,jj,kk] <- 
              beta.pacific*ks.pacific[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.mottled*ks.mottled[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.crested*ks.crested[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.bw*ks.bw[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.spectacled*ks.spectacled[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.whiskered*ks.whiskered[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.gbarred*ks.gbarred[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.stygian*ks.stygian[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
              beta.ghorned*ks.ghorned[jj,kk,lookup.hhttii.array[hh,tt,ii]]
            
          }
        }
      }
    }
  }

  
  
  # Likelihood
  for(hh in 1:n.route){ # 6 routes
    for(tt in 1:n.year){ # all years

      

      
      # Detection by route, year, survey station, and broadcast
      for(ii in 1:n.survey){ # 1 to 3 surveys per year
        
        # Occupancy by route and year and survey
        z[hh,tt,ii] ~ dbern(psi[hh,tt]) 
        
        for(jj in 1:10){ # 10 stations per route
          for(kk in 1:n.broadcast){ # before or after broadcast
            # observations by route, year, survey, station, pre/post broadcast
            y[jj,kk,lookup.hhttii.array[hh,tt,ii]] ~ 
              dbern(p[hh,tt,ii,jj,kk])
            

            
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
  ks.pacific = ks.pacific,
  ks.mottled = ks.mottled,
  ks.crested = ks.crested,
  ks.bw = ks.bw,
  ks.spectacled = ks.spectacled,
  ks.whiskered = ks.whiskered,
  ks.gbarred = ks.gbarred,
  ks.stygian = ks.stygian,
  ks.ghorned = ks.ghorned,
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
                parameters.to.save = c("psi",
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