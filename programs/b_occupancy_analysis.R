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
load(file = "data/processed_data/ks_jags_input.Rdata")
load(file = "data/processed_data/owl_data.Rdata")


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







#' Turn ys and ks into arrays
mottd.ys.array <- array(as.numeric(unlist(mottd.ys)), 
                        dim = c(10, 2, length(lookup.hhttii.names)))

#' Demonstrate that in Route 1, year 1, and survey 1, 
#' Ys pull up stations*broadcast presence/absence data and
#' Ks pull up stations*broadcast covariate data
#' 
mottd.ys.array[,,lookup.hhttii.array[1,1,1]]
ks.prebroad[,,lookup.hhttii.array[1,1,1]]
ks.bw[,,lookup.hhttii.array[1,1,1]]




#' _____________________________________________________________________________
#' ## Write model
#' 
model.occ <- function(){
  # Priors
  beta.prebroad ~ dt(0, pow(2.5, -2), 1)
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
      mu.psi[hh,tt] ~ dunif(0.01,0.99)
      lnrho.psi[hh,tt] ~ dnorm(5,1)%_%T(0.01,10)
      rho.psi[hh,tt] <- exp(lnrho.psi[hh,tt])
      a.psi[hh,tt] <- mu.psi[hh,tt]*rho.psi[hh,tt]
      b.psi[hh,tt] <- rho.psi[hh,tt]-(mu.psi[hh,tt]*rho.psi[hh,tt])
      

      
      psi[hh,tt] ~ dbeta(a.psi[hh,tt], b.psi[hh,tt])%_%T(0.0001,0.99)
      
      for(ii in 1:n.survey){ # 1 to 3 surveys per year

        
        for(jj in 1:10){ # 10 stations per route
          for(kk in 1:n.broadcast){ # before or after broadcast
            
            
            p[hh,tt,ii,jj,kk] <- exp(logit.p[hh,tt,ii,jj,kk])/(1+exp(logit.p[hh,tt,ii,jj,kk]))
            
            logit.p[hh,tt,ii,jj,kk] <- 
              beta.prebroad*ks.prebroad[jj,kk,lookup.hhttii.array[hh,tt,ii]]+
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
        

        
        for(jj in 1:n.station){ # 10 stations per route
          # Occupancy by route and year and survey
          z[hh,tt,ii,jj] ~ dbern(psi[hh,tt]) 
          
          for(kk in 1:n.broadcast){ # before or after broadcast
            # observations by route, year, survey, station, pre/post broadcast
            y[jj,kk,lookup.hhttii.array[hh,tt,ii]] ~ 
              dbern(eff.p[hh,tt,ii,jj,kk])
            
            eff.p[hh,tt,ii,jj,kk] <- p[hh,tt,ii,jj,kk]*z[hh,tt,ii,jj]
            
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
  y = mottd.ys.array,
  n.route = n.route,
  n.year = n.year,
  n.survey = n.survey,
  n.station = n.station,
  lookup.hhttii.array = lookup.hhttii.array,
  n.broadcast = n.broadcast
)
z.init <- array(as.numeric(1), 
                dim = c(n.route, n.year, n.survey, n.station))

#' ## Run model
#' 
mottd.jagsout <- jags(data = mottd.jag.data, 
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