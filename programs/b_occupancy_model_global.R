#' # Occupancy model
#' 
#' Description: This program uses a Bayesian occupancy model to analyze
#' occupancy of each route and year for each species of owl. 
#' 
#' 


#' _____________________________________________________________________________
#' ## Write model
#' 
model.occ <- function(){
  # Priors for determining the probability of detection based on broadcast species
  beta.prebroad ~ dt(0, pow(2.5, -2), 1) #p(detection) before any broadcast
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
      #Hyperpriors for Psi ensure a flexible and uninformed prior
      mu.psi[hh,tt] ~ dunif(0.01,0.99)
      lnrho.psi[hh,tt] ~ dnorm(5,1)%_%T(0.01,10)
      rho.psi[hh,tt] <- exp(lnrho.psi[hh,tt])
      a.psi[hh,tt] <- mu.psi[hh,tt]*rho.psi[hh,tt]
      b.psi[hh,tt] <- rho.psi[hh,tt]-(mu.psi[hh,tt]*rho.psi[hh,tt])
      
      
      # Prior for Psi, which will vary by route (hh) and year (tt)
      psi[hh,tt] ~ dbeta(a.psi[hh,tt], b.psi[hh,tt])%_%T(0.0001,0.99)
      
      
      for(ii in 1:n.survey){ # 1 to 3 surveys per year
        for(jj in 1:10){ # 10 stations per route
          for(kk in 1:n.broadcast){ # before or after broadcast
            
            # Function that creates logistic equation for p(detection)
            
            # p = p(detection), which varies by route, year, survey, station, and 
            #       broadcast period (pre- or post-broadcast)
            p[hh,tt,ii,jj,kk] <- exp(logit.p[hh,tt,ii,jj,kk])/
              (1+exp(logit.p[hh,tt,ii,jj,kk]))
            
            # Logistic regression equation
            logit.p[hh,tt,ii,jj,kk] <- 
              beta.prebroad*ks.prebroad[hh,jj,kk]+
              beta.pacific*ks.pacific[hh,jj,kk]+
              beta.mottled*ks.mottled[hh,jj,kk]+
              beta.crested*ks.crested[hh,jj,kk]+
              beta.bw*ks.bw[hh,jj,kk]+
              beta.spectacled*ks.spectacled[hh,jj,kk]+
              beta.whiskered*ks.whiskered[hh,jj,kk]+
              beta.gbarred*ks.gbarred[hh,jj,kk]+
              beta.stygian*ks.stygian[hh,jj,kk]+
              beta.ghorned*ks.ghorned[hh,jj,kk]
            
          }
        }
      }
    }
  }
  
  
  
  # Likelihood
  for(hh in 1:n.route){ # 6 routes
    for(tt in 1:n.year){ # all years
      for(ii in 1:n.survey){ # 1 to 3 surveys per year
        for(jj in 1:n.station){ # 10 stations per route
          # Occupancy by route and year and survey and station based on 
          #     psi, probability of occupancy for each route/year
          z[hh,tt,ii,jj] ~ dbern(psi[hh,tt]) 
          
          for(kk in 1:n.broadcast){ # before or after broadcast
            # Detection by route, year, survey station, and broadcast period
            
            # Binary observations by route, year, survey, station, pre/post broadcast
            y[hh,tt,ii,jj,kk] ~ 
              dbern(eff.p[hh,tt,ii,jj,kk])
            
            # Effective p(detection), which depends on occupany (z) = 1 for 
            #    that route/year/survey/station
            eff.p[hh,tt,ii,jj,kk] <- p[hh,tt,ii,jj,kk]*z[hh,tt,ii,jj]
            
          }
        }
      }
    }
  }
}



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/b_occupancy_model_global.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 