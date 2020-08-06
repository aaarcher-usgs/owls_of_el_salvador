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
head(mottd.jags)

#' _____________________________________________________________________________
#' ## Define variables
#' 
#' Observations
y <- mottd.jags[,grep(colnames(mottd.jags), pattern = "^y")] # includes year & yearindex
y <- y[,! colnames(y) %in% c("year", "yearIndex")]
n.route <- nrow(y)
n.surveys <- ncol(y)

#' _____________________________________________________________________________
#' ## Write model
#' 
model.occ <- function(){
  # Priors
  alpha.p ~ dunif(-5, 5)
  beta.p ~ dunif(-5, 5)
  psi ~ dunif(0,1)
  
  # Likelihood
  for(hh in 1:n.route){
    z[hh] ~ dbern(psi[hh])
    for(jk in 1:n.surveys){
      y[hh,jk] ~ dbern(eff.p[hh,jk])
      eff.p[hh,jk] <- z[hh]*p[hh,jk]
      logit.p[hh,jk] <- alpha.p + beta.p*k[jk]
      p[hh,jk] <- exp(logit.p[hh,jk])(1+exp(logit.p[hh,jk]))
      
      # Fit statistics
      Presi[hh,jk] <- abs(y[hh,jk]-p[hh,jk]) # absolute residual
      y.new[hh,jk] ~ dbern(eff.p[hh,jj])
      Presi.new[hh,jk] <- abs(y.new[hh,jk]-p[hh,jk])
    }
  }
  fit <- sum(Presi[hh,jk]) # Discrepancy for actual data set
  fit.new <- sum(Presi.new[hh,jk]) # Discrepancy for replicate dataset
  
  # Derived quantities
  off.fs <- sum(z[])
}

#' _____________________________________________________________________________
#' ## Compile data for model
#' 




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