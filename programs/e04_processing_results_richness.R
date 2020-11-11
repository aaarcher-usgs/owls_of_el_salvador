#' # Processing Richness Analysis
#' 
#' Description: This program takes posteriors from a Bayesian richness model to analyze
#' richness by route and year and prepares it for plotting.
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
library(tidyr)
library(dplyr)
library(MCMCvis)

#' Clear environment and set seed
#' 
#' *Note: for reproducibility, it is important to include these. Clearing the
#' environment ensures that you have specified all pertinent files that need
#' to be loaded, and setting the seed ensures that your analysis is 
#' repeatable*
remove(list = ls())
set.seed(258854)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' 
#' Jagsout Richness Model
#load(file = "data/output_data/richness_jagsout.Rdata")

#' Access tables, in Rdata format
#' 
load(file = "data/processed_data/tables_global.Rdata")

#' Species accounts to summarize by route
#' 
load("../data/output_data/richness_species_accounts.Rdata")

#' Survey list
(route.names <- c("EI1", "EI2", "M1", "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)

#' 
#' There were 10 observed owls (if we pool all three unknown observations)
(species.names <- unique(tab.owls$Owl_Species_ID))
n.species <- length(species.names)

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' Create traceplots of parameters (creates PDF)
#'
#' Richness
# MCMCtrace(richness.jagsout,
#           pdf = TRUE,
#           open_pdf = FALSE,
#           filename = "richnessTrace",
#           wd = "output")


#' Species presence by route
#' 
species.accounts.byRt <- as.data.frame(matrix(
  NA, nrow = length(unique(species.accounts$Route)), 
  ncol = ncol(species.accounts)
  ))
colnames(species.accounts.byRt) <- colnames(species.accounts)
species.accounts.byRt$Route <- unique(species.accounts$Route)
species.accounts.byRt$Year <- c("2003-2013","2003-2013",
                                "2003-2013","2003-2013",
                                "2009-2012","2004-2012")
species.accounts.byRt$Region <- rep(unique(species.accounts$Region),each=2)
for(hh in 1:nrow(species.accounts.byRt)){
  temp.route <- species.accounts.byRt$Route[hh]
  temp.accounts <- species.accounts[species.accounts$Route == temp.route,]
  for(ss in 1:10){ #across all 10 species
    species.accounts.byRt[hh,ss] <- ifelse(sum(temp.accounts[,ss],na.rm = T)>0,1,0)
  }
}

#' _____________________________________________________________________________
#' ## Save files
#' 
save(species.accounts.byRt, file = "data/output_data/richness_species_accounts_byRt.Rdata")

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/e04_processing_results_richness.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 