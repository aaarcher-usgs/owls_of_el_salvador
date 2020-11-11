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
load(file = "data/output_data/richness_jagsout.Rdata")

#' Access tables, in Rdata format
#' 
load(file = "data/processed_data/tables_global.Rdata")

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
#' ezspin(file = "programs/e04_processing_results_richness.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 