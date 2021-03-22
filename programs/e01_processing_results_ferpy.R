#' # Processing Occupancy Analysis
#' 
#' Description: This program takes posteriors from a Bayesian occupancy model to analyze
#' occupancy of each route and year for each species of owl and prepares it for plotting.
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
#' Jagsout FerPy
load(file = "data/output_data/ferpy_jagsout.Rdata")

#' Survey list
(route.names <- c("EI1", "EI2", "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' Create traceplots of parameters (creates PDF)
#'
#' Ferpy
# MCMCtrace(ferpy.jagsout,
#           pdf = TRUE,
#           open_pdf = FALSE,
#           filename = "ferpyTrace",
#           wd = "output")




#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' ### Calculate median, 95% quantile and 5% quantile of psi posteriors by route
#' and year
#' 
#' For all analysis, remove years and routes that were not surveyed
(exclude.byrow <- c(3,16:20,33,37,47))
all.rows <- 1:(length(route.names)*length(year.index))
(include.byrow <- all.rows[!all.rows %in% exclude.byrow])
#' 

#' Ferpy
#' 
psi.post.ferpy <- MCMCsummary(ferpy.jagsout, 
                              params = "psi", 
                              Rhat = TRUE,
                              n.eff = TRUE,
                              probs = c(0.05, 0.5, 0.95))
psi.post.ferpy$Year <- rep(year.names, each = length(route.index))
psi.post.ferpy$Route <- rep(c("EI.1", "EI.2", "M.2", "N.1", "N.2"), length(year.index))
psi.post.ferpy$Species <- "FerPy"
psi.post.ferpy <- psi.post.ferpy[include.byrow,]




#' Populate Region
#' 
psi.post.ferpy$Region[psi.post.ferpy$Route == "EI.1" |
                        psi.post.ferpy$Route == "EI.2"] <- "El Imposible"
psi.post.ferpy$Region[psi.post.ferpy$Route == "N.1" |
                        psi.post.ferpy$Route == "N.2"] <- "Nancuchiname"
psi.post.ferpy$Region[psi.post.ferpy$Route == "M.1" |
                        psi.post.ferpy$Route == "M.2"] <- "Montecristo"
colnames(psi.post.ferpy)
colnames(psi.post.ferpy) <- c("Psi.mean", "Psi.sd", "Psi.LL05", "Psi.median",
                              "Psi.UL95", "Psi.Rhat", "Psi.neff", "Year", "Route",
                              "Species", "Region")


#' 
#' 
#' 
#' ### Calculate average occupancy probability by route and species
#' 
#' 
#' Need to delete a few years by route:
#' 
all.years <- 1:length(year.index)
exc.EI.1 <- 4
inc.EI.1 <- all.years[!all.years %in% exc.EI.1]
exc.EI.2 <- c(4,8,10)
inc.EI.2 <- all.years[!all.years %in% exc.EI.2]
exc.M.2 <- c(1,4,7,11)
inc.M.2 <- all.years[!all.years %in% exc.M.2]
exc.N.1 <- 4
inc.N.1 <- all.years[!all.years %in% exc.N.1]
exc.N.2 <- 4
inc.N.2 <- all.years[!all.years %in% exc.N.2]
#' 


#' Psi posteriors for Ferpy
#' 
ferpy.chains <- MCMCpstr(ferpy.jagsout, 
                         params = "psi",
                         type = "chains")



#' Calculate by species and route
#' 
psi.means.ferpy <- as.data.frame(matrix(NA, ncol = 5, nrow = length(route.names)))
colnames(psi.means.ferpy) <- c("Species", "Route", "Psi.LL05", "Psi.median", "Psi.UL95")
psi.means.ferpy$Species <- "FerPy"
psi.means.ferpy$Route <- route.names

#' Populate quantile results
#' 
psi.means.ferpy[psi.means.ferpy$Route == "EI1",3:5] <- 
  quantile(ferpy.chains$psi[1,inc.EI.1,], probs = c(0.05, 0.5, 0.95))
psi.means.ferpy[psi.means.ferpy$Route == "EI2",3:5] <- 
  quantile(ferpy.chains$psi[2,inc.EI.2,], probs = c(0.05, 0.5, 0.95))
psi.means.ferpy[psi.means.ferpy$Route == "M2",3:5] <- 
  quantile(ferpy.chains$psi[3,inc.M.2,], probs = c(0.05, 0.5, 0.95))
psi.means.ferpy[psi.means.ferpy$Route == "N1",3:5] <- 
  quantile(ferpy.chains$psi[4,inc.N.1,], probs = c(0.05, 0.5, 0.95))
psi.means.ferpy[psi.means.ferpy$Route == "N2",3:5] <- 
  quantile(ferpy.chains$psi[5,inc.N.2,], probs = c(0.05, 0.5, 0.95))



#' Populate Region
#' 
psi.means.ferpy$Region[psi.means.ferpy$Route == "EI1" |
                   psi.means.ferpy$Route == "EI2"] <- "El Imposible"
psi.means.ferpy$Region[psi.means.ferpy$Route == "N1" |
                   psi.means.ferpy$Route == "N2"] <- "Nancuchiname"
psi.means.ferpy$Region[psi.means.ferpy$Route == "M1" |
                   psi.means.ferpy$Route == "M2"] <- "Montecristo"


#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Probability of detection was a function of broadcast species
#' 
#' 
p.det.post.ferpy <- MCMCsummary(ferpy.jagsout, 
                                params = grep("^beta", 
                                              ferpy.jagsout$parameters.to.save, 
                                              value = T), 
                                Rhat = TRUE,
                                n.eff = TRUE,
                                probs = c(0.05, 0.5, 0.95))
p.det.post.ferpy$Species <- "FerPy"
p.det.post.ferpy$broadcast.param <- grep("^beta", 
                                         ferpy.jagsout$parameters.to.save, 
                                         value = T)
p.det.post.ferpy



#' Merge data together and change column names
#' 
colnames(p.det.post.ferpy)
colnames(p.det.post.ferpy) <- c("mean", "sd", "LL05", "median", "UL95", "Rhat",
                          "n.eff", "Species", "broadcast.param")

#' Transform to probability scale
#' 
p.det.post.ferpy$LL05.plogis <- plogis(p.det.post.ferpy$LL05)
p.det.post.ferpy$median.plogis <- plogis(p.det.post.ferpy$median)
p.det.post.ferpy$UL95.plogis <- plogis(p.det.post.ferpy$UL95)

#' Save broadcast species as a factor, ordered by confidence
#' 
p.det.post.ferpy$Broadcast <- factor(p.det.post.ferpy$broadcast.param,
                                  levels = c("beta.prebroad", "beta.mottled",
                                             "beta.pacific", "beta.crested",
                                             "beta.bw", "beta.spectacled",
                                             "beta.whiskered", "beta.gbarred",
                                             "beta.stygian", "beta.ghorned"),
                                  labels = c("Pre-broadcast", "Mottled Owl",
                                             "Pacific Screech-Owl", "Crested Owl", 
                                             "Black-and-white Owl",
                                             "Spectacled Owl", 
                                             "Whiskered Screech-Owl", "Fulvous Owl",
                                             "Stygian Owl", "Great Horned Owl"))

#' _____________________________________________________________________________
#' ## Save files
#' 
#' Psi Posteriors by year and route
save(psi.post.ferpy, file = "data/plotting_data/ferpy_psi_posteriors_RtYr.Rdata")

#' Psi posteriors across years by species and route
save(psi.means.ferpy, file = "data/plotting_data/ferpy_psi_posteriors_RtSpp.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
save(p.det.post.ferpy, file = "data/plotting_data/ferpy_p_detection_posteriors.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/e01_processing_results_ferpy.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 