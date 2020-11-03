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
#' Jagsout Mottd
load(file = "data/output_data/mottd_jagsout.Rdata")

#' Survey list
(route.names <- c("EI1", "EI2", "M1",  "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' Create traceplots of parameters (creates PDF)
#'
#' Mottd
# MCMCtrace(mottd.jagsout,
#           pdf = TRUE,
#           open_pdf = FALSE,
#           filename = "mottdTrace",
#           wd = "output")




#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' ### Calculate median, 95% quantile and 5% quantile of psi posteriors by route
#' and year
#' 
#' For all analysis, remove years and routes that were not surveyed
(exclude.byrow <- c(3,4,9,15,19:24,27,33,40,44,56,63,64))
all.rows <- 1:(length(route.index)*length(year.index))
(include.byrow <- all.rows[!all.rows %in% exclude.byrow])
#' 

#' Mottd
#' 
psi.post.mottd <- MCMCsummary(mottd.jagsout, 
                              params = "psi", 
                              Rhat = TRUE,
                              n.eff = TRUE,
                              probs = c(0.05, 0.5, 0.95))
psi.post.mottd$Year <- rep(year.names, each = length(route.names))
psi.post.mottd$Route <- rep(c("EI.1", "EI.2", "M.1", "M.2", "N.1", "N.2"), length(year.index))
psi.post.mottd$Species <- "Mottd"
psi.post.mottd <- psi.post.mottd[include.byrow,]




#' Populate Region
#' 
psi.post.mottd$Region[psi.post.mottd$Route == "EI.1" |
                        psi.post.mottd$Route == "EI.2"] <- "El Imposible"
psi.post.mottd$Region[psi.post.mottd$Route == "N.1" |
                        psi.post.mottd$Route == "N.2"] <- "Nancuchiname"
psi.post.mottd$Region[psi.post.mottd$Route == "M.1" |
                        psi.post.mottd$Route == "M.2"] <- "Montecristo"
colnames(psi.post.mottd)
colnames(psi.post.mottd) <- c("Psi.mean", "Psi.sd", "Psi.LL05", "Psi.median",
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
exc.M.1 <- c(1:6,11)
inc.M.1 <- all.years[!all.years %in% exc.M.1]
exc.M.2 <- c(1,4,7,11)
inc.M.2 <- all.years[!all.years %in% exc.M.2]
exc.N.1 <- 4
inc.N.1 <- all.years[!all.years %in% exc.N.1]
exc.N.2 <- 4
inc.N.2 <- all.years[!all.years %in% exc.N.2]
#' 


#' Psi posteriors for Mottd
#' 
mottd.chains <- MCMCpstr(mottd.jagsout, 
                         params = "psi",
                         type = "chains")



#' Calculate by species and route
#' 
psi.means.mottd <- as.data.frame(matrix(NA, ncol = 5, nrow = length(route.names)))
colnames(psi.means.mottd) <- c("Species", "Route", "Psi.LL05", "Psi.median", "Psi.UL95")
psi.means.mottd$Species <- "Mottd"
psi.means.mottd$Route <- route.names

#' Populate quantile results
#' 
psi.means.mottd[psi.means.mottd$Route == "EI1",3:5] <- 
  quantile(mottd.chains$psi[1,inc.EI.1,], probs = c(0.05, 0.5, 0.95))
psi.means.mottd[psi.means.mottd$Route == "EI2",3:5] <- 
  quantile(mottd.chains$psi[2,inc.EI.2,], probs = c(0.05, 0.5, 0.95))
psi.means.mottd[psi.means.mottd$Route == "M1",3:5] <- 
  quantile(mottd.chains$psi[3,inc.M.1,], probs = c(0.05, 0.5, 0.95))
psi.means.mottd[psi.means.mottd$Route == "M2",3:5] <- 
  quantile(mottd.chains$psi[4,inc.M.2,], probs = c(0.05, 0.5, 0.95))
psi.means.mottd[psi.means.mottd$Route == "N1",3:5] <- 
  quantile(mottd.chains$psi[5,inc.N.1,], probs = c(0.05, 0.5, 0.95))
psi.means.mottd[psi.means.mottd$Route == "N2",3:5] <- 
  quantile(mottd.chains$psi[6,inc.N.2,], probs = c(0.05, 0.5, 0.95))



#' Populate Region
#' 
psi.means.mottd$Region[psi.means.mottd$Route == "EI1" |
                   psi.means.mottd$Route == "EI2"] <- "El Imposible"
psi.means.mottd$Region[psi.means.mottd$Route == "N1" |
                   psi.means.mottd$Route == "N2"] <- "Nancuchiname"
psi.means.mottd$Region[psi.means.mottd$Route == "M1" |
                   psi.means.mottd$Route == "M2"] <- "Montecristo"


#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Probability of detection was a function of broadcast species
#' 
#' Mottd
#' 
p.det.post.mottd <- MCMCsummary(mottd.jagsout, 
                                params = grep("^beta", 
                                              mottd.jagsout$parameters.to.save, 
                                              value = T), 
                                Rhat = TRUE,
                                n.eff = TRUE,
                                probs = c(0.05, 0.5, 0.95))
p.det.post.mottd$Species <- "Mottd"
p.det.post.mottd$broadcast.param <- grep("^beta", 
                                         mottd.jagsout$parameters.to.save, 
                                         value = T)
p.det.post.mottd



#' Merge data together and change column names
#' 
colnames(p.det.post.mottd)
colnames(p.det.post.mottd) <- c("mean", "sd", "LL05", "median", "UL95", "Rhat",
                          "n.eff", "Species", "broadcast.param")

#' Transform to probability scale
#' 
p.det.post.mottd$LL05.plogis <- plogis(p.det.post.mottd$LL05)
p.det.post.mottd$median.plogis <- plogis(p.det.post.mottd$median)
p.det.post.mottd$UL95.plogis <- plogis(p.det.post.mottd$UL95)

#' Save broadcast species as a factor, ordered by confidence
#' 
p.det.post.mottd$Broadcast <- factor(p.det.post.mottd$broadcast.param,
                                  levels = c("beta.prebroad", "beta.mottled",
                                             "beta.pacific", "beta.crested",
                                             "beta.bw", "beta.spectacled",
                                             "beta.whiskered", "beta.gbarred",
                                             "beta.stygian", "beta.ghorned"),
                                  labels = c("Pre-broadcast", "Mottled",
                                             "Pacific", "Crested", "Black and White",
                                             "Spectacled", "Whiskered", "Guat Barred",
                                             "Stygian", "Great Horned"))

#' _____________________________________________________________________________
#' ## Save files
#' 
#' Psi Posteriors by year and route
save(psi.post.mottd, file = "data/output_data/mottd_psi_posteriors_RtYr.Rdata")

#' Psi posteriors across years by species and route
save(psi.means.mottd, file = "data/output_data/mottd_psi_posteriors_RtSpp.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
save(p.det.post.mottd, file = "data/output_data/mottd_p_detection_posteriors.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/e02_processing_results_mottd.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 