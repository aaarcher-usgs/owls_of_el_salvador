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
#' Jagsout Specd
load(file = "data/output_data/specd_jagsout.Rdata")

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
#' Specd
# MCMCtrace(specd.jagsout,
#           pdf = TRUE,
#           open_pdf = FALSE,
#           filename = "specdTrace",
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

#' Specd
#' 
psi.post.specd <- MCMCsummary(specd.jagsout, 
                              params = "psi", 
                              Rhat = TRUE,
                              n.eff = TRUE,
                              probs = c(0.05, 0.5, 0.95))
psi.post.specd$Year <- rep(year.names, each = length(route.index))
psi.post.specd$Route <- rep(c("EI.1", "EI.2", "M.2", "N.1", "N.2"), length(year.index))
psi.post.specd$Species <- "Specd"
psi.post.specd <- psi.post.specd[include.byrow,]




#' Populate Region
#' 
psi.post.specd$Region[psi.post.specd$Route == "EI.1" |
                        psi.post.specd$Route == "EI.2"] <- "El Imposible"
psi.post.specd$Region[psi.post.specd$Route == "N.1" |
                        psi.post.specd$Route == "N.2"] <- "Nancuchiname"
psi.post.specd$Region[psi.post.specd$Route == "M.1" |
                        psi.post.specd$Route == "M.2"] <- "Montecristo"
colnames(psi.post.specd)
colnames(psi.post.specd) <- c("Psi.mean", "Psi.sd", "Psi.LL05", "Psi.median",
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


#' Psi posteriors for Specd
#' 
specd.chains <- MCMCpstr(specd.jagsout, 
                         params = "psi",
                         type = "chains")



#' Calculate by species and route
#' 
psi.means.specd <- as.data.frame(matrix(NA, ncol = 5, nrow = length(route.names)))
colnames(psi.means.specd) <- c("Species", "Route", "Psi.LL05", "Psi.median", "Psi.UL95")
psi.means.specd$Species <- "Specd"
psi.means.specd$Route <- route.names

#' Populate quantile results
#' 
psi.means.specd[psi.means.specd$Route == "EI1",3:5] <- 
  quantile(specd.chains$psi[1,inc.EI.1,], probs = c(0.05, 0.5, 0.95))
psi.means.specd[psi.means.specd$Route == "EI2",3:5] <- 
  quantile(specd.chains$psi[2,inc.EI.2,], probs = c(0.05, 0.5, 0.95))
psi.means.specd[psi.means.specd$Route == "M2",3:5] <- 
  quantile(specd.chains$psi[3,inc.M.2,], probs = c(0.05, 0.5, 0.95))
psi.means.specd[psi.means.specd$Route == "N1",3:5] <- 
  quantile(specd.chains$psi[4,inc.N.1,], probs = c(0.05, 0.5, 0.95))
psi.means.specd[psi.means.specd$Route == "N2",3:5] <- 
  quantile(specd.chains$psi[5,inc.N.2,], probs = c(0.05, 0.5, 0.95))



#' Populate Region
#' 
psi.means.specd$Region[psi.means.specd$Route == "EI1" |
                   psi.means.specd$Route == "EI2"] <- "El Imposible"
psi.means.specd$Region[psi.means.specd$Route == "N1" |
                   psi.means.specd$Route == "N2"] <- "Nancuchiname"
psi.means.specd$Region[psi.means.specd$Route == "M1" |
                   psi.means.specd$Route == "M2"] <- "Montecristo"


#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Probability of detection was a function of broadcast species
#' 
#' 
p.det.post.specd <- MCMCsummary(specd.jagsout, 
                                params = grep("^beta", 
                                              specd.jagsout$parameters.to.save, 
                                              value = T), 
                                Rhat = TRUE,
                                n.eff = TRUE,
                                probs = c(0.05, 0.5, 0.95))
p.det.post.specd$Species <- "Specd"
p.det.post.specd$broadcast.param <- grep("^beta", 
                                         specd.jagsout$parameters.to.save, 
                                         value = T)
p.det.post.specd



#' Merge data together and change column names
#' 
colnames(p.det.post.specd)
colnames(p.det.post.specd) <- c("mean", "sd", "LL05", "median", "UL95", "Rhat",
                          "n.eff", "Species", "broadcast.param")

#' Transform to probability scale
#' 
p.det.post.specd$LL05.plogis <- plogis(p.det.post.specd$LL05)
p.det.post.specd$median.plogis <- plogis(p.det.post.specd$median)
p.det.post.specd$UL95.plogis <- plogis(p.det.post.specd$UL95)

#' Save broadcast species as a factor, ordered by confidence
#' 
p.det.post.specd$Broadcast <- factor(p.det.post.specd$broadcast.param,
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
save(psi.post.specd, file = "data/output_data/specd_psi_posteriors_RtYr.Rdata")

#' Psi posteriors across years by species and route
save(psi.means.specd, file = "data/output_data/specd_psi_posteriors_RtSpp.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
save(p.det.post.specd, file = "data/output_data/specd_p_detection_posteriors.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/e03_processing_results_specd.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 