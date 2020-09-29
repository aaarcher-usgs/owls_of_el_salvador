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

#' Jagsout Ferpy
#' 
load(file = "data/output_data/ferpy_jagsout.Rdata")

#' Jagsout Specd
#' 
load(file = "data/output_data/specd_jagsout.Rdata")

#' Survey list
load(file = "data/processed_data/survey_list.Rdata")
survey_list
(route.names <- c("EI1", "EI2", "M1",  "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' Create traceplots of parameters (creates PDF)
#'
#' FerPy
MCMCtrace(ferpy.jagsout, 
          pdf = TRUE,
          open_pdf = FALSE,
          filename = "ferpyTrace",
          wd = "output")

#' Mottd
MCMCtrace(mottd.jagsout, 
          pdf = TRUE,
          open_pdf = FALSE,
          filename = "mottdTrace",
          wd = "output")

#' Specd
MCMCtrace(specd.jagsout, 
          pdf = TRUE,
          open_pdf = FALSE,
          filename = "specdTrace",
          wd = "output")


#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' ### Calculate median, 95% quantile and 5% quantile of psi posteriors by route
#' and year
#' 
#' For all analysis, remove years and routes that were not surveyed
(exclude.byrow <- c(3,4,9,15,19:24,27,33,40,44,56,63,64))
all.rows <- 1:66
(include.byrow <- all.rows[!all.rows %in% exclude.byrow])
#' 
#' FerPy
#' 
psi.post.ferpy <- MCMCsummary(ferpy.jagsout, 
                              params = "psi", 
                              Rhat = TRUE,
                              n.eff = TRUE,
                              probs = c(0.05, 0.5, 0.95))
psi.post.ferpy$Year <- rep(1:11, each = 6)
psi.post.ferpy$Route <- rep(c("EI.1", "EI.2", "M.1", "M.2", "N.1", "N.2"), 11)
psi.post.ferpy$Species <- "FerPy"
psi.post.ferpy <- psi.post.ferpy[include.byrow,]

#' Mottd
#' 
psi.post.mottd <- MCMCsummary(mottd.jagsout, 
                              params = "psi", 
                              Rhat = TRUE,
                              n.eff = TRUE,
                              probs = c(0.05, 0.5, 0.95))
psi.post.mottd$Year <- rep(1:11, each = 6)
psi.post.mottd$Route <- rep(c("EI.1", "EI.2", "M.1", "M.2", "N.1", "N.2"), 11)
psi.post.mottd$Species <- "Mottd"
psi.post.mottd <- psi.post.mottd[include.byrow,]

#' Specd
#' 
psi.post.specd <- MCMCsummary(specd.jagsout, 
                              params = "psi", 
                              Rhat = TRUE,
                              n.eff = TRUE,
                              probs = c(0.05, 0.5, 0.95))
psi.post.specd$Year <- rep(1:11, each = 6)
psi.post.specd$Route <- rep(c("EI.1", "EI.2", "M.1", "M.2", "N.1", "N.2"), 11)
psi.post.specd$Species <- "Specd"
psi.post.specd <- psi.post.specd[include.byrow,]

psi.posteriors <- rbind(psi.post.ferpy, psi.post.mottd, psi.post.specd)


#' Populate Region
#' 
psi.posteriors$Region[psi.posteriors$Route == "EI.1" |
                        psi.posteriors$Route == "EI.2"] <- "El Imposible"
psi.posteriors$Region[psi.posteriors$Route == "N.1" |
                        psi.posteriors$Route == "N.2"] <- "Nancuchiname"
psi.posteriors$Region[psi.posteriors$Route == "M.1" |
                        psi.posteriors$Route == "M.2"] <- "Montecristo"
colnames(psi.posteriors)
colnames(psi.posteriors) <- c("Psi.mean", "Psi.sd", "Psi.LL05", "Psi.median",
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
all.years <- 1:11
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
#' Psi posteriors for Ferpy
#' 
ferpy.chains <- MCMCpstr(ferpy.jagsout, 
                         params = "psi",
                         type = "chains")

#' Psi posteriors for Mottd
#' 
mottd.chains <- MCMCpstr(mottd.jagsout, 
                         params = "psi",
                         type = "chains")

#' Psi posteriors for Specd
#' 
specd.chains <- MCMCpstr(specd.jagsout, 
                         params = "psi",
                         type = "chains")

#' Calculate by species and route
#' 
psi.means <- as.data.frame(matrix(NA, ncol = 5, nrow = n.route*3))
colnames(psi.means) <- c("Species", "Route", "Psi.LL05", "Psi.median", "Psi.UL95")
psi.means$Species <- rep(c("Mottd", "FerPy", "Specd"), each = n.route)
psi.means$Route <- rep(route.names, 3)

#' Populate quantile results
#' 
psi.means[psi.means$Species == "Mottd" & psi.means$Route == "EI1",3:5] <- 
  quantile(mottd.chains$psi[1,inc.EI.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Mottd" & psi.means$Route == "EI2",3:5] <- 
  quantile(mottd.chains$psi[2,inc.EI.2,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Mottd" & psi.means$Route == "M1",3:5] <- 
  quantile(mottd.chains$psi[3,inc.M.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Mottd" & psi.means$Route == "M2",3:5] <- 
  quantile(mottd.chains$psi[4,inc.M.2,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Mottd" & psi.means$Route == "N1",3:5] <- 
  quantile(mottd.chains$psi[5,inc.N.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Mottd" & psi.means$Route == "N2",3:5] <- 
  quantile(mottd.chains$psi[6,inc.N.2,], probs = c(0.05, 0.5, 0.95))

psi.means[psi.means$Species == "FerPy" & psi.means$Route == "EI1",3:5] <- 
  quantile(ferpy.chains$psi[1,inc.EI.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "FerPy" & psi.means$Route == "EI2",3:5] <- 
  quantile(ferpy.chains$psi[2,inc.EI.2,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "FerPy" & psi.means$Route == "M1",3:5] <- 
  quantile(ferpy.chains$psi[3,inc.M.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "FerPy" & psi.means$Route == "M2",3:5] <- 
  quantile(ferpy.chains$psi[4,inc.M.2,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "FerPy" & psi.means$Route == "N1",3:5] <- 
  quantile(ferpy.chains$psi[5,inc.N.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "FerPy" & psi.means$Route == "N2",3:5] <- 
  quantile(ferpy.chains$psi[6,inc.N.2,], probs = c(0.05, 0.5, 0.95))

psi.means[psi.means$Species == "Specd" & psi.means$Route == "EI1",3:5] <- 
  quantile(specd.chains$psi[1,inc.EI.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Specd" & psi.means$Route == "EI2",3:5] <- 
  quantile(specd.chains$psi[2,inc.EI.2,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Specd" & psi.means$Route == "M1",3:5] <- 
  quantile(specd.chains$psi[3,inc.M.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Specd" & psi.means$Route == "M2",3:5] <- 
  quantile(specd.chains$psi[4,inc.M.2,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Specd" & psi.means$Route == "N1",3:5] <- 
  quantile(specd.chains$psi[5,inc.N.1,], probs = c(0.05, 0.5, 0.95))
psi.means[psi.means$Species == "Specd" & psi.means$Route == "N2",3:5] <- 
  quantile(specd.chains$psi[6,inc.N.2,], probs = c(0.05, 0.5, 0.95))

#' Populate Region
#' 
psi.means$Region[psi.means$Route == "EI1" |
                   psi.means$Route == "EI2"] <- "El Imposible"
psi.means$Region[psi.means$Route == "N1" |
                   psi.means$Route == "N2"] <- "Nancuchiname"
psi.means$Region[psi.means$Route == "M1" |
                   psi.means$Route == "M2"] <- "Montecristo"


#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Probability of detection was a function of broadcast species
#' 
#' FerPy
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

#' Specd
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

#' Merge data together and change column names
#' 
p.det.post <- rbind(p.det.post.ferpy,
                    p.det.post.mottd,
                    p.det.post.specd)

colnames(p.det.post)
colnames(p.det.post) <- c("mean", "sd", "LL05", "median", "UL95", "Rhat",
                          "n.eff", "Species", "broadcast.param")

#' Transform to probability scale
#' 
p.det.post$LL05.plogis <- plogis(p.det.post$LL05)
p.det.post$median.plogis <- plogis(p.det.post$median)
p.det.post$UL95.plogis <- plogis(p.det.post$UL95)



#' _____________________________________________________________________________
#' ## Save files
#' 
#' Psi Posteriors by year and route
save(psi.posteriors, file = "data/output_data/psi_posteriors_RtYr.Rdata")

#' Psi posteriors across years by species and route
save(psi.means, file = "data/output_data/psi_posteriors_RtSpp.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
save(p.det.posteriors, file = "data/output_data/p_detection_posteriors.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/c_processing_results.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 