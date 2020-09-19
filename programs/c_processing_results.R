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
#' ### Calculate median, 95% quantile and 5% quantile of psi posteriors by route
#' and year
#' 
#' Create blank tables
#' 
psi.posteriors <- as.data.frame(matrix(NA, 
                                       nrow = length(survey_list)*3, #surveys for 3 species
                                       ncol = 1))

#' Populate Survey List, repeated for each species
colnames(psi.posteriors) <- c("Route.Year") 
psi.posteriors$Route.Year <- rep(survey_list, 3)

#' Populate Route 
psi.posteriors$Route <- unlist(psi.posteriors %>% 
                                 separate(Route.Year, c("Route"), extra = "drop"))
table(psi.posteriors$Route)

#' Populate Region
#' 
psi.posteriors$Region[psi.posteriors$Route == "EI1" |
                        psi.posteriors$Route == "EI2"] <- "El Imposible"
psi.posteriors$Region[psi.posteriors$Route == "N1" |
                        psi.posteriors$Route == "N2"] <- "Nancuchiname"
psi.posteriors$Region[psi.posteriors$Route == "M1" |
                        psi.posteriors$Route == "M2"] <- "Montecristo"

#' Populate Year
#' 
psi.posteriors$Year <- as.numeric(unlist(psi.posteriors %>% 
  separate(Route.Year, c("Route", "Year")) %>% # select both route and year
  select("Year")))
table(psi.posteriors$Year)


#' Populate Species
#'
psi.posteriors$Species <- rep(c("Mottd", "Specd", "FerPy"), each = length(survey_list))

#' Head of table
#' 
head(psi.posteriors)

#' ### Populate Psi
#' 
for(ht in 1:length(survey_list)){
  temp.survey <- survey_list[ht]
  temp.route <- unique(psi.posteriors$Route[psi.posteriors$Route.Year==temp.survey])
  hh <- route.index[route.names == temp.route]
  temp.year <- unique(psi.posteriors$Year[psi.posteriors$Route.Year==temp.survey])
  tt <- year.index[year.names == temp.year]
  
  # Mottd results
  mottd.temp <- mottd.jagsout$BUGSoutput$sims.list$psi[,hh,tt]
  psi.posteriors$Psi.50[
    psi.posteriors$Route == temp.route &
    psi.posteriors$Year == temp.year &
    psi.posteriors$Species == "Mottd"
    ] <- median(mottd.temp)
  
  psi.posteriors$Psi.05[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "Mottd"
    ] <- quantile(mottd.temp, prob = 0.05)
  
  psi.posteriors$Psi.95[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "Mottd"
    ] <- quantile(mottd.temp, prob = 0.95)
  
  # Ferpy results
  ferpy.temp <- ferpy.jagsout$BUGSoutput$sims.list$psi[,hh,tt]
  psi.posteriors$Psi.50[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "FerPy"
    ] <- median(ferpy.temp)
  
  psi.posteriors$Psi.05[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "FerPy"
    ] <- quantile(ferpy.temp, prob = 0.05)
  
  psi.posteriors$Psi.95[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "FerPy"
    ] <- quantile(ferpy.temp, prob = 0.95)
  
  # Specd results
  specd.temp <- specd.jagsout$BUGSoutput$sims.list$psi[,hh,tt]
  psi.posteriors$Psi.50[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "Specd"
    ] <- median(specd.temp)
  
  psi.posteriors$Psi.05[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "Specd"
    ] <- quantile(specd.temp, prob = 0.05)
  
  psi.posteriors$Psi.95[
    psi.posteriors$Route == temp.route &
      psi.posteriors$Year == temp.year &
      psi.posteriors$Species == "Specd"
    ] <- quantile(specd.temp, prob = 0.95)
  
}

#' ### Calculate average occupancy probability by route and species
#' 
#' Blank data frame to store data
psi.means <- as.data.frame(matrix(NA, nrow = length(route.names)*3, ncol = 5))
colnames(psi.means) <- c("Species", "Route", "Psi.50", "Psi.05", "Psi.95")
psi.means$Species <- rep(c("Mottd", "Specd", "FerPy"), each = 6)
psi.means$Route <- rep(route.names, 3)
psi.means$Region <- rep(c("El Imposible", "Montecristo", "Nancuchiname"), each = 2)


for(hh in 1:length(route.names)){
  temp.route <- route.names[hh]
  
  # Mottd
  temp.mottd.posteriors <- mottd.jagsout$BUGSoutput$sims.list$psi[,hh,]
  psi.means$Psi.50[psi.means$Species == "Mottd" & 
                       psi.means$Route == temp.route] <- median(temp.mottd.posteriors)
  psi.means$Psi.05[psi.means$Species == "Mottd" & 
                      psi.means$Route == temp.route] <- 
    quantile(temp.mottd.posteriors, probs = 0.05)
  psi.means$Psi.95[psi.means$Species == "Mottd" & 
                     psi.means$Route == temp.route] <- 
    quantile(temp.mottd.posteriors, probs = 0.95)
  
  # Ferpy
  temp.ferpy.posteriors <- ferpy.jagsout$BUGSoutput$sims.list$psi[,hh,]
  psi.means$Psi.50[psi.means$Species == "FerPy" & 
                       psi.means$Route == temp.route] <- median(temp.ferpy.posteriors)
  psi.means$Psi.05[psi.means$Species == "FerPy" & 
                      psi.means$Route == temp.route] <- 
    quantile(temp.ferpy.posteriors, probs = 0.05)
  psi.means$Psi.95[psi.means$Species == "FerPy" & 
                     psi.means$Route == temp.route] <- 
    quantile(temp.ferpy.posteriors, probs = 0.95)
  
  # Specd
  temp.specd.posteriors <- specd.jagsout$BUGSoutput$sims.list$psi[,hh,]
  psi.means$Psi.50[psi.means$Species == "Specd" & 
                       psi.means$Route == temp.route] <- median(temp.specd.posteriors)
  psi.means$Psi.05[psi.means$Species == "Specd" & 
                      psi.means$Route == temp.route] <- 
    quantile(temp.specd.posteriors, probs = 0.05)
  psi.means$Psi.95[psi.means$Species == "Specd" & 
                     psi.means$Route == temp.route] <- 
    quantile(temp.specd.posteriors, probs = 0.95)
  
}
head(psi.means)

#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Probability of detection was a function of broadcast species
#' 
#' Create blank data frame to hold results
p.det.posteriors <- as.data.frame(matrix(NA, nrow = 30, ncol = 5))
colnames(p.det.posteriors) <- c("Species", "Broadcast", 
                                "p.det.50", "p.det.05", "p.det.95")

#' Populate species and broadcast species
#' 
p.det.posteriors$Species <- rep(c("Mottd", "FerPy", "Specd"), 10)
p.det.posteriors$Broadcast <- rep(c("beta.prebroad", "beta.bw",
                                    "beta.crested", "beta.gbarred",
                                    "beta.ghorned", "beta.mottled",
                                    "beta.pacific", "beta.spectacled",
                                    "beta.stygian", "beta.whiskered"), 
                                  each = 3)
p.det.posteriors$Broadcast <- factor(p.det.posteriors$Broadcast, 
                                     levels = c("beta.prebroad", "beta.bw",
                                                "beta.crested", "beta.gbarred",
                                                "beta.ghorned", "beta.mottled",
                                                "beta.pacific", "beta.spectacled",
                                                "beta.stygian", "beta.whiskered"),
                                     labels = c("Prebroadcast", "BW", "Crested",
                                                "Gbarred", "Ghorned", "Mottled",
                                                "Pacific", "Specd", "Stygian", "Whiskered"))
#' Extract posteriors as new dataframes for each owl species
ferpy.posts <- ferpy.jagsout$BUGSoutput$sims.list
mottd.posts <- mottd.jagsout$BUGSoutput$sims.list
specd.posts <- specd.jagsout$BUGSoutput$sims.list

#' Save parameters as characters for extracting
parameters <- c("beta.prebroad", "beta.bw",
                "beta.crested", "beta.gbarred",
                "beta.ghorned", "beta.mottled",
                "beta.pacific", "beta.spectacled",
                "beta.stygian", "beta.whiskered")
labels <- c("Prebroadcast", "BW", "Crested",
            "Gbarred", "Ghorned", "Mottled",
            "Pacific", "Specd", "Stygian", "Whiskered")

#' Fill in table
#' 
for(kk in 1:length(parameters)){
  temp.mottd.posteriors <- mottd.posts[[parameters[kk]]]
  temp.ferpy.posteriors <- ferpy.posts[[parameters[kk]]]
  temp.specd.posteriors <- specd.posts[[parameters[kk]]]

  p.det.posteriors$p.det.50[p.det.posteriors$Species == "Mottd"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(median(temp.mottd.posteriors))
  p.det.posteriors$p.det.05[p.det.posteriors$Species == "Mottd"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(quantile(temp.mottd.posteriors, probs = 0.05))
  p.det.posteriors$p.det.95[p.det.posteriors$Species == "Mottd"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(quantile(temp.mottd.posteriors, probs = 0.95))
  
  p.det.posteriors$p.det.50[p.det.posteriors$Species == "FerPy"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(median(temp.ferpy.posteriors))
  p.det.posteriors$p.det.05[p.det.posteriors$Species == "FerPy"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(quantile(temp.ferpy.posteriors, probs = 0.05))
  p.det.posteriors$p.det.95[p.det.posteriors$Species == "FerPy"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(quantile(temp.ferpy.posteriors, probs = 0.95))
  
  
  p.det.posteriors$p.det.50[p.det.posteriors$Species == "Specd"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(median(temp.specd.posteriors))
  p.det.posteriors$p.det.05[p.det.posteriors$Species == "Specd"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(quantile(temp.specd.posteriors, probs = 0.05))
  p.det.posteriors$p.det.95[p.det.posteriors$Species == "Specd"&
                              p.det.posteriors$Broadcast == labels[kk]] <- 
    plogis(quantile(temp.specd.posteriors, probs = 0.95))
}

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