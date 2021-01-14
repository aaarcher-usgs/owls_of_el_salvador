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
#' Jagsout Mottd
#load(file = "data/output_data/richness_jagsout_posteriors.Rdata")

#' Richness Posteriors 
load(file = "data/output_data/richness_psi_posteriors.Rdata")
load(file = "data/output_data/richness_psi_posteriors_RtYr.Rdata")

#' Posteriors
#' 
#' Mu(psi)
load(file = "data/output_data/richness_mu_psi_posteriors.Rdata")

#' Rho(psi)
#' 
load(file = "data/output_data/richness_rho_psi_posteriors.Rdata")

#' Omega
#' #' 
load(file = "data/output_data/richness_omega_posteriors.Rdata")

#' Spp.occ
#' #' 
load(file = "data/output_data/richness_spp_occ_posteriors.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
load(file = "data/output_data/richness_p_detection_posteriors.Rdata")
# delete spurious columns?
p.det.post.richness <- p.det.post.richness[,!is.na(colnames(p.det.post.richness))]
colnames(p.det.post.richness)



#' Species present
#' 
load(file = "data/output_data/richness_species_present_posteriors.Rdata")

#' Access tables, in Rdata format
#' 
load(file = "data/processed_data/tables_global.Rdata")

#' Species accounts to summarize by route
#' 
load("data/output_data/richness_species_accounts.Rdata")

#' Survey list
(route.names <- c("EI1", "EI2", "M1", "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)

#' 
#' There were 10 observed owls (if we include unknown observations)
(species.names <- unique(tab.owls$Owl_Species_ID))
n.species <- length(species.names)
n.species.aug <- n.species + 5

#' _____________________________________________________________________________
#' ## Post-process Psi = Probability of occupancy
#' 
#' Survey list
(route.names <- c("EI1", "EI2", "M1", "M2",  "N1",  "N2") )
route.index <- 1:length(route.names)
(year.names <- 2003:2013)
year.index <- 1:length(year.names)
n.route <- length(route.index)


#' ### Calculate median, 95% quantile and 5% quantile of psi posteriors by route
#' and year
#' 
#' For all analysis, remove years and routes that were not surveyed
(exclude.byrow <- c(3,4,9,15,19:24,27,33,40,44,56,63,64))
all.rows <- 1:(length(route.index)*length(year.index))
(include.byrow <- all.rows[!all.rows %in% exclude.byrow])
#' 

#' Richness
#' 
tempYear <- rep(year.names, each = length(route.index))
tempYear <- tempYear[include.byrow]
tempRoute <- 
  rep(c("EI1", "EI2", "M1", "M2", "N1", "N2"), length(year.index))
tempRoute <- tempRoute[include.byrow]
richness.RtYr.post <- richness.RtYr.post[include.byrow,]
richness.RtYr.post$Year <- tempYear
richness.RtYr.post$Route <- tempRoute


#' Populate Region
#' 
richness.RtYr.post$Region[richness.RtYr.post$Route == "EI1" |
                       richness.RtYr.post$Route == "EI2"] <- "El Imposible"
richness.RtYr.post$Region[richness.RtYr.post$Route == "N1" |
                       richness.RtYr.post$Route == "N2"] <- "Nancuchiname"
richness.RtYr.post$Region[richness.RtYr.post$Route == "M1" |
                       richness.RtYr.post$Route == "M2"] <- "Montecristo"
colnames(richness.RtYr.post)
colnames(richness.RtYr.post) <- c("Richness.mean", "Richness.sd", "Richness.LL05", 
                             "Richness.median",
                             "Richness.UL95", "Richness.Rhat", "Richness.neff", 
                             "Year", "Route", "Region")#, "richness.d", "richness.detected")




#' _____________________________________________________________________________
#' ## Calculate number of species detected in each year/route
#' 
#' Create a table to determine what species were detected in what routes/years
species.accounts <- as.data.frame(matrix(
  NA, nrow = nrow(richness.RtYr.post), ncol = n.species
))
colnames(species.accounts) <- species.names
species.accounts$Year <- richness.RtYr.post$Year
species.accounts$Route <- richness.RtYr.post$Route
species.accounts$Region <- richness.RtYr.post$Region

#' 
#' 
richness.RtYr.post$richness.detected <- NA
for(rr in 1:nrow(richness.RtYr.post)){
  temp.route <- richness.RtYr.post$Route[rr]
  temp.year <- richness.RtYr.post$Year[rr]
  temp.surveys <- tab.survey$Survey_ID[tab.survey$Route_ID == temp.route &
                                         tab.survey$year == temp.year]
  temp.stations <- tab.stations$Stations_ID[tab.stations$Survey_ID %in% temp.surveys]
  temp.owls.data <- tab.owls[tab.owls$Stations_ID %in% temp.stations,]
  if(nrow(temp.owls.data)>0){
    temp.n.species.detected <- length(unique(temp.owls.data$Owl_Species_ID))
    richness.RtYr.post$richness.detected[rr] <- temp.n.species.detected
    for(ss in 1:temp.n.species.detected){
      temp.species <- unique(temp.owls.data$Owl_Species_ID)[ss]
      species.accounts[rr,temp.species] <- 1
    }
  }else{
    richness.RtYr.post$richness.detected[rr] <- 0
  }
}



#' _____________________________________________________________________________
#' ## Probability of Detection
#' 
#' Merge data together and change column names
#' 
colnames(p.det.post.richness)
colnames(p.det.post.richness) <- c("mean", "sd", "LL05", "median", "UL95", "Rhat",
                                   "n.eff", "broadcast.param")

#' Transform to probability scale
#' 
p.det.post.richness$LL05.plogis <- plogis(p.det.post.richness$LL05)
p.det.post.richness$median.plogis <- plogis(p.det.post.richness$median)
p.det.post.richness$UL95.plogis <- plogis(p.det.post.richness$UL95)

#' Save broadcast species as a factor, ordered by confidence
#' 
p.det.post.richness$Broadcast <- factor(p.det.post.richness$broadcast.param,
                                        levels = c("beta.prebroad", "beta.mottled",
                                                   "beta.pacific", "beta.crested",
                                                   "beta.bw", "beta.spectacled",
                                                   "beta.whiskered", "beta.gbarred",
                                                   "beta.stygian", "beta.ghorned"),
                                        labels = c("Pre-broadcast", "Mottled Owl",
                                                   "Pacific Screech-owl", "Crested Owl", 
                                                   "Black-and-white Owl",
                                                   "Spectacled Owl", 
                                                   "Whiskered Screech-owl", "Fulvous Owl",
                                                   "Stygian Owl", "Great Horned Owl"))


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
  for(ss in 1:n.species){ #across all 9 species
    species.accounts.byRt[hh,ss] <- sum(temp.accounts[,ss],na.rm = T)
  }
}

(full.species.list <- c(colnames(species.accounts)[c(1,3:4,2,5:9)], 
                       "Crested","Burrowing", 
                       "BW","Striped", 
                       "Saw-whet"))

sci.names.list <- c("Ciccaba virgata",#mottled
                    "Glaucidium brasilianum",#ferpy
                    "Pulsatrix perspicillata",#specd
                    "Megascops cooperi", #pacsc
                    "Tyto alba", #barn
                    "Asio stygius", #stygian
                    "M. trichopsis",#whiskered
                    "Bubo virginianus",#grhor
                    "Strix fulvescens",#fulvous
                    "Lophostrix cristata",#crested
                    "Athene cunicularia",#burrowing
                    "Ciccaba nigrolineata", #bw
                    "Pseudoscops clamator",#striped
                    "Aegolius ridgwayi"#unspotted saw-whet
                    )
common.names.list <- c("Mottled",#mottled
                       "Ferruginous Pygmy",#ferpy
                       "Spectacled",#specd
                       "Pacific Screech", #pacsc
                       "Barn", #barn
                       "Stygian", #stygian
                       "Whiskered Screech",#whiskered
                       "Great Horned",#grhor
                       "Fulvous",#fulvous
                       "Lophostrix cristata",#crested
                       "Burrowing",#burrowing
                       "Black-and-white",#bw
                       "Striped",#striped
                       "Unspotted Saw-whet"#unspotted saw-whet
)
species.counts.long <- as.data.frame(matrix(
  c(full.species.list, sci.names.list, common.names.list),
  ncol = 3,
  nrow = n.species.aug,
  byrow = F
))
colnames(species.counts.long) <- c("ID", "Scientific", "Common")
rownames(species.counts.long) <- full.species.list
species.counts.long$N2 <- species.counts.long$N1 <-
  species.counts.long$M2 <- species.counts.long$M1 <-
  species.counts.long$EI2 <- species.counts.long$EI1 <- 0


for(hh in 1:n.route){
  temp.route <- species.accounts.byRt$Route[hh]
  temp.surveys <- tab.survey$Survey_ID[tab.survey$Route_ID==temp.route]
  temp.stations <- 
    tab.stations$Stations_ID[tab.stations$Survey_ID %in% temp.surveys]
  temp.accounts <- tab.owls[tab.owls$Stations_ID %in% temp.stations,]
  for(ss in 1:n.species){ #across all 13 species
    temp.species <- full.species.list[ss]
    species.counts.long[temp.species,temp.route] <- 
      nrow(temp.accounts[temp.accounts$Owl_Species_ID==temp.species,])
  }
  for(ss in (n.species+1):n.species.aug){
    temp.species <- full.species.list[ss]
    species.counts.long[temp.species,temp.route] <- 0
  }
}
species.counts.long


#' _____________________________________________________________________________
#' ## Save files
#' 
#' Species Accounts
#' 
save(species.accounts, file = "data/output_data/richness_species_accounts.Rdata")

save(species.accounts.byRt, species.counts.long,
     file = "data/output_data/richness_species_accounts_byRt.Rdata")

#' Detection, ready for plotting
#' 
#' 
save(p.det.post.richness, p.det.chains,
    file = "data/plotting_data/richness_p_detection_posteriors.Rdata")

#' Richness Posteriors 
save(richness.post, richness.chains, 
    file = "data/plotting_data/richness_psi_posteriors.Rdata")
save(richness.RtYr.chains, richness.RtYr.post,
    file = "data/plotting_data/richness_psi_posteriors_RtYr.Rdata")

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/e04_processing_results_richness.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 