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
load("data/output_data/richness_species_accounts.Rdata")

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
    species.accounts.byRt[hh,ss] <- sum(temp.accounts[,ss],na.rm = T)
  }
}

(full.species.list <- c(colnames(species.accounts)[c(1,3:4,2,5:9)], 
                       "Crested","Burrowing", 
                       "BW","Striped", 
                       "Saw-whet", "Unk"))

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
                    "Aegolius ridgwayi",#unspotted saw-whet
                    "Unknown"
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
                       "Unspotted Saw-whet",#unspotted saw-whet
                       "Unknown"
)
species.counts.long <- as.data.frame(matrix(
  c(full.species.list, sci.names.list, common.names.list),
  ncol = 3,
  nrow = 15,
  byrow = F
))
colnames(species.counts.long) <- c("ID", "Scientific", "Common")
rownames(species.counts.long) <- full.species.list
species.counts.long$N2 <- species.counts.long$N1 <-
  species.counts.long$M2 <- species.counts.long$M1 <-
  species.counts.long$EI2 <- species.counts.long$EI1 <- 0


for(hh in 1:6){
  temp.route <- species.accounts.byRt$Route[hh]
  temp.surveys <- tab.survey$Survey_ID[tab.survey$Route_ID==temp.route]
  temp.stations <- 
    tab.stations$Stations_ID[tab.stations$Survey_ID %in% temp.surveys]
  temp.accounts <- tab.owls[tab.owls$Stations_ID %in% temp.stations,]
  for(ss in 1:9){ #across all 13 species
    temp.species <- full.species.list[ss]
    species.counts.long[temp.species,temp.route] <- 
      nrow(temp.accounts[temp.accounts$Owl_Species_ID==temp.species,])
  }
  for(ss in 10:14){
    temp.species <- full.species.list[ss]
    species.counts.long[temp.species,temp.route] <- 0
  }
  for(ss in 15){
    temp.species <- full.species.list[ss]
    species.counts.long[temp.species,temp.route] <- 
      nrow(temp.accounts[temp.accounts$Owl_Species_ID==temp.species,])
  }
}



#' _____________________________________________________________________________
#' ## Save files
#' 
save(species.accounts.byRt, species.counts.long,
     file = "data/output_data/richness_species_accounts_byRt.Rdata")

#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/e04_processing_results_richness.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 