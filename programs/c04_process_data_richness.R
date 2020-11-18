#' # Processing data from Access
#' 
#' Description: This program takes raw data exported 
#' from Access database and organizes it to better understand what data are 
#' available and process it for analysis.
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
library(readxl)
library(lubridate)
library(dplyr)
library(ImportExport)

#' Clear environment and set seed
#' 
#' *Note: for reproducibility, it is important to include these. Clearing the
#' environment ensures that you have specified all pertinent files that need
#' to be loaded, and setting the seed ensures that your analysis is 
#' repeatable*
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' 
#' Access tables, in Rdata format
#' 
load(file = "data/processed_data/tables_global.Rdata")

#' Supplemental JAGS table
#' 
load(file = "data/processed_data/data_jags_global.Rdata")








#' _____________________________________________________________________________
#' ## Prepare for JAGS
#' 

#' ### Create tables for Ys and Broadcast species covariates
#' 
#' 
#' Create an array of Ys (detections/non-detections) for each of the **possible**
#' surveys
#' 
#' Specifically: 6 routes (hh), 10 stations (jj), up to 3 surveys per year (ii), 
#' 11 total years (tt)
#' and 2 broadcast times (kk)
#' 
#' 
#' There will be 198 total matrices, each with 10 rows (j, stations) and 2 columns 
#' (k, before or after broadcast)
#' 
#' 
#' 



#' #### Define variables
#' 
#' Observations
n.route <- length(unique(data.jags$Route_ID)) # hh
route.names <- unique(data.jags$Route_ID)

n.year <- length(min(data.jags$year):max(data.jags$year)) # tt
year.names <- min(data.jags$year):max(data.jags$year)

n.survey <- max(data.jags$order) # ii
n.station <- 10 # jj
n.broadcast <- 2 # kk

#' **Augmenting species number**
#' 
#' There were 10 observed owls (if we pool all three unknown observations)
(species.names <- unique(tab.owls$Owl_Species_ID))
n.species <- length(species.names)

#' We should augment with M >> n, where n = 10
#' 
#' Owls of El Salvador lists 13 total species recorded in El Salvador (Table 11.1, p401)
#' 
n.aug <- 5 #number of species that may have been present but were undetected
n.species.aug <- n.species + n.aug



#' ### Create Broadcast arrays
ks <-  array(0, dim = c(n.route, 
                        n.station, 
                        n.broadcast))
ks.prebroad <- ks.mottled <- ks.pacific <- ks.crested <- ks.bw <- ks.spectacled <-
  ks.whiskered <- ks.gbarred <- ks.stygian <- ks.ghorned <- ks

broadcast.array <- array(c(
  rep(c("PacSc", "Mottd", "Crested", "BW", "Specd"),4), # routes EI1/EI2
  rep(c("Whisk", "Mottd", "Guat Barred", "Styg", "GrHor"),4), # routes M1/M2
  rep(c("PacSc", "Mottd", "Crested", "BW", "Specd"),4)), # routes N1/N2
  dim = c(n.station, n.route))
broadcast.array

for(hh in 1:n.route){
  for(jj in 1:n.station){
    ks.prebroad[hh,jj,1] <- 1
    ks.prebroad[hh,jj,2] <- 0
    
    if(broadcast.array[jj,hh] == "PacSc"){
      ks.pacific[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "Mottd"){
      ks.mottled[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "Crested"){
      ks.crested[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "BW"){
      ks.bw[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "Specd"){
      ks.spectacled[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "Whisk"){
      ks.whiskered[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "Guat Barred"){
      ks.gbarred[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "Styg"){
      ks.stygian[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "GrHor"){
      ks.ghorned[hh,jj,2] <- 1
    }
  }
}
#' Check a couple examples:
#' 
ks.crested[1,,] #EI1, Crested
ks.mottled[1,,] #EI1, Mottled
ks.mottled[3,,] #M1, Mottled
ks.stygian[3,,] #M1, Stygian
ks.stygian[5,,] #N1, Stygian



#' ## OWL DATA
#' 
#' 
#' Create blank arrays
ys.aug <- array(NA, dim = c(n.route, 
                            n.year, 
                            n.species.aug, 
                            n.survey, 
                            n.station, 
                            n.broadcast))



for(hh in 1:n.route){
  temp.route <- route.names[hh]
  for(tt in 1:n.year){
    temp.year <- year.names[tt]
    
    for(ii in 1:n.survey){
      temp.surveyID <- 
        tab.survey$Survey_ID[tab.survey$hRt_tYr_iSvy == 
                               paste(temp.route,temp.year,ii, sep = ".")]
      if(length(temp.surveyID) == 0){ # No survey for this year/route/survey combo
        
        print(paste("No survey", year.names[tt], route.names[hh], ii, sep = " "))
        
        # Ys will be NA because no survey
        ys.aug[hh,tt,ss,ii,,] <- NA
        
      }else{ #if there was a survey
        for(jj in 1:n.station){
          temp.stationID <- 
            tab.stations$Stations_ID[tab.stations$Station == 
                                       paste(temp.route,jj, sep = ".")&
                                       tab.stations$Survey_ID == temp.surveyID]
          
          if(length(temp.stationID) == 0){ # no survey at that station
            print(paste("No survey at station", 
                        year.names[tt], route.names[hh], ii, jj, sep = " "))
            ys.aug[hh,tt,ss,ii,jj,] <- NA
          }else{ #if there was a survey at that station
            
            # For species observed
            for(ss in 1:n.species){
              temp.species <- species.names[ss]
              temp.owls <- tab.owls[tab.owls$Stations_ID == temp.stationID &
                                      tab.owls$Owl_Species_ID == temp.species,]
              
              temp.n.rows <- nrow(temp.owls)
              if(temp.n.rows > 0){
                # Was observation before or after broadcast?
                # kk = 1 for before broadcast
                # kk = 2 for after broadcast
                logic.prebroadcast <- c(temp.owls$Minute_1, temp.owls$Minute_2)
                logic.postbroadcast <- c(temp.owls$`Minute_6-12`)
                
                ys.aug[hh,tt,ss,ii,jj,1] <- 
                  ifelse(sum(logic.prebroadcast)>0, yes = 1, no = 0)
                ys.aug[hh,tt,ss,ii,jj,2] <- 
                  ifelse(sum(logic.postbroadcast)>0, yes = 1, no = 0)
              }else{
                ys.aug[hh,tt,ss,ii,jj,] <- 0
              }
              
              
            }
            
            # For augmented (unobserved) species
            for(ss in (n.species+1):n.species.aug){
              ys.aug[hh,tt,ss,ii,jj,] <- 0
            }
            
          }
        }
      }
    }
    
  }
}

#' **Test a few**
#' 
#' Example data
ys.aug[1,1,1,1,,] #EI1, 2003, Mottd, Survey 1
ys.aug[1,1,3,1,,] #EI1, 2003, FerPy, Survey 1
# Corresponds with these data:
tab.owls[tab.owls$Stations_ID %in% c(92:98,100:101),2:7]

#' No survey in this year
#' 
ys.aug[1,1,1,3,,] #EI1, 2003, Mottd, Survey 3

#' Surveys only for first three stations 
#' 
ys.aug[1,8,1,1,,] #EI1, 2010, Mottd, Survey 1
# Corresponds with these data:
tab.owls[tab.owls$Stations_ID %in% c(615:617),2:7]

#' Results for augmented species
ys.aug[1,1,11,1,,] #EI1, 2003, Augmented Species 1, Survey 1
ys.aug[1,8,11,1,,] #EI1, 2010, Augmented Species 1, Survey 1 





#' _____________________________________________________________________________
#' ## Save files
#' 
#' Processed Owl Data
save(ys.aug,
     file = "data/processed_data/augmented_jags_ys.Rdata")

#' Processed Support data
save(ks.prebroad, ks.pacific, ks.mottled, ks.crested, ks.bw, ks.spectacled, 
  ks.whiskered, ks.gbarred, ks.stygian, ks.ghorned,
  file = "data/processed_data/augmented_jags_ks.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/c04_process_data_richness.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 
#' 
#' 
