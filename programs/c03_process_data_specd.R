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
#' **For Specd, we assume no probability of occupancy at M1, so remove M1 from analysis**
#' 
#' Create an array of Ys (detections/non-detections) for each of the **possible**
#' surveys.
#' 
#' Specifically: 5 routes (hh), 10 stations (jj), up to 3 surveys per year (ii), 
#' 11 total years (tt)
#' and 2 broadcast times (kk)
#' 
#' 
#' There will be 165 total matrices, each with 10 rows (j, stations) and 2 columns 
#' (k, before or after broadcast)
#' 
#' 
#' 



#' #### Define variables
#' 
#' Observations
n.route <- length(unique(data.jags$Route_ID[data.jags$Route_ID != "M1"])) # hh
(route.names <- unique(data.jags$Route_ID[data.jags$Route_ID != "M1"])) #exclude M1

n.year <- length(min(data.jags$year):max(data.jags$year)) # tt
year.names <- min(data.jags$year):max(data.jags$year)

n.survey <- max(data.jags$order) # ii
n.station <- 10 # jj
n.broadcast <- 2 # kk



#' ### Create Broadcast arrays
ks <-  array(0, dim = c(n.route, n.year, n.survey, n.station, n.broadcast))
ks.prebroad <- ks.mottled <- ks.pacific <- ks.crested <- ks.bw <- ks.spectacled <-
  ks.whiskered <- ks.gbarred <- ks.stygian <- ks.ghorned <- ks

broadcast.array <- array(c(
  rep(c("pacific", "mottled", "crested", "bw", "spectacled"),4), # routes EI1/EI2
  rep(c("whiskered", "mottled", "gbarred", "stygian", "ghorned"),2), # routes M1/M2
  rep(c("pacific", "mottled", "crested", "bw", "spectacled"),4)), # routes N1/N2
  dim = c(n.station, n.route))
broadcast.array

for(hh in 1:n.route){
  for(tt in 1:n.year){
    for(ii in 1:n.survey){
      for(jj in 1:n.station){
        ks.prebroad[hh,tt,ii,jj,1] <- 1
        ks.prebroad[hh,tt,ii,jj,2] <- 0
        
        if(broadcast.array[jj,hh] == "pacific"){
          ks.pacific[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "mottled"){
          ks.mottled[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "crested"){
          ks.crested[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "bw"){
          ks.bw[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "spectacled"){
          ks.spectacled[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "whiskered"){
          ks.whiskered[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "gbarred"){
          ks.gbarred[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "stygian"){
          ks.stygian[hh,tt,ii,jj,2] <- 1
        }else if(broadcast.array[jj,hh] == "ghorned"){
          ks.ghorned[hh,tt,ii,jj,2] <- 1
        }
      }
    }
  }
}
#' Check a couple examples:
#' 
ks.crested[1,1,1,,] #EI1
ks.mottled[1,1,1,,] #EI1
ks.mottled[3,1,1,,] #M1
ks.stygian[3,1,1,,] #M1
ks.stygian[4,1,1,,] #N1



#' ## OWL DATA
#' 
#' 
#' Create blank arrays
specd.ys <- array(NA, dim = c(n.route, n.year, n.survey, n.station, n.broadcast))

#' Remove all non-specd data from tab.owls
#' 
specd.data <- tab.owls[tab.owls$Owl_Species_ID == "Specd",]
table(specd.data$Owl_Species_ID)


for(hh in 1:n.route){
  temp.route <- route.names[hh]
  for(tt in 1:n.year){
    temp.year <- year.names[tt]
    for(ii in 1:n.survey){
      temp.surveyID <- 
        tab.survey$Survey_ID[tab.survey$hRt_tYr_iSvy == paste(
          temp.route,temp.year,ii, sep = ".")]
      if(length(temp.surveyID) == 0){ # No survey for this year/route/survey combo
        
        print(paste("No survey ", year.names[tt], route.names[hh], ii, sep = " "))
        
        # Ys will be NA because no survey
        specd.ys[hh,tt,ii,,] <- NA
        
      }else{ #if there was a survey, populate ys appropriately
        for(jj in 1:n.station){
          temp.stationID <- 
            tab.stations$Stations_ID[tab.stations$Station == paste(temp.route,jj, sep = ".")&
                                       tab.stations$Survey_ID == temp.surveyID]
          
          if(length(temp.stationID) == 0){ # no survey at that station
            print(paste("No survey at station", year.names[tt], route.names[hh], ii, jj, sep =" "))
            specd.ys[hh,tt,ii,jj,] <- NA
          }else{
            temp.owls <- specd.data[specd.data$Stations_ID == temp.stationID,]
            
            temp.n.rows <- nrow(temp.owls)
            if(temp.n.rows > 0){
              # Was observation before or after broadcast?
              # kk = 1 for before broadcast
              # kk = 2 for after broadcast
              logic.prebroadcast <- c(temp.owls$Minute_1, temp.owls$Minute_2)
              logic.postbroadcast <- c(temp.owls$`Minute_6-12`)
              
              specd.ys[hh,tt,ii,jj,1] <- ifelse(sum(logic.prebroadcast)>0, yes = 1, no = 0)
              specd.ys[hh,tt,ii,jj,2] <- ifelse(sum(logic.postbroadcast)>0, yes = 1, no = 0)
            }else{
              specd.ys[hh,tt,ii,jj,] <- 0
            }
          }
        }
      }
    }
  }
}

#' Test a few
#' 
specd.ys[1,1,1,,]
specd.ys[4,3,1,,]
specd.ys[1,1,3,,]
specd.ys[1,8,1,,]





#' _____________________________________________________________________________
#' ## Save files
#' 
#' Processed Owl Data
save(specd.ys,
     file = "data/processed_data/specd_jags_ys.Rdata")

#' Processed Support data
save(ks.prebroad, ks.pacific, ks.mottled, ks.crested, ks.bw, ks.spectacled, 
  ks.whiskered, ks.gbarred, ks.stygian, ks.ghorned,
  file = "data/processed_data/specd_jags_ks.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/c03_process_data_specd.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 
#' 
#' 
