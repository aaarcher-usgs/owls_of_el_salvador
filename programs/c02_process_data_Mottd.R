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



#' ### Create Broadcast arrays
#' 
ks <-  array(0, dim = c(n.route, n.station, n.broadcast))
ks.prebroad <- ks.mottled <- ks.pacific <- ks.crested <- ks.bw <- ks.spectacled <-
  ks.whiskered <- ks.gbarred <- ks.stygian <- ks.ghorned <- ks

broadcast.array <- array(c(
  rep(c("pacific", "mottled", "crested", "bw", "spectacled"),4), # routes EI1/EI2
  rep(c("whiskered", "mottled", "gbarred", "stygian", "ghorned"),4), # routes M1/M2
  rep(c("pacific", "mottled", "crested", "bw", "spectacled"),4)), # routes N1/N2
  dim = c(n.station, n.route))
broadcast.array

for(hh in 1:n.route){
  for(jj in 1:n.station){
    ks.prebroad[hh,jj,1] <- 1
    ks.prebroad[hh,jj,2] <- 0
    
    if(broadcast.array[jj,hh] == "pacific"){
      ks.pacific[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "mottled"){
      ks.mottled[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "crested"){
      ks.crested[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "bw"){
      ks.bw[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "spectacled"){
      ks.spectacled[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "whiskered"){
      ks.whiskered[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "gbarred"){
      ks.gbarred[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "stygian"){
      ks.stygian[hh,jj,2] <- 1
    }else if(broadcast.array[jj,hh] == "ghorned"){
      ks.ghorned[hh,jj,2] <- 1
    }
  }
}
#' Check a couple examples:
#' 
ks.crested[1,,] #EI1
ks.mottled[1,,] #EI1
ks.mottled[3,,] #M1
ks.stygian[3,,] #M1
ks.stygian[5,,] #N1


#' ## OWL DATA
#' 
#' 
#' Create blank arrays
mottd.ys <- array(NA, dim = c(n.route, n.year, n.survey, n.station, n.broadcast))

#' Remove all non-mottled data from tab.owls
#' 
mottd.data <- tab.owls[tab.owls$Owl_Species_ID == "Mottd",]
table(mottd.data$Owl_Species_ID)


for(hh in 1:n.route){
  temp.route <- route.names[hh]
  for(tt in 1:n.year){
    temp.year <- year.names[tt]
    for(ii in 1:n.survey){
      temp.surveyID <- 
        tab.survey$Survey_ID[tab.survey$hRt_tYr_iSvy == paste(temp.route,temp.year,ii, sep = ".")]
      if(length(temp.surveyID) == 0){ # No survey for this year/route/survey combo
        
        print(paste("No survey", year.names[tt], route.names[hh], ii, sep = " "))
        
        # Ys will be NA because no survey
        mottd.ys[hh,tt,ii,,] <- NA
        
      }else{ #if there was a survey, populate ys appropriately
        for(jj in 1:n.station){
          temp.stationID <- 
            tab.stations$Stations_ID[tab.stations$Station == paste(temp.route,jj, sep = ".")&
                                       tab.stations$Survey_ID == temp.surveyID]
          
          if(length(temp.stationID) == 0){ # no survey at that station
            print(paste("No survey at station", year.names[tt], route.names[hh], ii, jj, sep = " "))
            mottd.ys[hh,tt,ii,jj,] <- NA
          }else{
            temp.owls <- mottd.data[mottd.data$Stations_ID == temp.stationID,]
            
            temp.n.rows <- nrow(temp.owls)
            if(temp.n.rows > 0){
              # Was observation before or after broadcast?
              # kk = 1 for before broadcast
              # kk = 2 for after broadcast
              logic.prebroadcast <- c(temp.owls$Minute_1, temp.owls$Minute_2)
              logic.postbroadcast <- c(temp.owls$`Minute_6-12`)
              
              mottd.ys[hh,tt,ii,jj,1] <- ifelse(sum(logic.prebroadcast)>0, yes = 1, no = 0)
              mottd.ys[hh,tt,ii,jj,2] <- ifelse(sum(logic.postbroadcast)>0, yes = 1, no = 0)
            }else{
              mottd.ys[hh,tt,ii,jj,] <- 0
            }
          }
        }
      }
    }
  }
}

#' Test a few
#' 
mottd.ys[1,1,1,,]
mottd.ys[1,1,3,,]
mottd.ys[1,8,1,,]





#' _____________________________________________________________________________
#' ## Save files
#' 
#' Processed Owl Data
save(mottd.ys,
     file = "data/processed_data/mottd_jags_ys.Rdata")

#' Processed Support data
save(ks.prebroad, ks.pacific, ks.mottled, ks.crested, ks.bw, ks.spectacled, 
  ks.whiskered, ks.gbarred, ks.stygian, ks.ghorned,
  file = "data/processed_data/mottd_jags_ks.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/c02_process_data_mottd.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 
#' 
#' 
