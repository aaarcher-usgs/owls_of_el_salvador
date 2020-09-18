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
#' 
#' ### Route Table
#' 
#' Routes are the six unique transects or routes with two per protected area (Forest).
#' There are unique IDs that correspond with protected area, and six unique route
#' names, as well.
tab.route <- read_xlsx(path = "data/raw_data/Route_Table.xlsx",
                       sheet = "Route_Table")
tab.route

#' ### Stations Table
#' 
#' Each route consists of 10 stations. There is no table with information about 
#' the 60 total stations. Instead, this stations table has each station repeated 
#' for each survey night with unique measurements of temperature, barometer, 
#' broadcast species, and background noise level (from 0 to 3). The
#' link between these stations and the actual individual surveys is the field "Survey_ID".
#' 
tab.stations <- read_xlsx(path = "data/raw_data/Stations_Table.xlsx",
                          sheet = "Stations_Table")
dim(tab.stations)
head(tab.stations)
unique(tab.stations$Station)
length(unique(tab.stations$Station))

#' The Station_Start_Time did not read in correctly because in Excel it is assuming 
#' the origin date of 1899-12-31. The correct date will come from the Survey Table, below.
head(tab.stations$Station_Start_Time)



#' ### Survey Table
#' 
#' This includes 2002 surveys that have to be deleted from analysis.
tab.survey <- read_xlsx(path = "data/raw_data/Survey_Table.xlsx",
                       sheet = "Survey_Table")
table(tab.survey$Route_ID)

#' Remove data for year = 2002
tab.survey$year <- lubridate::year(tab.survey$Survey_Date)
table(tab.survey$year)
(survey_id_2002 <- tab.survey$Survey_ID[tab.survey$year==2002])
tab.survey <- tab.survey[tab.survey$year >= 2003,]
table(tab.survey$year)

#' Also remove 2002 surveys from stations data
table(tab.stations$Survey_ID)
(station_id_2002 <- tab.stations$Stations_ID[tab.stations$Survey_ID %in% survey_id_2002])
tab.stations <- tab.stations[!tab.stations$Survey_ID %in% survey_id_2002,]
table(tab.stations$Survey_ID)


#' ### Owls Table
#' 
#' This table has all observed owls.
tab.owls <- read_xlsx(path = "data/raw_data/Owls_Table.xlsx",
                      sheet = "Owls_Table")
# Dimensions of owls table (rows, columns)
dim(tab.owls)
# Head of owls table
head(tab.owls)


#' Remove data for year = 2002 from owls table
#' 
#' 
tab.owls <- tab.owls[! tab.owls$Stations_ID %in% station_id_2002,]
dim(tab.owls)

#' Owl Number is character, but should be numerical
# Is it a character because it has any letter values? 
table(tab.owls$Owl_Number)
# No.... In that case, convert to numeric
tab.owls$Owl_Number <- as.numeric(tab.owls$Owl_Number)
summary(tab.owls$Owl_Number)

#' How many of each species of owl are there?
table(tab.owls$Owl_Species_ID)




#' _____________________________________________________________________________
#' ## Prepare for JAGS
#' 
#' For surveys that happened multiple times per year, determine the order of
#' those surveys with new variable "order"
#' 
# Create a unique identifier for each route per year
tab.survey$hRt_tYr <- paste(tab.survey$Route_ID, tab.survey$year, sep = ".")
route.year <- sort(unique(tab.survey$hRt_tYr))

# Create new blank column and then populate based on the order of surveys per
# route and year.
tab.survey$order <- NA
for(ht in 1:length(route.year)){
  order.dates.per.survey.per.yr <- 
    order(tab.survey$Survey_Date[tab.survey$hRt_tYr == route.year[ht]])
  tab.survey$order[tab.survey$hRt_tYr == route.year[ht]] <- order.dates.per.survey.per.yr
}

# Concatenate that order index with route/year column to create unique identifier
# for each different survey
tab.survey$hRt_tYr_iSvy <- paste(tab.survey$hRt_tYr, tab.survey$order, sep = ".")
head(tab.survey$hRt_tYr_iSvy)








#' ### Begin with survey data, order and clean
#' 
#' Sort survey data by Route then survey date
data.jags <- dplyr::arrange(tab.survey, Route_ID, Survey_Date)

#' Drop unneccessary columns
colnames(data.jags)
temp.names.keep <- c("Survey_ID", "Route_ID", "Survey_Date", 
                     "year", "hRt_tYr", "order", "hRt_tYr_iSvy")
data.jags <- data.jags[,temp.names.keep]
head(data.jags)

#' Add year index
#' 
#' 
years.analysis <- min(data.jags$year):max(data.jags$year)
data.jags$yearIndex <- NA
for(tt in 1:length(years.analysis)){
  data.jags$yearIndex[data.jags$year == years.analysis[tt]] <- tt
}

# Double check that year counts match
table(data.jags$year)
table(data.jags$yearIndex)


#' _____________________________________________________________________________
#' ## Create tables for Ys and Broad Cast species covariates
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
# Create blank lists
ys <- list(NA)
ks <- list(NA)

# How many total matrices should there be?
(total.num.hti <- nrow(tab.route)* #6, number of routes h
  max(tab.survey$order)*    #3, max number of surveys i per year
  length(min(tab.survey$year):max(tab.survey$year))) #11, number of total years

# Create lists of blank matrices and name them appropriately
for(hti in 1:total.num.hti){
  ys[[hti]] <- matrix(NA, ncol = 2, nrow = 10)
  ks[[hti]] <- matrix(NA, ncol = 2, nrow = 10)
}
names(ys) <- names(ks) <- 
  paste(rep(tab.route$Route_ID, each = 33), 
        rep(rep(min(tab.survey$year):max(tab.survey$year), each = 3),6),
        rep(1:3, 66),
        sep=".")
names(ys)[1:20]


#' Process broadcast species data
#' 
#' Each station should always have the same broadcast species, which are listed
#' in "tab.stations"
#' 
#' 
stationIDs <- unique(tab.stations$Station)
for(jj in 1:length(stationIDs)){
  temp.ks <- unique(tab.stations$Broadcast_Species[tab.stations$Station==stationIDs[jj]])
  if(length(temp.ks)==1){
    print(paste0("Station ", stationIDs[jj], " had broadcast species ", temp.ks))
  }else{
    print(paste0("Station ", stationIDs[jj], " has >1 broadcast species listed:", temp.ks))
  }
}

#' Stations N2.6, N2.8, and N2.9 were not surveyed on a couple occasions, 
#' and got values of "NA" for those broadcast species. 
#' 
surveys.NAs <- tab.stations$Survey_ID[is.na(tab.stations$Broadcast_Species)]
(stations.NAs <- tab.stations$Station[is.na(tab.stations$Broadcast_Species)])
# Note: These stations will need to be replaced with NAs in the Ys below, NOT ZEROS
#' 
#' Now, replace NAs with "correct" broadcast species for covariate purposes
#' 
for(xx in 1:length(stations.NAs)){
  owlnames <- tab.stations$Broadcast_Species[tab.stations$Station %in% stations.NAs[xx]]
  temp.k <- unique(owlnames[!is.na(owlnames)])
  tab.stations$Broadcast_Species[tab.stations$Station %in% stations.NAs[xx]] <- temp.k
}
unique(tab.stations$Broadcast_Species[tab.stations$Station %in% stations.NAs])


#' Create matrices for covariate of broadcast species (shared intercept)
#'
#' 
(route.Names <- unique(tab.route$Route_ID))
(broadcast.species <- unique(tab.stations$Broadcast_Species))
(broadcast.species.index <- 1:length(broadcast.species))
ks.index.numb <- ks
for(hh in 1:length(route.Names)){
  
  for(ht in 1:length(grep(pattern = route.Names[hh], names(ks)))){
    
    for(jj in 1:10){ # across all 10 stations per route
      
      # Find all the tables in the ks list with correct Route and fill in intercept
      ks[[grep(pattern = route.Names[hh], names(ks))[ht]]][jj,1] <- 0 #intercept pre-broadcast
      ks.index.numb[[grep(pattern = route.Names[hh], 
                          names(ks.index.numb))[ht]]][jj,1] <- 0 #intercept pre-broadcast
      
      # Find all the tables in the ks list with correct Route and fill in intercept
      ks[[grep(pattern = route.Names[hh], names(ks))[ht]]][jj,2] <- unique(
        tab.stations$Broadcast_Species[tab.stations$Station == paste0(route.Names[hh],".",jj)]
      )
      ks.index.numb[[grep(pattern = route.Names[hh], names(ks))[ht]]][jj,2] <- 
        broadcast.species.index[broadcast.species == ks[[grep(pattern = route.Names[hh], names(ks))[ht]]][jj,2]]
      
    }
    
  }
  
}





#' _____________________________________________________________________________
#' ## Process by species of owl
#' 
columns.to.keep <- c("Owl_ID", "Stations_ID", "Owl_Species_ID", 
                     "Owl_Number", "Minute_1", "Minute_2", "Minute_6-12")

#' 
#' ### Mottled Owl
#' 
#' Duplicate data set up
mottd.ys <- ys


#' Separate out only Mottled Owls data and join with stations table
#' 
tab.owls.mottd <- tab.owls[tab.owls$Owl_Species_ID=="Mottd",columns.to.keep]
mottd.master <- left_join(x = tab.owls.mottd, y = tab.stations, by = "Stations_ID")

#' Process observations for each station and survey night and survey period
#' 
#' Create for-loop to convert owl observations into binary data
for(hh in 1:length(route.Names)){ # across routes
  for(tt in 1:length(min(tab.survey$year):max(tab.survey$year))){ # across years
    for(ii in 1:max(tab.survey$order)){ # across surveys per year
      
      temp.route <- route.Names[hh]
      temp.year <- c(min(tab.survey$year):max(tab.survey$year))[tt]
      temp.survey <- ii
      temp.survey_ID <- tab.survey$Survey_ID[tab.survey$Route_ID == temp.route &
                                               tab.survey$year == temp.year &
                                               tab.survey$order == ii]
      survey.notexist.test <- length(temp.survey_ID) == 0
      if(survey.notexist.test == FALSE){
        for(jj in 1:10){ # across stations
          owls_observed <- mottd.master[mottd.master$Survey_ID==temp.survey_ID &
                                        mottd.master$Station == paste0(temp.route,".",jj),]
          
          
          # Was observation before or after broadcast?
          # kk = 1 for before broadcast
          # kk = 2 for after broadcast
          logic.prebroadcast <- c(owls_observed$Minute_1, owls_observed$Minute_2)
          logic.postbroadcast <- c(owls_observed$`Minute_6-12`)
          
          mottd.ys[[paste0(temp.route,".",temp.year,".",temp.survey)]][jj,1] <- #look up rt.year.survey 
            ifelse(sum(logic.prebroadcast>0), yes = 1, no = 0)
          mottd.ys[[paste0(temp.route,".",temp.year,".",temp.survey)]][jj,2] <- #look up rt.year.survey 
            ifelse(sum(logic.postbroadcast>0), yes = 1, no = 0)
        }
        
      }
      
      
      
    }
  }
}

#' Verify that it looks correct: 
#' 
mottd.ys[[1]]
mottd.master[mottd.master$Survey_ID==20,1:9]



#' ## Prepare data for saving
#' 
#' _____________________________________________________________________________
#' ## Define variables
#' 
#' Observations
n.route <- length(unique(data.jags$Route_ID)) # hh
route.names <- unique(data.jags$Route_ID)

n.year <- length(min(data.jags$year):max(data.jags$year)) # tt
year.names <- min(data.jags$year):max(data.jags$year)

n.survey <- max(data.jags$order) # ii
n.station <- 10 # jj
n.broadcast <- 2 # kk

#' Create a lookup table to link the route.year.survey dataset of Ys with a numerical index 1:198

lookup.hhttii.names <- names(mottd.ys)
lookup.hhttii.numb <- 1:length(lookup.hhttii.names)
lookup.hhttii.array <- array(NA, dim = c(n.route, n.year, n.survey))

for(ii in 1:n.survey){
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      
      temp.record <- 
        lookup.hhttii.numb[lookup.hhttii.names == paste0(route.names[hh],".",year.names[tt],".",ii)]
      
      lookup.hhttii.array[hh,tt,ii] <- temp.record
      
    }
  }
}
lookup.hhttii.array

#' Turn ks into arrays
ks.array <- array(unlist(ks), dim = c(10, 2, length(lookup.hhttii.names)))
ks.array.index <- array(as.numeric(unlist(ks.index.numb)), dim = c(10, 2, length(lookup.hhttii.names)))

#' Convert ks into a series of 10 model matrices
#' 
#' For example means parameterization, w/ 1s for Pacific screech owl in one matrix, etc
#' 
#' Pre-broadcast
ks.prebroad <- array(as.numeric(rep(c(1,0), each = 10)), dim = c(10,2, length(lookup.hhttii.names)))
ks.prebroad[,,1]
#' 
ks.pacific.list <- rapply(ks, function(x) ifelse(x == "Pacific Screech Owl", 1, 0), how = "replace")
ks.pacific <- array(as.numeric(unlist(ks.pacific.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.mottled.list <- rapply(ks, function(x) ifelse(x == "Mottled Owl", 1, 0), how = "replace")
ks.mottled <- array(as.numeric(unlist(ks.mottled.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.crested.list <- rapply(ks, function(x) ifelse(x == "Crested Owl", 1, 0), how = "replace")
ks.crested <- array(as.numeric(unlist(ks.crested.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.bw.list <- rapply(ks, function(x) ifelse(x == "Black and White Owl", 1, 0), how = "replace")
ks.bw <- array(as.numeric(unlist(ks.bw.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.spectacled.list <- rapply(ks, function(x) ifelse(x == "Spectacled Owl", 1, 0), how = "replace")
ks.spectacled <- array(as.numeric(unlist(ks.spectacled.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.whiskered.list <- rapply(ks, function(x) ifelse(x == "Whiskered", 1, 0), how = "replace")
ks.whiskered <- array(as.numeric(unlist(ks.whiskered.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.gbarred.list <- rapply(ks, function(x) ifelse(x == "Guat. Barred Owl", 1, 0), how = "replace")
ks.gbarred <- array(as.numeric(unlist(ks.gbarred.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.stygian.list <- rapply(ks, function(x) ifelse(x == "Stygian Owl", 1, 0), how = "replace")
ks.stygian <- array(as.numeric(unlist(ks.stygian.list)), dim = c(10, 2, length(lookup.hhttii.names)))

ks.ghorned.list <- rapply(ks, function(x) ifelse(x == "Great Horned Owl", 1, 0), how = "replace")
ks.ghorned <- array(as.numeric(unlist(ks.ghorned.list)), dim = c(10, 2, length(lookup.hhttii.names)))



#' All levels of k
#' 
k.names <- unique(as.character(ks.array[,2,]))
ks.levels <- c(0, 1:length(k.names)) # 0 if pre-broadcast, 1:9 if post-broadcast





#' _____________________________________________________________________________
#' ## Save files
#' 
save(data.jags, mottd.ys, file = "data/processed_data/owl_data.Rdata")
save(
  ks, ks.index.numb, ks.array.index, 
  k.names, ks.levels, 
  ks.prebroad, ks.pacific, ks.mottled, ks.crested, ks.bw, ks.spectacled, 
  ks.whiskered, ks.gbarred, ks.stygian, ks.ghorned,
  file = "data/processed_data/ks_jags_input.Rdata")
     
     





#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/a_process_data.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 
#' 
#' 
