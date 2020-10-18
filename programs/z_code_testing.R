library(readxl)
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

#' Make "None" and "NoID" match
#' 
tab.owls$Owl_Species_ID <- ifelse(test = tab.owls$Owl_Species_ID == "None" |
                                    tab.owls$Owl_Species_ID == "NoID",
                                  yes = "Unk",
                                  no = tab.owls$Owl_Species_ID)
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

#' Concatenate that order index with route/year column to create unique identifier
#' for each different survey
tab.survey$hRt_tYr_iSvy <- paste(tab.survey$hRt_tYr, tab.survey$order, sep = ".")
head(tab.survey$hRt_tYr_iSvy)

#' Create table of all conducted surveys by route and year
#' 
survey_list <- sort(unique(tab.survey$hRt_tYr))







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

# _____________________________________                                           new code below



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
n.species <- length(unique(tab.owls$Owl_Species_ID)) # ss
n.aug <- 20 # number of "undetected species of owls" where n.aug = M >> N


#' 
#' ## Broadcast Data
#' 
ks <-  array(0, dim = c(n.route, n.year, n.survey, n.station, n.broadcast))
ks.prebroad <- ks.mottled <- ks.pacific <- ks.crested <- ks.bw <- ks.spectacled <-
  ks.whiskered <- ks.gbarred <- ks.stygian <- ks.ghorned <- ks

broadcast.array <- array(c(
  rep(c("pacific", "mottled", "crested", "bw", "spectacled"),4), # routes EI1/EI2
  rep(c("whiskered", "mottled", "gbarred", "stygian", "ghorned"),4), # routes M1/M2
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



#' ## OWL DATA
#' 
#' 
#' Create blank arrays
ys <- array(NA, dim = c(n.route, n.year, n.survey, n.station, n.broadcast))
mottd.ys <- ferpy.ys <- specd.ys <- ys

# Add all species together where each species has it's own results
ys.allspecies <- array(NA, dim = c(n.route, n.year, n.survey, 
                                   n.station, n.broadcast, n.species+n.aug))

for(hh in 1:n.route){
  temp.route <- route.names[hh]
  for(tt in 1:n.year){
    temp.year <- year.names[tt]
    for(ii in 1:n.survey){
      temp.surveyID <- 
        tab.survey$Survey_ID[tab.survey$hRt_tYr_iSvy == paste(temp.route,temp.year,ii, sep = ".")]
      for(jj in 1:n.station){
        temp.stationID <- 
          tab.stations$Stations_ID[tab.stations$Station == paste(temp.route,jj, sep = ".")&
                                                     tab.stations$Survey_ID == temp.surveyID]
        temp.owls <- tab.owls[tab.owls$Stations_ID == temp.stationID,]
        
        temp.n.rows <- nrow(temp.owls)
        
        
          if(temp.owls$Owl_Species_ID %in% "Mottd"){
            mottd.temp <- temp.owls[temp.owls$Owl_Species_ID == "Mottd",]
            # Was observation before or after broadcast?
            # kk = 1 for before broadcast
            # kk = 2 for after broadcast
            logic.prebroadcast <- c(mottd.temp$Minute_1, mottd.temp$Minute_2)
            logic.postbroadcast <- c(mottd.temp$`Minute_6-12`)
            
            mottd.ys[hh,tt,ii,jj,1] <- ifelse(sum(logic.prebroadcast)>0, yes = 1, no = 0)
            mottd.ys[hh,tt,ii,jj,2] <- ifelse(sum(logic.postbroadcast)>0, yes = 1, no = 0)
            
            ys.allspecies[hh,tt,ii,jj,1,1] <- ifelse(sum(logic.prebroadcast)>0, 
                                                     yes = 1, no = 0)
            ys.allspecies[hh,tt,ii,jj,2,1] <- ifelse(sum(logic.postbroadcast)>0, 
                                                     yes = 1, no = 0)
          }else if(temp.owls$Owl_Species_ID %in% "FerPy"){
            ferpy.temp <- temp.owls[temp.owls$Owl_Species_ID == "FerPy",]
            # Was observation before or after broadcast?
            # kk = 1 for before broadcast
            # kk = 2 for after broadcast
            logic.prebroadcast <- c(temp.owls$Minute_1, temp.owls$Minute_2)
            logic.postbroadcast <- c(temp.owls$`Minute_6-12`)
          
            ferpy.ys[hh,tt,ii,jj,1] <- ifelse(sum(logic.prebroadcast)>0, yes = 1, no = 0)
            ferpy.ys[hh,tt,ii,jj,2] <- ifelse(sum(logic.postbroadcast)>0, yes = 1, no = 0)
            
            ys.allspecies[hh,tt,ii,jj,1,2] <- ifelse(sum(logic.prebroadcast)>0, 
                                                     yes = 1, no = 0)
            ys.allspecies[hh,tt,ii,jj,2,2] <- ifelse(sum(logic.postbroadcast)>0, 
                                                     yes = 1, no = 0)
          }else if(temp.owls$Owl_Species_ID %in% "Specd"){
            specd.temp <- temp.owls[temp.owls$Owl_Species_ID == "Specd",]
            # Was observation before or after broadcast?
            # kk = 1 for before broadcast
            # kk = 2 for after broadcast
            logic.prebroadcast <- c(temp.owls$Minute_1, temp.owls$Minute_2)
            logic.postbroadcast <- c(temp.owls$`Minute_6-12`)
            
            specd.ys[hh,tt,ii,jj,1] <- ifelse(sum(logic.prebroadcast)>0, yes = 1, no = 0)
            specd.ys[hh,tt,ii,jj,2] <- ifelse(sum(logic.postbroadcast)>0, yes = 1, no = 0)
            
            ys.allspecies[hh,tt,ii,jj,1,3] <- ifelse(sum(logic.prebroadcast)>0, 
                                                     yes = 1, no = 0)
            ys.allspecies[hh,tt,ii,jj,2,3] <- ifelse(sum(logic.postbroadcast)>0, 
                                                     yes = 1, no = 0)
          }else{
            mottd.ys[hh,tt,ii,jj,] <- 0
            ferpy.ys[hh,tt,ii,jj,] <- 0
            specd.ys[hh,tt,ii,jj,] <- 0
          
          
        }
      }
    }
  }
}








# _____________________________________                                           old code below


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








#' ## Prepare support data for saving
#' 
#' _____________________________________________________________________________


#' Create a lookup table to link the route.year.survey 
#' dataset of Ys with a numerical index 1:198

lookup.hhttii.names <- names(ys)
lookup.hhttii.numb <- 1:length(lookup.hhttii.names)
lookup.hhttii.array <- array(NA, dim = c(n.route, n.year, n.survey))

for(ii in 1:n.survey){
  for(hh in 1:n.route){
    for(tt in 1:n.year){
      
      temp.record <- 
        lookup.hhttii.numb[
          lookup.hhttii.names == paste0(route.names[hh],".",year.names[tt],".",ii)]
      
      lookup.hhttii.array[hh,tt,ii] <- temp.record
      
    }
  }
}
lookup.hhttii.array

#' Turn ks into arrays
ks.array <- array(unlist(ks), dim = c(10, 2, length(lookup.hhttii.names)))
ks.array.index <- array(as.numeric(unlist(ks.index.numb)), 
                        dim = c(10, 2, length(lookup.hhttii.names)))

#' Convert ks into a series of 10 model matrices
#' 
#' For example means parameterization, w/ 1s for Pacific screech owl in one matrix, etc
#' 
#' Pre-broadcast
ks.prebroad <- array(as.numeric(rep(c(1,0), each = 10)), 
                     dim = c(10,2, length(lookup.hhttii.names)))
ks.prebroad[,,1]
#' 
ks.pacific.list <- 
  rapply(ks, function(x) ifelse(x == "Pacific Screech Owl", 1, 0), how = "replace")
ks.pacific <- array(as.numeric(unlist(ks.pacific.list)), 
                    dim = c(10, 2, length(lookup.hhttii.names)))

ks.mottled.list <- 
  rapply(ks, function(x) ifelse(x == "Mottled Owl", 1, 0), how = "replace")
ks.mottled <- array(as.numeric(unlist(ks.mottled.list)), 
                    dim = c(10, 2, length(lookup.hhttii.names)))

ks.crested.list <- 
  rapply(ks, function(x) ifelse(x == "Crested Owl", 1, 0), how = "replace")
ks.crested <- array(as.numeric(unlist(ks.crested.list)), 
                    dim = c(10, 2, length(lookup.hhttii.names)))

ks.bw.list <- 
  rapply(ks, function(x) ifelse(x == "Black and White Owl", 1, 0), how = "replace")
ks.bw <- array(as.numeric(unlist(ks.bw.list)), 
               dim = c(10, 2, length(lookup.hhttii.names)))

ks.spectacled.list <- 
  rapply(ks, function(x) ifelse(x == "Spectacled Owl", 1, 0), how = "replace")
ks.spectacled <- array(as.numeric(unlist(ks.spectacled.list)), 
                       dim = c(10, 2, length(lookup.hhttii.names)))

ks.whiskered.list <- 
  rapply(ks, function(x) ifelse(x == "Whiskered", 1, 0), how = "replace")
ks.whiskered <- array(as.numeric(unlist(ks.whiskered.list)), 
                      dim = c(10, 2, length(lookup.hhttii.names)))

ks.gbarred.list <- 
  rapply(ks, function(x) ifelse(x == "Guat. Barred Owl", 1, 0), how = "replace")
ks.gbarred <- array(as.numeric(unlist(ks.gbarred.list)), 
                    dim = c(10, 2, length(lookup.hhttii.names)))

ks.stygian.list <- 
  rapply(ks, function(x) ifelse(x == "Stygian Owl", 1, 0), how = "replace")
ks.stygian <- array(as.numeric(unlist(ks.stygian.list)), 
                    dim = c(10, 2, length(lookup.hhttii.names)))

ks.ghorned.list <- 
  rapply(ks, function(x) ifelse(x == "Great Horned Owl", 1, 0), how = "replace")
ks.ghorned <- array(as.numeric(unlist(ks.ghorned.list)), 
                    dim = c(10, 2, length(lookup.hhttii.names)))



#' All levels of k
#' 
k.names <- unique(as.character(ks.array[,2,]))
ks.levels <- c(0, 1:length(k.names)) # 0 if pre-broadcast, 1:9 if post-broadcast

