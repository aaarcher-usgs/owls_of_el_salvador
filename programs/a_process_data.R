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

#' The Station_Start_Time did not read in correctly because in Excel it is assuming 
#' the origin date of 1899-12-31. The correct date will come from the Survey Table, below.
head(tab.stations$Station_Start_Time)

#' Fix a couple incorrectly coded broadcast species in stations table
#' 
tab.stations$Broadcast_Species[tab.stations$Broadcast_Species == "Black and White Owl" &
                                 tab.stations$Station == "M2.4"] <- "Stygian Owl"



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
# Create a unique identifier for eacy route per year
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
data.jags$yearIndex <- 
  ifelse(test = data.jags$year == 2003,
         yes = 1,
         no = ifelse(test = data.jags$year == 2004,
                     yes = 2,
                     no = ifelse(data.jags$year == 2005,
                                 yes = 3,
                                 no = ifelse(data.jags$year == 2006,
                                             yes = 4,
                                             no = ifelse(data.jags$year == 2007,
                                                         yes = 5,
                                                         no = ifelse(data.jags$year == 2008,
                                                                     yes = 6,
                                                                     no = data.jags$year))))))
data.jags$yearIndex <- 
  ifelse(data.jags$year == 2009,
         yes = 7, 
         no = ifelse(data.jags$year == 2010, 
                     yes = 8,
                     no = ifelse(data.jags$year == 2011,
                                 yes = 9,
                                 no = ifelse(data.jags$year == 2012,
                                             yes = 10,
                                             no = ifelse(data.jags$year == 2013,
                                                         yes = 11,
                                                         no = data.jags$yearIndex)))))
# Double check that year counts match
table(data.jags$year)
table(data.jags$yearIndex)


#' _____________________________________________________________________________
#' ## Create tables for Ys and Broad Cast species covariates
#' 
#' 
#' Create an array of Ys (detections/non-detections) for each of the 84 different
#' surveys
#' 
#' 
#' There will be 84 total matrices, each with 10 rows (j, stations) and 2 columns 
#' (k, before or after broadcast)
#' 
#' 
ys <- list(NA)
ks <- list(NA)
for(hti in 1:nrow(data.jags)){
  ys[[hti]] <- 
  ks[[hti]] <- matrix(NA, ncol = 2, nrow = 10)
}
names(ys) <- data.jags$hRt_tYr_iSvy
names(ks) <- data.jags$hRt_tYr_iSvy


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
(route.Index <- 1:length(route.Names))

(k.full <- unique(tab.stations$Broadcast_Species)) # full name of k species
(k.index <- letters[1:length(k.full)]) # index for k species in letters
(k.factor <- as.factor(k.index))

for(rr in 1:nrow(data.jags)){ # go over each row of owl observations
  ii <- data.jags$Survey_ID[rr] # Unique Survey ID
  hh <- tab.survey$Route_ID[tab.survey$Survey_ID == ii] # Unique Route ID
  tt <- tab.survey$year[tab.survey$Survey_ID == ii] # year of survey
  ii.order <- data.jags$order[data.jags$Survey_ID==ii]
  
  
  for(jj in 1:10){ # across all 10 stations
    
    # Fill in broadcast species
    # unique station id
    temp.station <- tab.stations$Stations_ID[tab.stations$Survey_ID==ii & 
                                               tab.stations$Station == paste0(hh,".",jj)]
    
    # determine species name (full)
    temp.species <- unique(
      tab.stations$Broadcast_Species[tab.stations$Station == temp.station]
    )
    
    ks[[paste0(hh,".",tt,".",ii.order)]][jj,1] <- 0
    ks[[paste0(hh,".",tt,".",ii.order)]][jj,2] <- 
      tab.stations$Broadcast_Species[tab.stations$Stations_ID == temp.station]
  }
}

# ks <- NULL
# for(hh in 1:length(route.Names)){ # across 6 routes
#   
#   # Create blank 10 x 2 matrix in for each route
#   ks[[route.Names[hh]]] <- matrix(NA, ncol = 2, nrow = 10)
#   
#   for(jj in 1:10){ # across 10 stations per route
#     # station name (rr.jj)
#     temp.station.name <- paste0(route.Names[hh], ".", jj)
#     
#     # determine species name (full)
#     temp.species <- unique(
#       tab.stations$Broadcast_Species[tab.stations$Station == temp.station.name]
#     )
#     
#     # fill in 0, 
#     ks[[route.Names[hh]]][jj,] <- c(0, k.factor[k.full == temp.species])
#   }
# }

#' Create new table for looking up how many surveys per route (rows) per year (cols)
#' 
surveys.lookup <- as.data.frame(matrix(NA, nrow = length(route.Names), 
                                       ncol = length(unique(data.jags$year))))
colnames(surveys.lookup) <- unique(data.jags$year)
year.names <- colnames(surveys.lookup)
rownames(surveys.lookup) <- route.Names
for(hh in 1:nrow(surveys.lookup)){
  for(tt in 1:ncol(surveys.lookup)){
    tempdata <- data.jags[data.jags$Route_ID==route.Names[hh] &
                            data.jags$year == year.names[tt],]
    if(nrow(tempdata)==0){
      surveys.lookup[route.Names[hh],year.names[tt]] <- NA
    }else{
      surveys.lookup[route.Names[hh],year.names[tt]] <- max(tempdata$order)
    }
  }
}
surveys.lookup


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
for(rr in 1:nrow(data.jags)){ # go over each row of owl observations
  ii <- data.jags$Survey_ID[rr] # Unique Survey ID
  hh <- tab.survey$Route_ID[tab.survey$Survey_ID == ii] # Unique Route ID
  tt <- tab.survey$year[tab.survey$Survey_ID == ii] # year of survey
  ii.order <- data.jags$order[data.jags$Survey_ID==ii]
  
  # Select all owl observations made during each survey (ii)
  survey_data <- mottd.master[mottd.master$Survey_ID==ii,]
  

  
  for(jj in 1:10){ # across all 10 stations
    
    # Were there any owls observed at each station (jj)?
    
    # Set up logical test with a length statement
      # If = 0, no observations for that station
      # If >= 1, there were observations for that station 
    test.y <- sum(survey_data$Station == paste0(hh,".",jj), na.rm = T)
    if(test.y == 0){
      mottd.ys[[paste0(hh,".",tt,".",ii.order)]][jj,1] <- 0 #before broadcast
      mottd.ys[[paste0(hh,".",tt,".",ii.order)]][jj,2] <- 0 #and after broadcast are 0s
    }else{
      owls.observed <- survey_data[survey_data$Station == paste0(hh,".",jj),]
      # Was observation before or after broadcast?
      # kk = 1 for before broadcast
      # kk = 2 for after broadcast
      logic.prebroadcast <- c(owls.observed$Minute_1,owls.observed$Minute_2)
      logic.postbroadcast <- c(owls.observed$`Minute_6-12`)
      
      mottd.ys[[paste0(hh,".",tt,".",ii.order)]][jj,1] <- #look up rt.year.survey 
        ifelse(sum(logic.prebroadcast>0), yes = 1, no = 0)
      mottd.ys[[paste0(hh,".",tt,".",ii.order)]][jj,2] <- #look up rt.year.survey 
        ifelse(sum(logic.postbroadcast>0), yes = 1, no = 0)
      
    }
  }
}
summary(mottd.ys$EI1.2003.1)
head(mottd.ys$EI1.2003.1)

#' _____________________________________________________________________________
#' ## Balance survey design 
#' 
#' As shown above, there are going to be some Route/years that were not surveyed
#' at all. Those will not have a y-matrix of NAs, so we must add in y-matrices of
#' NAs for those in order to be able to cycle through JAGS model
#' 
(mottd.surveys.lookup <- surveys.lookup)

for(hh in 1:nrow(surveys.lookup)){
  for(tt in 1:ncol(surveys.lookup)){
    if(is.na(surveys.lookup[hh,tt])){
      mottd.ys[[paste0(route.Names[hh],".",year.names[tt],".1")]] <- 
        matrix(NA, ncol = 2, nrow = 10)
      mottd.surveys.lookup[hh,tt] <- 1
    }
  }
}
(mottd.surveys.lookup)
sum(mottd.surveys.lookup, na.rm = T)
length(mottd.ys)

#' Remove stations that were not actually surveyed and replace with NAs for Y
#' 
stations.NAs
# data for those specific records are from these surveys
surveys.NAs
data.jags$hRt_tYr_iSvy[data.jags$Survey_ID %in% surveys.NAs]
for(xx in 1:length(stations.NAs)){ #loop over three stations to modify
  hRt_tYr_iSvy <- data.jags$hRt_tYr_iSvy[data.jags$Survey_ID == surveys.NAs[xx]]
  tempstation <- stations.NAs[xx] # ID of specific station # to convert to NA
  # strip off station number, which is same as row number of ys
  temp.split <- unlist(strsplit(tempstation, split = "[.]"))
  mottd.ys[[hRt_tYr_iSvy]][as.numeric(temp.split[2]),] <- NA
  print(mottd.ys[[hRt_tYr_iSvy]])
}








#' _____________________________________________________________________________
#' ## Save files
#' 
save(data.jags, mottd.ys, ks, mottd.surveys.lookup,
     file = "data/processed_data/mottd_jags_input.Rdata")




#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/a_process_data.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 