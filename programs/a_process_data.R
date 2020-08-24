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


#' 
#' Create an array of Ys (detections/non-detections) for each of the 84 different
#' surveys
#' 







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

#' Create blank fields for all of observed presences
#' 
#' There will be 84 total matrices, each with 10 rows (j, stations) and 2 columns 
#' (k, before or after broadcast)
#' 
#' 
ys <- list(NA)
for(hti in 1:nrow(data.jags)){
  ys[[hti]] <- matrix(NA, ncol = 2, nrow = 10)
}
names(ys) <- data.jags$hRt_tYr_iSvy


#' _____________________________________________________________________________
#' ## Process by species of owl
#' 
columns.to.keep <- c("Owl_ID", "Stations_ID", "Owl_Species_ID", 
                     "Owl_Number", "Minute_1", "Minute_2", "Minute_6-12")
#' 
#' ### Mottled Owl
#' 
#' Duplicate data set up
mottd.jags <- data.jags


#' Separate out only Mottled Owls data and join with stations table
#' 
tab.owls.mottd <- tab.owls[tab.owls$Owl_Species_ID=="Mottd",columns.to.keep]
mottd.master <- left_join(x = tab.owls.mottd, y = tab.stations, by = "Stations_ID")

#' Process observations for each station and survey night and survey period
#' 
#' Create for-loop to convert owl observations into binary data
for(rr in 1:nrow(mottd.jags)){ # go over each row of owl observations
  ii <- mottd.jags$Survey_ID[rr] # Unique Survey ID
  hh <- tab.survey$Route_ID[tab.survey$Survey_ID == ii] # Unique Route ID
  
  # Select all owl observations made during each survey (ii)
  survey_data <- mottd.master[mottd.master$Survey_ID==ii,]
  
  for(jj in 1:10){ # across all 10 stations
    # Were there any owls observed at each station (jj)?
    
    # Set up logical test with a length statement
    # If = 0, no observations for that station
    # If >= 1, there were observations for that station 
    test.y <- sum(survey_data$Station == paste0(hh,".",jj), na.rm = T)
    if(test.y == 0){
      mottd.jags[rr,paste0("y.",jj,".1")] <- 0 #before broadcast
      mottd.jags[rr,paste0("y.",jj,".2")] <- 0 #and after broadcast are 0s
    }else{
      owls.observed <- survey_data[survey_data$Station == paste0(hh,".",jj),]
      # Was observation before or after broadcast?
      # kk = 1 for before broadcast
      # kk = 2 for after broadcast
      logic.prebroadcast <- c(owls.observed$Minute_1,owls.observed$Minute_2)
      logic.postbroadcast <- c(owls.observed$`Minute_6-12`)
      mottd.jags[rr,paste0("y.",jj,".1")] <- ifelse(sum(logic.prebroadcast>0),
                                                    yes = 1, no = 0)
      mottd.jags[rr,paste0("y.",jj,".2")] <- ifelse(sum(logic.postbroadcast>0),
                                                    yes = 1, no = 0)
    }
  }
}
summary(mottd.jags)
head(mottd.jags)






# ' ###################################
#       old code below







ys <- matrix(NA, nrow = nrow(data.jags), ncol = 20)
iter.index <- matrix(1:20, nrow = 10, ncol = 2, byrow = T)
for(jj in 1:10){ #loops over stations
  for(kk in 1:2){ #loops over broadcast periods
    colnames(ys)[iter.index[jj,kk]] <- paste("y", jj, kk, sep = ".")
  }
}
head(ys)





#' Add a column for each Station ID
#' 
data.jags$station.1 <- paste(data.jags$Route_ID, 1, sep = ".")
data.jags$station.2 <- paste(data.jags$Route_ID, 2, sep = ".")
data.jags$station.3 <- paste(data.jags$Route_ID, 3, sep = ".")
data.jags$station.4 <- paste(data.jags$Route_ID, 4, sep = ".")
data.jags$station.5 <- paste(data.jags$Route_ID, 5, sep = ".")
data.jags$station.6 <- paste(data.jags$Route_ID, 6, sep = ".")
data.jags$station.7 <- paste(data.jags$Route_ID, 7, sep = ".")
data.jags$station.8 <- paste(data.jags$Route_ID, 8, sep = ".")
data.jags$station.9 <- paste(data.jags$Route_ID, 9, sep = ".")
data.jags$station.10 <- paste(data.jags$Route_ID, 10, sep = ".")



#' Attach to data.jags
#' 
#data.jags <- cbind(data.jags, ys)





#' Process observations for each station and survey night and survey period
#' 
#' Create for-loop to convert owl observations into binary data
for(rr in 1:nrow(mottd.jags)){
  ii <- mottd.jags$Survey_ID[rr] # Unique Survey ID
  hh <- tab.survey$Route_ID[tab.survey$Survey_ID == ii] # Unique Route ID
  
  # Select all owl observations made during each survey (ii)
  survey_data <- mottd.master[mottd.master$Survey_ID==ii,]
  
  for(jj in 1:10){
    # Were there any owls observed at each station (jj)?
    
    # Set up logical test with a length statement
      # If = 0, no observations for that station
      # If >= 1, there were observations for that station 
    test.y <- sum(survey_data$Station == paste0(hh,".",jj), na.rm = T)
    if(test.y == 0){
      mottd.jags[rr,paste0("y.",jj,".1")] <- 0 #before broadcast
      mottd.jags[rr,paste0("y.",jj,".2")] <- 0 #and after broadcast are 0s
    }else{
      owls.observed <- survey_data[survey_data$Station == paste0(hh,".",jj),]
        # Was observation before or after broadcast?
          # kk = 1 for before broadcast
          # kk = 2 for after broadcast
      logic.prebroadcast <- c(owls.observed$Minute_1,owls.observed$Minute_2)
      logic.postbroadcast <- c(owls.observed$`Minute_6-12`)
      mottd.jags[rr,paste0("y.",jj,".1")] <- ifelse(sum(logic.prebroadcast>0),
                                                    yes = 1, no = 0)
      mottd.jags[rr,paste0("y.",jj,".2")] <- ifelse(sum(logic.postbroadcast>0),
                                                    yes = 1, no = 0)
    }
  }
}
summary(mottd.jags)
head(mottd.jags)




#' ## Process broadcast species data
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

#' ### Merge all stations, survey, and owl data together
#' 
#' 
master.data <- left_join(x = tab.owls, y = tab.stations, by = "Stations_ID")
master.data <- left_join(x = master.data, y = tab.survey, by = "Survey_ID")




#' _____________________________________________________________________________
#' ## Save files
#' 
save(mottd.jags, file = "data/processed_data/mottd_jags_input.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/a_process_data.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 