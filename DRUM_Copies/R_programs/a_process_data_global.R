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
#' 
#' ### Route Table
#' 
#' Routes are the six unique transects or routes with two per protected area (Forest).
#' There are unique IDs that correspond with protected area, and six unique route
#' names, as well.
tab.route <- read.csv(file = "data/raw_data/Route_Table.csv", 
                      header = T, 
                      stringsAsFactors = F)
tab.route

#' ### Stations Table
#' 
#' Each route consists of 10 stations. There is no table with information about 
#' the 60 total stations. Instead, this stations table has each station repeated 
#' for each survey night with unique measurements of temperature, barometer, 
#' broadcast species, and background noise level (from 0 to 3). The
#' link between these stations and the actual individual surveys is the field "Survey_ID".
#' 
tab.stations <- read.csv(file = "data/raw_data/Stations_Table.csv",
                         header = T,
                         stringsAsFactors = F)
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
tab.survey <- read.csv(file = "data/raw_data/Survey_Table.csv",
                       header = T,
                       stringsAsFactors = F)
table(tab.survey$Route_ID)
str(tab.survey)
dim(tab.survey)

#' Remove data for year = 2002
# First, have to convert from MM/DD/YY to date structure
tab.survey$Survey_Date <- lubridate::as_date(x = tab.survey$Survey_Date, format = "%m/%d/%y")
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
tab.owls <- read.csv(file = "data/raw_data/Owls_Table.csv", 
                     header = T, 
                     stringsAsFactors = F)
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

#' Remove "None" owls - these are records that don't have associated owls IDed at those surveys
#' 
tab.owls <- tab.owls[tab.owls$Owl_Species_ID != "None" &
                       tab.owls$Owl_Species_ID != "NoID",]


table(tab.owls$Owl_Species_ID)



#' _____________________________________________________________________________
#' ## Other global processing steps
#' 

#' ### Create order to surveys per year
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





#' ### Create JAGS data, global
#' 
#' Sort survey data by Route then survey date
data.jags <- dplyr::arrange(tab.survey, Route_ID, Survey_Date)

#' Drop unnecessary columns
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


#' ### Process broadcast species data
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
# Note: These stations will need to be replaced with NAs in the Ys, NOT ZEROS
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
#' ## Save files
#' 
#' 
#' Access tables, in Rdata format
#' 
save(tab.owls, tab.route, tab.stations, tab.survey, 
     file = "data/processed_data/tables_global.Rdata")

#' Supplemental JAGS table
#' 
save(data.jags,
     file = "data/processed_data/data_jags_global.Rdata")

#' Supplemental JAGS table
#' 
save(survey_list,
     file = "data/processed_data/survey_list_global.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/a_process_data_global.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 
#' 
#' 
