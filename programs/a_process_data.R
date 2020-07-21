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
#' ### Begin with survey data, order and clean
#' 
#' Sort survey data by Route then survey date
data.jags <- dplyr::arrange(tab.survey, Route_ID, Survey_Date)

#' Drop unneccessary columns
colnames(data.jags)
temp.names.keep <- c("Survey_ID", "Route_ID", "Survey_Date", "year")
data.jags <- data.jags[,temp.names.keep]

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

#' ### Merge all stations, survey, and owl data together
#' 
#' 
master.data <- left_join(x = tab.owls, y = tab.stations, by = "Stations_ID")
master.data <- left_join(x = master.data, y = tab.survey, by = "Survey_ID")

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
tab.owls.mottd <- tab.owls[tab.owls$Owl_Species_ID=="Mottd",columns.to.keep]


#' _____________________________________________________________________________
#' ## Save files
#' 



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/a_process_data.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 