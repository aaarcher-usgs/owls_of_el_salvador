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

#' Make "None" and "NoID" match
#' 
tab.owls$Owl_Species_ID <- ifelse(test = tab.owls$Owl_Species_ID == "None" |
                                    tab.owls$Owl_Species_ID == "NoID",
                                  yes = "Unk",
                                  no = tab.owls$Owl_Species_ID)
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
