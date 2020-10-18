library(readxl)
remove(list = ls())
set.seed(2583722)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' 
#' 



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

