#' # Graphing Occupancy Analysis
#' 
#' Description: This program graphs occupancy analysis results for owls of El 
#' Salvador.
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
library(ggplot2)
library(ggthemes)

#' Clear environment and set seed
#' 
#' *Note: for reproducibility, it is important to include these. Clearing the
#' environment ensures that you have specified all pertinent files that need
#' to be loaded, and setting the seed ensures that your analysis is 
#' repeatable*
remove(list = ls())
set.seed(587453)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' 
#' Psi Posteriors by year and route
save(psi.posteriors, file = "data/output_data/psi_posteriors_RtYr.Rdata")

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' ### By Route and Year
#' 
#' Mottd
#+ mottd_psi_byYr
ggplot(data = psi.posteriors[psi.posteriors$Species=="Mottd",], 
       aes(x = as.character(Year), y = Psi.50, group = Route, shape = Route))+
  geom_pointrange(aes(ymin = Psi.05, ymax = Psi.95, color = Route),
                  position = position_dodge(width = .1))+
  geom_line(aes(color = Route))+
  scale_color_manual(values = c("blue", "lightblue", "green", "lightgreen", "red", "pink"))+
  scale_shape_manual(values = c(0, 16, 0, 16, 0, 16))+
  facet_wrap(~Region, nrow = 3)+
  theme_minimal()+
  xlab("Year")+
  ylab("Probability of Occupancy")+
  ggtitle("Mottd")

#' FerPy
#+ ferpy_psi_byYr
ggplot(data = psi.posteriors[psi.posteriors$Species=="FerPy",], 
       aes(x = as.character(Year), y = Psi.50, group = Route, shape = Route))+
  geom_pointrange(aes(ymin = Psi.05, ymax = Psi.95, color = Route),
                  position = position_dodge(width = .1))+
  geom_line(aes(color = Route))+
  scale_color_manual(values = c("blue", "lightblue", "green", "lightgreen", "red", "pink"))+
  scale_shape_manual(values = c(0, 16, 0, 16, 0, 16))+
  facet_wrap(~Region, nrow = 3)+
  theme_minimal()+
  xlab("Year")+
  ylab("Probability of Occupancy")+
  ggtitle("FerPy")
  
#' Specd
#+ specd_psi_byYr
ggplot(data = psi.posteriors[psi.posteriors$Species=="Specd",], 
       aes(x = as.character(Year), y = Psi.50, group = Route, shape = Route))+
  geom_pointrange(aes(ymin = Psi.05, ymax = Psi.95, color = Route),
                  position = position_dodge(width = .1))+
  geom_line(aes(color = Route))+
  scale_color_manual(values = c("blue", "lightblue", "green", "lightgreen", "red", "pink"))+
  scale_shape_manual(values = c(0, 16, 0, 16, 0, 16))+
  facet_wrap(~Region, nrow = 3)+
  theme_minimal()+
  xlab("Year")+
  ylab("Probability of Occupancy")+
  ggtitle("Specd")


#' ### By Route, averages
#'
#+ psi_means
ggplot(data = psi.means, aes(x = Route, y = Psi.50, group = Species))+
  geom_bar(stat = "identity", aes(fill = Species), position= position_dodge())+
  geom_linerange(aes(ymin = Psi.05, ymax = Psi.95), position = position_dodge(width = 0.9))+
  facet_wrap(~Region, nrow = 3,scales = "free_x")+
  ylim(c(0,1))+
  scale_fill_manual(values = c("blue", "darkgreen", "red"))+
  theme_minimal()+
  ylab("Probability of Occupancy")+
  xlab("Route")

#' _____________________________________________________________________________
#' ## p = Probability of detection
#' 
#' Probability of detection was a function of what broadcast species was used, 
#' with a constant probability of detection for all pre-broadcast time periods.
#' 
#' 
#+ p_detection, fig.width = 18
ggplot(data = p.det.posteriors, aes(y = p.det.50, x = Broadcast, group = Species))+
  geom_bar(stat = "identity", position = position_dodge(), aes(fill = Species))+
  geom_linerange(aes(ymin = p.det.05, ymax = p.det.95), position = position_dodge(0.9))+
  scale_fill_manual(values = c("blue", "darkgreen", "red"))+
  ylab("Probability of Detection")+
  xlab("Broadcast Species")+
  theme_minimal()
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
#' ezspin(file = "programs/d_graphing_results.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 