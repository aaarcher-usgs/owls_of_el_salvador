#' # Graphing Occupancy Analysis - Combined Species
#' 
#' Description: This program graphs occupancy analysis results for owls of El 
#' Salvador. Plots show the posteriors of Psi and P(detection). 
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
load(file = "data/output_data/specd_psi_posteriors_RtYr.Rdata")
load(file = "data/output_data/ferpy_psi_posteriors_RtYr.Rdata")
load(file = "data/output_data/mottd_psi_posteriors_RtYr.Rdata")

#' Psi posteriors across years by species and route
load(file = "data/output_data/specd_psi_posteriors_RtSpp.Rdata")
load(file = "data/output_data/ferpy_psi_posteriors_RtSpp.Rdata")
load(file = "data/output_data/mottd_psi_posteriors_RtSpp.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
load(file = "data/output_data/specd_p_detection_posteriors.Rdata")
load(file = "data/output_data/ferpy_p_detection_posteriors.Rdata")
load(file = "data/output_data/mottd_p_detection_posteriors.Rdata")

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' ### By Route, averages
#'
#' Add in blanks for M1 for FerPy and Specd
psi.means.specd <- rbind(psi.means.specd, 0)
psi.means.specd$Route[max(nrow(psi.means.specd))] <- "M1"
psi.means.specd$Region[max(nrow(psi.means.specd))] <- "Montecristo"
psi.means.specd$Species[max(nrow(psi.means.specd))] <- "Specd"

psi.means.ferpy <- rbind(psi.means.ferpy, 0)
psi.means.ferpy$Route[max(nrow(psi.means.ferpy))] <- "M1"
psi.means.ferpy$Region[max(nrow(psi.means.ferpy))] <- "Montecristo"
psi.means.ferpy$Species[max(nrow(psi.means.ferpy))] <- "FerPy"
#'
#' Merge data
psi.means <- rbind(psi.means.ferpy, psi.means.specd, psi.means.mottd)

#'
#+ psi_means
ggplot(data = psi.means, aes(x = Route, y = Psi.median, group = Species))+
  geom_bar(stat = "identity", aes(fill = Species), position= position_dodge())+
  geom_linerange(aes(ymin = Psi.LL05, ymax = Psi.UL95), position = position_dodge(width = 0.9))+
  facet_wrap(~Region, nrow = 3,scales = "free_x")+
  ylim(c(0,1))+
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086"))+
  theme_minimal()+
  ylab("Probability of Occupancy")+
  xlab("Route")

#' _____________________________________________________________________________
#' ## p = Probability of detection
#' 
#' Merge data
p.det.post <- rbind(p.det.post.ferpy, p.det.post.mottd, p.det.post.specd)
#' Probability of detection was a function of what broadcast species was used, 
#' with a constant probability of detection for all pre-broadcast time periods.
#' 
#' 
#+ p_detection, fig.height = 8
ggplot(data = p.det.post, aes(y = median.plogis, x = Broadcast, group = Species))+
  geom_bar(stat = "identity", position = position_dodge(), aes(fill = Species))+
  geom_linerange(aes(ymin = LL05.plogis, ymax = UL95.plogis), position = position_dodge(0.9))+
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086"))+
  geom_hline(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",], 
             aes(yintercept = median.plogis))+
  geom_hline(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",], 
             aes(yintercept = LL05.plogis), color = "#ffff99")+
  geom_hline(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",], 
             aes(yintercept = UL95.plogis), color = "#ffff99")+
  facet_wrap(~Species, nrow = 3)+
  ylab("Probability of Detection")+
  xlab("Broadcast Species")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x  = element_blank())

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
#' ezknitr::ezspin(file = "programs/g_graphing_results_global.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 