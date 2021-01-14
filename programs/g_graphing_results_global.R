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
load(file = "data/plotting_data/richness_psi_posteriors_RtYr.Rdata")

#' Psi posteriors across years by species and route
load(file = "data/output_data/specd_psi_posteriors_RtSpp.Rdata")
load(file = "data/output_data/ferpy_psi_posteriors_RtSpp.Rdata")
load(file = "data/output_data/mottd_psi_posteriors_RtSpp.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
load(file = "data/output_data/specd_p_detection_posteriors.Rdata")
load(file = "data/output_data/ferpy_p_detection_posteriors.Rdata")
load(file = "data/output_data/mottd_p_detection_posteriors.Rdata")
load(file = "data/plotting_data/richness_p_detection_posteriors.Rdata")

#' Species accounts
#' 
load(file = "data/output_data/richness_species_accounts.Rdata")

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
psi.means$code <- ifelse(psi.means$Species == "FerPy", "FEPO", 
                         ifelse(psi.means$Species == "Mottd", "MOOW", "SPEO"))

#' Last minute fix to route names
#' 
table(psi.means$Route)
psi.means$Route <- ifelse(psi.means$Route == "EI1", "EI-1",
                          ifelse(psi.means$Route == "EI2", "EI-2",
                                 ifelse(psi.means$Route == "M1", "M-1",
                                        ifelse(psi.means$Route == "M2", "M-2",
                                               ifelse(psi.means$Route == "N1", "N-1",
                                                      "N-2")))))

#'
#+ psi_means, fig.width = 2.8346, dpi = 600, fig.height = 4.5
ggplot(data = psi.means, aes(x = Route, y = Psi.median, group = code))+
  geom_bar(stat = "identity", aes(fill = code), position= position_dodge())+
  geom_linerange(aes(ymin = Psi.LL05, ymax = Psi.UL95), position = position_dodge(width = 0.9))+
  facet_wrap(~Route, nrow = 3,scales = "free_x")+
  ylim(c(0,1))+
  scale_fill_manual(values = c("#cccccc", "#969696", "#525252"))+
  theme_minimal()+
  ylab("Probability of Occupancy")+
  xlab("Route")+
  theme(legend.position = "bottom")+
  theme(legend.text=element_text(size=6), 
        legend.title = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size=8),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 6),
        legend.key.size = unit(1, "line"))

#' 
#' ### By Route, and by year
#'
#' Merge psi posts
#' 
psi.post.all <- rbind(psi.post.ferpy, psi.post.mottd, psi.post.specd)
psi.post.all <- rbind(psi.post.all, 0)
psi.post.all$Year[psi.post.all$Psi.mean == 0] <- 2000
psi.post.all$Species[psi.post.all$Psi.mean == 0] <- "FerPy"
psi.post.all$Route[psi.post.all$Psi.mean == 0] <- "M.1"
psi.post.all <- rbind(psi.post.all, 0.00001)
psi.post.all$Year[psi.post.all$Psi.mean == 0.00001] <- 2000
psi.post.all$Species[psi.post.all$Psi.mean == 0.00001] <- "Specd"
psi.post.all$Route[psi.post.all$Psi.mean == 0.00001] <- "M.1"

psi.post.all$code <- ifelse(psi.post.all$Species == "FerPy", "FEPO", 
                            ifelse(psi.post.all$Species == "Mottd", "MOOW", "SPEO"))
#'
#' Last minute fix for Route names
psi.post.all$Route <- ifelse(psi.post.all$Route == "EI.1", "EI-1",
                          ifelse(psi.post.all$Route == "EI.2", "EI-2",
                                 ifelse(psi.post.all$Route == "M.1", "M-1",
                                        ifelse(psi.post.all$Route == "M.2", "M-2",
                                               ifelse(psi.post.all$Route == "N.1", "N-1",
                                                      "N-2")))))
table(psi.post.all$Route)
#'
#+ psi_byYr, fig.width = 5.67, dpi = 600, fig.height = 4.6
ggplot(data = psi.post.all, 
       aes(x = Year, y = Psi.median))+
  geom_linerange(aes(ymin = Psi.LL05, ymax = Psi.UL95), color = "#b5b5b5")+
  geom_line()+
  geom_point()+
  #facet_grid(code~Route)+
  facet_grid(Route~code)+
  theme_minimal()+
  xlab("Year")+
  ylab("Probability of Occupancy")+
  scale_x_continuous(breaks = 2003:2013, limits = c(2003,2013))+
  theme(axis.text.x = element_text(size=6, angle = 90),
        axis.text.y = element_text(size=6),
        axis.title = element_text(size=8),
        #axis.title.x = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA, color = "#969696"),
        strip.text = element_text(size =6, margin = margin(0,0.1,0.1,0.1, "cm")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


#' _____________________________________________________________________________
#' ## p = Probability of detection
#' 
#' Merge data
p.det.post.richness$Species <- "All Species"
#' 
p.det.post <- rbind(p.det.post.ferpy, p.det.post.mottd, 
                    p.det.post.specd,p.det.post.richness)
table(p.det.post$Broadcast)

table(p.det.post$Species)
p.det.post$Species[p.det.post$Species == "FerPy"] <- "FEPO"
p.det.post$Species[p.det.post$Species == "Mottd"] <- "MOOW"
p.det.post$Species[p.det.post$Species == "Specd"] <- "SPEO"



#' 
#' 
#' Probability of detection was a function of what broadcast species was used, 
#' with a constant probability of detection for all pre-broadcast time periods.
#' 
#' 
#+ p_detection, fig.width = 2.8346, dpi = 600, fig.height = 3
ggplot(data = p.det.post, aes(y = median.plogis, x = Broadcast, group = Species))+
  #geom_bar(stat = "identity", position = position_dodge(), 
  #         color = "#dddddd", fill = "#dddddd")+
  
  scale_fill_manual(values = c("darkgrey", "#7fc97f", "#beaed4", "#fdc086"))+
  #geom_hline(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",], 
  #           aes(yintercept = median.plogis), color = "#777777")+
  #geom_rect(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",],
  #            aes(ymin = LL05.plogis, max = UL95.plogis, xmin = -Inf, xmax = Inf), fill = "red")+
  geom_hline(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",], 
             aes(yintercept = LL05.plogis), color = "#999999")+
  geom_hline(data = p.det.post[p.det.post$broadcast.param == "beta.prebroad",], 
             aes(yintercept = UL95.plogis), color = "#999999")+
  geom_point(size = 0.5)+
  geom_linerange(aes(ymin = LL05.plogis, ymax = UL95.plogis), 
                 position = position_dodge(0.9))+
  facet_wrap(~Species, nrow = 2)+
  ylab("Probability of Detection")+
  xlab("Broadcast Species")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        legend.position = "none",
        axis.text.y = element_text(size=6),
        axis.title = element_text(size=6),
        panel.border = element_rect(linetype = "solid", fill = NA, color = "#969696"),
        strip.text = element_text(size =6, margin = margin(0,0.1,0.1,0.1, "cm")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


#' _____________________________________________________________________________
#' ## Richness by Route and Year
#' 
#' Last minute fix for Route names
table(richness.RtYr.post$Route)
richness.RtYr.post$Route <- ifelse(richness.RtYr.post$Route == "EI1", "EI-1",
                           ifelse(richness.RtYr.post$Route == "EI2", "EI-2",
                                  ifelse(richness.RtYr.post$Route == "M1", "M-1",
                                         ifelse(richness.RtYr.post$Route == "M2", "M-2",
                                                ifelse(richness.RtYr.post$Route == "N1", "N-1",
                                                       "N-2")))))
table(richness.RtYr.post$Route)
#' 
#' All 6 routes
#+ richness_byRtYr, fig.width = 5.67, dpi = 600, fig.height = 4.4
ggplot(data = richness.RtYr.post, 
       aes(x = Year, y = Richness.median, group = Route))+
  geom_ribbon(aes(ymax = richness.detected, ymin = 0), fill = "#cccccc")+
  geom_ribbon(aes(ymin = Richness.median, 
                  ymax = Richness.UL95), fill = "#999999")+
  geom_ribbon(aes(ymin = richness.detected, 
                  ymax = Richness.median), fill = "#999999")+
  geom_pointrange(aes(ymin = Richness.LL05, ymax = Richness.UL95))+
  geom_line()+
  facet_wrap(~Route, nrow = 3)+
  theme_minimal()+
  xlab("Year")+
  ylab("Species Richness")+
  scale_x_continuous(breaks = 2003:2013)+
  scale_y_continuous(breaks = seq(0,14,by=2), limits = c(0,6))+
  theme(axis.text.x = element_text(size=6, angle = 90),
        axis.text.y = element_text(size=6),
        axis.title = element_text(size=8),
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, color = "#969696"),
        strip.text = element_text(size =6, margin = margin(0,0.1,0.1,0.1, "cm")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())



#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezknitr::ezspin(file = "programs/g_graphing_results_global.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 