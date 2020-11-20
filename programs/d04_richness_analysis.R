#' # Analyzing Species Richness for each Route and Year
#' 
#' Description: This program uses a Bayesian species richness model and data 
#' augmentation to analyze
#' species richness of each route and year. 
#' 
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
library(R2jags)

#' Clear environment and set seed
#' 
#' *Note: for reproducibility, it is important to include these. Clearing the
#' environment ensures that you have specified all pertinent files that need
#' to be loaded, and setting the seed ensures that your analysis is 
#' repeatable*
remove(list = ls())
set.seed(1770562)

#' _____________________________________________________________________________
#' ## Load Data
#' 
#' 
#' Processed Owl Data
load(file = "data/processed_data/augmented_jags_ys.Rdata")


#' Processed Support data
load(file = "data/processed_data/augmented_jags_ks.Rdata")

#' Access tables, in Rdata format
#' 
load(file = "data/processed_data/tables_global.Rdata")

#' Supplemental JAGS table
#' 
load(file = "data/processed_data/data_jags_global.Rdata")


#' _____________________________________________________________________________
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


#' **Augmenting species number**
#' 
#' There were 10 observed owls (if we pool all three unknown observations)
(species.names <- unique(tab.owls$Owl_Species_ID))
n.species <- length(species.names)

#' We should augment with M >> n, where n = 10
#' 
#' Owls of El Salvador lists 13 total species recorded in El Salvador (Table 11.1, p401)
#' 
n.aug <- 5 #number of species that may have been present but were undetected
n.species.aug <- n.species + n.aug




#' _____________________________________________________________________________
#' ## Read model
#' 
source(file = "programs/b_richness_model_global.R")


#' _____________________________________________________________________________
#' ## Compile data for Mottd model
#' 
richness.jags.data <- list(
  ks.prebroad = ks.prebroad,
  ks.pacific = ks.pacific,
  ks.mottled = ks.mottled,
  ks.crested = ks.crested,
  ks.bw = ks.bw,
  ks.spectacled = ks.spectacled,
  ks.whiskered = ks.whiskered,
  ks.gbarred = ks.gbarred,
  ks.stygian = ks.stygian,
  ks.ghorned = ks.ghorned,
  y = ys.aug,
  n.route = n.route,
  n.year = n.year,
  n.survey = n.survey,
  n.station = n.station,
  n.broadcast = n.broadcast,
  n.species.aug = n.species.aug
)

#' Initial values have 1s for each obs of z
z.init <- array(NA, 
                dim = c(n.route, n.year, n.species.aug, n.survey))
w.init <- array(NA,
                dim = c(n.route, n.species.aug))

for(hh in 1:n.route){
  for(tt in 1:n.year){
    for(ss in 1:n.species.aug){

      
      for(ii in 1:n.survey){
        if(sum(is.na(ys.aug[hh,tt,ss,ii,,])==FALSE)==0){ # no survey conducted here
          z.init[hh,tt,ss,ii] <- 0
        }else {
          z.init[hh,tt,ss,ii] <- max(ys.aug[hh,tt,ss,ii,,], na.rm = T)
        } 
      }
    }
  }
}
for(hh in 1:n.route){
    for(ss in 1:n.species){
      w.init[hh,ss] <- max(z.init[hh,,ss,], na.rm = T)
    }
    for(ss in (n.species+1):n.species.aug){
      w.init[hh,ss] <- 0
    }
}

#' Check initials (should be summary of ys)
#'
#' 
#' Owl names correspond with 1:10 rows (z) or 1:10 cols (w)
species.names
#' 
w.init
#' 
z.init[4,3,,] # rows are different species, M2, 2005
z.init[3,9,,] # rows are different species, M1, 2011
z.init[5,6,,] # rows are different species, N1, 2008


#' ## Run models
#' 
#' Richness, 
#+ jags, eval = FALSE
richness.jagsout <- jags(data = richness.jags.data, 
                      inits = function(){list(z = z.init, w = w.init)}, 
                      parameters.to.save = c("mu.psi",
                                             "rho.psi",
                                             "omega",
                                             "spp.occ",
                                             "richness",
                                             "richness.RtYr",
                                             "beta.prebroad",
                                             "beta.pacific",
                                             "beta.mottled",
                                             "beta.crested",
                                             "beta.bw",
                                             "beta.spectacled",
                                             "beta.whiskered",
                                             "beta.gbarred",
                                             "beta.stygian",
                                             "beta.ghorned",
                                             "species.present"), 
                      model.file = model.richness, 
                      n.chains = 3,
                      n.iter = 20000, #10000
                      n.burnin = 2000, #1000
                      n.thin = 2)

#' _____________________________________________________________________________
#' ## Psi = Probability of occupancy
#' 
#' Create traceplots of parameters (creates PDF)
#'
#' Richness
# MCMCtrace(richness.jagsout,
#           pdf = TRUE,
#           open_pdf = FALSE,
#           filename = "richnessTrace",
#           wd = "output")


#' ## Posteriors for saving
#' 
#' Mu(psi)
#' 
mu.psi.post <- MCMCsummary(richness.jagsout, 
                             params = "mu.psi", 
                             Rhat = TRUE,
                             n.eff = TRUE,
                             probs = c(0.05, 0.5, 0.95))
mu.psi.chains <- MCMCchains(richness.jagsout, 
                            params = "mu.psi")

#' Rho(psi)
#' 
rho.psi.post <- MCMCsummary(richness.jagsout, 
                           params = "rho.psi", 
                           Rhat = TRUE,
                           n.eff = TRUE,
                           probs = c(0.05, 0.5, 0.95))
rho.psi.chains <- MCMCchains(richness.jagsout, 
                            params = "rho.psi")


#' Omega Post
#' 
omega.post <- MCMCsummary(richness.jagsout, 
                          params = "omega", 
                          Rhat = TRUE,
                          n.eff = TRUE,
                          probs = c(0.05, 0.5, 0.95))
omega.chains <- MCMCchains(richness.jagsout, 
                             params = "rho.psi")

#' Species Occupancy Post
#' 
spp.occ.post <- MCMCsummary(richness.jagsout, 
                          params = "spp.occ", 
                          Rhat = TRUE,
                          n.eff = TRUE,
                          probs = c(0.05, 0.5, 0.95))
spp.occ.chains <- MCMCchains(richness.jagsout, 
                              params = "spp.occ")

#' Richness
#' 
richness.post <- MCMCsummary(richness.jagsout, 
                             params = "richness", 
                             Rhat = TRUE,
                             n.eff = TRUE,
                             probs = c(0.05, 0.5, 0.95))
richness.chains <- MCMCchains(richness.jagsout, 
                            params = "richness")

#' Richness by route/year
#' 
richness.RtYr.post <- MCMCsummary(richness.jagsout, 
                             params = "richness.RtYr", 
                             Rhat = TRUE,
                             n.eff = TRUE,
                             probs = c(0.05, 0.5, 0.95))
richness.RtYr.chains <- MCMCchains(richness.jagsout, 
                              params = "richness.RtYr")

#' Probability of detection was a function of broadcast species
#' 
#' 
p.det.post.richness <- MCMCsummary(richness.jagsout, 
                                   params = grep("^beta", 
                                                 richness.jagsout$parameters.to.save, 
                                                 value = T), 
                                   Rhat = TRUE,
                                   n.eff = TRUE,
                                   probs = c(0.05, 0.5, 0.95))
p.det.post.richness$broadcast.param <- grep("^beta", 
                                            richness.jagsout$parameters.to.save, 
                                            value = T)
p.det.post.richness
p.det.chains <- MCMCchains(richness.jagsout, 
                                   params = grep("^beta", 
                                                 richness.jagsout$parameters.to.save, 
                                                 value = T))

#' Species presence by route/year
#' 
species.present.post <- MCMCsummary(richness.jagsout, 
                                  params = "species.present", 
                                  Rhat = TRUE,
                                  n.eff = TRUE,
                                  probs = c(0.05, 0.5, 0.95))
species.present.chains <- MCMCchains(richness.jagsout, 
                                   params = "species.present")



#' _____________________________________________________________________________
#' ## Save files
#' 
#' Jagsout Mottd
#+ savejags, eval = F
#save(richness.post, file = "data/output_data/richness_jagsout_posteriors.Rdata")
#' Richness Posteriors 
# save(richness.post, richness.chains, 
#      file = "data/output_data/richness_psi_posteriors.Rdata")
# save(richness.RtYr.chains, richness.RtYr.post,
#      file = "data/output_data/richness_psi_posteriors_RtYr.Rdata")

#' Save posteriors
#' 
#' Mu(psi)
# save(mu.psi.post, mu.psi.chains, 
#      file = "data/output_data/richness_mu_psi_posteriors.Rdata")

#' Rho(psi)
#' 
# save(rho.psi.chains, rho.psi.post, 
#      file = "data/output_data/richness_rho_psi_posteriors.Rdata")

#' Omega
#' #' 
#' save(omega.chains, omega.post, 
#'      file = "data/output_data/richness_omega_posteriors.Rdata")

#' Spp.occ
#' #' 
# save(spp.occ.post, spp.occ.chains, 
#      file = "data/output_data/richness_spp_occ_posteriors.Rdata")

#' Probability of detection by broadcast species and species of analysis
#' 
# save(p.det.post.richness, p.det.chains,
#      file = "data/output_data/richness_p_detection_posteriors.Rdata")

#' Species present
#' 
# save(species.present.chains, species.present.post, 
#     file = "data/output_data/richness_species_present_posteriors.Rdata")




#' Species Accounts
#' 
save(species.accounts, file = "data/output_data/richness_species_accounts.Rdata")


#' _____________________________________________________________________________
#' ### Footer
#' 
#' Session Info
devtools::session_info()
#' This document was "spun" with:
#' 
#' ezspin(file = "programs/d04_richness_analysis.R", out_dir = "output", fig_dir = "figures", keep_md = F)
#' 