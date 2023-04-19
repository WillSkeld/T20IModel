library(coda)
library(shinystan)
library(ggplot2)
library(bayesplot)
check_hmc_diagnostics(hfitex20t)
launch_shinystan_demo()
?shinystan

shiny20t <- as.shinystan(hfitex20t)
launch_shinystan(shiny20t)

posterior_20t <- as.array(hfitex20t)
lp_20t <- log_posterior(hfitex20t)
np_20t <- nuts_params(hfitex20t)
rhat_20t <- rhat(hfitex20t, pars = c("mu_g", "sigma_g", "GME_adj[1]",
                                     "mu_o", "sigma_o", "OPP_adj[1]",
                                     "mu_s", "sigma_s", "SEA_adj[1]",
                                     "mu_p", "sigma_p", "PLY_adj[1]",
                                     "LST", "IGS"))
?rhat
ratios_20t <- neff_ratio(hfitex20t, pars = c("mu_g", "sigma_g", "GME_adj[1]",
                                             "mu_o", "sigma_o", "OPP_adj[1]",
                                             "mu_s", "sigma_s", "SEA_adj[1]",
                                             "mu_p", "sigma_p", "PLY_adj[1]",
                                             "LST", "IGS"))
print(ratios_20t)

color_scheme_set("darkgray")
mcmc_parcoord(posterior_20t, pars = c("mu_g", "sigma_g",
                                      "mu_o", "sigma_o",
                                      "mu_p", "sigma_p",
                                      "mu_s", "sigma_s"), np = np_20t)

mcmc_pairs(posterior_20t, np = np_20t, pars = c("mu_p","sigma_p","PLY_adj[1]"),
           off_diag_args = list(size = 0.75))

color_scheme_set("mix-brightblue-gray")
mcmc_trace(posterior_20t, pars = "sigma_o", np = np_20t) + 
  xlab("Post-warmup iteration")

mcmc_nuts_divergence(np_20t, lp_20t)
mcmc_nuts_energy(np_20t)

mcmc_rhat(rhat_20t) + yaxis_text(hjust = 1)
mcmc_neff(ratios_20t, size = 2) + yaxis_text(hjust = 1)

mcmc_acf(posterior_20t, pars = c("GME_adj[1]",
                                 "OPP_adj[1]",
                                  "SEA_adj[1]",
                                  "PLY_adj[1]",
                                 "LST", "IGS"), lags = 10)
