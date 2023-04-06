library(rstan)
library("bayesplot")
library("ggplot2")
gamestate <-model.matrix(~gamestate, data = xrpoex2)[,-1]
striker <-model.matrix(~striker + 0, data = xrpoex2)
season <-model.matrix(~season.x, data = xrpoex2)[,-1]
opposition <-model.matrix(~bowling_team, data = xrpoex2)[,-1]
innings <-model.matrix(~innings, data = xrpoex2)[,-1]
wickets_lost2 <-xrpoex2$wickets_lost_yet

###SIMPLE MODEL

h_data_1 <- list(
  K = nrow(gamestate),
  P = length(unique(xrpoex2$striker)),
  G = 2,
  S = (length(unique(xrpoex2$season.x))-1),
  O = (length(unique(xrpoex2$bowling_team))-1),
  W = length(wickets_lost2),
  I = (length(unique(xrpoex2$innings))-1),
  Player = striker,
  Season = season,
  Gamestate = gamestate,
  Opposition = opposition,
  WicketsLost = wickets_lost2,
  Innings = innings,
  Runs = as.integer(xrpoex2$runs_per_over),
  faced = xrpoex2$faced
)

options(max.print=1000000)

##hfit3 <- stan(
  #file = "histan.stan",  
  #data = h_data_1,    
  #cores = 1, chains = 4, 
  #iter = 2000, warmup = 1000,
  #control = list(adapt_delta=0.95, max_treedepth=55)    
#)

hfitex2 <- stan(
  file = "histan.stan",  
  data = h_data_1,    
  cores = 1, chains = 4, 
  iter = 2000, warmup = 1000,
  control = list(adapt_delta=0.95, max_treedepth=55)    
)

traceplot(hfitex2, c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"))
mcmc_pairs(hfitex2, pars = c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"),
           off_diag_args = list(size = 0.75))
mcmc_nuts_energy(nuts_params(hfitex2))
mcmc_parcoord(hfitex2, c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"),
              np = nuts_params(hfitex2))

attributes(hfit3@sim[["samples"]][[1]])[["sampler_params"]][["n_leapfrog__"]]

###FULL MODEL

h_data_2 <- list(
  K = nrow(gamestate),
  P = length(unique(xrpoex$striker)),
  G = (length(unique(xrpoex$gamestate))-1),
  S = (length(unique(xrpoex$season.x))-1),
  O = (length(unique(xrpoex$bowling_team))-1),
  W = length(wickets_lost2),
  Player = striker,
  Season = season,
  Gamestate = gamestate,
  Opposition = opposition,
  WicketsLost = wickets_lost2,
  Runs = as.integer(xrpoex$runs_per_over),
  faced = xrpoex$faced
)

hfitopt <- stan(
  file = "optmodel.stan",  
  data = h_data_2,    
  chains = 1,             
  warmup = 1000,      
  iter = 2000,            
  cores = 1,
  control = list(adapt_delta=0.95, max_treedepth=25)             
)
