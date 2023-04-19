library(rstan)
library(dplyr)
library("bayesplot")
library("ggplot2")
xcsData <- xrpo_c %>% filter(wickets_lost_yet < 11)
#2017 2014

xcsDataLite01 <- xcsData %>% filter((striker == 'EJG Morgan' |
                                                           striker == 'SA Yadav' |
                                                           striker == 'V Kohli' |
                                                           striker == 'Babar Azam' |
                                                           striker == 'AJ Finch' |
                                                           striker == 'DA Warner' |
                                                           striker == 'CH Gayle' |
                                                           striker == 'Mohammad Rizwan' |
                                                           striker == 'DP Conway' |
                                                           striker == 'SR Watson'|
                                        striker == 'GJ Maxwell' |
                                       striker == 'Fakhar Zaman' |
                                       striker == 'TM Dilshan' |
                                       striker == 'BB McCullum' |
                                       striker == 'AD Hales' |
                                       striker == 'F du Plessis' |
                                       striker == 'KL Rahul' |
                                       striker == 'DJ Malan' |
                                       striker == 'C Munro' |
                                       striker == 'KP Pietersen'
                                                          ))

xcsDataLite01 <- xcsDataLite01 %>% mutate(gamestate = case_when(over< 7 ~ 'pp', 
                                                                over<15 ~ 'regular', 
                                                                over>14 ~ 'death'))

xcsDataLite01 <- xcsDataLite01 %>% mutate(era = case_when(season.x == "2023" | season.x == "2022/23"| 
                                                          season.x == "2022" | season.x == "2021/22" ~ '2022-23',
                                                          season.x == "2021" | season.x == "2020/21"| 
                                                          season.x == "2020" | season.x == "2019/20" ~ '2020-21',
                                                          season.x == "2019" | season.x == "2018/19"| 
                                                          season.x == "2018" | season.x == "2017/18" ~ '2018-19',
                                                          season.x == "2017" | season.x == "2016/17"| 
                                                          season.x == "2016" | season.x == "2015/16" ~ '2016-17',
                                                          season.x == "2015" | season.x == "2014/15"| 
                                                          season.x == "2014" | season.x == "2013/14" ~ '2014-15',
                                                          season.x == "2013" | season.x == "2012/13"| 
                                                          season.x == "2012" | season.x == "2011/12" ~ '2012-13',
                                                          season.x == "2011" | season.x == "2010/11"| 
                                                          season.x == "2010" | season.x == "2009/10" ~ '2010-11',
                                                          season.x == "2009" | season.x == "2008/09"| 
                                                          season.x == "2008" | season.x == "2007/08" ~ '2008-09',
                                                          season.x == "2007" | season.x == "2006/07"| 
                                                          season.x == "2006" | season.x == "2005/06" ~ '2006-07',
                                                          season.x == "2005" | season.x == "2004/05"| 
                                                          season.x == "2004" | season.x == "2003/04" ~ '2004-05',
                                                          ))
xcsDataLite01 <- xcsDataLite01 %>% filter(!is.na(era))
playerlist <- unique(xcsDataLite01$striker)
xcsDataLite01 <- xcsDataLite01 %>% filter(striker %in% playerlist)
xcsDataLite01$gamestate <- factor(xcsDataLite01$gamestate)
xcsDataLite01$gamestate <- relevel(xcsDataLite01$gamestate, "regular")
gamestate <-model.matrix(~gamestate, data = xcsDataLite01)[,-1]

striker <-model.matrix(~striker + 0, data = xcsDataLite01)
xcsDataLite01$era <- factor(xcsDataLite01$era)
xcsDataLite01$era <- relevel(xcsDataLite01$era, "2022-23")
season <-model.matrix(~era, data = xcsDataLite01)[,-1]
xcsDataLite01$bowling_team <- factor(xcsDataLite01$bowling_team)
xcsDataLite01$bowling_team <- relevel(xcsDataLite01$bowling_team, "England")
opposition <-model.matrix(~bowling_team, data = xcsDataLite01)[,-1]
innings <-model.matrix(~innings, data = xcsDataLite01)[,-1]
wickets_lost2 <-xcsDataLite01$wickets_lost_yet

###SIMPLE MODEL

h_data_1 <- list(
  K = nrow(gamestate),
  P = length(unique(xcsDataLite01$striker)),
  G = 2,
  S = (length(unique(xcsDataLite01$era))-1),
  O = (length(unique(xcsDataLite01$bowling_team))-1),
  W = length(wickets_lost2),
  I = (length(unique(xcsDataLite01$innings))-1),
  Player = striker,
  Season = season,
  Gamestate = gamestate,
  Opposition = opposition,
  WicketsLost = wickets_lost2,
  Innings = innings,
  Runs = as.integer(xcsDataLite01$runs_per_over),
  faced = xcsDataLite01$faced
)

options(max.print=1000000)

##hfit3 <- stan(
  #file = "histan.stan",  
  #data = h_data_1,    
  #cores = 1, chains = 4, 
  #iter = 2000, warmup = 1000,
  #control = list(adapt_delta=0.95, max_treedepth=55)    
#)
#hfitex21s - big boi
#hfitex20 - 20 players all eras - 910 seconds
#hfitex50 - 10 players all eras - 345 seconds
#hfitex30 - 20 players all eras - 917 seconds


hfitex20treps <- stan(
  file = "histan.stan",  
  data = h_data_1,    
  cores = 1, chains = 1, 
  iter = 200, warmup = 100,
  control = list(adapt_delta=0.999, max_treedepth=55)    
)

xhfitex20f <- stan(
  file = "histan.stan",  
  data = h_data_1,    
  cores = 1, chains = 2, 
  iter = 25000, warmup = 5000,
  control = list(adapt_delta=0.95, max_treedepth=55)    
)

traceplot(hfitex20s, c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"))
mcmc_pairs(hfitex20s, pars = c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"),
           off_diag_args = list(size = 0.75))
mcmc_nuts_energy(nuts_params(hfitex20s))
mcmc_parcoord(hfitex20s, c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"),
              np = nuts_params(hfitex20s))

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
  warmup = 5000,      
  iter = 20000,            
  cores = 1,
  control = list(adapt_delta=0.95, max_treedepth=25)             
)
