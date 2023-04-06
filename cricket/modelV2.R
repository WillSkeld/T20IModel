library(rstan)
library(dplyr)
library("bayesplot")
library("ggplot2")
v2Data <- xrpo_c %>% filter(!is.na(venue.x) & !is.na(city) & wickets_lost_yet < 11)
v2Test1 <- v2Data %>% filter((striker == "JC Buttler" | striker == "DA Miller") & 
                               (season.x == "2021" | season.x == "2021/22" |
                                season.x == "2022/23"))
v2Test1 <- v2Test1 %>% mutate(gamestate = case_when(over< 7 ~ 'pp', 
                                                    over<15 ~ 'regular', 
                                                    over>14 ~ 'death'))
v2Data$city <- factor(v2Data$city)


gamestate <-model.matrix(~gamestate, data = v2Test1)[,-1]
striker <-model.matrix(~striker + 0, data = v2Test1)
season <-model.matrix(~season.x, data = v2Test1)[,-1]
opposition <-model.matrix(~bowling_team, data = v2Test1)[,-1]
innings <-model.matrix(~innings, data = v2Test1)[,-1]
city <- model.matrix(~city, data = v2Test1)[,-1]
wickets_lost2 <-v2Test1$wickets_lost_yet

v2_setup <- list(
  K = nrow(gamestate),
  P = length(unique(v2Test1$striker)),
  G = 2,
  S = (length(unique(v2Test1$season.x))-1),
  O = (length(unique(v2Test1$bowling_team))-1),
  W = length(wickets_lost2),
  I = (length(unique(v2Test1$innings))-1),
  C = (length(unique(v2Test1$city))-1),
  Player = striker,
  Season = season,
  City = city,
  Gamestate = gamestate,
  Opposition = opposition,
  WicketsLost = wickets_lost2,
  Innings = innings,
  Runs = as.integer(v2Test1$runs_per_over),
  faced = v2Test1$faced
)

v2fit <- stan(
  file = "modelv2.stan",  
  data = v2_setup,    
  cores = 1, chains = 1, 
  iter = 1100, warmup = 1000,
  control = list(adapt_delta=0.95, max_treedepth=55)    
)
