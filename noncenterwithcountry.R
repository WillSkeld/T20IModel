library(rstan)
library(dplyr)
library("bayesplot")
library("ggplot2")
xccData <- xrpo_c %>% filter(wickets_lost_yet < 11)
xccDataLite <- xccData %>% filter((striker == "JC Buttler" | striker == "DA Miller") & 
                               (season.x == "2021" | season.x == "2021/22" |
                                  season.x == "2022/23"))
xccDataLite <- xccDataLite %>% mutate(gamestate = case_when(over< 7 ~ 'pp', 
                                                    over<15 ~ 'regular', 
                                                    over>14 ~ 'death'))

xccDataLite$striker <- factor(xccDataLite$striker)
xccDataLite$season.x <- factor(xccDataLite$season.x)
xccDataLite$bowling_team <- factor(xccDataLite$bowling_team)
xccDataLite$innings <- factor(xccDataLite$innings)
xccDataLite$country <- factor(xccDataLite$country)
  
  
gamestate <-model.matrix(~gamestate, data = xccDataLite)[,-1]
striker <-model.matrix(~striker + 0, data = xccDataLite)
season <-model.matrix(~season.x, data = xccDataLite)[,-1]
opposition <-model.matrix(~bowling_team, data = xccDataLite)[,-1]
innings <-model.matrix(~innings, data = xccDataLite)[,-1]
country <- model.matrix(~country, data = xccDataLite)[,-1]
wickets_lost2 <-xccDataLite$wickets_lost_yet


xcc_lite_setup <- list(
  K = nrow(gamestate),
  P = length(unique(xccDataLite$striker)),
  G = 2,
  S = (length(unique(xccDataLite$season.x))-1),
  O = (length(unique(xccDataLite$bowling_team))-1),
  W = length(wickets_lost2),
  I = (length(unique(xccDataLite$innings))-1),
  C = (length(unique(xccDataLite$country))-1),
  Player = striker,
  Season = season,
  Country = country,
  Gamestate = gamestate,
  Opposition = opposition,
  WicketsLost = wickets_lost2,
  Innings = innings,
  Runs = as.integer(xccDataLite$runs_per_over),
  faced = xccDataLite$faced
)

xccLitefit <- stan(
  file = "modelxccLite.stan",  
  data = xcc_lite_setup,    
  cores = 2, chains = 1, 
  iter = 1100, warmup = 1000,
  control = list(adapt_delta=0.95, max_treedepth=55)    
)
