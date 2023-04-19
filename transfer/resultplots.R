library(invgamma)
library(gridExtra)
library(tidyverse)
library(ggpubr)
#distribution plots
p1 <- ggplot(data = data.frame(x = c(-1, 1.5)), aes(x)) +
  stat_function(fun = dnorm, n = 1001, args = list(mean = log(7.5/6), sd = 0.5), linetype = "dashed") + ylab("Density") +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0.33, sd = 0.07)) + xlab("mu_pi") 
  
p1

p2 <- ggplot(data = data.frame(x = c(0, 0.75)), aes(x)) +
  stat_function(fun = dinvgamma, n = 1001, args = list(shape = 3, scale = 1), linetype = "dashed") + ylab("Denisty") +
  stat_function(fun = dnorm, n = 1001, args = list(mean = 0.05, sd = 0.01)) + xlab("sigma_pi")
grid.arrange(p1,p2, ncol=2, top = "Player Population Parameter Distibutions")


postSamples <- extract(hfitex20s, pars = c("GME_adj[1]","GME_adj[2]"))

combined <- bind_rows(list(gme1 = data.frame(postSamples$`GME_adj[1]`),
                           gme2 = data.frame(postSamples$`GME_adj[2]`)),
                      .id = "Source")

ggboxplot(combined, x = "Source", y = )
#box plots - gamestate
#box plots - gamestate
p3 <- mcmc_areas(hfitex20t, pars=c("GME_adj[1]", "GME_adj[2]"), prob = 0.5, 
                 transformations = "exp") + xlab("Multiplicative Game-State Effect") +
  ylab("Density") + geom_vline(xintercept = 1, linetype = "dashed")+  
  ggtitle("Transformed Game-State Posterior Distributions") +  
  scale_y_discrete(labels = c("exp(GME_adj[1])" = "Death Overs", "exp(GME_adj[2])" = "Powerplay Overs"))
p3


exp(-.1)
11.3 9.6 13.1

mcmc_areas_ridges(hfitex20t, pars=c("mu_g", "GME_adj[1]", "GME_adj[2]"), prob = 0.9,prob_outer = 0.9, transformations = "exp") + xlab("Game-Sate Effect") +
  ylab("Density") + ggtitle("Transformed Game-State Posterior Distributions")


#season comparison

p4 <- mcmc_areas(hfitex20t, pars=c("SEA_adj[1]","SEA_adj[2]", "SEA_adj[3]", "SEA_adj[4]", "SEA_adj[5]",
                                   "SEA_adj[6]", "SEA_adj[7]", "SEA_adj[8]", "SEA_adj[9]"), prob = 0.5, 
                 prob_outer = 0.95, transformations = "exp", point_est = "mean") 

p4 <- mcmc_intervals(hfitex20t, regex_pars="SEA_adj", prob = 0.5, 
                     prob_outer = 0.9, transformations = "exp", point_est = "mean") + xlab("Multiplicative Era Effect") +
  scale_y_discrete(labels = c("exp(SEA_adj[2])" = "2006/07", "exp(SEA_adj[3])" = "2008/09", 
                              "exp(SEA_adj[4])" = "2010/11", "exp(SEA_adj[5])" = "2012/13",
                              "exp(SEA_adj[6])" = "2014/15", "exp(SEA_adj[7])" = "2016/17", 
                              "exp(SEA_adj[8])" = "2018/19", "exp(SEA_adj[9])" = "2020/21", 
                              "exp(SEA_adj[1])" = "2004/05")) + geom_vline(xintercept = 1, linetype = "dashed") + 
  ggtitle("Interval Plot for Posterior Season Effects")
p4
#opp by opp
head(opposition)
p5 <- mcmc_areas_ridges(hfitex20t, pars="mu_o", regex_pars = "OPP_adj", prob = 0.5, 
                        prob_outer = 0.99, transformation = "exp") + 
  scale_y_discrete(labels = c( "exp(mu_o)" = "mu_theta", "exp(OPP_adj[2])" = "Australia", "exp(OPP_adj[3])" = "Bangladesh", 
                               "exp(OPP_adj[1])" = "Afghanistan", "exp(OPP_adj[4])" = "India",
                               "exp(OPP_adj[5])" = "New Zealand", "exp(OPP_adj[6])" = "Pakistan", 
                               "exp(OPP_adj[7])" = "South Africa", "exp(OPP_adj[8])" = "Sri Lanks", 
                               "exp(OPP_adj[10])" = "Zimbabwe", "exp(OPP_adj[9])" = "West Indies"), limits = rev) + 
  geom_vline(xintercept = 1, linetype = "dashed") + ggtitle("Density Plot of Transformed Posterior Opposition Effects") +
  xlab("Multiplicative Effect")
p5
exp(-0.05*8)
