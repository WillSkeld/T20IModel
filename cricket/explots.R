library(posterior)
plot(hfitex2, pars = c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"))
head(opposition)
#1-5 - MILLER, coway, uttler, rizzzz, kohli

plot(hfitex2, pars = c("OPP_adj[5]", "OPP_adj[1]",  "OPP_adj[3]", "OPP_adj[4]"),
     show_density = TRUE, ci_level = 0.5, fill_color = "red") + 
  ggtitle("Estimated Posterior Distributions")

plot(hfitex2, plotfun = "trace", 
     pars = c("PLY_adj[1]", "PLY_adj[2]","PLY_adj[3]", "PLY_adj[4]", "PLY_adj[5]"),
     inc_warmup = TRUE)

#2021, Australia, death overs
exp(.51 -0.19 + 0.14)*6  #9.5rpo
exp(.40 -0.19 + 0.14)*6  #8.5rpo
exp(.67 -0.19 + 0.14)*6  #11.2rpo
exp(.44 -0.19 + 0.14)*6  #8.9rpo
exp(.47 -0.19 + 0.14)*6  #9.1rpo

exp(.51-0.04)/exp(.51)
#Kiwis - 0.7 rpo harder than Afghan

1-exp(-0.19)
