library(MASS)
library(performance)
xrpo_c1 <- xrpo_c %>% filter(innings == 1 & !is.na(venue.x) & wickets_lost_yet < 11)
kxrpo_c1
m0<- glm.nb(runs_per_over ~ offset(log(faced)) + over , data = xrpo_c1)
summary(m0)
plot(m0)
m1<- glm.nb(runs_per_over ~ over , data = xrpo_c1) 
m1
summary(m1)

#mixing - trace plots
#priors
#non-centered parameterisation
#continuous - test in freq. and check different scenarios 
#sample sizes - if run time long, total faced?, random selection of the data

m2<- glm(runs_per_over ~ gamestate + season.x + bowling_team + offset(faced), 
         data = xrpo_c1, family = quasipoisson )
summary(m2)
m3<- glm.nb(runs_per_over ~ gamestate + season.x + bowling_team + offset(faced), 
            data = xrpo_c1)
summary(m3)
compare_performance(m2,m3, rank=TRUE)
m4 <- glm.nb(runs_per_over ~ offset(log(faced)) + over + venue.x, data = xrpo_c1) 
summary(m4)

