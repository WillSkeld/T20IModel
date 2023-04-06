library(AER)
poisson
xrpo_c1 <- xrpo_c %>% filter(innings == 1 & !is.na(venue.x) & wickets_lost_yet < 11)

p0<- glm(runs_per_over ~ offset(log(faced)) + over ,family = poisson, data = xrpo_c1)
summary(p0)

65211/31493

dispersiontest(p0,trafo=2)
?dispersiontest

mean(xrpo_c1$runs_per_over)
var(xrpo_c1$runs_per_over)
