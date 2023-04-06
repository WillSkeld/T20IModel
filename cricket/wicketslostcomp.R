library(MASS)
library(performance)

xrpo_c1 <- xrpo_c %>% filter(innings == 1 & !is.na(venue.x) & wickets_lost_yet < 11)
m0<- glm.nb(runs_per_over ~ offset(log(faced)) + gamestate + season.x + 
              striker + bowling_team + wickets_lost_yet, data = xrpo_c1)
summary(m0)
xrpo_c1$wickets_lost_yet <- factor(xrpo_c1$wickets_lost_yet)

m1<- glm.nb(runs_per_over ~ offset(log(faced)) + gamestate + season.x + 
              striker + bowling_team + wickets_lost_yet + innings, data = xrpo_c3)
summary(m1)

compare_performance(m0,m1, rank = TRUE)



