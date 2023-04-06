library(performance)
tester0 <- brm(runs_per_over ~ 0 + offset(log(faced)) + over, 
           data = xrpo_c2f,
           family = "negbinomial2")
summary(tester0)

# rescale venue.x - coutry??

tester3 <- brm(runs_per_over ~ 0 + offset(log(faced)) + over + striker, 
               data = xrpo_c2f,
               family = negbinomial)
tester3
summary(tester3)


##ot as good as 3
tester5 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + over, 
               data = xrpo_c2f,
               family = negbinomial)
tester5
summary(tester5)

compare_performance(tester0, tester3, tester5, rank = TRUE)

#wickets lost yet is 50-50 

tester7 <- brm(runs_per_over ~ 0 + offset(log(faced)) + wickets_lost_yet + over, 
               data = xrpo_c2f,
               family = negbinomial)
summary(tester7)

tester10 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + striker + wickets_lost_yet + over, 
                data = xrpo_c2f,
                family = negbinomial)
summary(tester10)

compare_performance( tester3, tester7, tester10, rank = TRUE)

tester11 <- brm(runs_per_over ~ 0 + offset(log(faced)) + wickets_lost_yet + over
                + season.x, 
                data = xrpo_c2f,
                family = negbinomial)
summary(tester11)

tester12 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + wickets_lost_yet + over
                + season.x, 
                data = xrpo_c2f,
                family = negbinomial)
summary(tester12)


compare_performance(tester3, tester11, tester12, rank = TRUE)

