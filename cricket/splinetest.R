library(mgcv)
library(performance)
xrpo_c1 <- xrpo_c %>% filter(innings == 1 & !is.na(venue.x) & wickets_lost_yet < 11 &
                               season.x == "2022/23" | season.x == "2021/22")
xrpo_c1$bowling_team <- factor(xrpo_c1$bowling_team)
xrpo_c1$season.x <- factor(xrpo_c1$season.x) 
xrpo_c1$striker <- factor(xrpo_c1$striker)

xrpo_c2$bowling_team <- factor(xrpo_c2$bowling_team)
xrpo_c2$season.x <- factor(xrpo_c2$season.x)
xrpo_c2$striker <- factor(xrpo_c2$striker)

# tester12 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + wickets_lost_yet + over
#                 + season.x, 
#                 data = xrpo_c2f,
#                 family = negbinomial)
# summary(tester12)


s0 <- gam(runs_per_over~offset(log(faced)) + over, 
          data = xrpo_c1,
          family = nb)

summary(s0)

vis.gam(s0, plot.type = "contour", color = "terrain", main = "tensor product")

s1 <- gam(runs_per_over~offset(log(faced)) + te(over), 
          data = xrpo_c1,
          family = nb)

summary(s1)

vis.gam(s1, plot.type = "contour", color = "terrain", main = "tensor product")


s2 <- gam(runs_per_over~ te(over) + offset(log(faced)) + te(wickets_lost_yet), 
          data = xrpo_c1,
          family = nb)

summary(s2)

vis.gam(s2, view = c("over", "wickets_lost_yet"), plot.type = "contour", 
        color = "terrain", main = "tensor product")


s3 <- gam(runs_per_over~ te(over, wickets_lost_yet) + offset(log(faced)), 
          data = xrpo_c1,
          family = nb)

summary(s3)

vis.gam(s3, view = c("over", "wickets_lost_yet"), plot.type = "contour", 
        color = "terrain", main = "tensor product")

compare_performance(s0, s1, s2, s3, rank = TRUE)


s4 <- gam(runs_per_over~ te(over, wickets_lost_yet) + te(bowling_team, bs = 're') + 
            offset(log(faced)), 
          data = xrpo_c1,
          family = nb)

summary(s4)

s5 <- gam(runs_per_over~ te(over, wickets_lost_yet) + 
            ti(bowling_team, bs = 're') + ti(bowling_team, over, bs = 'fs') ++ 
            offset(log(faced)), 
          data = xrpo_c1,
          family = nb)

summary(s5)

vis.gam(s5, view = c("bowling_team", "over"), plot.type = "contour", 
        color = "terrain", main = "tensor product")

compare_performance(s3, s5, rank = TRUE)


# s7 <- gam(runs_per_over~ te(over, bowling_team, wickets_lost_yet, bs = 'fs') + 
#             offset(log(faced)), 
#           data = xrpo_c1,
#           family = nb)
# 
summary(s7)
# 
# vis.gam(s7, view = c("bowling_team", "over"), plot.type = "contour", 
#         color = "terrain", main = "tensor product")
# 
 compare_performance(s6, s5, s7, s8, rank = TRUE)


### season


s8 <- gam(runs_per_over~ offset(log(faced)) +te(over, wickets_lost_yet) + 
            ti(bowling_team, bs = 're') + ti(season.x, bs = 're') + 
            #ti(bowling_team, over, bs = 'fs') + 
            ti(season.x, bowling_team, bs = 'fs'), 
          data = xrpo_c1,
          family = nb)

summary(s8)


vis.gam(s8, view = c( "bowling_team", "season.x"), plot.type = "contour", 
        color = "terrain", main = "tensor product")


### striker

s9 <- gam(runs_per_over~ offset(log(faced)) +te(over, wickets_lost_yet) + 
            ti(bowling_team, bs = 're') + ti(season.x, bs = 're') + 
            #ti(bowling_team, over, bs = 'fs') + 
            ti(season.x, bowling_team, bs = 'fs')+
            ti(striker, bs = 're'), 
          data = xrpo_c2,
          family = nb)

summary(s9)


vis.gam(s9, view = c( "bowling_team", "season.x"), plot.type = "contour", 
        color = "terrain", main = "tensor product")
