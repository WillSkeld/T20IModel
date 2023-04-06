library(brms)
library(lme4)


xrpo_c1 <- xrpo_c %>% filter(innings==1 & !is.na(venue.x))

xrpo_c2 <- xrpo_c1 %>% filter(striker=='AD Hales' | striker=='JJ Roy' | 
                                striker=='SPD Smith' | striker=='KL Rahul' |
                                striker=='RG Sharma' | striker=='SPD Smith' |
                                striker=='C Green' |striker=='DA Warner')

xrpo_c2f <- xrpo_c2 %>% mutate(gamestate = case_when(over<15 ~ 'regular', over>14 ~ 'death'))
xrpo_c2f$gamestate <- factor(xrpo_c2f$gamestate, levels=c("regular", "death"))


rm1 <- brm(runs_per_over ~ 0 + offset(log(faced)) + striker, 
             data = xrpo_c2,
             family = negbinomial)

summary(rm1)

rm2 <- brm(runs_per_over ~ 0 + offset(log(faced)) + over, 
           data = xrpo_c2,
           family = "negbinomial2")
summary(rm2)

rm3 <- brm(runs_per_over ~  over + venue.x, 
           data = xrpo_c1,
           family = "negbinomial2")
summary(rm3)

compare_performance(rm1,rm2, rank = TRUE)

##powerplay vs not 

m1 <- lm(runs_per_over ~ offset(log(faced)) + gamestate, data = xrpo_c2f)
m1
summary(m1)

tester1 <- brm(runs_per_over ~ 0 + offset(log(faced)) + gamestate, 
               data = xrpo_c2f,
               family = "negbinomial2")
tester1
summary(tester1)

compare_performance(rm1,rm2, tester1, rank = TRUE)

##player factor 
m2 <- lmer(runs_per_over ~ + offset(faced)+ gamestate + 1|striker, data = xrpo_c2f)
m2
summary(m2)

gm2 <- glmer.nb(runs_per_over ~ offset(log(faced)) + over + 1|striker, data = xrpo_c2f)
summary(gm2)


### adding player

tester2 <- brm(runs_per_over ~ 0 + offset(log(faced)) + gamestate + striker, 
               data = xrpo_c2f,
               family = negbinomial)
tester2
summary(tester2)

tester3 <- brm(runs_per_over ~ 0 + offset(log(faced)) + over + striker, 
               data = xrpo_c2f,
               family = negbinomial)
tester3
summary(tester3)

compare_performance(tester1, tester2,tester3, rank = TRUE)

#owling team

m1 <- lm(runs_per_over ~ 0 + offset(faced) + bowling_team + striker + over, data = xrpo_c2f)
summary(m1)

gmT <- glm.nb(runs_per_over ~ 0 + offset(log(faced))+ bowling_team + over + striker, data = xrpo_c2f)
summary(gmT)

tester5 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + over, 
               data = xrpo_c2f,
               family = negbinomial)
tester5
summary(tester5)

tester6 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + striker + over, 
               data = xrpo_c2f,
               family = negbinomial)
tester6
summary(tester6)

compare_performance(tester3, tester5, tester6, rank = TRUE)


#wickets_lost_yet 
mw <- lm(runs_per_over ~ 0 + offset(faced) + bowling_team + striker + over + wickets_lost_yet, 
         data = xrpo_c2f)
summary(mw)

gmT <- glm.nb(runs_per_over ~ 0 + offset(log(faced))+ bowling_team + over 
              + striker + wickets_lost_yet, data = xrpo_c2f)
summary(gmT)



tester7 <- brm(runs_per_over ~ 0 + offset(log(faced)) + wickets_lost_yet + gamestate, 
               data = xrpo_c2f,
               family = negbinomial)
summary(tester7)

tester8 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + wickets_lost_yet + over, 
               data = xrpo_c2f,
               family = negbinomial)
summary(tester8)

tester9 <- brm(runs_per_over ~ 0 + offset(log(faced)) + striker + wickets_lost_yet + over, 
               data = xrpo_c2f,
               family = negbinomial)
summary(tester9)

tester10 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + striker + wickets_lost_yet + gamestate, 
               data = xrpo_c2f,
               family = negbinomial)
summary(tester10)

compare_performance(tester10, tester7)

#year

my <- lm(runs_per_over ~ 0 + offset(faced) + bowling_team + striker + over 
         + wickets_lost_yet + season.x, 
         data = xrpo_c2f)
summary(my)

myp <- lm(runs_per_over ~ 0 + offset(faced) + over + wickets_lost_yet + season.x, 
         data = xrpo_c2f)
summary(myp)

gmY <- glm.nb(runs_per_over ~ 0 + offset(log(faced))+ bowling_team + over + season.x 
              + striker + wickets_lost_yet, data = xrpo_c2f)
summary(gmY)

gmYp <- glm.nb(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + season.x +
                wickets_lost_yet, data = xrpo_c2f)
summary(gmYp)



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

tester13 <- brm(runs_per_over ~ 0 + offset(log(faced)) + striker + wickets_lost_yet + over
               + season.x, 
               data = xrpo_c2f,
               family = negbinomial)
summary(tester13)

tester14 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + striker + wickets_lost_yet + over
                + season.x, 
                data = xrpo_c2f,
                family = negbinomial)
summary(tester14)

compare_performance(tester11, tester12, tester13, tester14, rank = TRUE)

tester15 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + striker + wickets_lost_yet + over
                + season.x + season.x:bowling_team, 
                data = xrpo_c2f,
                family = negbinomial)
summary(tester15)

tester16 <- brm(runs_per_over ~ 0 + offset(log(faced)) + bowling_team + wickets_lost_yet + over
                + season.x + season.x:bowling_team, 
                data = xrpo_c1,
                family = negbinomial)
summary(tester16)

compare_performance(tester12, tester14, tester15, tester16, rank = TRUE)

