testmodel1 <- lm(runs_per_over ~ offset(faced) + over, data = xrpo_c1)
summary(testmodel1)
xrpo_c1ft <- xrpo_c1 %>% mutate(gamestate = case_when(over< 7 ~ 'pp', over<15 ~ 'regular', over>14 ~ 'death'))
xrpo_c1ft$gamestate <- factor(xrpo_c1ft$gamestate, levels=c("pp", "regular", "death"))

testmodel2 <- lm(runs_per_over ~ 0 + offset(faced) + gamestate, data = xrpo_c1ft)
summary(testmodel2)

compare_performance(testmodel1, testmodel2)

testmodel3 <- glm.nb(runs_per_over ~ 0 + offset(log(faced)) + gamestate, data = xrpo_c1ft) 
summary(testmodel3)

testmodel4 <- glm.nb(runs_per_over ~ 0 + offset(log(faced)) + over, data = xrpo_c1) 
summary(testmodel4)

compare_performance(testmodel3, testmodel4, rank = TRUE)

testmodel5 <- glm.nb(runs_per_over ~ 0 + offset(log(faced)) + gamestate + venue.x, data = xrpo_c1ft) 
summary(testmodel5)

testmodel6 <- glm.nb(runs_per_over ~ 0 + offset(log(faced)) + over + venue.x, data = xrpo_c1) 
summary(testmodel6)

compare_performance(testmodel3, testmodel4, testmodel5, testmodel6, rank = TRUE)
