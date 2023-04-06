library(performance)
m1
r2(m1)
icc(m1)

check_overdispersion(m1)
check_overdispersion(m2)

check_zeroinflation(m1)
check_zeroinflation(m2)

check_model(m2)

compare_performance(m0,m1,m2,m3, rank = TRUE)
