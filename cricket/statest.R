library(rstan)
test_data <- list(
  K = 8,
  y = c(0.6, 1.542, 0.123, 1.34, 0.78, 1.11, 1.33, 0.93)
)

  fit1 <- stan(
  file = "test.stan",  
  data = test_data,    
  chains = 4,             
  warmup = 1000,          
  iter = 2000,            
  cores = 1,              
  refresh = 0             
)

fit1
