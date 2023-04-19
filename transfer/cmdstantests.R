install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
install_cmdstan()
mod <- cmdstan_model("histan.stan")
mod$print()
cmdstan_path()
cmdstan_version()
fit <- mod$sample(
  data = h_data_1, 
  chains = 4, 
  iter_sampling = 1500, iter_warmup = 1000,
  adapt_delta=0.99, max_treedepth=55,
  refresh = 20 
)

?sample
