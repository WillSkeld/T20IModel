library(loo)
loo1 <- loo(hfitex20t, save_psis = TRUE)
log_lik_1 <- extract_log_lik(hfitex20t, merge_chains = FALSE)
