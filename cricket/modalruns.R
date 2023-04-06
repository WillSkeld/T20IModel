t<- xrpo_c %>% group_by(innings,over, runs_per_over) %>%
  summarise(
    freq = length(runs_per_over)) %>% 
  arrange(desc(freq)) %>% 
group_by(innings,over) %>%
  slice(1:2)

t
