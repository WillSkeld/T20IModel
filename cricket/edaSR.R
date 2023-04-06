t20i_bbb <- fetch_cricsheet(competition = "t20is", gender = "male")
t20i_bbb <- t20i_bbb %>% mutate(match_id = factor(match_id)) 
t20i_match_info <- fetch_cricsheet(competition = "t20is", type = "match", gender = "male") %>% 
  mutate(match_id = factor(match_id)) %>% print(n = 'team1')


overdist <- t20i_bbb %>%
  group_by(over, match_id, innings) %>%
  summarise(
    runs_off_bat_total = sum(runs_off_bat)
    )
plot1 <- overdist%>% ggplot(aes(x=runs_off_bat_total)) + geom_bar(stat = "bin") + 
  xlim(0,36) + facet_wrap(~over)
  
plot1
