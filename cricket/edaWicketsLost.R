t20i_bbb <- fetch_cricsheet(competition = "t20is", gender = "male")
t20i_bbb <- t20i_bbb %>% mutate(match_id = factor(match_id), 
                                innings = factor(innings), 
                                over = factor(over)) 

t20i_bbb_f <- t20i_bbb %>% filter(innings<3 & wickets_lost_yet < 10)
xrwl <- t20i_bbb_f %>% 
  group_by(wickets_lost_yet, over, match_id, innings)%>%
  summarise(
    runs_per_over = sum(runs_off_bat),
    total = length(unique(match_id, over)),
    .groups = "keep"
  )

xrwl
plot <- xrwl %>% ggplot(aes(x=runs_per_over, 
                            y=(..density..))) + geom_histogram(binwidth = 1) + 
  facet_wrap(~wickets_lost_yet) + ggtitle("Run Distribution per Over by Wickets Lost")
plot

