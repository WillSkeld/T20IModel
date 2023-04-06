library(cricketdata)
library(readr)
library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)
library(gghighlight)
library(ggtext)
library(patchwork)
library(knitr)
library(gridExtra)

# Fetch ball-by-ball data
t20i_bbb <- fetch_cricsheet(competition = "t20is", gender = "male")
t20i_bbb <- t20i_bbb %>% mutate(match_id = factor(match_id)) 
t20i_match_info <- fetch_cricsheet(competition = "t20is", type = "match", gender = "male") %>% 
  mutate(match_id = factor(match_id))
t20i_match_info <- t20i_match_info %>% 
  filter((team1 == "England" | team1 == "South Africa" | team1 == "Australia" 
         | team1 == "India" | team1 == "West Indies" | team1 == "New Zealand"
         | team1 == "Afghanistan" | team1 == "Bangladesh" | team1 == "Pakistan" 
         | team1 == "Zimbabwe") & (team2 == "England" | team2 == "South Africa" | team2 == "Australia" 
         | team2 == "India" | team2 == "West Indies" | team2 == "New Zealand"
         | team2 == "Afghanistan" | team2 == "Bangladesh" | team2 == "Pakistan" 
         | team2 == "Zimbabwe"))



t20i_match_info %>% group_by(team1) %>%
  summarise(total= length(team1)) %>% print(n = 90)







xrpb <- t20i_bbb %>%
  group_by(over, ball) %>%
  summarise(
    runs_off_bat_total = sum(runs_off_bat),
    total = length(ball)
  ) %>%
  mutate(
    over_ball = paste(over, ball),
    runs_per_ball = round(runs_off_bat_total / total, 2)
  ) 

xrpb
plot <- xrpb%>% ggplot(aes(x=over_ball, y=runs_per_ball)) + geom_bar(stat = "identity")
plot

t20i_bbb_f <- t20i_bbb %>% filter(innings!=3 & innings!=4)

xrpo <- t20i_bbb_f %>% 
  group_by(over, innings)%>%
  summarise(
    runs_off_bat_total = sum(runs_off_bat),
    total = length(unique(match_id, over)),
    .groups = "keep"
  ) %>%
  mutate(
    runs_per_over = round(runs_off_bat_total / total, 2),
  )
xrpo
xrpo1 <- xrpo %>% filter(innings==1)
xrpo2 <- xrpo %>% filter(innings==2)

hfit3$n_leapfrog__

max_innings_over <- t20i_bbb_f %>% group_by(innings, match_id) %>%
  summarise(max_over = max(over))
join1 <- max_innings_over %>% left_join(., t20i_match_info, by = "match_id")
join2 <- join1 %>% left_join(., t20i_bbb_f, by = c("match_id", "innings"))
xrpo_c <- join2 %>% 
  group_by(venue.x, over, innings, match_id, striker) %>%
  summarise(
    faced = sum(over/over),
    runs_per_over = sum(runs_off_bat),
    total = length(unique(match_id, over)),
    overs_remaining = max_over-over,
    wickets_lost_yet = wickets_lost_yet,
    bowling_team = bowling_team,
    batting_team = batting_team,
    city = city,
    season.x = season.x,
    .groups = "keep"
  )  %>% distinct()
xrpo_c

##################################################
plot0 <- xrpo %>% ggplot(aes(x=over, y=runs_per_over/2)) + geom_line(stat = "identity") + ylim(0,12) +
   xlab("Over") + ylab("Average Runs per Over")
   
plot0
plot1 <- xrpo1%>% ggplot(aes(x=over, y=runs_per_over)) + geom_line(stat = "identity") + ylim(0,12)+
   xlab("Over") + ylab("Average Runs per Over") + ggtitle("First Innings")
plot2 <- xrpo2%>% ggplot(aes(x=over, y=runs_per_over)) + geom_line(stat = "identity") + ylim(0,12)+
   xlab("Over") + ylab("Average Runs per Over") + ggtitle("Second Innings")
plot1
plot2
grid.arrange(plot1, plot2)
plot3 <- xrpo_c  %>% filter(bowling_team== "Australia" || bowling_team== "England" ||
  bowling_team== "Afghanistan" ||bowling_team== "India" ) %>% ggplot(aes(x=runs_per_over, 
                                                                         y=(..density..))) + 
  geom_histogram(binwidth = 1) +  xlim(0,36) + facet_wrap(~bowling_team)
plot3
# plot31 <- xrpo_c  %>% filter(innings==1) %>% ggplot(aes(x=runs_per_over)) + 
#   geom_histogram(binwidth = 1) + 
#   xlim(0,36) + facet_wrap(~over)+xlab("Runs Scored") + ggtitle("First Innings Run Distribution by Over")
# 
# 
# plot32 <- xrpo_c  %>% filter(innings==2) %>% ggplot(aes(x=runs_per_over)) + 
#   geom_histogram(binwidth = 1) + 
#   xlim(0,36) + facet_wrap(~over) +xlab("Runs Scored") + ggtitle("Second Innings Run Distribution by Over")
# plot3 #cool
# plot31
# plot32
# 
# plot4 <- xrpo_c  %>% ggplot(aes(x=runs_per_over)) + geom_histogram(binwidth = 1) + 
#   xlim(0,36) + facet_wrap(~overs_remaining+innings)
# plot4
# plot41 <- xrpo_c %>% filter(innings==1) %>% ggplot(aes(x=runs_per_over)) + geom_histogram(binwidth = 1) + 
#   xlim(0,36) + facet_wrap(~overs_remaining)
# plot41
# plot42 <- xrpo_c %>% filter(innings==2)  %>% ggplot(aes(x=runs_per_over)) + 
#   geom_histogram(binwidth = 1) + 
#   xlim(0,36) + facet_wrap(~overs_remaining)
# plot42
# 
