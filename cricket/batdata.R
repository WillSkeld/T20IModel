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

t20i_bbb_f <- t20i_bbb %>% filter(innings!=3 & innings!=4)
over_data <- t20i_bbb_f %>% 
  group_by(over, innings)%>%
  summarise(
    runs_off_bat_total = sum(runs_off_bat),
    total = length(unique(match_id, over)),
    .groups = "keep"
  ) %>%
  mutate(
    runs_per_over = round(runs_off_bat_total / total, 2),
  )

max_innings_over <- t20i_bbb_f %>% group_by(innings, match_id) %>%
  summarise(max_over = max(over))
join1 <- max_innings_over %>% left_join(., t20i_match_info, by = "match_id")
join2 <- join1 %>% left_join(., t20i_bbb_f, by = c("match_id", "innings"))
adjusted_over_data <- join2 %>% 
  group_by(venue.x, over, innings, match_id, striker) %>%
  summarise(
    faced = sum(over/over),
    runs_per_over = sum(runs_off_bat),
    total = length(unique(match_id, over)),
    overs_remaining = max_over-over,
    wickets_lost_yet = wickets_lost_yet,
    bowling_team = bowling_team,
    season.x = season.x,
    .groups = "keep"
  )  %>% distinct()