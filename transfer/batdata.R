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
length(unique(t20i_bbb$striker))
t20i_match_info <- fetch_cricsheet(competition = "t20is", type = "match", gender = "male") %>% 
  mutate(match_id = factor(match_id))
t20i_match_info <- t20i_match_info %>% filter((team1 == "England" | team1 == "South Africa" | team1 == "Australia" 
          | team1 == "India" | team1 == "West Indies" | team1 == "New Zealand"
          | team1 == "Afghanistan" | team1 == "Bangladesh" | team1 == "Pakistan" 
          | team1 == "Zimbabwe" | team1 == "Sri Lanka") & (team2 == "England" | team2 == "South Africa" | team2 == "Australia" | 
                                     team2 == "India" | team2 == "West Indies" | team2 == "New Zealand" |
                                    team2 == "Afghanistan" | team2 == "Bangladesh" | team2 == "Pakistan" | 
                                    team2 == "Zimbabwe" | team2 == "Sri Lanka"))
  
  # mutate(country = case_when(((city=="Adelaide") | (city=="Melbourne") | 
  #                                        (city=="Brisbane") | (city=="Perth") | (city=="Hobart") |
  #                                        (city=="Sydney") | (city=="Canberra") | city=="Carrara") ~ 'Australia',
  #                                     ((city=="Southampton") | (city=="Bristol") | (city=="London") | 
  #                                        (city=="Manchester") | (city=="Nottingham") | (city=="Birmingham") |
  #                                        (city=="Cardiff") | (city=="Chester-le-Street") | (city=="East London") |
  #                                        (city=="Taunton") | (city=="Leeds")) ~ 'England',
  #                                     ((city=="Johannesburg") | (city=="Cape Town") | (city=="Durban") | 
  #                                        (city=="Port Elizabeth") | (city=="Centurion") | (city=="Bloemfontein") |
  #                                        (city=="Potchefstroom") | city =="Kimberley" | city == "Paarl") ~ 'South Africa',
  #                                     ((city=="Barbados") | (city=="Trinidad") | (city=="St Kitts") | 
  #                                        (city=="St Lucia") | (city=="Guyana") | (city=="Antigua") |
  #                                        (city=="St Vincent") | (city=="Dominica") | (city=="Jamaica") | 
  #                                        city=="Kingston" | city=="Basseterre" | city == "Bridgetown" |
  #                                        city== "Gros Islet" | city=="Lauderhill" |city == "Providence" |
  #                                        city=="Roseau" | city =="St George's" | city == "Tarouba") ~ 'West Indies',
  #                                     ((city=="Mumbai") | city == "Cuttack" | city == "Dehra Dun" |
  #                                        (city=="Kolkata") | (city=="Chennai") | (city=="Pune") |
  #                                        (city=="Delhi") | (city=="Nagpur") | (city=="Lucknow") |
  #                                        (city=="Hyderabad") | (city=="Jaipur") | (city=="Ahmedabad") |
  #                                        (city=="Bangalore")| (city=="Bengaluru")|city=="Chandigarh" |
  #                                        city == "Dehradun" | city=="Dharmasala" | city=="Dharamsala" | city=="Guwahati" |
  #                                        city == "Kanpur" | city == "Ranchi" | city=="Thiruvananthapuram" |
  #                                        city == "Visakhapatnam" | city == "Rajkot" | city  == "Indore") ~ 'India',
  #                                     ((city=="Auckland") | (city=="Christchurch") | (city=="Wellington") | 
  #                                        (city=="Mount Maunganui") | city=="Hamilton" | city=="Napier" |
  #                                        city == "Nelson" | city=="Dunedin") ~ 'new Zealand',
  #                                     city=="Abu Dhabi" | city=="Dubai" | city=="Nairobi" | city == "Sharjah"~ 'Other',
  #                                     (city=="Mirpur") | (city=="Chattogram") | (city=="Chittagong") | 
  #                                       (city=="Dhaka") | (city=="Khulna") | (city=="Sylhet") ~ 'Bangladesh',
  #                                     (city=="Karachi") | (city=="Lahore") ~ 'Pakistan',
  #                                     city=="Harare" | city=="Bulawayo"  ~ 'Zimbabwe'))

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
join1 <- max_innings_over %>% left_join(., t20i_match_info %>% filter(!is.na(city)), by = "match_id")
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
    season.x = season.x,
    .groups = "keep"
  )  %>% distinct()

