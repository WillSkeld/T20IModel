xrpo_cfull <- xrpo_c %>% filter(!is.na(venue.x) & wickets_lost_yet < 11)
xrpoex <- xrpo_cfull %>% filter(striker == "V Kohli" | striker == "JC Buttler")
xrpoex <- xrpoex %>% mutate(gamestate = case_when(over< 7 ~ 'pp', over<15 ~ 'regular', over>14 ~ 'death'))

xrpoex2 <- xrpo_cfull %>% filter((striker == "V Kohli" | striker == "JC Buttler" |
                                   striker == "Mohammad Rizwan" | striker == "DA Miller" |
                                   striker == "DP Conway") & (season.x == "2021" |
                                                                season.x == "2021/22" |
                                                                season.x == "2022" |
                                                                season.x == "2022/23"))
xrpoex2 <- xrpoex2 %>% mutate(gamestate = case_when(over< 7 ~ 'pp', over<15 ~ 'regular', over>14 ~ 'death'))
