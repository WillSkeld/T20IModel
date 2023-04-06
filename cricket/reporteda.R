library(cricketdata)
t20i_match_infoR <- fetch_cricsheet(competition = "t20is", 
                                    type = "match", gender = "male")

unique(intersect(t20i_match_infoR$team1, t20i_match_infoR$team2))

       