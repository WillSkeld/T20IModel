library(cricketdata)
library(readr)
library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)
library(gghighlight)
library(ggtext)
library(patchwork)

# Fetch ball-by-ball data
t20is_bbb <- fetch_cricsheet(competition = "t20is", gender = "male")
t20is_bbb
# Fetch match metadata
t20i_match_info <- fetch_cricsheet(competition = "t20is", type = "match", gender = "male")
t20i_match_info

batting_per_year <- t20is_bbb %>%
  group_by(striker, match_id) %>%
  summarise(
    runs_off_bat_total = sum(runs_off_bat),
    faced = length(ball),
    .groups = "keep"
  ) %>%
  mutate(
    avg = round(runs_off_bat_total / faced, 1),
    strike_rate = round(runs_off_bat_total / faced * 100, 1)
  ) %>%
  mutate(is_hales = (striker == "Alex Hales"))
py<-batting_per_year
py
over<- t20is_bbb$over
runs_off_bat<- t20is_bbb$runs_off_bat
t20is_pp<- t20is_bbb %>% filter(over<7)
t20is_npp<- t20is_bbb %>% filter(over>6)
py['avg']

ppbatting_per_year <- t20is_pp %>%
  group_by(striker, match_id) %>%
  summarise(
    runs_off_bat_pp = sum(runs_off_bat),
    faced = length(ball),
    .groups = "keep"
  ) %>%
  mutate(
    avg = round(runs_off_bat_pp / faced, 1),
    strike_rate = round(runs_off_bat_pp / faced * 100, 1)
  ) %>%
  mutate(is_hales = (striker == "AD Hales"))
pp<-ppbatting_per_year
pp
ppbatting_per_year %>% filter(is_hales == TRUE)


nppbatting_per_year <- t20is_npp %>%
  group_by(striker) %>%
  summarise(
    innings_total = length(unique(match_id)),
    runs_off_bat_npp = sum(runs_off_bat),
    faced = length(ball),
    .groups = "keep"
  ) %>%
  mutate(
    runs_per_innings_avg = round(runs_off_bat_npp / innings_total, 1),
    strike_rate = round(runs_off_bat_npp / faced * 100, 1)
  ) %>%
  filter(innings_total > 2) %>%
  mutate(is_hales = (striker == "AD Hales"))
npp<-nppbatting_per_year
tavg<- unlist(py['avg'][,1])
tavg
ppf<- unlist(pp['faced'][,1])
ppf
tf<- unlist(npp['faced'][,1])
tf
ppavg<- unlist(pp['runs_off_bat_pp'][,1])
ppavg
m1<-lm(ppavg~ppf)
m1
plot(m1)
plot(ppf,ppavg)
abline(m1)
f <-unlist(py['faced'][,1])
s<-unlist(py['runs_off_bat_total'][,1])
sr<-unlist(py['strike_rate'][,1])
hist(f, breaks = 80)
hist(s, breaks = 80)
hist(sr, breaks = 80)
lm(s~sr+I(sr^2))
plot(sr,s)
