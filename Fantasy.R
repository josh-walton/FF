# Required packages ####

# install.packages('tidyverse')
# install.packages('readxl')
# install.packages('dplyr')
# install.packages('janitor')
# install.packages('data.table')
# install.packages('lubridate')
# install.packages('forecast')
# install.packages('stats')
# install.packages('forcats')
# install.packages('writexl')
# install.packages('openxlsx2')
# install.packages('scales')

library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(data.table)
library(lubridate)
library(forecast)
library(stats)
library(forcats)
library(writexl)
library(openxlsx2)
library(scales)
library(YFAR)


# Data ####

league_id <- '805301'

app_id <- "2hRoZn0L"

app_name <- "Bethelonians"

my_key <- 'dj0yJmk9VFJ6Q2pCWHNwcTh5JmQ9WVdrOU1taFNiMXB1TUV3bWNHbzlNQT09JnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PTM0'

my_secret <- '305b9bdd42fa15b5ed6c8c7fa3c33744b6cc6db1'

options("httr_oob_default" = T)

# Authorization ####

# my_token$refresh()

# <- YFAR::y_create_token(my_key, my_secret, app_name) # <-- used at first, maybe only needs refresh from now on

# League Data ####

league_info <- as.data.frame(y_games(my_token))

bethel <- league_info %>% 
  filter(league_name == "Bethelonians") %>% 
  select(league_name,
         league_key,
         league_id,
         league_start_date,
         league_end_date,
         league_season,
         game_key,
         game_id)
  
# Team Keys ####

teams_24 <- y_teams("449.l.805301", my_token) %>% 
  select(team_key,
         team_name)

# Head to Head Data ####

mu <- y_matchups("449.l.805301.t.1", my_token) %>%
  filter(matchup_team_name != "Walton", matchup_week %in% c(1:4)) %>% 
  select(matchup_week,
         matchup_week_start,
         matchup_week_end,
         matchup_winner_team_key,
         matchup_team_key,
         matchup_team_id,
         matchup_team_name,
         matchup_team_points_total,
         matchup_team_projected_points_total
         )

team_score <- y_matchups("449.l.805301.t.1", my_token) %>%
  filter(matchup_team_name == "Walton", matchup_week %in% c(1:4)) %>% 
  select(matchup_week,
         matchup_week_start,
         matchup_week_end,
         matchup_winner_team_key,
         matchup_team_key,
         matchup_team_id,
         matchup_team_name,
         matchup_team_points_total,
         matchup_team_projected_points_total
  )

for (x in 1:10) {
  f <- y_matchups(paste0("449.l.805301.t.", x), my_token) %>%
    filter(matchup_week %in% c(1:4),
           matchup_team_key == paste0("449.l.805301.t.", x)) %>% 
    select(matchup_week,
           matchup_week_start,
           matchup_week_end,
           matchup_winner_team_key,
           team_key = matchup_team_key,
           team_name = matchup_team_name,
           team_points_total = matchup_team_points_total,
           team_projected_points_total = matchup_team_projected_points_total
    ) %>% 
  assign(paste0("team_",x,"_stats"), f)
}

# Full matchup data test ####

for (x in 1:10) {
  f <- y_matchups(paste0("449.l.805301.t.", x), my_token) %>%
    filter(matchup_week %in% c(1:4)) %>% 
    select(matchup_week,
           matchup_week_start,
           matchup_week_end,
           matchup_winner_team_key,
           team_key = matchup_team_key,
           team_name = matchup_team_name,
           team_points_total = matchup_team_points_total,
           team_projected_points_total = matchup_team_projected_points_total
    )

    assign(paste0("team_",x,"_full"), f)
}

# %>% 
#   group_by(matchup_week) %>% 
#   mutate(matchup_id = paste0(matchup_week,"_",sample(0:10000000, 1))) %>% 
#   ungroup()

# Bind Rows ####

b <- bind_rows(team_1_full,
               team_2_full,
               team_3_full,
               team_4_full,
               team_5_full,
               team_6_full,
               team_7_full,
               team_8_full,
               team_9_full,
               team_10_full) %>% 
  distinct(team_name, matchup_week, .keep_all = T)

b %>% 
  distinct(team_name, matchup_week)

b %>%
  group_by_all() %>%
  filter(n()>1) %>%
  ungroup()
  
  
  

# Standings ####

standings <- y_standings("449.l.805301.t.1",my_token)


# Scoreboard ####
sb <- y_scoreboard("449.l.805301",my_token)
