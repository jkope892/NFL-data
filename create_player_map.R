library(tidyverse)
library(glue)
library(nflfastR)
library(humaniformat)
library(janitor)

seasons <- 2011:2019

# My function for importing pbp data and joining with Lee Sharpe's games file
get_nflfastr_pbp <- function(seasons) {
    
    pbp <- purrr::map_df(seasons, function(x) {
        
        read_rds(
            url(
                glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
            )
        )
    }) %>%
        select(!c(away_score:game_stadium)) %>%
        clean_pbp()
    
    report("Loading game data")
    games <- read_csv("http://www.habitatring.com/games.csv", 
                      col_types = cols(away_moneyline = col_number(),
                                       home_moneyline = col_number(),
                                       under_odds = col_number(),
                                       over_odds = col_number(),
                                       away_spread_odds = col_number(),
                                       home_spread_odds = col_number())) %>%
        mutate(game_id=as.character(game_id)) %>% 
        filter(season >= 2000 & !is.na(result)) %>%
        select(!c(season, game_type, week))
    
    pbp <- pbp %>%
        left_join(games, by = c("game_id"="game_id","away_team"="away_team","home_team"="home_team")) %>%
        rename(season = season.x,
               stadium = stadium.y) %>%
        select(!c(season.y, stadium.x, lateral_receiver_player_id:defensive_extra_point_conv, 
                  home_timeouts_remaining:timeout_team, return_touchdown:fumble, punt_inside_twenty:tackled_for_loss, punt_blocked, own_kickoff_recovery:qb_hit,
                  field_goal_result:extra_point_result, assist_tackle:lateral_recovery)) %>%
        relocate(season)
    
}

# Get nflfastR 2.0 pbp data and filter for player data
pbp <- get_nflfastr_pbp(seasons)

pbp_players <- pbp %>%
    select(season:game_id, week, posteam, defteam, passer:receiver, passer_id:receiver_id, 
           old_game_id, play_type) 

    pbp_players$season <- as.numeric(pbp_players$season)
    pbp_players$week <- as.numeric(pbp_players$week)
    pbp_players$play_id <- as.character(pbp_players$play_id)
    pbp_players$old_game_id <- as.character(pbp_players$old_game_id)

# Read in legacy player data with old gsis ID's and positions
# NOTE: I used an old pbp file with Lee Sharpe's position data which I don't think is
# available yet since after the ID's changed, but I uploaded the file to my repo so you
# can pull from there for now
gsis_data <- read_rds(url("https://github.com/jkope892/NFL-data/blob/master/legacy_player_positions.rds?raw=TRUE"))

# Join nflfastR 2.0 pbp with legacy data
players_map <- left_join(pbp_players, gsis_data) %>%
    mutate(passer_team = case_when(
        !is.na(passer) ~ posteam),
        rusher_team = case_when(
            !is.na(rusher) ~ posteam
        ),
        receiver_team = case_when(
            !is.na(receiver) ~ posteam
        )) %>%
    relocate(passer_team, .after = passer) %>%
    relocate(rusher_team, .after = rusher) %>%
    relocate(receiver_team, .after = receiver) %>%
    relocate(passer_player_position, passer_id, passer_gsis_id, .after = passer_team) %>%
    relocate(rusher_player_position, rusher_id, rusher_gsis_id, .after = rusher_team) %>%
    relocate(receiver_player_position, receiver_id, receiver_gsis_id, .after = receiver_team) %>%
    relocate(old_game_id, .after = game_id) %>%
    select(!c(week, defteam, play_type))