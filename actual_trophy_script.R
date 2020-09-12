# My 4 tasks

# General necessities

# Libraries
library(tidyverse)
library(nflfastR)
library(httr)

# Mappings
player_mappings <- readRDS("current_most_complete_map.rds")

# Position information
positions <- readRDS("players_and_ids.rds") %>%
  select(primary_position, players = player_id)

# Rosters
# Grab Rosters from Sleeper (this part can be automated in a script later)
base_url <- "https://api.sleeper.app/v1/league/604138427104952320/"
rosters_url <- paste0(base_url, "rosters")

rost_get <- GET(rosters_url)
rost_content <- content(rost_get)

extract_rosters <- function(lst){
  user_id <- lst$owner_id
  players <- lst$players
  plays <- rbind(players)
  starters <- lst$starters
  starts <- rbind(starters)
  
  return(tibble(user = user_id, starters = list(starters), players = list(players)))
}

rosters <- lapply(rost_content, extract_rosters)
rosters_df <- bind_rows(rosters) 

# Make a quick function that modifies players to be a df in which col1 is player, col2 is starter/not
fix_fn <- function(start, play){
  starters <- do.call(rbind.data.frame, start)
  names(starters) <- "starters"
  
  players <- do.call(rbind.data.frame, play)
  names(players) <- "players"
  
  players <- players %>%
    mutate(is_starter = players %in% starters$starters)
  
  return(players)
}

# Roster info with who is a starter
updated_rosters <- rosters_df %>%
  mutate(fix = map2(starters, players, fix_fn)) %>%
  select(-c(starters, players)) %>%
  unnest(fix)

# Rosters also with names
named_rosters <- updated_rosters %>%
  left_join(player_mappings, by = c("players" = "player_id")) %>%
  left_join(positions, by = "players")

# pbp data
pbp_2020 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")
)

# I need to make some modifications so that this is joinable, currently there are IDs everywhere
# Easiest one would be to make this into pass and rush plays separately and create join rules
smaller_pbp <- pbp_2020 %>%
  select(play_id, game_id, home_team, away_team, week, 
         yardline_100, game_seconds_remaining, game_half, qtr,
         drive, drive_play_count, 
         down, goal_to_go, ydstogo, desc, play_type, yards_gained,
         total_home_score, total_away_score, complete_pass, ep, epa,
         air_epa, yac_epa, comp_air_epa, comp_yac_epa,
         fumble_lost, interception, two_point_conv_result, touchdown, pass_touchdown,
         receiver_player_id, rusher_player_id, passer_player_id) %>%
  mutate(two_point_conv_result = if_else(two_point_conv_result %in% "success", 1, 0))

# This currently uses targets, not completions. Fix
pass_2020 <- smaller_pbp %>%
  filter(play_type %in% "pass")
rush_2020 <- smaller_pbp %>%
  filter(play_type %in% "run")
st_and_penalties <- smaller_pbp %>%
  filter(!(play_type %in% c("pass", "run")))

# Idea as to how to calculate fantasy points
  # Just add fantasy column for convenience
rusher_fantasy_and_epa <- rush_2020 %>%
  mutate(position = "rusher",
         relevant_player_id = rusher_player_id,
         fant_pt = 0.1 * yards_gained + 6 * touchdown + 2 * two_point_conv_result)

passer_fantasy_and_epa <- pass_2020 %>%
  mutate(position = "passer",
         relevant_player_id = passer_player_id,
         fant_pt = 0.04 * yards_gained + 4 * touchdown + 2 * two_point_conv_result)

receiver_fantasy_and_epa <- pass_2020 %>%
  mutate(position = "receiver",
         relevant_player_id = receiver_player_id,
         fant_pt = 0.5 * complete_pass + 0.1 * yards_gained +
              6 * touchdown + 2 * two_point_conv_result)

# Note this will have duplicated rows 
recombined_fantasy <- rusher_fantasy_and_epa %>%
  bind_rows(passer_fantasy_and_epa) %>%
  bind_rows(receiver_fantasy_and_epa)

# 1. Hit the Showers Early
# Team had the biggest point discrepancy between 1st and 2nd half of games
player_halves <- recombined_fantasy %>%
  group_by(week, game_id, game_half, relevant_player_id) %>%
  summarize(tot_fant_pts = sum(fant_pt, na.rm = TRUE)) %>%
  pivot_wider(names_from = game_half, values_from = tot_fant_pts) %>%
  mutate(Half1 = if_else(is.na(Half1), 0, Half1),
         Half2 = if_else(is.na(Half2), 0, Half2),
         discrepancy = Half1 - Half2) %>%
  arrange(desc(discrepancy)) %>%
  filter(!is.na(relevant_player_id))

showers <- named_rosters %>%
  inner_join(player_halves, by = c("new_id" = "relevant_player_id")) %>%
  filter(is_starter) %>%
  group_by(user, week) %>%
  summarize(early_game = sum(discrepancy)) %>%
  arrange(desc(early_game))

# 2. Any Given Sunday
# Started the player with the largest Z score or largest difference from last week
  # Dont yet know how to do this, just go with largest difference for now
gap_fn <- function(curr_week, last_week){
  if(length(last_week) == 1){
    print("No previous games, default to 0.")
    combined <- curr_week %>%
      rename(curr_week_fant_pts = tot_fant_pts) %>%
      mutate(curr_week_fant_pts = if_else(is.na(curr_week_fant_pts), 0 , curr_week_fant_pts),
             improvement = curr_week_fant_pts)
    return(combined)
  }
  
  combined <- curr_week %>%
    rename(curr_week_fant_pts = tot_fant_pts) %>%
    left_join(last_week, by = "relevant_player_id") %>%
    mutate(curr_week_fant_pts = if_else(is.na(curr_week_fant_pts), 0 , curr_week_fant_pts),
           tot_fant_pts = if_else(is.na(tot_fant_pts), 0, tot_fant_pts),
           improvement = curr_week_fant_pts - tot_fant_pts) %>%
    select(relevant_player_id, improvement)
  
  return(combined)
}

# Separate into two steps to avoid issue, only will work week 2 and beyond
lagged_performance <- recombined_fantasy %>%
  group_by(game_id, week, away_team, home_team, relevant_player_id) %>%
  summarize(tot_fant_pts = sum(fant_pt, na.rm = TRUE)) %>%
  arrange(desc(week)) %>%
  nest(-c(game_id, week, away_team, home_team)) %>%
  pivot_longer(cols = c(away_team, home_team), names_to = "home_away",
               values_to = "teams_involved") %>%
  group_by(teams_involved) %>%
  filter(week == first(week) | week == lead(week, default = 0)) %>%
  mutate(n_games = n(),
         game_n = row_number()) %>%
  pivot_wider(names_from = game_n, names_prefix = "games_", values_from = data) %>%
  filter(n_games > 1)
  
# Check for games_2: I think this is in the right order, wont know until week 2
if("games_2" %in% names(lagged_performance)){
  lagged_perf_update <- lagged_performance %>%
    mutate(improvement = map2(games_1, games_2, gap_fn))
}
# Probably still needs a bit of work


# 3. Home Runs Only
# Started the player with the most fantasy points generated per touch or target, QBs included
  # Minimum 5 touch or target
# Dont add week filter, let this be a running calc for each week
home_runs <- recombined_fantasy %>%
  group_by(week, game_id, relevant_player_id) %>%
  summarize(n_touches = n(),
            fant_per_touch = sum(fant_pt, na.rm=TRUE) / n_touches) %>%
  filter(n_touches >= 5, !is.na(relevant_player_id)) %>%
  arrange(desc(fant_per_touch))
  
homers <- named_rosters %>%
  inner_join(home_runs, by = c("new_id" = "relevant_player_id")) %>%
  filter(is_starter) %>%
  group_by(week) %>%
  arrange(desc(fant_per_touch)) %>%
  slice(1)

# 4. My Name is Versatility
# Started the player that generated the most unconventional points for that role 
# (rushing yards for receivers, receiving yards for QB, passing yards for any skill position)
fant_pt_by_off_pos <- recombined_fantasy %>%
  group_by(game_id, week, relevant_player_id, position) %>%
  summarize(tot_fant_pts = sum(fant_pt)) %>%
  filter(!is.na(relevant_player_id))

off_role <- named_rosters %>%
  inner_join(fant_pt_by_off_pos, by = c("new_id" = "relevant_player_id")) %>%
  filter(primary_position %in% c("WR", "TE") & position %in% c("rusher", "passer") |
           primary_position %in% "RB" & position %in% "passer" |
           primary_position %in% "QB" & position %in% "receiver") %>%
  group_by(week) %>%
  arrange(desc(tot_fant_pts)) %>%
  slice(1)

# 9 - Running Backs Don't Matter:
# Had the highest ADP RB to average negative EPA per touch, minimum 10 touches 
# ( I think there is a version of EPA for OL controlled, perhaps from BDB2?)


# 10 - I thought we were Playing Basketball?
# Start the Quarterback that led the most 3 and out drives
# This gets the count of QBs with the most 3 and out drives.
qb_drive_cnt <- smaller_pbp  %>% select(game_id,drive,drive_play_count,passer_player_id) %>% group_by(game_id,drive) %>% 
  filter(!is.na(drive)) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  ungroup() %>% 
  group_by(passer_player_id) %>%
  summarise(n = sum(drive_play_count==3))


# 11 - The other side of the football: 
# Start the highest scoring defense.
