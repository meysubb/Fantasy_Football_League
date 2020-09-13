# Generate the fun categories I highlighted in Slack chat with Meyappan and Lucas
library(tidyverse)
library(nflfastR)
library(httr)

# Load mappings
sleeper_league <- readRDS("players_and_ids.rds")
mappings <- readRDS("id_mapping.rds") %>%
  mutate(last_name = str_extract(full_name, "[A-z\\-\\']+$"),
         f_last = paste0(substr(full_name, 1, 1), ".", last_name)) %>%
  select(-last_name)

# Update to one global list of relevant factors for our fantasy league
  # Note that DEF exists, it just wont map very well to nflfastR
just_fantasy_elig <- sleeper_league %>%
  select(position, primary_position, status, full_name, height, years_exp, age,
         gsis_id, high_school, player_id) %>%
  left_join(mappings %>% select(-full_name), by = "gsis_id") %>%
  filter(!is.na(new_id))

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

# Join back with extra identifiers
rosters_and_ids <- updated_rosters %>%
  left_join(just_fantasy_elig, by = c("players" = "player_id"))

# So Rookies dont work
# Need to convert Rookies to F.LastName and try to map accordingly
rookies <- sleeper_league %>%
  filter(years_exp == 0) %>%
  mutate(f_last = paste0(substr(first_name, 1, 1), ".", last_name)) %>%
  select(full_name, f_last, position, primary_position, contains("id"))

rookies_pbp_name_key <- pbp_2020 %>%
  select(season, week, home_team, away_team, play_id, ends_with("player_name")) %>%
  pivot_longer(ends_with("player_name"),
               names_to = "player_desc",
               values_to = "player_key",
               values_drop_na = TRUE)
rookies_pbp_id_key <- pbp_2020 %>%
  select(season, week, home_team, away_team, play_id, ends_with("player_id")) %>%
  pivot_longer(ends_with("player_id"),
               names_to = "player_desc",
               values_to = "new_id",
               values_drop_na = TRUE)

rookies_pbp <- rookies_pbp_name_key %>%
  bind_cols(rookies_pbp_id_key %>% select(new_id)) %>%
  select(player_key, new_id) %>%
  distinct()

# Doesnt work overly well, but good enough
rookies_joined <- rookies %>%
  filter(primary_position %in% c("QB", "WR", "TE", "RB", "FB", "HB"),
         !(f_last %in% "D.Johnson")) %>%
  left_join(rookies_pbp, by = c("f_last" = "player_key"))

# basically rookies will only show up when they appear in the boxscore
# Until then we have no way to map them
current_rookies <- rookies_joined %>%
  filter(!is.na(new_id)) %>%
  select(full_name, f_last, gsis_id, player_id, new_id)

# Add the current rookies to the set
mappings_with_rookies <- mappings %>%
  bind_rows(current_rookies)


# Now lets pair this with their total fantasy points this season and this week
  # Doing so will likely require me to bring in nflfastR here
  # Going to need to manually enumerate which is easy enough
pbp_2020 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")
  )

# I need to make some modifications so that this is joinable, currently there are IDs everywhere
# Easiest one would be to make this into pass and rush plays separately and create join rules
smaller_pbp <- pbp_2020 %>%
  select(play_id, game_id, home_team, away_team, week, 
         yardline_100, game_seconds_remaining, game_half, qtr,
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

# Can quickly calculate fantasy points for passing and rushing
rusher_fantasy_and_epa <- rush_2020 %>%
  group_by(rusher_player_id) %>%
  summarize(n_rushes = n(),
            rush_fant_pt = sum(0.1 * yards_gained + 6 * touchdown + 2 * two_point_conv_result),
            rush_epa = sum(epa))

passer_fantasy_and_epa <- pass_2020 %>%
  group_by(passer_player_id) %>%
  summarize(n_passes = n(),
            pass_fant_pt = sum(0.04 * yards_gained + 4 * touchdown + 2 * two_point_conv_result),
            pass_epa = sum(epa),
            tot_air_epa = sum(air_epa))

receiver_fantasy_and_epa <- pass_2020 %>%
  group_by(receiver_player_id) %>%
  summarize(n_targets = n(),
            n_receptions = sum(complete_pass),
            rec_fant_pt = 0.5 * n_receptions + sum( 0.1 * yards_gained +
                                6 * touchdown + 2 * two_point_conv_result),
            rec_epa = sum(epa),
            tot_yac_epa = sum(yac_epa))

fantasy_results <- receiver_fantasy_and_epa %>%
  rename(player_id = receiver_player_id) %>%
  full_join(passer_fantasy_and_epa, by = c("player_id" = "passer_player_id")) %>%
  full_join(rusher_fantasy_and_epa, by = c("player_id" = "rusher_player_id")) %>%
  rowwise() %>%
  mutate(tot_fant_points = sum(c(rec_fant_pt, pass_fant_pt, rush_fant_pt), na.rm=TRUE),
         tot_epa = sum(c(rec_epa, pass_epa, rush_epa), na.rm=TRUE),
         tot_individual_epa = sum(c(tot_yac_epa, tot_air_epa), na.rm=TRUE))

mappings_with_rookies %>%
  inner_join(fantasy_results, by = c("new_id" = "player_id")) %>%
  View()

mappings_with_rookies %>%
  select(full_name, f_last, gsis_id, player_id, new_id) %>%
  saveRDS("current_most_complete_map.rds")
