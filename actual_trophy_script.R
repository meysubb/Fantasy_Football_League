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

# Also realized I forgot to get real user names
user_url <- paste0(base_url, "users")
user_get <- GET(user_url)
user_content <- content(user_get)

user_df <- tibble(user_content) %>%
  unnest_wider(col = user_content) %>%
  unnest_wider(col = metadata, names_sep = "_") %>%
  select(user_id, metadata_team_name) %>%
  filter(!is.na(metadata_team_name))

# pbp data
pbp_2020 <- readRDS(
  url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds")
)

# Possible additional info for gsis IDs
gsis_ids <- read_csv(
  url("https://raw.githubusercontent.com/z-feldman/nfl_data/master/data/nfl_rosters.csv")
)

# I need to make some modifications so that this is joinable, currently there are IDs everywhere
# Easiest one would be to make this into pass and rush plays separately and create join rules
smaller_pbp <- pbp_2020 %>%
  select(play_id, game_id, game_date, home_team, away_team, week, posteam_type,
         yardline_100, game_seconds_remaining, game_half, qtr,
         down, goal_to_go, ydstogo, desc, play_type, yards_gained,
         start_time, game_seconds_remaining,
         total_home_score, total_away_score, complete_pass, ep, epa, wpa,
         air_epa, yac_epa, comp_air_epa, comp_yac_epa,
         air_wpa, yac_wpa, comp_air_wpa, comp_yac_wpa,
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
  arrange(desc(early_game)) %>%
  left_join(user_df, by = c("user" = "user_id"))

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
  mutate(week = as.double(week),
         previous_week_join = ifelse(week == 1, NA, week + 1)) %>%
  ungroup()

current_week <- lagged_performance %>%
  select(week, this_week = data, teams_involved)

previous_week <- lagged_performance %>%
  select(week = previous_week_join, last_week = data, teams_involved)

lagged_together <- current_week %>%
  left_join(previous_week, by = c("week", "teams_involved")) 

lagged_update <- lagged_together %>%
  mutate(this_length = map_dbl(this_week, ~length(.)),
         last_length = map_dbl(last_week, ~length(.))) %>%
  filter(week > 1, this_length > 0, last_length > 0) %>%
  mutate(new_data = map2(this_week, last_week,
                         ~ .x %>% inner_join(.y, by = "relevant_player_id",
                                             copy = TRUE))) %>%
  select(week, teams_involved, new_data) %>%
  unnest(new_data) %>%
  mutate(discrepancy = tot_fant_pts.x - tot_fant_pts.y,
         scaled_discrepancy = if_else(tot_fant_pts.y < 1,
                                      0,
                                      tot_fant_pts.x / tot_fant_pts.y)) %>%
  arrange(desc(scaled_discrepancy))

# Now bring this back
named_rosters %>%
  left_join(lagged_update %>%
              select(week, relevant_player_id, discrepancy, scaled_discrepancy) %>%
              filter(!is.na(relevant_player_id)),
            by = c("new_id" = "relevant_player_id")) %>%
  arrange(desc(scaled_discrepancy)) %>%
  left_join(user_df, by = c("user" = "user_id")) %>%
  View()

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
  slice(1) %>%
  left_join(user_df, by = c("user" = "user_id"))

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
  slice(1) %>%
  left_join(user_df, by = c("user" = "user_id"))

# 5. The Tenure of Jeff Fisher
# Closest team to the median but below
jeff_fisher <- recombined_fantasy %>%
  group_by(week, game_id, relevant_player_id) %>%
  summarize(tot_fant_pts = sum(fant_pt, na.rm = TRUE)) %>%
  filter(!is.na(relevant_player_id))

jeff_fisher_rosters <- named_rosters %>%
  filter(is_starter) %>%
  left_join(jeff_fisher, by = c("new_id" = "relevant_player_id")) %>%
  group_by(user) %>%
  summarize(tot_pts = sum(tot_fant_pts, na.rm=TRUE)) %>%
  arrange(desc(tot_pts)) %>%
  slice(6)

# 6. Fantasy Point Dont Matter
# Had the starters that generated the largest Total EPA in a given week
  # Passing: QBs get air_epa, receiver get yac_epa
epa_kings <- recombined_fantasy %>%
  group_by(week, game_id, relevant_player_id) %>%
  mutate(new_epa = case_when(position %in% "receiver" ~ yac_epa,
                             position %in% "passer" ~ air_epa,
                             TRUE ~ epa)) %>%
  summarize(tot_epa = sum(new_epa)) %>%
  filter(!is.na(relevant_player_id))

epa_team <- named_rosters %>%
  filter(is_starter) %>%
  left_join(epa_kings, by = c("new_id" = "relevant_player_id")) %>%
  filter(!is.na(week)) %>%
  group_by(week, user) %>%
  summarize(tot_epa_user = sum(tot_epa, na.rm=TRUE)) %>%
  arrange(desc(tot_epa_user)) %>%
  left_join(user_df, by = c("user" = "user_id"))

# 7. At Least We Won On the Field
# Had the starters that generated the largest Total WPA in a given week
wpa_kings <- recombined_fantasy %>%
  group_by(week, game_id, relevant_player_id) %>%
  mutate(new_wpa = case_when(position %in% "receiver" ~ yac_wpa,
                             position %in% "passer" ~ air_wpa,
                             TRUE ~ wpa)) %>%
  summarize(tot_wpa = sum(new_wpa)) %>%
  filter(!is.na(relevant_player_id))

wpa_team <- named_rosters %>%
  filter(is_starter) %>%
  left_join(wpa_kings, by = c("new_id" = "relevant_player_id")) %>%
  filter(!is.na(week)) %>%
  group_by(week, user) %>%
  summarize(tot_wpa_user = sum(tot_wpa, na.rm=TRUE)) %>%
  arrange(desc(tot_wpa_user)) %>%
  left_join(user_df, by = c("user" = "user_id"))

# A special attempt here: Can I do the matchup tracker?
# To do so I would need to make a global time tracker within games and then
# create a cumulative sum of fantasy points. I guess I would do this at a
# user level instead of a player level to get a more condensed file.
# From there I would just plot points against time facet_wrapped() by matchup.
# This sounds doable, only thing I need now is the start time and a time tracker
# Could do something like 3600 - game_time_remaining + start time
# Oh wow, the variable is actually called start_time
# Unique key is then game_date, start_time + game_seconds_remaining
matchup <- recombined_fantasy %>%
  mutate(game_date_check = paste0(game_date, " ", start_time),
         game_date_time = as.POSIXct(paste0(game_date, " ", start_time)),
         global_timer = game_date_time + 3600 - game_seconds_remaining) %>%
  arrange(global_timer) %>%
  group_by(relevant_player_id) %>%
  mutate(fant_pt_tracker = cumsum(fant_pt)) %>%
  ungroup()

# Now join back the appropriate owner and track
matchup_tracker <- matchup %>%
  filter(!is.na(relevant_player_id)) %>%
  inner_join(named_rosters %>% filter(is_starter) %>% select(new_id, user),
            by = c("relevant_player_id" = "new_id")) %>%
  filter(!is.na(user)) %>%
  arrange(global_timer) %>%
  group_by(user) %>%
  mutate(tot_user_fant_pts = cumsum(fant_pt)) %>%
  left_join(user_df, by = c("user" = "user_id"))

# Below is not yet working
adjust_date_times <- matchup_tracker %>%
  ungroup() %>%
  arrange(global_timer) %>%
  # Now its 0 to 4 essentially, but in terms of seconds
  # So its 0-3600, then 2 days + 0-3600
  # 10AM SUN to 8PM THURS is 4 + 24 + 24 + 10
  # 4PM MON to 9PM SUN to THURS is 16 + 3 + 62
  mutate(new_timer = global_timer - first(global_timer)) %>%
  mutate(adj_seconds = as.numeric(
    case_when(new_timer > 280000 ~ new_timer - 91 * 3600,
              new_timer > 10000 ~ new_timer - 62 * 3600,
              TRUE ~ new_timer)))

# Manually modify matchups this week
adjust_date_times <- adjust_date_times %>%
  mutate(matchups = case_when(
    metadata_team_name %in% c("We The North", "Run Run Pass Punt") ~ "1",
    metadata_team_name %in% c("Sundog Millionaire", "Republic of Dan Marino") ~ "2",
    metadata_team_name %in% c("WRGalore", "Pandamic ") ~ "3",
    metadata_team_name %in% c("Zelous Wheeler and Dealer", "Beef Ribs") ~ "4",
    TRUE ~ "5"))

# Quick check
adjust_date_times %>%
  ggplot(aes(x = adj_seconds, y = tot_user_fant_pts, col = metadata_team_name)) +
  geom_point() +
  geom_line() +
  labs(color = "Team Name") +
  xlab("Matchup Time") + ylab("Cumulative Fantasy Points") +
  ggtitle("Week 1 Matchups") +
  theme(axis.text.x = element_blank()) +
  theme_bw()

# Matchup 1
adjust_date_times %>%
  filter(matchups %in% "5") %>%
  ggplot(aes(x = adj_seconds, y = tot_user_fant_pts, col = metadata_team_name)) +
  geom_point() +
  geom_line(size = 2) +
  labs(color = "Team Name") +
  xlab("Matchup Time") + ylab("Cumulative Fantasy Points") +
  ggtitle("Week 1 Matchups") +
  theme_bw() +
  theme(axis.text.x = element_blank())

