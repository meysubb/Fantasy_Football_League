# Zelus fantasy league

library(tidyverse)
library(httr)
base_url <- "https://api.sleeper.app/v1/league/604138427104952320/"
url <- paste0(base_url, "rosters") 


r_get <- GET(url)
s_content <- content(r_get)

extract_standings_info <- function(lst){
  sets <- as.data.frame(lst$settings)
  sets$user_id <- lst$owner_id
  return(sets)
}

s_clean <- lapply(s_content,extract_standings_info)
standings_df <- bind_rows(lapply(s_clean,as.data.frame.list))


users_url <- paste0(base_url, "users")

us_get <- GET(users_url)
us_content <- content(us_get)

extract_df <- function(lst){
  user_id <- lst$user_id
  display_name <- lst$display_name
  vec <- c(user_id,display_name)
  names(vec) <- c("user_id","name")
  return(vec)
}

user_names <- lapply(us_content, extract_df)
user_df <- bind_rows(lapply(user_names, as.data.frame.list))


final_df <- standings_df %>% inner_join(user_df)
write_csv(final_df,"sleeper_current_standings.csv")

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

hold <- rosters_df %>% slice(1) %>% 
  mutate(fix = map2(starters, players, fix_fn))

# Do these IDs match nflfastR?
seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

# No, have to name match which is kind of lame
# all_players_url <- "https://api.sleeper.app/v1/players/nfl"
# all_players <- GET(all_players_url)
# all_players_cont <- content(all_players)
# 
# # I can probably convert this to tibble
# players_tibble <- tibble(data = all_players_cont) %>%
#   unnest_wider(data) %>%
#   unnest_wider(fantasy_positions)
# 
# # Rename positions
# players_tibble <- players_tibble %>%
#   rename("primary_position" = `...1`, 
#          "secondary_position" = `...2`,
#          "tertiary_position" = `...3`)
# saveRDS(players_tibble, "players_and_ids.rds")
players_tibble <- readRDS("players_and_ids.rds")

# Can I slice off the first set of digist in the pbp id's and join with sportsradar_id?
mod_pbp <- pbp %>%
  select(passer_player_id, passer_player_name) %>%
  group_by(passer_player_id, passer_player_name) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(passer_player_id = str_remove(passer_player_id, "[0-9]+\\-"))

mod_sleeper <- players_tibble %>%
  filter(position %in% "QB") %>%
  mutate(adj_sportradar = str_remove(sportradar_id, "[0-z]+\\-")) %>%
  select(full_name, adj_sportradar)

# Fails :(
mod_pbp %>%
  inner_join(mod_sleeper, by = c("passer_player_id" = "adj_sportradar"))

# Checking for id matching with rds files from ffanalytics. Need to use load() bc .rda
load("C:/Users/reyers9597/Downloads/ff_player_data.rda")
load("C:/Users/reyers9597/Downloads/nfl_cols.rda")

ff_player_data %>% View()

# Stuff from nflfastR
rosters_nflfastR <- readRDS("C:/Users/reyers9597/Downloads/roster.rds") %>%
  select(teamPlayers.displayName, teamPlayers.gsisId) %>%
  distinct()
legacy_mappings <- readRDS("C:/Users/reyers9597/Downloads/legacy_id_map.rds")

# Heyyy this has nflid and gsis id
  # Surely at least one works?
# First gsis
unique_sleeper <- players_tibble %>%
  select(full_name, gsis_id, player_id) %>%
  distinct() %>%
  mutate(gsis_id = trimws(gsis_id)) %>%
  filter(!is.na(gsis_id))

sleeper_and_nflfastr <- unique_sleeper %>%
  left_join(rosters_nflfastR, by = c("gsis_id" = "teamPlayers.gsisId"))

# Legacy mappings are even better somehow? But only for old players
# Need the same gsis -> new_id mapping that they have here
seasons <- 2010:2019
legacy_pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/legacy-data/play_by_play_{x}.rds")
    )
  )
})

legacy_pbp_players <- legacy_pbp %>%
  select(season, week, home_team, away_team, play_id, ends_with("player_id")) %>%
  mutate(week = if_else(week == 22, 21, week)) %>%
  pivot_longer(ends_with("player_id"),
               names_to = "player_desc",
               values_to = "gsis_id",
               values_drop_na = TRUE)

pbp_players <- pbp %>%
  select(season, week, home_team, away_team, play_id, ends_with("player_id")) %>%
  pivot_longer(ends_with("player_id"),
               names_to = "player_desc",
               values_to = "new_id",
               values_drop_na = TRUE)

custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x <- x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

both_eras_id_map <- legacy_pbp_players %>%
  left_join(
    pbp_players,
    by = c("season", "week", "home_team", "away_team", "play_id", "player_desc")
  ) %>%
  group_by(gsis_id) %>%
  mutate(new_id = custom_mode(new_id)) %>%
  ungroup() %>%
  select(gsis_id, new_id) %>%
  distinct() %>%
  select( gsis_id, new_id) %>%
  #filter(gsis_id %in% both_eras$teamPlayers.gsisId) %>% 
  arrange(gsis_id) %>%
  drop_na()

# Lets see if this mapping fixes things
full_map <- unique_sleeper %>%
  left_join(both_eras_id_map, by = "gsis_id") %>%
  filter(!is.na(new_id))

saveRDS(full_map, "id_mapping.rds")
# It does! There are some duplicates with different new_id but same gsis_id
# This is probably what that custom_mode() function handles but I cant find in package