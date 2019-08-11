library(tidyverse)
library(httr)
url <- "https://api.sleeper.app/v1/league/307655088971718656/rosters"


r_get <- GET(url)
s_content <- content(r_get)

extract_standings_info <- function(lst){
  sets <- as.data.frame(lst$settings)
  sets$user_id <- lst$owner_id
  return(sets)
}

s_clean <- lapply(s_content,extract_standings_info)
standings_df <- bind_rows(lapply(s_clean,as.data.frame.list))


users_url <- "https://api.sleeper.app/v1/league/307655088971718656/users"

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
