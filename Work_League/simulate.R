library(tidyverse)
current_standings = read_csv('current_standings.csv') %>% select(-X1)

matchups <- read_csv('to_simulate.csv') %>% select(-X1)


playoff_df <- as.data.frame(current_standings$Owners)
playoff_df$made <- 0
colnames(playoff_df)[1] <- "Team"


library(foreach)
n <- 7000
pb <- txtProgressBar(0, n, style = 2)
## Generate random df scores
t <- foreach(i=1:7000) %do% {
  setTxtProgressBar(pb, i)

  scores <- as.data.frame(rnorm(18, mean = matchups$avg_scores, sd = matchups$sd))
  colnames(scores) <- "value"
  scores$Team <- matchups$Team
  
  sched2 <- inner_join(scores,matchups)
  
  sched_sim <- sched2 %>% group_by(Matchup) %>% 
    mutate(win = ifelse(value>lag(value),1,0),
           win = ifelse(is.na(win), 1-sum(win,na.rm = TRUE), win),
           loss = 1-win) %>% ungroup()
  
  sim_record <- sched_sim %>% group_by(Team) %>% 
    summarize(sim_win = sum(win),
              sim_loss = sum(loss),
              points_scored = sum(value)) %>% ungroup()
  
  ext_standings <- inner_join(current_standings,sim_record,by=c("Owners"="Team"))
  
  ext_standings <- ext_standings %>% 
    mutate(update_W = W + sim_win,
           update_L = L + sim_loss,
           win_pct = update_W/(update_L+update_W),
           upd_pts_scored = PTSF + points_scored)
  
  ### Select the Top in each division
  s <- ext_standings %>% group_by(Division) %>% 
    filter(win_pct == max(win_pct)) %>% mutate(playoffs=1)
  
  test <- ext_standings %>% anti_join(s) %>% arrange(-win_pct,-upd_pts_scored) %>% 
    mutate(rank= row_number(),
           playoffs = ifelse(rank<=6,1,0)) 
  
  final_playoffs <- s %>% full_join(test) %>% select(Owners,playoffs)
}

final_res <- bind_rows(t) %>% group_by(Owners) %>% 
  summarize(Playoff_Apps = sum(playoffs),
            pct_chance = Playoff_Apps/7000)

write_csv(final_res,"playoff_simulation.csv")
