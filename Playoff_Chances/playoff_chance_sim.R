library(tidyverse)
options(stringsAsFactors = FALSE)
standings <- read_csv("sleeper_current_standings.csv")

standings <- standings %>% mutate(
  avg_pts = (fpts + fpts_decimal/100)/(wins+losses)
)


playoff_df <- as.data.frame(standings$name)
playoff_df$made <- 0
colnames(playoff_df)[1] <- "Team"

score_std_dev <- sd(standings$avg_pts)

sched <- read_csv("r_sched_2.csv")
sched <- sched %>% mutate(
  Week = as.numeric(Week),
  Matchup = as.numeric(Matchup)
)
#sched <- sched %>% select(-X4,-X5)

library(foreach)
n <- 5000
pb <- txtProgressBar(0, n, style = 2)
## Generate random df scores
t <- foreach(i=1:5000) %do% {
  setTxtProgressBar(pb, i)
  scores <- as.data.frame(replicate(3,rnorm(10, mean = standings$avg_pts, sd = score_std_dev)))
  scores$Team <- standings$name
  ## Need to find a better way to extract this
  colnames(scores)[1:3] <- c(11,12,13)
  scores <- reshape2::melt(scores)
  colnames(scores)[2] <- "Week"
  scores$Week <- as.numeric(scores$Week)
  scores$Week[scores$Week == 1] <- 11
  scores$Week[scores$Week == 2] <- 12
  scores$Week[scores$Week == 3] <- 13
  
  
  sched2 <- inner_join(scores,sched)
  sched_sim <- sched2 %>% group_by(Week,Matchup) %>% 
    mutate(win = ifelse(value>lag(value),1,0),
           win = ifelse(is.na(win), 1-sum(win,na.rm = TRUE), win),
           loss = 1-win) %>% ungroup()
  
  sim_record <- sched_sim %>% group_by(Team) %>% 
    summarize(sim_win = sum(win),
              sim_loss = sum(loss),
              points_scored = sum(value)) %>% ungroup()
  
  
  ext_standings <- inner_join(standings,sim_record,by=c("name"="Team"))
  ext_standings <- ext_standings %>% 
    mutate(update_W = wins + sim_win,
           update_L = losses + sim_loss,
           win_pct = update_W/(update_L+update_W),
           upd_pts_scored = ppts + points_scored) %>% 
    arrange(-win_pct,-upd_pts_scored) %>% 
    mutate(rank= row_number(),
           playoffs = ifelse(rank<=6,1,0)) 
  
  playoff_odds <- ext_standings %>% select(name,playoffs)
  inner_join(playoff_df,playoff_odds,by=c("Team"="name"))
}


final_res <- bind_rows(t) %>% group_by(Team) %>% 
  summarize(Playoff_Apps = sum(playoffs),
         pct_chance = Playoff_Apps/5000)
