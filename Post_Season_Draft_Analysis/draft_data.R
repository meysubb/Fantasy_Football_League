#### Draft Assesment 

url <- "http://games.espn.com/ffl/tools/draftrecap?leagueId=657141"

library(rvest)
library(tidyverse)
options(stringsAsFactors = FALSE)
dat <- read_html(url) %>% html_nodes(".tableBody td") %>% html_text()

te <- matrix(dat,nrow=150,ncol=3,byrow = TRUE)
test <- as.data.frame(te)
colnames(test) <- c("draft_pos","player_name","fantasy_team")
test$draft_pos <- as.numeric(test$draft_pos)

test$player <- trimws(gsub("[*].*","",gsub("[,].*","",test$player_name)))
test$team <- trimws(gsub(".*[,]","",test$player_name))
test$player_name <- NULL
### Fantasy Football Analytics 

ffa <- read_csv("ffa_customrankings2017-0.csv")

ffa_sel <- ffa %>% select(player,playerposition,adp,points,lower,upper) %>% 
  rename(exp_pts= points)
### Change player names
test$player <- gsub("[']","",test$player)
test$player <- gsub("(^\\s+)|(\\s+$)", "", gsub("D/ST","",test$player))
test$player[20] <- trimws(gsub("II","",test$player[20]))
test$player <- trimws(gsub("Jr.","",test$player))
test$player <- trimws(gsub("Sr.","",test$player))
draft_data <- inner_join(test,ffa_sel)
setdiff(test$player,draft_data$player)
### Edit D/ST to remove duplicates
#dst <- test %>% filter(str_detect(player, 'D/ST'))
#dst <- dst$player %>% 
#  word_split() %>% 
#  sapply(., function(x) unbag(unique(x))) %>% 
#  rm_white_endmark() %>%  
#  rm_default(pattern="(^[a-z]{1})", replacement = "\\U\\1") %>%
#  unname()
#test$player[grep("D/ST",test$player)] <- dst


### Get Actual scores
### Not including D/ST
reps <- seq(0,500,by=50)
scoring_ls <- lapply(reps,function(x){
  url <- paste0("http://games.espn.com/ffl/leaders?startIndex=",x,"&leagueId=657141")
  dat <- read_html(url) %>% html_nodes("#playertable_0") %>% html_table()
  df <- dat[[1]]
  df <- df[-1,]
  play_team <- trimws(str_split_fixed(df$X1, ",", 2))
  df[,1:2] <- play_team
  colnames(df) <- df[1,]
  df <- df[-(1),]
  df <- df[ , -which(names(df)=='NA')]
  pat <- c(".1",".2",".3")
  rep <- c("_RUN","_REC","_MISC")
  colnames(df) <- mgsub(pat,rep,colnames(df))
  return(df)
})
# Transform scorers list to dataframe
scoring_df <- bind_rows(scoring_ls)


url <- "http://games.espn.com/ffl/leaders?startIndex=0&leagueId=657141"
# Get Kickers
k_dat <- read_html(url) %>% html_nodes("#playertable_1") %>% html_table()
k_df <- k_dat[[1]]
k_df <- k_df[-1,]
k_team <- trimws(str_split_fixed(k_df$X1, ",", 2))
k_df[,1:2] <- k_team
colnames(k_df) <- k_df[1,]
k_df <- k_df[-1,]
k_df <- k_df[ , -which(names(k_df)=='NA')]

### Get Defenses
d_dat <- read_html(url) %>% html_nodes("#playertable_2") %>% html_table() 
d_df <- d_dat[[1]]
d_df <- d_df[-1,]
d_team <- trimws(str_split_fixed(d_df$X1, " ", 2))
d_df[,1:2] <- d_team
colnames(d_df) <- d_df[1,]
colnames(d_df)[1] <- "PLAYER"
d_df <- d_df[-1,]
d_df <- d_df[ , -which(names(d_df)=='NA')]


players <- full_join(k_df,scoring_df)
all_players <- full_join(players,d_df)

all_players$PLAYER <- trimws(gsub("[*]","",all_players$PLAYER))
all_players$PLAYER <- trimws(gsub("Jr.","",all_players$PLAYER))
all_players$PLAYER <- trimws(gsub("Sr.","",all_players$PLAYER))
all_players$PLAYER <- gsub("[']","",all_players$PLAYER)
all_players$PLAYER[52] <- trimws(gsub("II","",all_players$PLAYER[52]))
## Clean up player names. 
setdiff(draft_data$player,all_players$PLAYER)

saveRDS(all_players,"Post_Season_Draft_Analysis/all_players_raw.RDS")
all_players_sel <- all_players %>% select(PLAYER,`TEAM POS`,PTS) %>% 
  mutate(PTS = as.numeric(PTS))

final_dat <- draft_data %>% inner_join(.,all_players_sel,by=c("player"="PLAYER"))
saveRDS(final_dat,"Post_Season_Draft_Analysis/final_players_data.RDS")
