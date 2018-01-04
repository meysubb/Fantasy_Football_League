players <- readRDS("Post_Season_Draft_Analysis/final_players_data.RDS")

### Basic plot before develop ranks
players_df <- players %>% mutate(
  diff = PTS - exp_pts,
  round = ceiling(draft_pos/10)
) %>% select(-`TEAM POS`)

players_df <- players_df[-2,]
players_df <- players_df[-12,]

labels <- players_df %>% filter(diff>50)
library(ggplot)
library(ggrepel)

ggplot(players_df,aes(x=exp_pts,y=PTS,color=playerposition)) + 
  geom_point(show.legend = F) + 
  geom_text_repel(data=labels,aes(label=player),show.legend = F) + 
  labs(x="Projected Points",y="Actual Points",
         title="Fantasy Football Performance",
         subtitle="2017 Season - ESPN",
       caption="@msubbaiah1") +
  theme_bw()

### Baseline Projections by Position (drafted players only)

projected_by_position <- players_df %>% group_by(playerposition) %>% 
  summarize(players_drafted=n(),
            avg_projected = mean(exp_pts),
            avg_value = mean(PTS),
            avg_diff = mean(diff)) %>% ungroup()

projected_by_round <- players_df %>% group_by(round) %>% 
  summarize(players_drafted=n(),
            avg_projected = mean(exp_pts),
            avg_value = mean(PTS),
            avg_diff = mean(diff)) %>% ungroup()

###
q <- players_df %>% mutate(
  val = diff/round,
  val2 = diff/draft_pos
) 

### PCA 
pca_train <- players_df %>% select(draft_pos,exp_pts,PTS)
prin_comp <- prcomp(pca_train, scale. = T)

library(ggfortify)
library(ggrepel)
biplot(prin_comp,xlabs=players_df[,3])

autoplot(prin_comp, data = players_df, colour = 'playerposition',label=T,shape=F) 
  


