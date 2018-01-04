players_df <- readRDS("Post_Season_Draft_Analysis/final_players_data.RDS")

### Basic plot before develop ranks
players_df <- players_df %>% mutate(
  diff = PTS - exp_pts
)

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
