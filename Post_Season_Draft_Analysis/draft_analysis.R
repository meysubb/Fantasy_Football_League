players <- readRDS("Post_Season_Draft_Analysis/final_players_data.RDS")

### Basic plot before develop ranks
players_df <- players %>% mutate(
  diff = PTS - exp_pts,
  round = ceiling(draft_pos/10)
) %>% select(-`TEAM POS`)

players_df <- players_df[-2,]
players_df <- players_df[-12,]



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



pl_test <- players_df %>% group_by(playerposition) %>% 
  mutate(posrank = rank(desc(PTS)))

### linear regression again
lin <- lm(draft_pos~exp_pts + playerposition+posrank,data=pl_test)

t <- pl_test %>% select(PTS,playerposition,posrank) 
colnames(t)[1] <- "exp_pts"
pl_test$pr_draftp <- predict(lin,t)

order <- order(pl_test$pr_draftp)

pl_test$pr_draft_round[order] <- c(rep(1:14, each=10),rep(15,6))

par(mfrow = c(2,2))
plot(lin,which=1:4)
### Plot 1 (Residuals vs. Fitted)
# linear assumption seems realistic
# variance of error terms are equal (or so it seems)
# a few outliers but not many

### Plot 2 (Q-Q plot)
## errors are normally distributed
# with the exception of 3 points. (1,135,143)
# corresponds to David Johnson, 

