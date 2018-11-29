library(tidyverse)

final_proj <- read_csv("Final-Projections.csv")
final_proj$X1 <- factor(final_proj$X1,levels = unique(final_proj$X1))

#colnames(final_proj) <- final_proj[1,]
#final_proj <- final_proj[-1,]
final_proj <- as.data.frame(final_proj)
melt_plot_dat <- reshape2::melt(final_proj,id="X1")

final_proj$win <- seq(0,12) 
melt_plot_dat_2 = final_proj %>% select(-X1) %>% reshape2::melt(id='win')
exp_wins <- melt_plot_dat_2 %>% group_by(variable) %>% summarize(
  expected_wins = sum(win*value)
)
colnames(exp_wins)[1] <- "Owners"


# grid <- with(melt_plot_dat, seq(min(value), max(value), length.out = 150))
# dens <- plyr::ddply(melt_plot_dat, "variable", function(x){
#   data.frame(
#     value = grid,
#     density = dnorm(grid, mean(x$value), sd(x$value))
#   )
# })

ggplot(melt_plot_dat,aes(x=X1,y=value,fill=value)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~variable,scales="free_x") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(limits=final_proj$X1) + 
  scale_fill_gradient(low='blue', high='red') + 
  labs(x="",y="Record",title="Simulated Records",caption="@msubbaiah1") + 
  scale_y_continuous(labels = scales::percent)

int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 

### Another plot of luck
current_stands <- read_csv("current_standings.csv") %>% select(-X1)

luck_stands <- current_stands %>% inner_join(exp_wins)

luck_stands <- luck_stands %>% mutate(pts_scored_rank =rank(-PTSF),
                                      Luck = rank-pts_scored_rank)

library(ggrepel)
ggplot(luck_stands,aes(y=W,x=pts_scored_rank,label=Owners)) + 
  geom_point() +
  geom_text_repel(aes(label = paste(Owners,":" ,Luck), color = Luck), vjust =1.5, size=3) + 
  scale_colour_gradient(low = "red", high = "black") +
  theme_bw() +
  geom_abline(slope=1) + 
  geom_text(aes(x = 8, y = 8, label = "Line of Luck"), angle = 35, nudge_y = .6) + 
  scale_x_continuous(breaks=int_breaks,limits=c(0,18)) +
  scale_y_continuous(breaks=int_breaks,limits=c(0,18)) + 
  labs(title= "Are You Over-Performing or Under-Performing?",
       subtitle = "The Luck Index measures how much better (or worse)\n your team is doing than what you should expect.",
       y="Actual Wins",x="Points Scored",caption="@msubbaiah1")
