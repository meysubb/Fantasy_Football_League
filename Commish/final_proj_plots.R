library(tidyverse)

final_proj <- read_csv("Final-Projections.csv")
final_proj$X1 <- factor(final_proj$X1,levels = unique(final_proj$X1))

#colnames(final_proj) <- final_proj[1,]
#final_proj <- final_proj[-1,]
final_proj <- as.data.frame(final_proj)

melt_plot_dat <- reshape2::melt(final_proj,id="X1")

ggplot(melt_plot_dat,aes(x=X1,y=value)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~variable) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(limits=final_proj$X1) + 
  labs(x="",y="Record",title="Simulated Records",caption="@msubbaiah1") + 
  scale_y_continuous(labels = scales::percent)

