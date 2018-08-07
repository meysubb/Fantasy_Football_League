### Draft Randomization - 2018 FF

## Randomize Draft Order

set.seed(21)
teams <- c("RB","Chai","RG", "Dhak","Saadu",
           "Suds","Subhang","Must","Bhargi","Mey")

prob_vec <- rep(0.10,10)

t <- replicate(1000,{sample(teams,replace=FALSE,prob=prob_vec)})
df <- data.frame(t)
s_df <- stack(df) %>% select(-ind) 
s_df <- s_df %>% mutate(
  order = rep(seq(1,10),times=nrow(s_df)/10)
)


draft_order <- s_df %>% group_by(values) %>% 
  summarize(avg_order = mean(order))
draft_order


tiebreaker <- c("Must","RB")
tb <- replicate(297,{sample(tiebreaker,replace=FALSE)})
t_df <- data.frame(tb)
ts_df <- stack(t_df) %>% select(-ind)
ts_df <- ts_df %>% mutate(
  order = rep(seq(1,2),times=nrow(ts_df)/2)
)
ts_final <- ts_df %>% group_by(values) %>% summarize(win_l = mean(order))
