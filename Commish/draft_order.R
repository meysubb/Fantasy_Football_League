## Randomize Draft Order

set.seed(76)
teams <- c("RB","Chai","RG", "Dhak","Saadu",
         "Suds","Just","Must","Bhargi","Mey")

library(foreach)
draft_order_random <- foreach(i = 1:100, .combine='rbind',.packages='mosaic') %do% {
  prob_vec <- rep(0.10,10)
  test <- resample(teams,replace=FALSE,prob=prob_vec)
  vec <- seq(1:10)
  result <- data.frame(name = test,ord=vec)
  result
}

draft_order_random$name <- as.factor(draft_order_random$name)

draft_order_random %>% group_by(name) %>% summarize(avg_order = mean(ord)) %>%
  arrange(avg_order)

#### For the 12 man league with my Analytics peeps
set.seed(55)
teams <- c("Jessica","Tim","Chris", "Nick","Corey","Brett",
           "Josh","Kyle","Reed","Ryan","Mey","Gihani")


draft_order_r <- foreach(i = 1:100, .combine='rbind',.packages='mosaic') %do% {
  prob_vec <- rep(1/12,12)
  test <- resample(teams,replace=FALSE,prob=prob_vec)
  vec <- seq(1:12)
  result <- data.frame(name = test,ord=vec)
  result
}

draft_order_r$name <- as.factor(draft_order_r$name)

draft_order_r %>% group_by(name) %>% summarize(avg_order = mean(ord)) %>%
  arrange(avg_order)

