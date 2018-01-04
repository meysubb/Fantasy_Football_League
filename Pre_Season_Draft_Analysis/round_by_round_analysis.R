library(tidyverse)
options(stringsAsFactors = FALSE)
data <- read.csv("ffa_customrankings2017-0.csv")
data <- data %>% mutate(diff = upper - lower)
### first round analysis
### No QB 

first_round <- data %>%  
  filter(!(playerposition %in% c("QB","TE")),adp <= 25,
         diff<40,
         adpDiff < 0) %>% arrange(-upper)
  
selected <- first_round$player
second_round <- data %>% filter(!(player %in% selected),
                                !(playerposition %in% c("QB","TE")),
                                adp <= 40) %>% 
  filter(diff <= quantile(diff,0.75),risk<=5) %>% arrange(-upper)
selected <- c(selected,second_round$player)

third_round <- data %>% filter(!(player %in% selected),
                               !(playerposition %in% c("QB","TE")),
                               adp <= 40,
                               adpDiff <= 20) %>% arrange(-upper)
selected <- c(selected,third_round$player)
fourth_round <- data %>% filter(!(player %in% selected),
                                !(playerposition %in% c("QB")),
                                adp<= 60) %>% arrange(-upper)

selected <- c(selected,fourth_round$player)

fifth_round <- data %>% filter(!(player %in% selected),
                               adp<= 70) %>% arrange(-upper)

qb <- data %>% filter(!(player %in% selected), playerposition == "QB") %>% 
  arrange(-upper) %>% filter(adp<=100)

selected <- c(selected,fifth_round$player)
remainder <- data %>% filter(!(player %in% selected),
                             !(playerposition %in% c("QB","K")),adp<=150) %>% 
  arrange(-upper)

kicker <- data %>% filter(playerposition == "K") %>% arrange(-upper)

def <- data %>% filter(playerposition == "DST") %>% arrange(-upper)
def_floor <- data %>% filter(playerposition == "DST") %>% arrange(diff)
