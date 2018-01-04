library(tidyverse)
options(stringsAsFactors = FALSE)
data <- read.csv("ffa_customrankings2017-0.csv")
data <- data %>% mutate(diff = upper - lower)

### RB Budget
rb1 <- data %>% filter(playerposition=="RB",auctionValue > 30)
rb2 <- data %>% filter(playerposition=="RB",auctionValue > 18, auctionValue <=30)
names_rb <- c(rb1$player,rb2$player)

remaining_rbs <- data %>% filter(playerposition=="RB",!(player %in% names_rb),
                                 auctionValue>5) %>% arrange(-upper)
### WR Budget
wr1 <- data %>% filter(playerposition == "WR",auctionValue >25)
wr2 <- data %>% filter(playerposition == "WR",auctionValue >10,auctionValue<=25)
names_wr <- c(wr1$player,wr2$player)

remaining_wrs <- data %>% filter(playerposition=="WR",!(player %in% names_wr),
                                 auctionValue>5) %>% arrange(-upper)

## TE Budget
te <- data %>% filter(playerposition == "TE",auctionValue>=5)
names_te <- te$player

### Flex Budget
flex <- data %>% filter(playerposition %in% c("RB","WR","TE"),auctionValue>=8,
                        !(player %in% c(names_rb,names_wr,names_te))) %>% arrange(-upper)

### QB Budget
qb <- data %>% filter(playerposition == "QB",auctionValue<=22,auctionValue>3)
