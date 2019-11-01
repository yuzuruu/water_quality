# water quality and ecosystem service
# Est: 01st. November 2019


library(tidyverse)
library(dplyr)
library(GGally)

rice.bid <- readxl::read_excel("rice_bid_ecosystem_service.xlsx")

rice.bid.01 <- 
  rice.bid %>% 
  dplyr::mutate(choice = as.factor(choice),
                bid = as.factor(bid)
  )

rice.bid.01.pairs.01 <- 
  rice.bid.01 %>% 
  ggpairs(columns = 3:ncol(rice.bid.01),
          aes(colour = choice,
              alpha = 0.5)
  )
rice.bid.01.pairs.02 <- 
  rice.bid.01 %>% 
  ggpairs(columns = 3:ncol(rice.bid.01),
          aes(colour = bid,
              alpha = 0.5
              )
  )

ggsave("rice.bid.01.pdf", 
       plot = rice.bid.01.pairs.01,
       width = 100,
       height = 100,
       units = "cm"
)

ggsave("rice.bid.02.pdf", 
       plot = rice.bid.01.pairs.02,
       width = 100,
       height = 100,
       units = "cm"
)
