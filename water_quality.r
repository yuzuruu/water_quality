# ---- load.packages ----
library(tidyverse)
library(GGally)
library(viridis)
library(viridisLite)

# ---- read.data ----
water.quality <- 
  readxl::read_excel("waterfactor.xlsx",
                     sheet = "water.factor.wet",
                     range = "A01:L31"
                     )
# omit unnecessary valuable
water.quality <- 
  water.quality %>% 
  dplyr::select(-Position)

# ---- pairs ----
# draw a pair plot
pairs.water.quality <- 
  water.quality %>% 
  ggpairs(columns = c(3:11),
          aes(colour = depth, 
              alpha = 0.5
              )
          ) 
print(pairs.water.quality)
# Save the pair plot
# ggsave("pairs.water.quality.png")


# hoge <- princomp(water.quality[,c(3:11)],
#                  scale = T
#                  )
# summary(hoge)
