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

# ---- logistic.regression ------
# replace "ps" and "pn" into "1" and "0"
# glm() function accepts integer for 1-0 
water.quality$depth[water.quality$depth=="ps"] <- 1 
water.quality$depth[water.quality$depth=="pn"] <- 0
# transform the valuable as numeric
water.quality$depth <- as.numeric(as.character(water.quality$depth))
water.quality %>% 
  (mutate_at(.,vars(3:11), funs((.-mean(.))/sd(.))))
         
         
         oxy ec cod bod nitrate amonium sat nhom)


# regression
wq.binomial.01 <- glm(depth ~ ph + cod + bod,
            data = water.quality,
            family = binomial
)
# show summary table
summary(wq.binomial.01)
# Akaike's information criterion
AIC(wq.binomial.01)
# plot the results
wq.binomial.01.plot <- 
  ggplot(water.quality, 
         aes(x=ph + cod + bod, 
             y=depth)
         ) + 
  geom_point() + 
  stat_smooth(method="glm", 
              method.args=list(family="binomial"), 
              se=TRUE
              ) +
  theme_classic()

