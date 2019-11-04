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
# replace "ps" and "pn" into "1" and "0"
# glm() function accepts integer for 1-0 
water.quality$depth[water.quality$depth=="ps"] <- 1 
water.quality$depth[water.quality$depth=="pn"] <- 0
water.quality$depth <- as.character(water.quality$depth)
# transform the valuable as numeric
water.quality$depth <- as.character(water.quality$depth)
water.quality.scale <- 
  water.quality %>% 
  mutate_at(.,vars(3:11), funs((.-mean(.))/sd(.)))
#
## --- END ---

# ---- pairs.plot ----
# draw a pair plot
# original numbers
pairs.water.quality <- 
  water.quality %>% 
  ggpairs(columns = c(3:11),
          aes(colour = depth, 
              alpha = 0.5
          )
  ) 
print(pairs.water.quality)
# Save the pair plot
# When you would like to save the figure,
# comment out the code below and run.
# ggsave("pairs.water.quality.png")

#scaled numbers
pairs.water.quality.scale <- 
  water.quality.scale %>% 
  ggpairs(columns = c(3:11),
          aes(colour = depth, 
              alpha = 0.5
          )
  ) 
print(pairs.water.quality.scale)
# Save the pair plot
# When you would like to save the figure,
# comment out the code below and run.
# ggsave("pairs.water.quality.scale.png")
#
## --- END ---

# ---- logistic.regression ------
# regression
water.quality.scale$depth <- as.numeric(as.character(water.quality$depth))
wq.binomial.01 <- glm(depth ~ ph + cod + bod,
                      data = water.quality.scale,
                      family = binomial
)
# show summary table
summary(wq.binomial.01)
# Akaike's information criterion
# AIC:33.52473
AIC(wq.binomial.01)

# plot the results
wq.binomial.01.plot <- 
  ggplot(water.quality.scale, 
         aes(x=ph + cod + bod, 
             y=depth)
  ) + 
  geom_point() + 
  stat_smooth(method="glm", 
              method.args=list(family="binomial"), 
              se=TRUE
  ) +
  theme_classic()
print(wq.binomial.01.plot)

#
## --- END ---


# Salinity content by period
# ---- read.data ----
salinity.water <- 
  readxl::read_excel("saliwater.xlsx",
                     sheet = "sali",
                     range = "A02:G07"
  )
colnames.salinity.water <- colnames(salinity.water)

# ---- rename and reshape data
salinity.water.data <- 
  salinity.water %>% 
  tidyr::gather(key = "month", 
                value = area, colnames.salinity.water[2:length(colnames.salinity.water)]) %>% 
  dplyr::rename(salinity = "Salinity content (%0)",
                month = month,
                area = area
                ) %>% 
  dplyr::mutate(
    salinity = factor(salinity),
    month = factor(month)
  )

# ---- draw a barplot ----
salinity.water.data %>% 
  ggplot(aes(x = month, 
             y = area
             )
         ) +
  geom_bar(aes(fill = salinity),
           stat = "identity",
           position = "stack"
  ) +
  scale_fill_viridis_d(
    option = "viridis") +
  labs(x = "Month", 
       y = "Area (Unit: ha)"
       ) +
  theme_classic()

#
## --- END ---
