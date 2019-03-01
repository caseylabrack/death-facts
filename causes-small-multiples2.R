library(tidyverse)
library(grid)

source("deth.R")

data = read_csv("by-age+cause.csv") %>% 
  select(-code)

data$cause = factor(data$cause)
data$cause = data$cause %>% fct_lump(n = 2, w = data$deaths)

p1 = ggplot(data, aes(age, deaths, fill = cause)) +
  geom_area() 
# +
#   facet_wrap(vars(cause), ncol = 5)

p1

numberOfCauses = 15

topCauses = data %>% 
  group_by(cause) %>% 
  summarize(total = sum(deaths)) %>% 
  top_n(numberOfCauses) %>% 
  arrange(-total)

shareOfDeaths = data %>%
  filter(cause %in% topCauses$cause) %>% 
  spread(key = cause, value = deaths) %>% 
  mutate(total = rowSums(.[2:(2+numberOfCauses-1)])) %>% 
  gather(key = cause, value = deaths, 2:(2+numberOfCauses-1)) %>% 
  mutate(share = deaths/total) %>% 
  select(-total,-deaths)

shareOfDeaths$cause = factor(shareOfDeaths$cause, levels = topCauses$cause)

p2 = ggplot(shareOfDeaths, aes(age,share)) +
  geom_area() +
  facet_wrap(vars(cause), ncol = 3)

# p2