library(tidyverse)
library(grid)

source("deth.R")

data = read_csv("by-age+cause.csv") 

ranked = data %>%
  select(-code) %>%
  spread(key = cause, value = deaths) %>%
  mutate_at(vars(-age), cumsum) %>%
  gather(key = cause, value = deaths, 2:16) %>% 
  filter(age==100) %>% 
  select(-age) %>% 
  arrange(-deaths) %>% 
  top_n(9)

colors = setNames(colors, levels(data$cause))

data$cause = factor(data$cause, levels = ranked$cause)

colors = c("#65999e",
           "#623d3b",
           "#9fcac0",
           "#3c4327",
           "#d5c8ab",
           "#2d554d",
           "#c29693",
           "#61816b",
           "#94886d")

topNineCauses = data %>% filter(cause %in% ranked$cause)

ggplot(topNineCauses, aes(x = age, y = deaths, fill = cause)) +
  geom_area() +
  facet_wrap(vars(cause), ncol = 3) +
  scale_y_continuous(name = "", expand = c(0,0), breaks = seq(0,350e3,150e3)) +
  scale_x_continuous(name = "Age", expand = c(0,0)) +
  scale_fill_manual(values = colors) +
  labs(title = "Popular Maladies",
       subtitle = "Deaths by Causes and Age",
       caption = 'Data: "Underlying Cause of Death 1999-2014," WONDER Online Database, Centers for Disease Control and Prevention.') +
  theme(legend.position = "none",
        plot.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(24,"pt"))

ggsave("_export/top-causes-multiples.png", width = 5, height = 5, dpi = 300, units = "in")