library(tidyverse)
library(grid)

source("deth.R")

# dev.off()

data = read_csv("by-age+cause.csv") %>% 
  select(-code)

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
  select(-total,-deaths) %>% 
  mutate(cause = toupper(cause))

# levels(shareOfDeaths$cause) = toupper(levels(shareOfDeaths$cause))

mean = shareOfDeaths %>%
  group_by(cause) %>% 
  summarize(mean = mean(share)) %>% 
  arrange(mean)
  
shareOfDeaths$cause = factor(shareOfDeaths$cause, levels = rev(mean$cause))

p = ggplot(shareOfDeaths, aes(age,share)) +
  geom_area(fill = defaultColor) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.6), 
                     breaks = seq(0,.6,.2), labels = function (x) { if_else(x==.6, "60%", as.character(x * 100)) }) +
  scale_x_continuous(expand = c(0,0), name = "Age") +
  labs(title = "Common Causes of Death at Every Age", subtitle = "Share of Deaths (Versus Other Top Causes)",
       caption = 'Data: "Underlying Cause of Death 1999-2017," WONDER Online Database, Centers for Disease Control and Prevention.') +
  facet_wrap(vars(cause), ncol = 3) +
  theme(axis.title.y = element_blank(),
        panel.spacing = unit(12,"pt")) +
  theme(strip.text = element_text(face = "bold", size = 9, color = hcl(0,0,30)))

g = ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2
g$layout$l[g$layout$name == "caption"] <- 2
grid.draw(g)

ggsave("export/common-causes_thumb.png", plot = g, width = 5, height = 8, dpi = 300, units = "in")