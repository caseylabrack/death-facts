library(tidyverse)
library(grid)

source("deth.R")

data = read_csv("by-age+cause.csv") %>% 
  select(-code)

data$cause = factor(data$cause)
data$cause = data$cause %>% fct_lump(n = 2, w = data$deaths)

dataOthers = data %>% 
  filter(cause == "Other") %>% 
  group_by(cause, age) %>% 
  summarize(deaths = sum(deaths)) 

dataAll = data %>%
  filter(cause != "Other") %>% 
  bind_rows(dataOthers)

dataAll$cause = factor(dataAll$cause, levels = rev(dataAll$cause %>% unique()))

ggplot(dataAll, aes(age, deaths, fill = cause)) +
  geom_area() +
  scale_x_continuous(expand = c(0,0), name = "Age") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1000000)) +
  scale_fill_manual(values = c(defaultColor, hcl(0,0,40), hcl(0,0,20)),
                    labels = c("Next 13 biggest causes", "Cancer", "Heart disease"),
                    guide = guide_legend(reverse = TRUE),
                    name = "") +
  labs(title = "It'll Probably be Heart Disease or Cancer", subtitle = "Total Deaths by Age",
       caption = 'Data: "Underlying Cause of Death 1999-2017," WONDER Online Database, Centers for Disease Control and Prevention.\n\nNote: Age 100 is a catch-all for all ages above 99 in the CDC data, hence the spike of deaths at that age.') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank())

ggsave("_export/the-big-two.png", width = 5, height = 5, dpi = 300, units = "in")