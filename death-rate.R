library(tidyverse)
library(grid)

source("deth.R")

data = read_tsv("by-age.tsv") %>%
  select(3:5) %>% 
  rename(Age = `Single-Year Ages Code`) %>%
  mutate_all(as.numeric) %>%
  mutate(Rate = Deaths/Population) %>% 
  filter(!is.na(Population))

p = ggplot(data, aes(Age,Rate)) +
  geom_area(fill = hcl(60,25,85)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.08),
                     labels = function (x) { x * 100 }, breaks = c(0,.01,.02,.04,.08)) +
  ggtitle("The Beginning of the End", subtitle = "Deaths per Hundred People by Age") +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank())

g = ggplotGrob(p)

g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2

# grid.draw(g)

ggsave("_export/rate.png", plot = g, width = 5, height = 3, dpi = 300, units = "in")