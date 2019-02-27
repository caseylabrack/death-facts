library(tidyverse)
library(grid)

source("deth.R")

dev.off()

data = read_tsv("by-age.tsv") %>%
  select(3:5) %>% 
  rename(Age = `Single-Year Ages Code`) %>%
  mutate_all(as.numeric) %>%
  mutate(Rate = Deaths/Population) %>% 
  filter(!is.na(Population))

p = ggplot(data, aes(Age,Rate)) +
  geom_area(fill = hcl(60,25,85)) +
  scale_x_continuous(expand = c(0,0), labels = function(x){ if_else(x==0,"<1",paste0(x))}) +
  scale_y_continuous(expand = c(0,0), limits = c(0,.085),
                     labels = function (x) { x * 100 }, breaks = c(0,.01,.02,.04,.08)) +
  labs(title = "The Beginning of the End", subtitle = "Deaths per Hundred People by Age",
       caption = 'Data: "Underlying Cause of Death 1999-2017," WONDER Online Database, Centers for Disease Control and Prevention.') +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank())

g = ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 2
g$layout$l[g$layout$name == "subtitle"] <- 2
g$layout$l[g$layout$name == "caption"] <- 2
grid.draw(g)

ggsave("_export/rate.png", plot = g, width = 4.5, height = 3, dpi = 300, units = "in")