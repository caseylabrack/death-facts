library(tidyverse)
library(grid)
library(showtext)
library(extrafont)

# font_add_google("Rock Salt", "rock")
# font_add_google("Gochi Hand", "gochi")
# font_add_google("Lobster", "lobster")
# 
showtext_auto()
# 
quartz()

theme_set(
  theme_bw() +
    theme(strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold", margin = margin(t = 6, r = 0, b = 24, l = 0, unit = "pt")),
          strip.background = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(24,"pt"),
          axis.ticks.x = element_blank(),
          # axis.line.x = element_line(size = .25),
          plot.caption = element_text(margin = margin(t=24,r=0,l=0,b=0,unit = "pt")))
)

data = read_csv("data.csv") 

ranked = data %>%
  select(-code) %>%
  spread(key = cause, value = deaths) %>%
  mutate_at(vars(-age), cumsum) %>%
  gather(key = cause, value = deaths, 2:16) %>% 
  filter(age==100) %>% 
  select(-age) %>% 
  arrange(-deaths) %>% 
  top_n(9)

data$cause = factor(data$cause, levels = rev(ranked$cause))

# colors = c(
#   "#264759",
#   "#d2d7bc",
#   "#264f4b",
#   "#ceb9a1",
#   "#585859",
#   "#a3cedb",
#   "#5f563e",
#   "#9fb3cd",
#   "#907d79")

colors = c("#65999e",
           "#623d3b",
           "#9fcac0",
           "#3c4327",
           "#d5c8ab",
           "#2d554d",
           "#c29693",
           "#61816b",
           "#94886d")

setNames(colors, levels(data$cause))

topNineCauses = data %>% filter(cause %in% ranked$cause)

# ggplot(topNineCauses, aes(x = age, y = deaths, fill = cause)) +
#   geom_area() +
#   facet_wrap(vars(cause), ncol = 3) +
#   scale_y_continuous(name = "", expand = c(0,0), breaks = seq(0,350e3,150e3)) +
#   scale_x_continuous(name = "Age", expand = c(0,0)) +
#   scale_fill_manual(values = colors) +
#   # labs(title = "Top Nine Causes of Death, 1997–2014", caption = "Source: Underlying Cause of Death Database, 1999–2014,\nCenters for Disease Control and Prevention.") +
#   theme(legend.position = "none",
#         plot.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank())
# 
# ggsave("multiples.pdf", width = 10, height = 4.5, units = "in", dpi = 600)

filled = ggplot(data = topNineCauses, mapping = aes(x = age, y = deaths, fill = cause)) +
  geom_area(position = "fill") +
  geom_text(data = topNineCauses %>% filter(age==100),
            mapping = aes(label = paste0(" ", cause)),
            position = position_fill(vjust = .5),
            hjust = 0) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = c(0,0), limits = c(0,100), name = "Age") +
  scale_y_continuous(expand = c(0,0), labels = function (x) { if_else(x==1, "100%", format(x * 100, nsmall = 0))}) +
  theme(legend.position = "none",
        plot.margin = margin(t = 10, r = 100, b = 0, l = 0, unit = "pt"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(family = "Courier New"))

filled

gt <- ggplotGrob(filled)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

ggsave("fill.png", plot = gt, width = 10, height = 4.5, units = "in", dpi = 600)
# ggsave("fill.pdf", width = 10, height = 4.5, units = "in", dpi = 600)