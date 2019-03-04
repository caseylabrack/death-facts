library(tidyverse)

theme_set(
  theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 24, l = 0, unit = "pt"),
                                   face = "italic"),
      plot.caption = element_text(hjust = 0, size = rel(.6),
                                  margin = margin(t = 12, r = 0, b = 0, l = 0, unit = "pt")),
      legend.position = "bottom", legend.justification = "right",
      legend.key.size = unit(9, "pt"),
      plot.margin = margin(t = 6, r = 12, b = 2, l = 6, unit = "pt")
    )
)

defaultColor = hcl(60,25,80)