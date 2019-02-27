library(tidyverse)

theme_set(
  theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 24, l = 0, unit = "pt"),
                                   face = "italic"),
      plot.caption = element_text(hjust = 0, size = rel(.6),
                                  margin = margin(t = 12, r = 0, b = 0, l = 0, unit = "pt"))
    )
)