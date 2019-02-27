library(tidyverse)

theme_set(
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 24, l = 0, unit = "pt"),
                                   face = "italic")
    )
)