

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(mlbstatsR)
library(ggtext)
library(ggimage)
library(stringr)

theme_ivo <- function() {
  theme_minimal(base_size = 9, base_family = "Chivo") %+replace%
    theme(
      panel.grid.minor = element_line(colour = "#e1e1e1"),
      panel.grid.major = element_line(color = "#e1e1e1"),
      plot.background = element_rect(fill = "#f4f4f4", color = "#f4f4f4"),
      plot.caption = element_markdown(size = 8.5)
    )
}

# Data Wrangler -----------------------------------------------------------

t <- get_reference_team_standings()
d <- get_mlb_teams()

d <- d %>%
  select(tm = name, liga, division) %>%
  mutate(
    division = str_replace_all(division, c("NL" = "", "AL" = "")),
    division = str_squish(division),
    liga = case_when(
      liga == "NL" ~ "National",
      liga == "AL" ~ "American",
      TRUE ~ liga
    )
  ) %>%
  left_join(t) %>%
  select(tm, liga, division, East = v_east, Central = v_cent, West = v_west)

df <- d %>%
  pivot_longer(cols = East:West) %>%
  group_by(tm) %>%
  mutate(score = ifelse(division == name, value, "")) %>%
  filter(score != "") %>%
  separate(score, c("w", "l"), sep = "-") %>%
  mutate_at(c("w", "l"), as.numeric) %>%
  select(tm, liga, division, w, l)

teamColor <- get_png_logos() %>%
  select(tm = full_name, logo = logologodefault)


# plot ------------------------------------------------------------------

p <- df %>%
  left_join(teamColor) %>%
  ggplot(aes(l, w)) +
  geom_image(aes(image = logo), size = .12) +
  facet_wrap(~ liga + division, scales = "free") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(17.5, 50.5), breaks = seq(18, 50, 5), position = "top") +
  scale_y_continuous(limits = c(14.5, 45.5), breaks = seq(15, 45, 5)) +
  theme_ivo() +
  theme(
    strip.placement = "outside",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 9, hjust = 0.5),
    axis.text.y = element_text(size = 9, hjust = 0.5),
    axis.title.x = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.title = element_markdown(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold"),
    plot.margin = margin(15, 25, 20, 5)
  ) +
  labs(
    x = "\nDerrotas",
    y = "Victorias",
    title = "Partidos Ganados vs Perdidos en la ***MLB***",
    subtitle = paste0("Entre equipos de la misma division to ", format(Sys.Date(), format = "%d %B, %Y")),
    caption = "<br>**Datos**: *mlbstatsR y @baseball_ref*  **Gráfico**: *Ivo Villanueva* **@elcheff**."
  )
ggsave("victoriasvsdivision.png", p, w = 12, h = 12, dpi = 300, type = "cairo")

# 7 de Septiembre 2021 -----------------------------------------------------
# Ivo Villanueva ----------------------------------------------------------
