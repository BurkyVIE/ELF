# LIBRARIES ----
library(tidyverse)
library(rnaturalearth)
library(sp)
library(ggrepel)

# SOURCE TEAMS ----
source("teams.r")

# DATA ----
# seas <- 2021; kind <- "Division"; textcol <- "black"
# seas <- 2022; kind <- "Conference"; textcol <- "grey50"
# seas <- 2023; kind <- "Conference"; textcol <- "white"
# seas <- 2024; kind <- "Conference"; textcol <- "white"
seas <- 2025; kind <- "Division"; textcol <- "white"


# data <- teamloc_elf %>% filter(Season == seas)
data <- teamloc_elf |>
  filter(Season == seas) |>
  group_by(Franchise) |>
  summarise(Lat = as.vector(t(Gs) %*% Lat), Long = as.vector(t(Gs) %*% Long),
            Conference = first(Conference),
            Division = first(Division))

mapdata <- ne_countries(scale = 50, returnclass = "sf")

with(data %>% filter(!is.na(Long)), SpatialPoints(cbind(Long, Lat))) %>%
  bbox() -> bbo

# VISUALISE ----
p <- ggplot() +
  geom_sf(data = mapdata, fill = "grey90", col = "grey30", size = 1.25) +
  # geom_point(data, mapping = aes(x = Long, y = Lat), color = "red3", size = 4) +
  geom_label_repel(data, mapping = aes(x = Long, y = Lat, label = Franchise, fill = .data[[kind]]), colour = textcol,
                   box.padding = unit(.6, "lines"), point.padding = unit(.4, "lines"), force_pull = .5,
                   label.r = unit(0.5, "lines"), label.size = NA, fontface = "bold", show.legend = TRUE) +
  ggimage::geom_image(data, mapping = aes(x = Long, y = Lat), image = "fb_32.png", size = .02) +
  # scale_fill_manual(name = "Conference", values = c(Western = "#1f3f77", Central = "#008000", Eastern = "#cd2028"), na.value = "#a39819") +
  scale_fill_manual(name = kind, values = colors_elf[[kind]] |> filter(Season == seas) |> select(Name, Color) |> deframe(), na.value = "#a39819") +
  scale_x_continuous(name = "Longitude", expand = expansion(mult = .05), labels = (\(x) paste0(x, "°", ifelse(x < 0, "W", "E")))) + # solves problems with incorrect interpretation of 'degrees'
  scale_y_continuous(name = "Latitude", expand = expansion(mult = .05), labels = (\(y) paste0(y, "°", ifelse(y < 0, "S", "N")))) +
  coord_sf(bbo[1,], bbo[2,], expand = TRUE) +
  labs(title = "ELF - European League of Football",
       subtitle = paste(seas, "Season")) +
  theme_bw(base_size = 12) +
  theme(panel.background = element_rect(fill = "#606e8c"),
        legend.position = "top",
        legend.key = element_rect(fill="white"))

xRange <- ggplot_build(p)$layout$panel_params[[1]]$x_range %>% diff()
yRange <- ggplot_build(p)$layout$panel_params[[1]]$y_range %>% diff()

windows(16, yRange * 16 / xRange * 1.5) # 1.5 ist ca Verhältnis Lat zu Long
plot(p)

# CLEAN UP ----
rm(seas, kind, textcol, data, mapdata, bbo, xRange, yRange, p)
