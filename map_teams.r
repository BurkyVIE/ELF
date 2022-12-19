# LIBRARIES ----
library(tidyverse)
library(rnaturalearth)
library(sp)
library(ggrepel)

# SOURCE TEAMS ----
source("teams.r")

# DATA ----
seas <- 2023

data <- teamloc_elf %>% filter(Season == seas)

mapdata <- ne_countries(scale = 10, returnclass = "sf")

with(data %>% filter(!is.na(Long)), SpatialPoints(cbind(Long, Lat))) %>%
  bbox() -> bbo

# VISUALISE ----
p <- ggplot() +
  geom_sf(data = mapdata, fill = "grey60", col = "white", size = 1.25) + #26ab65
  # geom_point(data, mapping = aes(x = Long, y = Lat), color = "red3", size = 4) +
  geom_label_repel(data, mapping = aes(x = Long, y = Lat, label = Franchise, fill = Conference), colour = "white",
                   box.padding = unit(.6, "lines"), point.padding = unit(.4, "lines"), force_pull = .5,
                   label.r = unit(0.5, "lines"), label.size = NA, fontface = "bold", show.legend = TRUE) +
  ggimage::geom_image(data, mapping = aes(x = Long, y = Lat), image = "fb_32.png", size = .02) +
  scale_fill_manual(name = "Conference", values = c(Western = "#1f3f77", Central = "#008000", Eastern = "#cd2028"), na.value = "#a39819") +
  scale_x_continuous(name = "Longitude", expand = expansion(mult = .05), labels = (\(x) paste0(x, "°", ifelse(x < 0, "W", "E")))) + # solves problems with incorrect interpretation of 'degrees'
  scale_y_continuous(name = "Latitude", expand = expansion(mult = .05), labels = (\(y) paste0(y, "°", ifelse(y < 0, "S", "N")))) +
  coord_sf(bbo[1,], bbo[2,], expand = TRUE) +
  labs(title = "ELF - European League of Football",
       subtitle = paste(seas, "Season")) +
  theme_bw(base_size = 12) +
  theme(panel.background = element_rect(fill = "#606e8c"))

xRange <- ggplot_build(p)$layout$panel_params[[1]]$x_range %>% diff()
yRange <- ggplot_build(p)$layout$panel_params[[1]]$y_range %>% diff()

windows(16, yRange * 16 / xRange * 1.5) # 1.5 ist ca Verhältnis Lat zu Long
plot(p)

# CLEAN UP ----
rm(seas, data, mapdata, bbo, xRange, yRange, p)
