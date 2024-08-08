# LIBRARIES ----
library(tidyverse)

# INPUT ----
selection <- "Vikings"

# DATA ----
dat <- filter(teaminfo_elf, Franchise == selection) |> 
  select(Season, Conference, Division) |> 
  left_join(standings, by = c("Season", "Conference", "Division")) |> 
  mutate(Conf = paste(Conference, "Conf."),
         Div = paste(Division, "Div."),
         ConfDiv = case_when(is.na(Conference) ~ Div,
                             is.na(Division) ~ Conf,
                             TRUE ~ paste(Conf, Div, sep = " / ")))

# VISUALISE ----
ggplot(dat) +
  aes(x = Week, y = Pct, color = Franchise) +
  geom_line(linewidth = 1.5, alpha = 2/3) +
  geom_point(size = 2.5, alpha = 3/4) +
  scale_x_continuous(breaks = seq(2, 20, by = 4), minor_breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(lim = c(0, 1)) +
  scale_color_viridis_d(option = "H") +
  # scale_color_brewer(palette = "YlOrBr") +
  facet_wrap(~Season+ConfDiv, ncol = 2) +
  labs(title = paste("ELF RS Ongoing for the", selection),
       subtitle = "Opponents according to Conference/Division") +theme_bw(base_size = 13) +
  theme(panel.background = element_rect(fill = "seagreen", colour = NA)) -> p

## Plot ----
windows(16, 9)
plot(p)

# CLEAN UP ----
rm(selection, dat, p)