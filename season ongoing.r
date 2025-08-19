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
                             TRUE ~ paste(Conf, Div, sep = " / "))) |> 
  left_join(teaminfo_elf, by = c("Season", "Franchise"))

# VISUALISE ----
ggplot(dat) +
  aes(x = Week, y = Pct, group = Franchise, color = Franchise == "Vikings") +
  # geom_line(linewidth = 1.5, alpha = .85) +
  geomtextpath::geom_textline(aes(label = Abb3), size = 4, alpha = .85, linewidth = 1.5, hjust = .99, show.legend = FALSE) +
  geom_point(mapping = aes(shape = bye), size = 3, alpha = .85, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2, 20, by = 4), minor_breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(lim = c(0, 1)) +
  scale_shape_manual(values = c(19, NA)) +
  scale_color_manual(values = c("grey50", "purple")) +
  facet_wrap(~Season+ConfDiv, ncol = 2) +
  labs(title = paste("ELF RS Ongoing for the", selection),
       subtitle = "Opponents according to Conference/Division") +theme_bw(base_size = 13) +
  theme(panel.background = element_rect(fill = "lightgreen", colour = NA),
        panel.grid = element_line(color = "white")) -> p

## Plot ----
windows(16, 9)
plot(p)

# CLEAN UP ----
rm(selection, dat, p)

