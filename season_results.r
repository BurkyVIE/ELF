library(tidyverse)

# data ----
he <- results |>
  select(Season, Week, Team, Result, PF, PA) |> 
  mutate(Result = factor(Result, levels = c("W", "L", "T")),
         Part = case_when(Week < 20 ~ "RS", TRUE ~ "PS"),
         EoS = case_when(Week < 20 ~ NA_character_,
                         Week == 97 ~ "lost WC",
                         Week == 98 ~ "lost PO",
                         Week == 99 & Result == "L" ~ "lost FI",
                         TRUE ~ "won FI"),
         one = 1L) |>
  pivot_wider(names_from = Result, values_from = one, names_expand = TRUE, values_fill = list(one = 0)) |> 
  group_by(Season, Part, Team) |> 
  summarise(across(c(PF, PA, W, L, T), ~sum(.)), EoS = last(EoS), .groups = "drop") |>
  mutate(WLT = case_when(T == 0 ~ paste0("(", W, "-", L, ")"),
                         TRUE ~ paste0("(", W, "-", L, "-", T, ")")),
         Pct = num((W + 1/2 * T) / (W + L + T), digits = 3)) |> 
  relocate(EoS, .after = Pct)

# seeds ----
seeds <- read_delim("Scores/Seeds.txt", quote = "'", col_types = "iic", lazy = FALSE)

# result----
EoS_results <- left_join(teaminfo_elf |> relocate(Season) |> arrange(Season, Franchise),
                         left_join(filter(he, Part == "RS") |> select(-c(Part, W:T, EoS)),
                                   filter(he, Part == "PS") |> select(-c(Part, W:T)),
                                   by = c("Team", "Season"), suffix = c("_RS", "_PS")),
                         by = c("Season", "Team")) |> 
  left_join(seeds, by = c("Season", "Franchise")) |> 
  relocate(Seed, .after = "Pct_RS")

rm(he)
