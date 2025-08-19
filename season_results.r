# LIBRARIES ----
library(tidyverse)

# DATA ----
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
         Pct = num((W + 1/2 * T) / (W + L + T), digits = 3, label = "rd_3")) |> 
  relocate(EoS, .after = Pct)

## Strength of Schedule ----
SoS <- function(Sea, Tea) {
  all_opps <- filter(results, Season == Sea, Team == Tea) |> group_by(Season, Opponent) |> summarise(n = n(), .groups = "drop")
  opp_gs <-left_join(all_opps, EoS_results |> filter(Season == Sea) |> select(Season, Team, Pct = Pct_RS), by = c("Season" = "Season", "Opponent" = "Team"))
  res <- as.numeric(t(opp_gs$n) %*% opp_gs$Pct / sum(opp_gs$n))
  return(res)
}

## Strength of Victory ----
SoV <- function(Sea, Tea) {
  def_opps <- filter(results, Season == Sea, Team == Tea, Result == "W") |> group_by(Season, Opponent) |> summarise(n = n(), .groups = "drop")
  opp_gs <- left_join(def_opps, EoS_results |> filter(Season == Sea) |> select(Season, Team, Pct = Pct_RS), by = c("Season" = "Season", "Opponent" = "Team"))
  res <- if(dim(opp_gs)[1] == 0) 0 else as.numeric(t(opp_gs$n) %*% opp_gs$Pct / sum(opp_gs$n))
  return(res)
}

## Seeds ----
seeds <- read_delim("Scores/Seeds.txt", quote = "'", col_types = "iic", lazy = FALSE)

# RESULT ----
EoS_results <- left_join(teaminfo_elf |> relocate(Season) |> arrange(Season, Franchise),
                         left_join(filter(he, Part == "RS") |> select(-c(Part, W:T, EoS)),
                                   filter(he, Part == "PS") |> select(-c(Part, W:T)),
                                   by = c("Team", "Season"), suffix = c("_RS", "_PS")),
                         by = c("Season", "Team")) |>
  mutate(SoV = num(map2_dbl(Season, Team, ~SoV(.x, .y)), digits = 3, label = "rd_3"),
         SoS = num(map2_dbl(Season, Team, ~SoS(.x, .y)), digits = 3, label = "rd_3")) |> 
  left_join(seeds, by = c("Season", "Franchise")) |> 
  relocate(c(SoV, SoS, Seed), .after = "Pct_RS")

# CLEAN UP ----
rm(he, SoV, SoS, seeds)

# RESPONSE ----
cat("\033[1;34m..ELF >\033[0m EoS_results generated \033[1;92m✔\033[0m\n")