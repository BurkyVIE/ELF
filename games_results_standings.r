# LIBRARIES ----
library(tidyverse)

# SOURCEN ----
source("teams.r")
source("import.r")

# DATA ----
## games ----
games <- data_raw |> 
  select(-file) |> 
  unnest_longer(Data) |> # full list of game data
  unpack(Data) |> 
  left_join(teaminfo_elf, by = c("Guest" = "Team", "Season")) |> # enrich team info
  nest(Guestdata = Franchise:Division) |> 
  left_join(teaminfo_elf, by = c("Home" = "Team", "Season")) |> 
  nest(Homedata = Franchise:Division) |>
  rowwise() |> # rowwise wg Season und Week
  mutate(GameID = paste0(Homedata$Abb2, Guestdata$Abb2, Season%%100, sprintf("%02d", Week)), # generate GameID
         GameID = str_replace(GameID, "97", "WC"),
         GameID = str_replace(GameID, "98", "PO"),
         GameID = str_replace(GameID, "99", "FI")) |> 
  ungroup() |> 
  relocate(GameID, .after = "Pts_H")

## results ----
results <- bind_rows(
  games |> rename(Team = Guest, Opponent = Home, PF = Pts_G, PA = Pts_H, Teamdata = Guestdata, Oppdata = Homedata) |> add_column(Home = FALSE),
  games |> rename(Team = Home, Opponent = Guest, PF = Pts_H, PA = Pts_G, Teamdata = Homedata, Oppdata = Guestdata) |> add_column(Home = TRUE)) |>
  filter(!is.na(PF)) |> 
  mutate(Result = case_when(PF > PA ~ "W",
                            PF < PA ~ "L",
                            PF == PA ~ "T",
                            TRUE ~ NA_character_)) |> 
  relocate(Result, Home, .after = "PA") |> 
  arrange(Season, Week, Kickoff)

### 2023 season Leipzig Kings folded after week 5 - games @/vs Cologne Centurions score 16-16 counted as W for Cologne
results <- rows_update(results, tibble(GameID = c("CCLK2307", "LKCC2312"), Team = "Leipzig Kings", Result = "L"), by = c("GameID", "Team"))
results <- rows_update(results, tibble(GameID = c("CCLK2307", "LKCC2312"), Team = "Cologne Centurions", Result = "W"), by = c("GameID", "Team"))
### Following the Leipzig Kings folding in week 12 Prague gave the home right to Fehervar, who otherwise may have lost two home games
### -> this only applies to the location the game was held, for statistics reasons nothing changed

## standings ----
# all season x week combinations ----
base <- filter(results, Week < 30) |> # no PS games
  select(Season, Week) |>
  unique() |>
  bind_rows(tibble(Season = c(2022L, 2023L, 2025L), Week = c(10L, 10L, 12L))) |> # general bye weeks in 2022 and 2023 in W10 and 2025 in W12
  arrange(Season, Week)

# corresponding teams ----
base <- left_join(base,
                  teaminfo_elf,
                  by = c("Season"),
                  relationship = "many-to-many")

# join results ----
data <- left_join(base,
                  select(results, Season, Week, Team, PF, PA, Result) |> 
                    mutate(Result = factor(Result, levels = c("W", "L", "T", "bye"))),
                  by = c("Season", "Week", "Team"))

# add bye information ----
data <- replace_na(data,
                   replace = list(PF = 0, PA = 0, Result = "bye"))

# standings ----
standings <- mutate(data, one = 1L) |> 
  pivot_wider(names_from = Result, values_from = one, names_expand = TRUE, values_fill = list(one = 0)) |> 
  group_by(Season, Team) |> 
  mutate(across(c(PF, PA, W, L, T), ~cumsum(.))) |>
  ungroup() |> 
  mutate(WLT = case_when(T == 0 ~ paste0("(", W, "-", L, ")"),
                         TRUE ~ paste0("(", W, "-", L, "-", T, ")")),
         Pct = num((W + 1/2 * T) / (W + L + T), digits = 3),
         bye = bye == 1) |> 
  select(!c(Abb2, Abb3, W:T)) |>
  relocate(bye, .after = Week) |> 
  arrange(Season, Franchise, Week)

# CLEAN UP ----
rm(base, data)

# RESPONSE ----
cat("..ELF > games, results and standings generated ✔\n")


