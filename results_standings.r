# LIBRARIES ----
library(tidyverse)

# SOURCEN ----
source("teams.r")
source("import.r")

# DATA ----
## results ----
raw <- data_raw %>% 
  select(-file) %>% 
  unnest_longer(Data) |> unpack(Data)

results <- bind_rows(
  raw %>% rename(Team = Guest, Opponent = Home, PF = Pts_G, PA = Pts_H) %>% add_column(Home = FALSE),
  raw %>% rename(Team = Home, Opponent = Guest, PF = Pts_H, PA = Pts_G) %>% add_column(Home = TRUE)
) %>%
  mutate(Result = case_when(PF > PA ~ "W",
                            PF < PA ~ "L",
                            PF == PA ~ "T",
                            TRUE ~ NA_character_)) %>% 
  relocate(Home, .after = "Team") %>% 
  arrange(Season, Week, hour(Kickoff), minute(Kickoff)) |>
  left_join(teaminfo_elf, by = c("Team", "Season")) |>
  nest(Teamdata = Franchise:Division) |> 
  left_join(teaminfo_elf, by = c("Opponent" = "Team", "Season")) |> 
  nest(Oppdata = Franchise:Division) |> 
  rowwise() |>
  mutate(GameID = case_when(Home ~ paste0(Teamdata["Abb"], Oppdata["Abb"], Season%%100, sprintf("%02d", Week)), # rowwise wg Season und Week
                            TRUE ~ paste0(Oppdata["Abb"], Teamdata["Abb"], Season%%100, sprintf("%02d", Week))),
         GameID = str_replace(GameID, "97", "WC"),
         GameID = str_replace(GameID, "98", "PO"),
         GameID = str_replace(GameID, "99", "FI")) |> 
  ungroup() |> 
  filter(!is.na(PF))

### 2023 season Leipzig Kings folded after week 5 - games @/vs Cologne Centurions score 16-16 counted as W for Cologne
results <- rows_update(results, tibble(GameID = c("CCLK2307", "LKCC2312"), Team = "Leipzig Kings", Result = "L"), by = c("GameID", "Team"))
results <- rows_update(results, tibble(GameID = c("CCLK2307", "LKCC2312"), Team = "Cologne Centurions", Result = "W"), by = c("GameID", "Team"))
### Following the Leipzig Kings folding in week 12 Prague gave the home right to Fehervar, who otherwise may have lost two home games
### -> this only applies to the location the game was held, for statistics reasons nothing changed

rm(raw)

## standings ----
# all season x week combinations ----
base <- filter(results, Week < 30) |> # no PS games
  select(Season, Week) |>
  unique() |>
  bind_rows(tibble(Season = c(2022, 2023), Week = c(10L, 10L))) |> # general bye weeks in 2022 and 2023 in W10
  arrange(Season, Week)

# corresponding teams
base <- left_join(base,
                  teaminfo_elf,
                  by = c("Season"),
                  relationship = "many-to-many")

# join results ----
data <- left_join(base,
                  select(results, Season, Week, Team, PF, PA, Result) |> 
                    mutate(Result = factor(Result, levels = c("W", "L", "T", "bye"))),
                  by = c("Season", "Week", "Team"))

# add bye information
data <- replace_na(data,
                   replace = list(PF = 0, PA = 0, Result = "bye")) # 

#  ----
standings <- mutate(data, one = 1L) |> 
  pivot_wider(names_from = Result, values_from = one, names_expand = TRUE, values_fill = list(one = 0)) |> 
  group_by(Season, Team) |> 
  mutate(across(c(PF, PA, W, L, T), ~cumsum(.))) |>
  ungroup() |> 
  mutate(WLT = case_when(T == 0 ~ paste0("(", W, "-", L, ")"),
                         TRUE ~ paste0("(", W, "-", L, "-", T, ")")),
         Pct = num((W + 1/2 * T) / (W + L + T), digits = 3),
         bye = bye == 1) |> 
  select(!c(Abb, W:T)) |>
  relocate(bye, .after = Week) |> 
  arrange(Season, Franchise, Week)

# CLEAN UP ----
rm(base, data)

## OLD VERSION standings ----
# standings <- results %>%
#   select(Season, Week, Kickoff, Team, Teamdata, Result, PF, PA) %>% 
#   unnest(Teamdata) |> 
#   # group_by(Season) %>% 
#   complete(Week = 1:max(Week[Week < 30]), nesting(Season, Team), # add bye-weeks
#            fill = list(PF = 0, PA = 0)) %>% 
#   # ungroup() %>%
#   mutate(Result = case_when(is.na(Result) ~ "bye",
#                             TRUE ~ Result),
#          W = ifelse(Result == "W", 1, 0),
#          L = ifelse(Result == "L", 1, 0),
#          T = ifelse(Result == "T", 1, 0),
#          Post = Week > 30) %>% 
#   group_by(Season, Team, Post) %>%
#   mutate_at(.vars = vars(PF:T), .funs = cumsum) %>% 
#   select(Season, Week, Kickoff, Team, Franchise, Division, Conference, Result, Post, PFc = PF, PAc = PA, W:T) %>%
#   ungroup() %>%
#   mutate(Pct = ((W + 1/2 * T) / (W + L + T)) %>%
#       round(3),
#       WLT = case_when(
#         T == 0 ~ paste0("(", W, "-", L, ")"),
#         TRUE ~ paste0("(", W, "-", L, "-", T, ")"))) %>% 
#   select(Season:Team, Franchise:Conference, Kickoff:WLT) %>% 
#   arrange(Season, Week, -Pct, -PFc, PAc)

# RESPONSE ----
cat("..ELF > results and standings generated ✔\n")
