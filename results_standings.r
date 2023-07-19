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
  arrange(Kickoff) |>
  left_join(teaminfo_elf, by = c("Team", "Season")) |>
  nest(Teamdata = Franchise:Division) |> 
  left_join(teaminfo_elf, by = c("Opponent" = "Team", "Season")) |> 
  nest(Oppdata = Franchise:Division) |> 
  rowwise() |>
  mutate(GameID = case_when(Home ~ paste0(Teamdata["Abb"], Oppdata["Abb"], Season%%100, sprintf("%02d", Week)), # rowwise wg Season und Week
                            TRUE ~ paste0(Oppdata["Abb"], Teamdata["Abb"], Season%%100, sprintf("%02d", Week))),
         GameID = str_replace(GameID, "98", "PO"),
         GameID = str_replace(GameID, "99", "FI")) |> 
  ungroup() |> 
  filter(!is.na(PF))

### 2023 season Leipzig Kings breakdown after week 5 - games @/vs Cologne Centurions score 16-16 counted as W dor Cologne
results <- rows_update(results, tibble(GameID = c("CCLK2307", "LKCC2312"), Team = "Leipzig Kings", Result = "L"), by = c("GameID", "Team"))
results <- rows_update(results, tibble(GameID = c("CCLK2307", "LKCC2312"), Team = "Cologne Centurions", Result = "W"), by = c("GameID", "Team"))

rm(raw)

## standings ----
standings <- results %>%
  select(Season, Week, Kickoff, Team, Teamdata, Result, PF, PA) %>% 
  unnest(Teamdata) |> 
  # group_by(Season) %>% 
  complete(Week = 1:max(Week[Week < 30]), nesting(Season, Team), # add bye-weeks
           fill = list(PF = 0, PA = 0)) %>% 
  # ungroup() %>%
  mutate(Result = case_when(is.na(Result) ~ "bye",
                            TRUE ~ Result),
         W = ifelse(Result == "W", 1, 0),
         L = ifelse(Result == "L", 1, 0),
         T = ifelse(Result == "T", 1, 0),
         Post = Week > 30) %>% 
  group_by(Season, Team, Post) %>%
  mutate_at(.vars = vars(PF:T), .funs = cumsum) %>% 
  select(Season, Week, Kickoff, Team, Franchise, Division, Conference, Result, Post, PFc = PF, PAc = PA, W:T) %>%
  ungroup() %>%
  mutate(Pct = ((W + 1/2 * T) / (W + L + T)) %>%
      round(3),
      WLT = case_when(
        T == 0 ~ paste0("(", W, "-", L, ")"),
        TRUE ~ paste0("(", W, "-", L, "-", T, ")"))) %>% 
  select(Season:Team, Franchise:Conference, Kickoff:WLT) %>% 
  arrange(Season, Week, -Pct, -PFc, PAc)

# RESPONSE ----
cat("..ELF > results and standings generated ✔\n")

