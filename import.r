# libraries ----
library(tidyverse)

# einlesen ----

## einlese-funktion ----
import <- function(df) {
  df %>% 
    mutate(Season = as.integer(substr(file, 1, 4)),
           Data = map(.x = file,
                      .f = ~ read_csv(.,
                                      lazy = FALSE,
                                      col_types = cols(Week = col_integer(),
                                                       Kickoff = col_character(),
                                                       Home = col_character(),
                                                       Guest = col_character(),
                                                       Pts_H = col_integer(),
                                                       Pts_G = col_integer()))
                      %>% mutate(Kickoff = as.POSIXct(Kickoff)))) %>% 
    relocate(Season) %>% 
    return()
}

## lese files ein ----
data_raw <- dir()[dir() %>% str_ends("ELF.txt")] %>%
  tibble(file = .) %>%
  import()

rm(import)

# results ----
raw <- data_raw %>% 
  select(-file) %>% 
  unnest(cols = Data)

results <- bind_rows(
  raw %>% rename(Team = Guest, Opponent = Home, PF = Pts_G, PA = Pts_H) %>% add_column(Home = FALSE),
  raw %>% rename(Team = Home, Opponent = Guest, PF = Pts_H, PA = Pts_G) %>% add_column(Home = TRUE)
) %>%
  mutate(Result = case_when(PF > PA ~ "W",
                            PF < PA ~ "L",
                            PF == PA ~ "T",
                            TRUE ~ NA_character_)) %>% 
  relocate(Home, .after = "Team") %>% 
  arrange(Kickoff)

rm(raw)

# standings ----
standings <- results %>%
  select(Season, Week, Kickoff, Team, Result, PF, PA) %>% 
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
  select(Season, Week, Kickoff, Team, Result, Post, PFc = PF, PAc = PA, W:T) %>%
  ungroup() %>%
  mutate(Pct = ((W + 1/2 * T) / (W + L + T)) %>%
      round(3),
    WLT = case_when(
      T == 0 ~ paste0("(", W, "-", L, ")"),
      TRUE ~ paste0("(", W, "-", L, "-", T, ")"))) %>% 
  left_join(teaminfo_elf, by = c("Season", "Team")) %>% # add remaining Infos
  select(Season:Team, Franchise:Conference, Kickoff:WLT) %>% 
  arrange(Season, Week, -Pct, -PFc, PAc)
