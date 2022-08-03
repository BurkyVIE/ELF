# libraries ----
library(tidyverse)

# mangle data ----
results %>%
  filter(!is.na(Result)) %>% 
  mutate(Result = ordered(Result, levels = c("W", "L", "T"))) %>% 
  group_by(Team) %>% 
  summarise(Results = list(Result), .groups = "drop",
            Gs = n(),
            PFc = sum(PF),
            PAc = sum(PA)) %>% 
  mutate(map_df(Results, ~unlist(.) %>% table(.)),
         across(W:T, as.integer),
         Pct = (W + 1/2 * T) / (W + L + T),
         PRat = PFc / (PFc + PAc)) %>% 
  arrange(desc(Pct), desc(PFc / Gs), (PAc / Gs))
