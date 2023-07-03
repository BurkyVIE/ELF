# function ----
scoring <- function(GameID = "RTVV2201") {

  ## libraries ----
  library(tidyverse)
    
  ## import GB ----
  compgb <- str_subset(readLines(paste0("GB/", GameID, ".txt")), "--- pagebreak ---", negate = TRUE) # pagebreaks entfernen
  
  ## find Scoring Summary ----
  a <- which(str_starts(compgb, "Qtr Time")) + 1    # scoring summary from...
  b <- which(str_starts(compgb, "Kickoff time")) -3 # ...to
  
  compgb[a:b] %>%
    enframe(value = "Data", name = NULL) %>%
    mutate(Quarter = as.integer(str_sub(Data, 1, 1)),
           Time = (lubridate::hms(paste0("0:",str_sub(Data, 5, 9)))),
           # Score = str_sub(Data, -7, -1)) %>% # no good, may catch last digit from Drive Info: "   0-0" is only six digits
           Score = str_extract(Data, "   \\d-\\d|\\d+ - \\d+")) %>% # assumes that there are always three blanks before the Score
    fill(Quarter, .direction = "down") %>% 
    separate(Score, sep = "-", into = c("Visitor", "Home"), convert = TRUE) %>% 
    mutate(across(Visitor:Home, ~as.integer(.)),
           ElapsedTime = as.numeric((lubridate::minutes(15 * (Quarter - 1))) + lubridate::minutes(15) - Time) / 60,
           Points = (Home + Visitor) - lag((Home + Visitor), 1, default = 0),
           Scoredby = str_sub(Data, 11, 12),
           GameID = GameID) %>% 
    return()
}


# graph ----

## magle data ----
GB_info %>%
  filter(str_detect(GameID, "RF"), str_sub(GameID, 5, 6) == "23", !str_ends(GameID, "PO|FI")) %>% # Vienna Vikings, Season == 2023, keine PO oder FI Spiele
  pull(GameID) %>% # get GameIDs
  map_df(~scoring(.)) %>% # get Scoring Summaries
  mutate(Points = case_when(Scoredby == "RF" ~ Points,
                            .default = -Points)) %>% 
  add_row(ElapsedTime = 0, Points = 0) %>% 
  arrange(ElapsedTime) %>%
  mutate(CumPoints = cumsum(Points / 4)) %>%
## plot ----  
  ggplot() +
  aes(x = ElapsedTime, y = CumPoints) +
  geom_vline(xintercept = (1:3) * 15, color = "grey95", lwd = 4, alpha = .5) +
  geom_step(linewidth = 3, color = "purple4") +
  geom_step(linewidth = 1, color = "purple") +
  scale_x_continuous(name = "Spielzeit [Minuten]", breaks = seq(0, 60, 15), minor_breaks = seq(0, 60, 5), expand = c(0, 0)) +
  scale_y_continuous(name = "Kumulierte Punkte für und gegen die Vikings") +
  labs(title = "Kombinierte Spiele der Vienna Vikings der 2023 ELF Saison bis inkl. Woche 5") +
  theme_bw() +
  theme(title =element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "seagreen", colour = NA))
