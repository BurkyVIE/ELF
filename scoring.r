# function ----
scoring <- function(GameID = "RTVV2201") {

  ## libraries ----
  library(tidyverse)
    
  ## import GB ----
  compgb <- str_subset(readLines(paste0("GB/", GameID, ".txt")), "--- pagebreak ---", negate = TRUE) # pagebreaks entfernen
  
  ## find Scoring Summary ----
  a <- which(str_starts(compgb, "Qtr Time")) + 1    # scoring summary from...
  b <- which(str_starts(compgb, "Kickoff time")) -3 # ...to
  
  tibble(GameID = GameID,
         Data = compgb[a:b]) %>%
    mutate(tmp = str_replace(Data, "(1st|2nd|3rd|4th|OT| )\\s+(\\d{2}:\\d{2})\\s(\\w{2}).*", "\\1~\\2~\\3")) %>% 
    separate(tmp, sep = "~", into = c("Quarter", "Time", "Scored")) %>% 
    mutate(Quarter = as.integer(case_when(Quarter == "OT" ~ "5", .default = str_sub(Quarter, 1, 1))),
           Time = (lubridate::hms(paste0("0:",str_sub(Data, 5, 9)))),
           # Score = str_sub(Data, -7, -1)) %>% # no good, may catch last digit from Drive Info: "   0-0" is only six digits
           Score = str_extract(Data, "   \\d-\\d|\\d+ - \\d+")) %>% # assumes that there are always three blanks before the Score
    fill(Quarter, .direction = "down") %>% 
    separate(Score, sep = "-", into = c("Visitor", "Home"), convert = TRUE) %>% 
    mutate(across(Visitor:Home, ~as.integer(.)),
           Gametime = as.numeric((lubridate::minutes(15 * (Quarter - 1))) + lubridate::minutes(15) - Time) / 60,
           Points = (Home + Visitor) - lag((Home + Visitor), 1, default = 0),
           Scoring = case_when(Points == 2 ~ str_extract(Data, "safety|PAT return"),
                           Points == 3 ~ str_extract(Data, "\\d+ yd field goal"),
                           Points %in% 6:8 ~ str_replace(Data, ".* (\\d+ yd (pass|run|\\w+( \\w+)? return|fumble recovery)).*((kick|pass|rush)( \\w+)?).*", "\\1 + \\4"))) %>% 
    mutate(Scoring = case_when(nchar(Scoring) > 50 ~ str_extract(Data, "\\d+ yd (pass|run|\\w+( \\w+)? return|fumble recovery)"),
                               .default = Scoring)) %>% # second evaluation for Data without PT information
    return()
}



# counting scores ----

he <- map_df(GB_info$GameID, ~scoring(.))
hesub <- filter(he, str_detect(GameID, "23"))
list('total point' = sum(hesub$Points),
     'sccoring drives' = dim(hesub)[1],
     'games' = length(unique(hesub$GameID)), # cave 0-0 will not get caught
     '2 points' = filter(hesub, Points == 2) %>% 
       group_by(Res = Scoring) %>% 
       tally(),
     'field goals' = filter(hesub, Points == 3) %>% 
       mutate(Res = "field goal") %>% 
       group_by(Res) %>% 
       tally(),
     'touchdowns' = filter(hesub, Points %in% 6:8) %>%
       mutate(TD = case_when(str_detect(Scoring, "yd run") ~ "run TD",
                             str_detect(Scoring, "yd pass") ~ "pass TD",
                             str_detect(Scoring, "recovery") ~ "recovery TD",
                             str_detect(Scoring, "return") ~ "return TD"), 
              PAT = case_when(Points == 6 ~ "w/o PAT",
                              Points == 7 ~ "+ PAT",
                              Points == 8 ~ "+ 2pt conv"),
              Res = paste(TD, PAT)) %>% 
       group_by(Res) %>% 
       tally())



# graph ----

## magle data ----
GB_info %>%
  filter(str_detect(GameID, "VV"), str_sub(GameID, 5, 6) == "23", !str_ends(GameID, "PO|FI")) %>% # Vienna Vikings, Season == 2023, keine PO oder FI Spiele
  pull(GameID) %>% # get GameIDs
  map_df(~scoring(.)) %>% # get Scoring Summaries
  mutate(Points = case_when(Scored == "VV" ~ Points,
                            .default = -Points)) %>% 
  add_row(Gametime = 0, Points = 0) %>% 
  arrange(Gametime) %>%
  mutate(CumPoints = cumsum(Points / 4)) %>% # !!! no of games !!!
## plot ----  
  ggplot() +
  aes(x = Gametime, y = CumPoints) +
  geom_vline(xintercept = (1:3) * 15, color = "grey95", lwd = 4, alpha = .5) +
  geom_step(linewidth = 3, color = "purple4") +
  geom_step(linewidth = 1, color = "purple") +
  scale_x_continuous(name = "Spielzeit [Minuten]", breaks = seq(0, 60, 15), minor_breaks = seq(0, 60, 5), expand = c(0, 0)) +
  scale_y_continuous(name = "Kumulierte Punkte für und gegen die Vikings") +
  labs(title = "Kombinierte Spiele der Vienna Vikings der 2023 ELF Saison bis inkl. Woche 5") +
  theme_bw() +
  theme(title =element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "seagreen", colour = NA))
