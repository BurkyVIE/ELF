# function ----
FirstDowns <- function(GameID = "VVBT2308") {

  ## libraries ----
  library(tidyverse)
  
  ## import GB ----
  compgb <- str_subset(readLines(paste0("GB/", GameID, ".txt")), "--- pagebreak ---", negate = TRUE) # pagebreaks entfernen
  
  ## find Scoring Summary ----
  a <- which(str_starts(compgb, "\\s*FIRST DOWNS")) + 1    # scoring summary from...
  b <- a + 2 # ...to
  
  tibble(GameID = GameID,
         Data = compgb[a:b]) %>% 
    separate(Data, into = c("Null", "How", "G", "H"), sep = "\\s{2,}", convert = TRUE) %>% 
    select(-Null)  %>% 
    mutate(GO = H, HO = G) %>% 
    pivot_longer(G:H, names_to = "Team", values_to = "FirstD") %>% 
    mutate(OppFD = case_when(Team == "H" ~ HO,
                           Team == "G" ~ GO),
           Team = case_when(Team == "H" ~ substr(GameID, 1, 2),
                            Team == "G" ~ substr(GameID, 3, 4))) %>% 
    select(-(GO:HO))
}

seas <- 2023

map_df(filter(GB_info, substr(GameID, 5, 6) == substr(seas, 3, 4), !str_ends(GameID, "PO|FI")) %>% pull(GameID), ~FirstDowns(.)) %>% 
  group_by(Team, How) %>% 
  summarise(Games = n(), FirstD = sum(FirstD), OppFD = sum(OppFD), .groups = "drop") %>% 
  pivot_wider(names_from = How, values_from = FirstD:OppFD) %>% 
  left_join(filter(teaminfo_elf, Season == seas), by = c("Team" = "Abb")) %>% 
  ggplot() +
  aes(x = FirstD_Passing/Games, y = FirstD_Rushing/Games) +
  # geom_point() +
  ggimage::geom_image(image = "fb_32.png", size = .03) +
  ggrepel::geom_label_repel(mapping = aes(label = Franchise), point.padding = 1.25, box.padding = 1, color = "brown") +
  scale_x_continuous(name = "Passing First Downs per Game", breaks = function(x)seq(0, x[2], by = 5)) +
  scale_y_continuous(name = "Rushing First Downs per Game", breaks = function(x)seq(0, x[2], by = 5)) +
  labs(title = "How First Downs Are Achieved",
       subtitle = paste(seas, "ELF Regular Season")) +
  theme_bw() +
  theme(title = element_text(size = 12),
        panel.background = element_rect(fill = "seagreen", colour = NA)) -> p

windows(9, 9)
plot(p)

rm(seas, p)