library(tidyverse)

### Scorigami
results %>% filter(Result == "W" | (Result == "T" & Home)) %>% #Winners or Home Team if Tied
  group_by(PF, PA) %>% 
  summarise(Season = list(unique(Season)),
            Count = n(),
            .groups = "drop") %>%
  add_case(PF = c(1, 1:5, 7), PA = c(0, rep (1, 6))) %>% # impossible scores
  add_case(expand_grid(PF = 0:max(.$PF), PA = 0:max(.$PA)) %>% filter(PF < PA)) %>%  # Winner scores less than Loser
  ggplot(mapping = aes(x = PF, y = PA)) +
  geom_tile(mapping = aes(fill = Count), color = '#1f1f1f') +
  scale_fill_distiller(palette = "GnBu", na.value = '#1f1f1f') +
  scale_x_continuous(expand = c(0, 0),limits = function(x)x+c(-.5, +.5), breaks = function(x)seq(0, x[2], by = 5), minor_breaks = NULL) +
  scale_y_continuous(expand = c(0, 0),limits = function(x)x+c(-.5, +.5), breaks = function(x)seq(0, x[2], by = 5), minor_breaks = NULL) +
  labs(x = "Winning Team Score", y = "Losing Team Score",
       title = "ELF Scorigami",
       subtitle = "Alltime ELF Scores, Regular and Playoff Games") +
  theme_bw(base_size = 13) -> p

#Plot
windows(16, 9)
plot(p)
rm(p)



### Latest Scorigamis
results %>% filter(Result == "W" | (Result == "T" & Home)) %>%
  #filter(Season >= 2000) %>% 
  mutate(Score = paste(str_pad(PF, 2, pad = "0"), str_pad(PA, 2, pad = "0"), sep = "-"),
         One = 1) %>% 
  group_by(Score) %>% 
  mutate(Counter = cumsum(One)) %>% #summarise(Count = max(Counter)) %>% ungroup() %>% arrange(desc(Count))
  ungroup() %>% 
  select(-(PF:PA), -One) %>% #arrange(desc(Counter))
  filter(Counter == 1) %>% 
  arrange(desc(Kickoff)) %>% 
  print()
