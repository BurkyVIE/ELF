# LIBRARIES ----
library(tidyverse)

# DATA ----
present <- 2024

## Complete list of Franchise, Team, ... by Season ----
teamdata_elf <- tribble(~Franchise, ~Team, ~Conference, ~Division, ~HomeField,
### A B C D E ----                        
         "Bravos", tribble(~Team, ~Abb, ~Season,
                           "Madrid Bravos", "MB", 2024:present),
                   tribble(~Conference, ~Season,
                           "TBD", 2024:present),
                   tribble(~Division, ~Season,
                           NA, NA),
                   tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                           "TBD", "Madrid, ES", NA, NA, 2024:present),
      "Centurions", tribble(~Team, ~Abb, ~Season,
                            "Cologne Centurions", "CC", 2021:present),
                    tribble(~Conference, ~Season,
                            "Southern", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Südstadion", "Cologne, DE", 50.9175, 6.9436, 2021:present),
         "Dragons", tribble(~Team, ~Abb, ~Season,
                            "Barcelona Dragons", "BD", 2021:present),
                    tribble(~Conference, ~Season,
                            "Southern", 2022,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Estadi Municipal de Reus", "Reus, ES", 41.156389, 1.085556, 2021:2022,
                            "Estadi Olímpic Lluís Companys", "Barcelona, ES", 41.364722, 2.155556, 2023:present),
      "Enthroners", tribble(~Team, ~Abb, ~Season,
                            "Fehervar Enthroners", "FE", 2023:present),
                    tribble(~Conference, ~Season,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "First Field", "Székesfehérvár, HU", 47.183636, 18.434803, 2023:present),
### F G H I J ----
            "Fire", tribble(~Team, ~Abb, ~Season,
                            "Rhein Fire", "RF", 2022:present),
                    tribble(~Conference, ~Season,
                            "Southern", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "MSV-Arena", "Duisburg, DE", 51.409033, 6.778664, 2022:present),
          "Galaxy", tribble(~Team, ~Abb, ~Season,
                            "Frankfurt Galaxy", "FG", 2021:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion am Bornheimer Hang", "Frankfurt, DE", 50.128056, 8.723333, 2021:present),
          "Guards", tribble(~Team, ~Abb, ~Season,
                            "Helvetic Guards", "HG", 2023),
                    tribble(~Conference, ~Season,
                            "Central", 2023),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Sportpark Bergholz", "Zürich, CH", 47.458333, 9.037222, 2023),
### K L M N O ----
           "Kings", tribble(~Team, ~Abb, ~Season,
                            "Leipzig Kings", "LK", 2021:2023),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Eastern", 2023:2023),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Alfred-Kunze-Sportpark", "Leipzig, DE", 51.358056, 12.307778, 2021,
                            "Bruno-Plache-Stadion", "Leipzig, DE", 51.302778, 12.419167, 2022:2023),
           "Lions", tribble(~Team, ~Abb, ~Season,
                            "Prague Lions", "PL", 2023:present),
                    tribble(~Conference, ~Season,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion FK Viktoria Žižkov", "Prague, CZ", 50.083872, 14.444589, 2023:present),
     "Mercenaries", tribble(~Team, ~Abb, ~Season,
                            "Helvetic Mercenaries", "HM", 2024:present),
                    tribble(~Conference, ~Season,
                            "Central", 2024:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Sportzentrum Kleinfeld", "Kriens, CH", 47.03, 8.288056, 2024:present),
"Musketeers", tribble(~Team, ~Abb, ~Season,
                            "Paris Musketeers", "PM", 2023:present),
                    tribble(~Conference, ~Season,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stade Jean-Bouin", "Paris, FR", 48.843056, 2.252778, 2023:present),
### P Q R S T ----
        "Panthers", tribble(~Team, ~Abb, ~Season,
                            "Panthers Wroclaw", "PW", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion Olimpijski", "Wroclaw, PL", 51.119444, 17.096667, 2021:present),
         "Raiders", tribble(~Team, ~Abb, ~Season,
                            "Raiders Tirol", "RT", 2022:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Tivoli Stadion Tirol", "Innsbruck, AT", 47.255833, 11.410833, 2022:present),
            "Rams", tribble(~Team, ~Abb, ~Season,
                            "Istanbul Rams", "IR", 2022),
                    tribble(~Conference, ~Season,
                            "Southern", 2022),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Maltepe Hasan Polat Stadium", "Istanbul, TR", 40.9205, 29.1313, 2022),
          "Ravens", tribble(~Team, ~Abb, ~Season,
                            "Munich Ravens", "MR", 2023:present),
                    tribble(~Conference, ~Season,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Sportpark Unterhaching", "Munich, DE", 48.073611, 11.615556, 2023:present),
          "Seamen", tribble(~Team, ~Abb, ~Season,
                            "Milano Seamen", "MS", 2023:present),
                    tribble(~Conference, ~Season,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Velodromo Vigorelli", "Milan, IT", 45.481389, 9.158056, 2023:present),
      "Sea Devils", tribble(~Team, ~Abb, ~Season,
                            "Hamburg Sea Devils", "HD", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion Hoheluft", "Hamburg, DE", 53.587222, 9.969722, 2021:present),
           "Surge", tribble(~Team, ~Abb, ~Season,
                            "Stuttgart Surge", "SS", 2021:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Waldau-Stadion", "Stuttgart, DE", 48.753944, 9.188417, 2021:present),
         "Thunder", tribble(~Team, ~Abb, ~Season,
                            "Berlin Thunder", "BT", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Friedrich-Ludwig-Jahn-Sportpark", "Berlin, DE", 52.543056, 13.405278, 2021:present),
### U V W X Y Z ----
         "Vikings", tribble(~Team, ~Abb, ~Season,
                            "Vienna Vikings", "VV", 2022:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Generali Arena", "Vienna, AT", 48.162345, 16.387156, 2022,
                            "Naturarena Hohe Warte", "Vienna, AT" , 48.248898, 16.359694, 2023:present)
                    )

# TRANSFORM ----
## Teamdata ----
teamdata_elf <- teamdata_elf |>
  mutate(across(Team:HomeField, ~ modify(.x, ~ unnest_longer(., "Season")))) # expandiere Seasons in nested tibbles

## Team_info ----
teaminfo_elf <-
  select(teamdata_elf, Franchise, Team) |> unnest_longer(Team) |> unpack(Team) |>                   # extrahiere TEAM Info
  left_join(
    select(teamdata_elf, Franchise, Conference) |> unnest_longer(Conference) |> unpack(Conference), # extrahiere CONFERENCE Info
    by = c("Franchise", "Season")) |>
  left_join(
    select(teamdata_elf, Franchise, Division) |> unnest_longer(Division) |> unpack(Division),       # extrahiere DIVISION Info
    by = c("Franchise", "Season"))  

## Team_location ----
teamloc_elf <- 
  select(teamdata_elf, Franchise, HomeField) |> unnest_longer(HomeField) |> unpack(HomeField) |> # extrahiere HOMEFIELD Info
  left_join(
    teaminfo_elf |> select(Season, Franchise, Conference, Division),
    by = c("Franchise", "Season"))

# DIVISION/CONFERENCE COLORS ----
## Colors ----
colors_elf <- list(
  Division = tribble(~Name, ~Color, ~Season,
                     "North", "#1f3f77", 2021,
                     "South", "#cd2028", 2021),
  Conference = tribble(~Name, ~Color, ~Season,
                       "Central", "#008000", 2022:present,
                       "Eastern", "#cd2028", 2023,
                       "Northern", "#1f3f77", 2022,
                       "Southern", "#cd2028", 2022,
                       "Western", "#1f3f77", 2023)
)

## Expandiere ----  
colors_elf <- lapply(colors_elf, unnest_longer, "Season")

# RESPONSE ----
cat("..ELF > Team data imported ✔\n")

# CLEAN UP ----
rm(present, teamdata_elf)