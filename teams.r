# LIBRARIES ----
library(tidyverse)

# DATA ----
present <- 2023

## Complete list of Franchise, Team, ... by Season ----
teamdata_elf <- tribble(~Franchise, ~Team, ~Conference, ~Division, ~HomeField,
### A B C D E ----                        
      "Centurions", tribble(~Team, ~Season,
                            "Cologne Centurions", 2021:present),
                    tribble(~Conference, ~Season,
                            "Southern", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Südstadion", "Cologne, DE", 50.9175, 6.9436, 2021:present),
         "Dragons", tribble(~Team, ~Season,
                            "Barcelona Dragons", 2021:present),
                    tribble(~Conference, ~Season,
                            "Southern", 2022,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Estadi Municipal de Reus", "Reus, ES", 41.156389, 1.085556, 2021:present),
      "Enthroners", tribble(~Team, ~Season,
                            "Fehérvár Enthroners", 2023:present),
                    tribble(~Conference, ~Season,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "MÁV-Előre-Stadion", "Székesfehérvár, HU", 47.183636, 18.434803, 2023:present),
### F G H I J ----
            "Fire", tribble(~Team, ~Season,
                            "Rhein Fire", 2022:present),
                    tribble(~Conference, ~Season,
                            "Southern", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "MSV-Arena", "Duisburg, DE", 51.409033, 6.778664, 2022:present),
          "Galaxy", tribble(~Team, ~Season,
                            "Frankfurt Galaxy", 2021:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion am Bornheimer Hang", "Frankfurt, DE", 50.128056, 8.723333, 2021:present),
          "Guards", tribble(~Team, ~Season,
                            "Helvetic Guards", 2023:present),
                    tribble(~Conference, ~Season,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Sportpark Bergholz", "Zürich, CH", 47.458333, 9.037222, 2023:present),
### K L M N O ----
           "Kings", tribble(~Team, ~Season,
                            "Leipzig Kings", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Alfred-Kunze-Sportpark", "Leipzig, DE", 51.358056, 12.307778, 2021,
                            "Bruno-Plache-Stadion", "Leipzig, DE", 51.302778, 12.419167, 2022:present),
           "Lions", tribble(~Team, ~Season,
                            "Prague Lions", 2023:present),
                    tribble(~Conference, ~Season,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "tbd", "Prague, CZ", 50.0875, 14.421389, 2023:present),
### P Q R S T ----
        "Panthers", tribble(~Team, ~Season,
                            "Panthers Wroclaw", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion Olimpijski", "Wroclaw, PL", 51.119444, 17.096667, 2021:present),
        "Paris FT", tribble(~Team, ~Season,
                            "Paris Football Team", 2023:present),
                    tribble(~Conference, ~Season,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "tbd", "Paris, FR", 48.856613, 2.352222, 2023:present),
         "Raiders", tribble(~Team, ~Season,
                            "Raiders Tirol", 2022:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Tivoli Stadion Tirol", "Innsbruck, AT", 47.255833, 11.410833, 2022:present),
            "Rams", tribble(~Team, ~Season,
                            "Istanbul Rams", c(2022, NULL)),
                    tribble(~Conference, ~Season,
                            "Southern", 2022),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Maltepe Hasan Polat Stadium", "Istanbul, TR", 40.9205, 29.1313, 2022),
          "Ravens", tribble(~Team, ~Season,
                            "Munich Ravens", 2023:present),
                    tribble(~Conference, ~Season,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "tbd", "Munich, DE", 48.1375, 11.575, 2023:present),
          "Seamen", tribble(~Team, ~Season,
                            "Milano Seamen", 2023:present),
                    tribble(~Conference, ~Season,
                            "Central", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Velodromo Vigorelli", "Milan, IT", 45.481389, 9.158056, 2023:present),
      "Sea Devils", tribble(~Team, ~Season,
                            "Hamburg Sea Devils", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Western", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Stadion Hoheluft", "Hamburg, DE", 53.587222, 9.969722, 2021:present),
           "Surge", tribble(~Team, ~Season,
                            "Stuttgart Surge", 2021:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022:present),
                    tribble(~Division, ~Season,
                            "South", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Waldau-Stadion", "Stuttgart, DE", 48.753944, 9.188417, 2021:present),
         "Thunder", tribble(~Team, ~Season,
                            "Berlin Thunder", 2021:present),
                    tribble(~Conference, ~Season,
                            "Northern", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            "North", 2021),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Friedrich-Ludwig-Jahn-Sportpark", "Berlin, DE", 52.543056, 13.405278, 2021:present),
### U V W X Y Z ----
         "Vikings", tribble(~Team, ~Season,
                            "Vienna Vikings", 2022:present),
                    tribble(~Conference, ~Season,
                            "Central", 2022,
                            "Eastern", 2023:present),
                    tribble(~Division, ~Season,
                            NA, NA),
                    tribble(~Stadium, ~Location, ~Lat, ~Long, ~Season,
                            "Generali Arena", "Vienna, AT", 48.162345, 16.387156, 2022,
                            "tbd", "Vienna, AT" ,48.17283953627955, 16.426366954450117, 2023:present)
                    )

# TRANSFORM ----
## Team_info ----
teaminfo_elf <- 
    teamdata_elf |> select(Franchise, Team) |> 
      mutate(Team = map(Team, ~unnest_longer(., "Season"))) |>
      unnest_longer(Team) |> unpack(Team) |> 
  left_join(
    teamdata_elf |> select(Franchise, Conference) |>
      mutate(Conference = map(Conference, ~unnest_longer(., "Season"))) |>
      unnest_longer(Conference) |> unpack(Conference),
    by = c("Franchise", "Season")) |> 
  left_join(
    teamdata_elf |> select(Franchise, Division) |>
      mutate(Division = map(Division, ~unnest_longer(., "Season"))) |>
      unnest_longer(Division) |> unpack(Division),
    by = c("Franchise", "Season")) |> 
  relocate(c(Franchise, Season, Team, Division)) |> 
  arrange(Franchise, Season)

## Team_location ----
teamloc_elf <- 
  teamdata_elf |> select(Franchise, HomeField) |> 
  mutate(HomeField = map(HomeField, ~unnest_longer(., "Season"))) |>
  unnest_longer(HomeField) |> unpack(HomeField) |> # unpack to '$'-notation (i.e. HomeField$Stadium, etc.)
  left_join(
    teaminfo_elf |> select(Season, Franchise, Conference, Division), by = c("Franchise", "Season")
    )

# CLEAN UP ----
rm(present, teamdata_elf)
