# LIBRARIES ----
library(tidyverse)

# SOURCEN ----
source("results_standings.r")

# GLOBALS ----
LoGB <- enframe(dir("GB/"), name = NULL, value = "file") |>
  mutate(GameID = str_sub(file, 1, 8),
         GameID = str_replace_all(GameID, # Liste der Umbennennungen für die Nachsaison
                                  c("21PO" = "2198", "21FI" = "2199",
                                    "22PO" = "2298", "22FI" = "2299")))

# FUNCTIONS ----
## scrape ganze Seite ----
scr_pag <- function(file, p = 1) {
  txt <- pdftools::pdf_text(paste0("GB/", file))
  pag <- unlist((txt)[[p]] |> strsplit("\n"))
  return(pag)
}

## scrape score by quarter ----
scr_sbq <- function(df) {
  tab <- df[11:13] |> # Zeilen 11 bis 13
    strsplit(" {2,}") |>
    unlist() |>
    matrix(nrow = 3, byrow = T) |>
    data.frame() |> 
    janitor::row_to_names(1) # Komplette Ergebnis-Tabelle inkl Quarter
  names(tab)[1:5] <- c("Team", "Q1", "Q2", "Q3", "Q4")
  tab <- as_tibble(tab) |>
    mutate(across(Q1:Total, ~as.integer(.)))
  return(tab)
}


# DATA ----
gb_info <- data_raw |>                                                                                         # Spielinformationen
  unnest_longer(Data) |>
  unpack(Data) |> 
  select(Season, Week, Guest, Home) |> 
  left_join(teaminfo_elf |> select(Season, Team, "G" = Abb), by = c("Season" = "Season", "Guest" = "Team")) |> # Abkürzung Guest
  left_join(teaminfo_elf |> select(Season, Team, "H" = Abb), by = c("Season" = "Season", "Home" = "Team")) |>  # Abkürzung Home
  mutate(GameID = paste0(H, G, Season%%100, sprintf("%02d", Week))) |>                                         # GameID
  left_join(LoGB, by = "GameID") |>                                                                            # Filename Gamebook
  mutate(Page1 = map(file, ~scr_pag(., 1)),
         Page2 = map(file, ~scr_pag(., 2)))

## Infos Seite 1 + 2 ----
gb_info <- gb_info |>
  mutate(Scores_Quarter = map(Page1, ~scr_sbq(.)),
         OT = map_lgl(Scores_Quarter, ~(dim(.)[2] > 6)),
         map_df(Scores_Quarter, ~.$Total |> set_names(c("Pts_G", "Pts_H"))),
         Att = map_int(Page1, ~str_replace(.[8], "Attendance:", "") |> as.integer()),
         map_df(Page2, ~strsplit(.[21], " {2,}") |> unlist() |> tail(2) |> as.integer() |> set_names(c("Yds_G", "Yds_H"))))

# RESPONSE ----
cat("..ELF > Gamebook information generated ✔\n")

## Zusatzinfos für results ----
transf <- gb_info |> 
  select(GameID, OT, Pts_G, Yds_G, Pts_H, Yds_H, Att) |> 
  mutate(Home = TRUE)
transf <- bind_rows(
  transf |> transmute(GameID, Home, OT, PF_gb = Pts_H, Yds_F = Yds_H, PA_gb = Pts_G, Yds_A = Yds_G, Att),
  transf |> transmute(GameID, Home = !Home, OT, PF_gb = Pts_G, Yds_F = Yds_G, PA_gb = Pts_H, Yds_A = Yds_H, Att)
)

## Zusammenhängen ----
test <- left_join(results, transf, by = c("GameID", "Home")) |> 
  relocate(OT:Att, .after = Result)

## Test ob Points übereinstimmen ----
test_wrong <- filter(test, PF != PF_gb | PA != PA_gb)
if(dim(test_wrong)[1] == 0) {
  results <- test |> select(!ends_with("_gb"))
  cat("..ELF > gamebook info added to results ✔\n")
} else {
  cat("..ELF > PROBLEM (non matching points):\n")
  print(test_wrong)
}

# CLEAN UP ----
rm(LoGB, scr_pag, scr_sbq, transf, test, test_wrong)
