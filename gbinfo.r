# LIBRARIES ----
library(tidyverse)

# GLOBAL
# Liste der Umbennennungen für die Nachsaison
week_renamer <- c("21PO" = "2198", "21FI" = "2199",
                  "22PO" = "2298", "22FI" = "2299")

# SOURCEN ----
source("results_standings.r")

# FUNCTIONS ----
## scrape ganze Seite ----
# scr_gb <- function(file) {
#   txt <- pdftools::pdf_text(file)
#   pag <- (txt) |> strsplit("\n")
#   return(list(pag[[1]][8], pag[[1]][11:13], pag[[2]][21]))
# }
scr_gb <- function(file) {
  txt <- readLines(file)
  pag <- split(txt, cumsum(txt == "--- pagebreak ---")) |> 
    lapply(function(x)x[x != "--- pagebreak ---"]) # entferen Seitenumbrüche (notwendig im langen Textfile)
  return(list(pag[[1]][8], pag[[1]][11:13], pag[[2]][21], pag[[1]][3]))
}

## scrape score by quarter ----
scr_sbq <- function(df) {
  tab <- strsplit(df, " {2,}") |>
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
# Infos aus Gamebooks
GB_info <- enframe(list.files("GB/", full.names = TRUE), name = NULL, value = "File") |>
  # filter(str_ends(File, ".pdf")) |>
  filter(str_ends(File, ".txt")) |>
  mutate(GameID = str_sub(File, 4, 11),
         GameID = str_replace_all(GameID, week_renamer),
         GB_Data = map(File, ~scr_gb(.)))

rm(week_renamer)

## Infos umwandel ----
GB_info <- GB_info |>
  mutate(Scores_Quarter = map(GB_Data, ~scr_sbq(.[[2]])),
         OT = map_lgl(Scores_Quarter, ~(dim(.)[2] > 6)),
         map_df(Scores_Quarter, ~.$Total |> set_names(c("Pts_G", "Pts_H"))),
         Att = map_int(GB_Data, ~str_replace(.[[1]], "Attendance:", "") |> as.integer()),
         map_df(GB_Data, ~strsplit(.[[3]], " {2,}") |> unlist() |> tail(2) |> as.integer() |> set_names(c("Yds_G", "Yds_H"))),
         Game = map_chr(GB_Data, ~str_trim(str_replace_all(.[[4]], "#\\d ", ""))))

# RESPONSE ----
cat("..ELF > Gamebook information generated ✔\n")

## Zusatzinfos für results ----
transf <- GB_info |> 
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
rm(scr_gb, scr_sbq, transf, test, test_wrong)
