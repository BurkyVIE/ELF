library(tidyverse)

gamebooks <- dir("GB", recursive = TRUE) |>
  enframe(name = NULL, value = "File") |>
  mutate(GameID = toupper(str_replace(File, pattern = ".*([a-zA-Z]{4}\\d{2}\\w{2}).*", replacement = "\\1")), Gamebook = TRUE)


left_join(games, select(gamebooks, -File), by = "GameID") |> filter(is.na(Gamebook), !is.na(Kickoff))
