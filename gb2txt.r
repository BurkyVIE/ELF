# Diese Funktion wandelt .pdf im Ordner GB in .txt um (kleiner Dateien)

library(tidyverse)

gb2txt <- function(file) {
  txt <- pdftools::pdf_text(file) |> strsplit("\n")
  collect <- NULL
  while(length(txt) > 0){
    collect <- c(collect, txt[[1]], "--- pagebreak ---")
    txt[[1]] = NULL
    }
  writeLines(collect, paste0(str_sub(file, 1, 11), ".txt"))
}

sapply(enframe(list.files("GB/", full.names = TRUE), name = NULL, value = "File") |>
        filter(str_ends(File, ".pdf")) |> 
        pull(File),
      gb2txt)

rm(gb2txt)
