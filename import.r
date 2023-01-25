# LIBRARIES ----
library(tidyverse)

# FUNCTIONS ----
## einlese-funktion ----
import <- function(df) {
  df %>% 
    mutate(Season = as.integer(substr(file, 1, 4)),
           Data = map(.x = file,
                      .f = ~ read_csv(.,
                                      lazy = FALSE,
                                      col_types = cols(Week = col_integer(),
                                                       Kickoff = col_character(),
                                                       Home = col_character(),
                                                       Guest = col_character(),
                                                       Pts_H = col_integer(),
                                                       Pts_G = col_integer()))
                      %>% mutate(Kickoff = as.POSIXct(Kickoff)))) %>% 
    relocate(Season) %>% 
    return()
}

## DATA ----
data_raw <- dir()[dir() %>% str_ends("ELF.txt")] %>%
  tibble(file = .) %>%
  import()

# RESPONSE ----
cat("..ELF > raw data imported ✔\n")

# CLEAN UP ----
rm(import)