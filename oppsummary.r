oppsummary <- function (team = "Vienna Vikings") {
  data <- filter(.GlobalEnv$results, Team == team)
  tab <- group_by(data, Opponent) |>
    summarise(W = sum(Result == "W"), L = sum(Result == "L"), T = sum(Result == "T")) |> 
    mutate(Opponent = paste("vs", Opponent),
           Pct = (W + 1/2 * T) / (W + L + T)) |> 
    arrange(desc(Pct), desc(W))
  ret <- tibble(Team = team,
                W = sum(data$Result == "W"), L = sum(data$Result == "L"), T = sum(data$Result == "T"),
                Pct = NA,
                Opp_n = dim(tab)[1],
                Opp_gt_half = sum(tab$Pct > .5),
                Data = list(tab))
  ret$Pct <- round((ret$W + 1/2 * ret$T) / (ret$W + ret$L + ret$T), 3)
  
  return(ret)
  
}
