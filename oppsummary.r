oppsummary <- function (team = "Vienna Vikings") {
  data <- filter(.GlobalEnv$results, Team == team)
  tab <- group_by(data, Opponent) |>
    summarise(W = sum(Result == "W"), L = sum(Result == "L"), T = sum(Result == "T")) |> 
    mutate(Opponent = paste("vs", Opponent),
           Pct = num((W + 1/2 * T) / (W + L + T), digits = 3, label ="rd_3")) |> 
    arrange(desc(Pct), desc(W))
  ret <- tibble(Team = team,
                W = sum(data$Result == "W"), L = sum(data$Result == "L"), T = sum(data$Result == "T"),
                Pct = NA,
                Opp_n = dim(tab)[1],
                Opp_gt_half = sum(tab$Pct > .5),
                Data = list(tab)) |> 
    mutate(Pct = num((W + 1/2 * T) / (W + L + T), digits = 3, label ="rd_3"))
  # ret$Pct <- num((ret$W + 1/2 * ret$T) / (ret$W + ret$L + ret$T), digits = 3, label ="rd_3")

  return(ret)
  
}
