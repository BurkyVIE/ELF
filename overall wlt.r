# libraries ----
library(tidyverse)

# mangle data ----
results |> 
  filter(!is.na(Result)) |> 
  mutate(Result = ordered(Result, levels = c("W", "L", "T"))) |>
  unnest(Teamdata) |> 
  group_by(Franchise) |> 
  summarise(Results = list(Result), .groups = "drop",
            Gs = n(),
            PFc = sum(PF),
            PAc = sum(PA)) |> 
  mutate(map_df(Results, ~unlist(.) %>% table(.)),
         across(W:T, as.integer),
         Pct = (W + 1/2 * T) / (W + L + T),
         PRat = PFc / (PFc + PAc),
         WRat = W / Gs,
         Pyt = 1 / (1 + (PAc / PFc) ** 2.37),
         Delta = (WRat-Pyt)) |> 
  arrange(desc(Pct), desc(PFc / Gs), (PAc / Gs)) -> res

# output result ----
print(res)

# plot ----
  ggplot(data = res) +
  aes(x = WRat, y = Pyt) +
  geom_abline(slope = , intercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  ggrepel::geom_label_repel(mapping = aes(label = Franchise, fill = Delta * 10)) +
  scale_x_continuous(name = "True Win Ratio") +
  scale_y_continuous(name = "Expectetd Win Ratio (Pythagorean)") +
  scale_fill_distiller(name = "Overwinning\nper 10 Games", palette = "RdYlGn", direction = 1, values = scales::rescale(c(range(res$Delta), 0)[c(1, 3, 2)])) +
  labs(title = "European League of Football",
       subtitle = "RS and PS Games since 2021") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_dark() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points")) -> p

plot(p)

# clean up ----
rm(res, p)