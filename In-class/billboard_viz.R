# plot rank by week for the billboard data

# load tidyverse
library(tidyverse)


# pivot data longer
billboard_long = billboard |>
  pivot_longer(
    cols = wk1:wk76,
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
    
  ) |>
  # change the week column to be numerical only
  mutate(week = parse_number(week))


# plot of rank by week (uses long-format data)
  
ggplot(billboard_long, aes(x = week, y = rank, group = track)) +
  geom_line() +
  scale_y_reverse() +
  labs(x = "Week", y = "Rank") +
  theme_bw()


