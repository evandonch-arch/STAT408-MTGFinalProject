library(tidyverse)

# start here
water_data = read_csv("CedarLower_water_sum_22.csv")

# try stuff here






# one version of the final code:
water_data = "CedarLower_water_sum_22.csv" |>
  readr::read_csv(
    skip = 2, # skip the first two lines of the file
    col_select = 1:3, # read only the first three columns of data
    col_names = FALSE, # don't try to name columns from a row of the file
    show_col_types = FALSE
  ) %>%
  dplyr::rename(
    "row.num" = X1,
    "datetime" = X2,
    "temperature" = X3
  )
