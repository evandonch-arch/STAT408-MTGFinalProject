# read and clean the students dataset
# Jess Kunke, STAT 408, Feb 2 2026

library(tidyverse)

students = "students.csv" |>
  read_csv(na = c("N/A", "")) |>
  janitor::clean_names() |>
  mutate(
    age = parse_number(if_else(age == "five", "5", age))
  )

write_csv(students, "students.csv")
