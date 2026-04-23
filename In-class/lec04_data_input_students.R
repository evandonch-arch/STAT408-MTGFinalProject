# read and clean the students dataset
# note: this is a list of code to run, but to see an example of a final script,
# see lec04_students_final_script.R

library(tidyverse)

# here is how I created the data file you are using:
# students = read_csv("https://pos.it/r4ds-students-csv")
# write_csv(students, "students.csv")

students = read_csv("students.csv")

students = read_csv("students.csv", na = c("N/A", ""))

students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

students |> janitor::clean_names()

students = students |>
  janitor::clean_names() |>
  mutate(
    age = parse_number(if_else(age == "five", "5", age))
  )

write_csv(students, "students.csv")
