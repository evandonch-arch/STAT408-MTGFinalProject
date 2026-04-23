# Lec 07:
# working with vectors and logical comparisons in R

library(tidyverse)
library(nycflights13)

# Intro to vectors and logicals ---------------------------------------------

# a vector is a sequence of values
# whatever you do to a free-floating vector, 
# you can do to columns of a dataset with mutate
# therefore, it can be helpful to test things out on a simple test vector

# making a vector with c() or seq() or :
c(2, 1, 5, 5, 8)
1:10
seq(2, 10, 1)
seq(2, 10, 2)
seq(2.1, 10, 1)

# filter uses logical vectors
flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

c(15:17, 25:30)
dep_time = flights$dep_time[c(15:17, 25:30)] # indexing: using [] to select certain elements of dep_time
dep_time
dep_time > 600
dep_time < 2000
dep_time > 600 & dep_time < 2000
arr_delay = flights$arr_delay[c(15:17, 25:30)]
abs(arr_delay) < 20
dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20

# logical operators: < > <= >= != == | & %in%
1:5 %in% c(1,5)

# how logical comparisons work with NAs (R4DS2e sections 12.2.2-12.2.3 and 12.3.3)
c(1, 2, NA) == NA # notice RStudio even gives you a warning that you probably want to use is.na instead
#> [1] NA NA NA
c(1, 2, NA) %in% NA
#> [1] FALSE FALSE  TRUE
is.na(c(1, 2, NA))

# numerical summaries of logical vectors using sum and mean (12.4.2)
# true and false are treated as 0 and 1 in R if you apply functions to them that are intended for numbers
dep_time > 600
length(dep_time > 600)
sum(dep_time > 600)
mean(dep_time > 600)
6/9

# using this with tidyverse on the flights dataset:
flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_short_delayed = mean(dep_delay > 0 & dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop" # this is the same as ungroup
  )

# Example: describe how the missing values in dep_time, sched_dep_time and 
# dep_delay are connected
flights |>
  mutate(
    na_dep_time = is.na(dep_time),
    na_dep_delay = is.na(dep_delay),
    na_sched_dep_time = is.na(sched_dep_time)
  ) |>
  count(na_dep_time, na_dep_delay, na_sched_dep_time)


# Practice

# 1. Find all flights where arr_delay is missing but dep_delay is not. 
#    Find all flights where neither arr_time nor sched_arr_time are missing, 
#    but arr_delay is.

flights |>
  mutate(
    na_arr_delay = is.na(arr_delay),
    na_dep_delay = is.na(dep_delay)
  ) |>
  count(na_arr_delay, na_dep_delay)

flights |>
  filter(is.na(arr_delay) & !is.na(dep_delay))

# 1175

flights |>
  filter(!is.na(arr_time) & !is.na(sched_arr_time) & is.na(arr_delay))

# 707

#
# 2. How many flights have a missing dep_time? What other variables are missing 
#    in these rows? What might these rows represent?
#
# 3. Assuming that a missing dep_time implies that a flight is cancelled, look 
#    at the number of cancelled flights per day. Is there a pattern? Is there a 
#    connection between the proportion of cancelled flights and the average 
#    delay of non-cancelled flights?





# Logical subsetting and summaries of logical variables -----------------------

# logical subsetting in base R and using this in tidyverse functions like summarize (12.4.3)
dep_time # vector of values
dep_time > 600 # vector of logicals telling you whether each value in the vector meets some condition
dep_time[dep_time > 600] # this is logical subsetting

# use: the following code (without logical subsetting) works if all you want to 
# know is the average delay mean(arr_delay) for delayed flights (arr_delay > 0)
# on each day in the dataset
flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

# now say that for each day, you wanted BOTH the average delay for delayed  
# flights AND the average "delay" for early flights
flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# Practice

# 1. How would you adjust the above code to add columns for the proportion of 
# flights that were early and the proportion of flights that were delayed?
#

flights |> 
  group_by(year, month, day) |> 
  summarize(
    n = n(),
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    behind_prop = mean(arr_delay > 0, na.rm = TRUE),
    ahead_prop = mean(arr_delay < 0, na.rm = TRUE),
    n_behind = sum(arr_delay > 0, na.rm = TRUE),
    .groups = "drop"
  )

# 2. What does sum(is.na(x)) tell you? How about mean(is.na(x))? You can think 
# about this with a test vector like x = c(5, NA, 2.2, 4, NA).

sum(is.na(c(5, NA, 2.2, 4, NA)))

mean(is.na(c(5, NA, 2.2, 4, NA)))



# conditional transformations: if_else, case_when, compatible types (12.5) -----

# if_else: if condition is true, then return value a, otherwise return value b
# if_else(condition, a, b)
x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")
if_else(x > 0, "+ve", "-ve", "???")
if_else(x < 0, -x, x) # what does this do? is there a function in R that gives the same result?

# you can nest if_else statements:
# if condition, then a, else (if condition, then b, else c)
if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")
# same statement, a little easier to follow:
if_else(
  x == 0, # if condition is true,
  "0",    # then return this value,
  if_else( # else check another condition
    x < 0, # if this condition is true,
    "-ve", # return this value,
    "+ve"  # otherwise (i.e. if x==0 and x<0 are both false) return this value
  ), 
  "???" # if missing (NA), return this value
)

# case_when is better suited to handle lots of different conditions or "cases"
flights |> 
  mutate(
    status = case_when(
      # condition           ~ output
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )


# Practice

# 1. A number is even if it’s divisible by two, which in R you can find out with 
#    x %% 2 == 0. Use this fact and if_else() to determine whether each number 
#    between 0 and 20 is even or odd.

x <- c(0:20)
x
if_else(x %% 2 == 0, "even", "odd")

# 
# 2. Given a vector of days like x <- c("Monday", "Saturday", "Wednesday"), use 
#    an if_else() statement to label them as weekends or weekdays.

x <- c("Monday", "Saturday", "Wednesday")

if_else(x)


# 
# 3. Use if_else() to compute the absolute value of a numeric vector called x.

x <- c(-5:5)
x 
if_else(x > 0, x, sqrt(x^2))

# 
# 4. Write a case_when() statement that uses the month and day columns from 
#    flights to label a selection of important US holidays (e.g., New Years Day, 
#    4th of July, Thanksgiving, and Christmas). First create a logical column 
#    that is either TRUE or FALSE, and then create a character column that 
#    either gives the name of the holiday or is NA.


# Midterm feedback survey
# https://forms.gle/5hi94HTwJoA2wNB66

