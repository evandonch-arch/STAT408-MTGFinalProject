# joins and missing data

library(tidyverse)
library(nycflights13)
# we've worked with the flights dataset, but there are actually four datasets 
# in this package that relate to it and can be used together with flights

# understanding keys (ids) -----------------

# A primary key is a variable or set of variables that uniquely identifies each 
#   observation.
# When more than one variable is needed, the primary key is called a compound primary key.
# If a primary key is not provided within a dataset, we may need to add one
# e.g. if airlines didn't have a primary key, we could do
# airlines |>
#   mutate(airlineID = 1:nrow(airlines))

# to identify the primary key:
# - first think about what you know about the dataset and what the observational units are
#     - does each row represent a person? a flight? a country-year combination?
#     - if you know this, then the corresponding variables should make up the primary key
#     - ALWAYS check whether that is the case; the result often does not match your expectations
#       (there could be data issues or you could misunderstand the data structure)
# - if you don't know, or if you want to check your understanding,
#   compare the number of distinct combinations of a set of variables against 
#   the number of rows of the dataset
# - the smallest set of variables that uniquely identifies the rows is the primary key


# airline names and codes
airlines
names(airlines)
nrow(airlines)
n_distinct(airlines$carrier)
n_distinct(airlines$name)
# either carrier or name could be used as the primary key
# we use carrier as the primary key because it's shorter and it appears in the other datasets

# airport data
airports
names(airports)
nrow(airports)
n_distinct(airports$faa)
# faa is the primary key
# equivalently:
airports |> select(faa) |> n_distinct()

# weather data at origin airports
weather
names(weather)
nrow(weather)
n_distinct(weather$origin)
n_distinct(weather$year)
n_distinct(weather$time_hour)
weather |> select(origin, year, time_hour) |> n_distinct()
weather |> select(origin, year) |> n_distinct()
weather |> select(origin, time_hour) |> n_distinct()
# weather has a compound primary key: 
# origin and time_hour together uniquely identify the observations (rows)

# various ways to add a primary key:
# (note: the argument .before = 1 makes the new column the first column)
weather = weather |> 
    mutate(id = 1:nrow(weather), .before = 1)

weather = weather |> 
  mutate(id = row_number(), .before = 1)

weather = weather |> 
  mutate(id = str_glue("{origin} {time_hour}"), .before = 1)




# practice ------------

# Identify the primary key for the other two datasets, planes and flights:

# plane data
planes
names(planes)
planes |> select(tailnum) |> n_distinct()

# flight data
flights
names(flights)
nrow(flights)
n_distinct(flights$flight)
n_distinct(flights$time_hour)
flights |> select(flight, time_hour, carrier) |> n_distinct()







# basic joins -------------------

# mutating joins combine variables from two data frames

# make a simpler dataset with just six variables so it's easier to see what's going on
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)

flights2

# if we want to add the full airline name to each row of flights2,
# we can join airlines to it:

# with left_join, the resulting data frame will have the same number of rows as the dataset you're joining to
joined_data = flights2 |>
  left_join(airlines)

# equivalently:
joined_data = left_join(flights2, airlines)
nrow(joined_data)
nrow(flights2)
nrow(airlines)
# flights2 is the first dataset you give to left_join
# airlines is the second
# joined_data is the result of the join

# if we want to add the temperature and wind speed for each flight at its departure time:
flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

# how would we add type, engines, and seats from the planes dataset to the flights2 dataset?
# (add code here)

flights2 |>
  left_join(planes |>
              select(tailnum, type, engines, seats))

# if left_join fails to find a match for a given row of the first dataset x in
# left_join(x, y), it assigns NA to the new variable(s) for that row
"N3ALAA" %in% flights2$tailnum
"N3ALAA" %in% planes$tailnum
joined_data = flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))

# sometimes we need to specify the join key

# example that doesn't work properly:
# We get a lot of missing matches because our join is trying to use tailnum and 
# year as a compound key, whereas we only want to join on tailnum.
#   - By default, R uses all variables in common between the two datasets as the join key.
#   - Both flights and planes have a year column but they mean different things: 
#     flights$year is the year the flight occurred and planes$year is the year the plane was built.
flights2 |> 
  left_join(planes)

# So we're going to need to provide an explicit specification with join_by():
flights2 |> 
  left_join(planes, join_by(tailnum))
# Note that the year variables are disambiguated in the output with a suffix 
# (year.x and year.y), which tells you whether the variable came from the x or
# y argument (the first or second dataset you provided to left_join). 
# You can override the default suffixes with the suffix argument.

# if the variable names are not the same in the two datasets:
flights2 |> 
  left_join(airports, join_by(dest == faa))

flights2 |> 
  left_join(airports, join_by(origin == faa))


# left, right, inner, and full joins
# inner_join(), right_join(), full_join() have the same interface as left_join()
# The difference is which rows they keep: 
# - left join keeps all the rows in x, 
# - the right join keeps all rows in y, 
# - the full join keeps all rows in either x or y, and 
# - the inner join only keeps rows that occur in both x and y

ages <- tribble(
  ~person, ~age,
  1, 20,
  2, 18,
  3, 32
)
foods <- tribble(
  ~person, ~food,
  1, "tacos",
  2, "pasta",
  4, "pie"
)
ages
foods

# what is each join doing?
left_join(ages, foods)
right_join(ages, foods)
inner_join(ages, foods)
full_join(ages, foods)



# filtering joins

airports |> 
  semi_join(flights2, join_by(faa == origin))

airports |> 
  semi_join(flights2, join_by(faa == dest))

flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)

flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)

# practice ---------------------

# try out exercises in Sections 19.2.4 and 19.3.4

# bonus material if you're interested:
# - Section 19.5 Non-equi joins
# - Section 18.3.3 Joins for revealing implicitly missing observations


