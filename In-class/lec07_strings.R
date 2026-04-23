# Strings and text

library(tidyverse)
library(babynames)

# Introduction ---------------------

# Today and this week, we are covering parts of R4DS2e Chapters 14-15 as well 
# as some additional topics/functions for string manipulation

# So what are strings, exactly?
# A **string** is a sequence of characters (letters, numbers, spaces, symbols) 
# beginning and ending with single or double quotes.

string1 <- "This is a string"
string2 <- 'To include a "quote" inside, use single quotes outside'

# We've seen many examples of strings already
# If you start a string and forget the end quotes, you will get the `+` continuation prompt
# - Type "Esc" to cancel and try again

# The main R package we'll use for strings is `stringr` and it's part of `tidyverse`

# - `stringr` functions start with `str_`, so if you load `tidyverse` or `stringr` 
# and type "str_", RStudio will remind you what functions are available and what they do



# Motivation and review ---------------------

# Let's practice `ifelse` and `case_when` on the survey datasets, then see how we can use string manipulation to potentially make some of those tasks easier

# General rule of thumb:
# - If you have a few isolated typos or issues, it might be easier to fix them 
#   with logical manipulation like `ifelse` or `case_when`
# - If instead you have patterns to the issues and there are many different 
#   specific strings that result, you probably want to use string manipulation 
#   like functions that start with `str_`

# reading in one of the datasets
# note: you might have to change the filepaths
stat408 <- read_csv("In-class/408 Survey.csv")

# variable types: all four datasets have mostly character, with one or more double

# for example:
glimpse(stat408)

# variables with a predefined set of possible values are great to handle as "factors" (next week)
# today we'll be dealing with variables that are good for logical manipulation and dealing with text
# - variables that should be numeric but have some text in them
# - variables that didn't have predefined options but we want to merge/edit some of the values
# - variables that we want to split up into other variables

# For example, in stat408, some good variables for each type are...

# good vars for factors: favorite dining
# good vars for text: major, favorite day
# good vars for logical manipulation: distance to campus

# example: let's look at the stat408 dataset
names(stat408)
# let's rename the variables first
# note: I put five variable names on a line to make it easier to tell how many 
# there are, whether I accidentally skipped one, etc.
names(stat408) = c("timestamp", "dist_to_campus", "where_from", "fav_class", "edrinks_per_wk", 
                   "fav_dining", "fav_NFL", "major", "year", "study_hours_per_wk",
                   "fav_day", "fav_season", "fav_water_pH", "time_to_campus", "hrs_sleep_per_day",
                   "fav_olympic")

# 1. check out the distance to campus variable and use ifelse or case_when to fix it


stat408fixed = stat408 |>
  mutate(
    dist_to_campus = ifelse(dist_to_campus == "2 miles", "2", dist_to_campus)) |>
  mutate(
    dist_to_campus = ifelse(dist_to_campus == "0.5 miles", "0.5", dist_to_campus)) |>
  mutate(
    dist_to_campus = ifelse(dist_to_campus == "0.5 mile", "0.5", dist_to_campus)) |>
  mutate(dist_to_campus = as.numeric(dist_to_campus))


stat408fixed = stat408 |>
  mutate(dist_to_campus = case_when(
    dist_to_campus == "2 miles" ~ "2",
    dist_to_campus %in% c("0.5 miles", "0.5 mile") ~ "0.5",
    .default = dist_to_campus
  )) |>
  mutate(dist_to_campus = as.numeric(dist_to_campus))


# 2. then try to come up with as many different solutions or variations on the solutions as you can

# then we'll go over some solutions together
# then we'll see how we can handle this more easily with string manipulation (today's topic)
str_split("Why hello there!", pattern = " ")
str_split("Why hello there!", pattern = "!")
str_split("Why hello there!", pattern = "")

str_split_1("Why hello there!", pattern = " ")[1]
str_split_i("Why hello there!", pattern = " ", 2)

stat408fixed = stat408 |>
  mutate(dist_to_campus = str_split_i(dist_to_campus, " ", 1)) |>
  mutate(dist_to_campus = as.numeric(dist_to_campus))

# A quick note about escape characters --------------

# What if you wanted to include just a single quote in a string? Or both single and double quotes?
# You "escape" it with a backslash `\`:

double_quote <- "\"" # this gives you a string with one double quote

# The backslash is the escape character, telling R that whatever character comes afterward should be used literally
# What if you want to type a backslash? You need to escape it!
  
backslash = "\\"

# A quick note about viewing strings --------------

# Notice that these first two options show the sequence of characters in R to generate the string:
double_quote
print(double_quote)

# while these last two options show the intended string:
cat(double_quote)
str_view(double_quote)

# For more about special characters and raw strings, see R4DS2e Chapter 14
# https://r4ds.hadley.nz/strings.html



# Creating data from strings ---------------------

# To take a sequence of strings and paste them together, use `paste`, `paste0` or `str_c`:

paste("Hi", "there", "!")
paste("Hi", "there", "!", sep = ",")
paste0("Hi", "there", "!")  # 0 = zero space between strings
paste0("Hi ", "there", "!")
str_c("Hi ", "there", "!")


# You can combine fixed and variable strings together:

df <- tibble(name = c("Sierra", "Diego", "Sam", NA)) # make a one-column dataframe called `df`
df # check it out
df |> 
  mutate(greeting = str_c("Hi ", name, "!")) # make a second column from the first

# Notice that it left the NA as NA. We can use `coalesce` to handle that differently:

df |> 
  mutate(
    greeting1 = str_c("Hi ", name, "!"),
    greeting2 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting3 = coalesce(str_c("Hi ", name, "!"), "Hi!")
  )


# Practice: here's a data frame with three variables:
df = data.frame(
  salutation = c("Hello", "Hi", "Greetings", "Yo"),
  name = c("Sydney", "Ben", "Olivia", "Sawyer"),
  time = c("day", "week", "month", "year")
)

# use str_c with mutate (as seen above) to create a new column called `greeting`
# so that the resulting dataframe looks like this:
# (you don't have to worry about coalesce here, just str_c and mutate)

#   salutation   name  time                                   greeting
#   1      Hello Sydney   day       Hello, Sydney! How's your day going?
#   2         Hi    Ben  week            Hi, Ben! How's your week going?
#   3  Greetings Olivia month Greetings, Olivia! How's your month going?
#   4         Yo Sawyer  year         Yo, Sawyer! How's your year going?

df |> 
  mutate(greeting = str_c(salutation, ", ", name, "! How's your ", time, " going?"))


# then we'll discuss an example of str_glue as an alternative

df |>
  mutate(greeting = str_glue("{salutation}, {name}! How's your {time} going?"))

# We see that str_c and str_glue work well when you want the output to be the 
# same length as the inputs. Therefore they work well with mutate. 
# What if you want a function that works well with summarize, i.e. something 
# that always returns a single string? That’s the job of str_flatten:
# it takes a character vector and combines the elements into a single string.
str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")
df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df # make sure you take a look so you see the object we're dealing with
df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))


# note: you've now seen three different ways to construct your own datasets:
# data.frame(), tibble(), and tribble().
# just like with c(), seq(), and : for vectors,
# these are particularly handy when you want to make a small/toy dataset to play with.



# Extracting data from strings --------------

# https://r4ds.hadley.nz/strings.html#extracting-data-from-strings

# Sometimes multiple variables are in a single column
# - we might want to separate those out into their own rows or their own columns

## separating into rows ------------

surveydata <- tibble(
  name = c("Charlie", "Saul", "Eli"),
  fav_letters = c("a,b,z", "d,x", "q")
)
surveydata |> 
  separate_longer_delim(fav_letters, delim = ",")
# here, separate_longer_delim does the opposite of which function we learned about above?

# if instead the answers were single characters next to each other, no commas,
# we could use separate_longer_position
surveydata <- tibble(
  name = c("Charlie", "Saul", "Eli"),
  fav_letters = c("abz", "dx", "q")
)
surveydata |> 
  separate_longer_position(fav_letters, width = 1)


## separating into columns -------------

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

# if you don't need the edition information, you can use NA:
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year")
  )

# if you want to separate information based on the position in the string 
# instead of a delimiter, use separate_wider_position
df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )

# practice: here is the code-edition-year example from above, except now there 
# are no delimiters between these three pieces of information.
# use separate_wider_position to separate out the code, edition, and year 
# information into three new columns
df3 <- tibble(x = c("a1012022", "b1022011", "e1512015"))
df3 |> 
  separate_wider_position(
    x, 
    widths = c(code = 3, edition = 1, year = 4)
  )

# you can use these even if the number of pieces of info in each entry is not 
# the same from row to row! but it can be more complicated
surveydata <- tibble(
  name = c("Charlie", "Saul", "Eli"),
  fav_letters = c("a,b,z", "d,x", "q")
)
surveydata |> 
  separate_wider_delim(
    fav_letters, 
    delim = ",", 
    names = paste0("letter_", 1:3), 
    too_few = "align_start"
  )
# See R4DS2e Section 14.4.3 for more info on how to diagnose potential problems
# when you create new columns
# https://r4ds.hadley.nz/strings.html#diagnosing-widening-problems




# Individual characters --------------

# https://r4ds.hadley.nz/strings.html#letters

str_length(c("a", "R for data science", NA))

babynames |>
  mutate(name_length = str_length(name))

# Now let's take the babynames dataset and use the count function to
# summarize the distribution of name length in the dataset
# e.g. your resulting dataframe should look something like this:
#     length      n
#      <int>  <int>
#   1      2   4660
#   2      3  41274
#   3      4 177838
#   4      5 404291
#   5      6 546519

# we've used count before on an existing column, but you can also 
# create a new variable for it to count.
# in other words, these two code chunks are the same:

babynames |>
  mutate(length = str_length(name)) |>
  count(length)

babynames |>
  count(length = str_length(name))

# what does adding the argument "wt = n" to the count() function do?

babynames |>
  count(length = str_length(name), wt = n)


# You can extract parts of a string using str_sub(string, start, end), 
# where start and end are the positions where the substring should 
# start and end. The start and end arguments are inclusive, so the
# length of the returned string will be end - start + 1:
  
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

# Use negative values to count back from the end of the string: 
# -1 is the last character, -2 is the second to last character, etc.

str_sub(x, -3, -1)

# Note that str_sub() won’t fail if the string is too short
str_sub("a", 1, 3)

# We could use str_sub() with mutate() to find the first and last letter of each name:
  
babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )


