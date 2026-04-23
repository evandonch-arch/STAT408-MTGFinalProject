# take a few minutes to read and run this code
# - what was this code probably written to do?
# - did you catch any likely errors?

library(tidyverse)

df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
)

df |> mutate(
  a = (a - min(a, na.rm = TRUE)) / 
    (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)),
  b = (b - min(a, na.rm = TRUE)) / 
    (max(b, na.rm = TRUE) - min(b, na.rm = TRUE)),
  c = (c - min(c, na.rm = TRUE)) / 
    (max(c, na.rm = TRUE) - min(c, na.rm = TRUE)),
  d = (d - min(d, na.rm = TRUE)) / 
    (max(d, na.rm = TRUE) - min(d, na.rm = TRUE)),
)

functionname = function(vec){
  body
}

functionname(vec = 1:6)

rescale01 = function(a){
  (a - min(a, na.rm = TRUE)) /
    (max(a, na.rm = TRUE) - min(a, na.rm = TRUE))
}

rescale01(1:6)


###25.2.5 Exercises
###Practice turning the following code snippets into functions. Think about what each function does. What would you call it? How many arguments does it need?

#mean(is.na(x))
#mean(is.na(y))
#mean(is.na(z))

#      x / sum(x, na.rm = TRUE)
#      y / sum(y, na.rm = TRUE)
#      z / sum(z, na.rm = TRUE)

#round(x / sum(x, na.rm = TRUE) * 100, 1)
#round(y / sum(y, na.rm = TRUE) * 100, 1)
#round(z / sum(z, na.rm = TRUE) * 100, 1)

#In the second variant of rescale01(), infinite values are left unchanged. Can you rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1?

#Given a vector of birthdates, write a function to compute the age in years.

#Write your own functions to compute the variance and skewness of a numeric vector. You can look up the definitions on Wikipedia or elsewhere.

#Write both_na(), a summary function that takes two vectors of the same length and returns the number of positions that have an NA in both vectors.




