# practice writing functions from last time ----------------

# 1.  Look at the following code snippet to see the repetition. 
#     What is the task that's being repeated? Write a function to do it, then 
#     rewrite the code snippet to use your function. 
cut = diamonds$cut
color = c(5, 3, 4, 2, 2)

mean(is.na(cut))
mean(is.na(color))
mean(is.na(clarity))


# input: vector of values (cut, color, clarity)
# output: 
proportion_na = function(vec){
  mean(is.na(vec))
}

proportion_na(color)


# 2. Write a function that takes a vector of temperature values in Fahrenheit and 
#    converts it to Celcius. Then test it on a short vector you make that contains
#    Fahrenheit temperature whose corresponding Celcius values you know.
x = c(32, 45, 67, 78, 212)

temps_c = function(temps_f){
  (temps_f - 32)/1.8
}

temps_c(x)


# 3. What value will the following code return? Think about it, then try it.
#    Then come up with a short but descriptive name for myfunction.
myfunction = function(a, n=1){
  n*sum(a)
}
myfunction(1:3) # what will this value be?
n = 2
myfunction(1:3) # what will this value be?
myfunction(1:3, 2) # what will this value be?








# writing functions for which an input is a dataframe column name ------------

diamonds |> 
  group_by(cut) |> 
  summarize(mean(carat))

# let's make this a function:
# - inputs: dataframe, grouping variable, variable to compute mean of
# - output: resulting summary table (dataframe)

grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by(group_var) |> 
    summarize(mean(mean_var))
}

grouped_mean(diamonds, cut, carat)

# to see what the problem is, let's make a test dataframe:

test_df <- tibble(
  mean_var = 1:6,
  group_var = rep(letters[1:3], each=2),
  year = rep(paste0("year", 1:2), each=3),
  x = seq(10, 110, 20),
  y = seq(110, 10, -20)
)

test_df |> 
  group_by(group_var) |> 
  summarize(mean(mean_var))

test_df |> 
  group_by(year) |> 
  summarize(mean(x))

test_df |> 
  group_by(year) |> 
  summarize(mean(y))

test_df |> grouped_mean(year, x)

test_df |> grouped_mean(year, y)

# we can fix the problem using embracing, {{ }}:
#   with {{ group_var }}, if group_var = year, then
#   group_by({{ group_var }}) means group_by(year)
grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by({{ group_var }}) |> 
    summarize(mean({{ mean_var }}))
}

test_df |> grouped_mean(year, x)



# iteration, case 1: computing the same thing over many/all columns ----------

mtcars |> summarize(
  n = n(),
  mpg = mean(mpg),
  cyl = mean(cyl),
  disp = mean(disp),
  hp = mean(hp),
  drat = mean(drat),
  wt = mean(wt),
  qsec = mean(qsec),
  vs = mean(vs),
  am = mean(am),
  gear = mean(gear),
  carb = mean(carb)
)


mtcars |> summarize(
  n = n(),
  across(everything(), mean),
)


gss_cat |> summarize(
  n = n(),
  across(everything(), mean),
)


gss_cat |> summarize(
  n = n(),
  across(where(is.numeric), mean),
)


gss_cat |> summarize(
  n = n(),
  across(starts_with("a") & where(is.numeric), mean),
)


gss_cat |> summarize(
  n = n(),
  across(starts_with("a") & where(is.numeric), function(x) mean(x, na.rm = TRUE)),
)


gss_cat |> summarize(
  n = n(),
  across(starts_with("a") & where(is.numeric), \(x) mean(x, na.rm = TRUE)),
)


# practice ------------

# Do one or more of the following tasks for practice; choose based on what you 
# want to practice.

# If you want to practice writing functions:
# 1. Look at your project code or some other code you've recently written.
#    Can you identify some spots where a function might be handy?
#    Write one or more functions to update your code.
# 2. If you want more practice with what we've covered, work through some of 
#    R4DS2e 25.2.5 and 25.3.5. If you want more practice first, start by reading 
#    and coding along through the previous sections leading up to the exercises.
# 3. If you want to learn about other types of functions, check out R4DS2e 25.4
#    on plot functions.

# If you want to practice iteration:
# 1. Look at your project code or some other code you've recently written.
#    Can you identify some spots where iteration might be handy? Try it out.
# 2. If you want more practice with what we've covered, read and code along 
#    with R4DS2e Section 26.2 and try out the exercises in 26.2.8. We'll cover
#    more about iteration later too.


