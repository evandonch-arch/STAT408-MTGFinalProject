# factors! (R4DS2e Chapter 16)

library(tidyverse)

# motivation for factors ----------------------------

# Last week, we talked about text columns with free-form answers
#   - e.g. What is your favorite sports drink?
# Today, we cover categorical variables, variables which have a 
# **finite, predetermined set of possible values**
#   - e.g. What is your favorite season? Fall, Winter, Spring, Summer
# You can either...
#  - ask it as a multiple-choice question to enforce that people give 
#    one of the possible answers, or
#  - ask it as a free-form question, then replace the answers with
#    the closest possible answer (e.g. replace "fall" and "Autumn" with "Fall")

# example: variable that records observation month
some_dates = c("Dec", "Apr", "Jan", "Mar")
# disadvantages to storing this as text:
# 1) you can have typos and variations that mean the same thing
some_dates_typo = c("Dec", "Apr", "Jam", "Mar")
# 2) it sorts alphabetically, instead of in calendar order
sort(some_dates)

# intro to factors ----------------------------

# we can fix this by making it a factor!
# two ways to do this:
# a. factor() (base-R, can be used with but does not require tidyverse)
# b. fct() (part of the forcats package, which is part of tidyverse)
# they behave a little differently, so we'll see both

# we can define the levels as a vector ahead of time:
# (this puts the months in calendar order)
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# a. using base-R factor()

# then we can specify the levels as we convert some_dates to factor type
dates_factor <- factor(some_dates, levels = month_levels)
dates_factor # these are in the order they appear in the dates_factor vector
sort(dates_factor) # now it sorts by calendar order!

# equivalently, you can define the levels within factor() instead of defining them ahead of time
dates_factor <- factor(
  some_dates,
  levels = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)

# note that any typos like "Jam" get silently converted to NA, even though 
# they were not missing values in the original data
dates_factor_wtypo <- factor(some_dates_typo, levels = month_levels)
dates_factor_wtypo

# if you don't specify levels, the default ordering is alphabetical:
factor(some_dates)



# b. using forcats::fct() (part of tidyverse)
# the arguments to fct() are the same as for factor(), but
# sorting and typo-handling are different

dates_factor <- fct(some_dates, levels = month_levels)
dates_factor # these are in the order they appear in the dates_factor vector
sort(dates_factor) # now it sorts by calendar order!

# just as before, you can define the levels within factor() instead of defining them beforehand
dates_factor <- fct(
  some_dates,
  levels = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)

# differences from factor():
# i) with fct, typos throw an error because they do not match any of the predefined levels
dates_factor_wtypo <- fct(some_dates_typo, levels = month_levels)
# Error in `fct()`:
# ! All values of `x` must appear in `levels` or `na`
# ℹ Missing level: "Jam"

# ii) also, if you don't specify levels, the default ordering is based on first appearance in the data:
fct(some_dates)

# whether you use factor() or fct(), you can get the list of levels 
# corresponding to a factor variable by using the levels() function
levels(dates_factor)


# Practice with code order ------------------------------

# If you run the code above in order as it was provided, 
# what lines define dates_factor **in its final form**?
# In other words, create a minimally reproducible script from the above code 
# that creates the factor variable dates_factor.

library(tidyverse)

some_dates = c("Dec", "Apr", "Jan", "Mar")

dates_factor <- fct(
  some_dates,
  levels = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)



# Practice with factors using our survey data -----------------

livboz <- read_csv("In-class/Data/Living in Bozeman.csv")
names(livboz)
names(livboz) = c(
  "timestamp",
  "time_lived_in_BZN",
  "do_for_fun",
  "area_of_BZN",
  "favorite_thing",
  "favorite_season",
  "restaurants",
  "miles_to_campus",
  "n_credits"
)

livboz$favorite_season
unique(livboz$favorite_season)

# base-R way:

# what is the order of the levels here? why?
factor(livboz$favorite_season)
# what is the order of the levels here? why?
fct(livboz$favorite_season)

# tidyverse way:

# let's make a factor version of favorite_season four different ways:
livboz = livboz |>
  mutate(
    fav_season_factor = factor(favorite_season),
    fav_season_fct = fct(favorite_season),
    fav_season_factorlvl = factor(
      favorite_season, levels = c("Fall", "Winter", "Spring", "Summer")
    ),
    fav_season_fctlvl = factor(
      favorite_season, levels = c("Fall", "Winter", "Spring", "Summer")
    )
  )

# first check the values of the columns: are they any different between the four columns?

# then what about the order of the levels? how do you know what order you'll get?
levels(livboz$fav_season_factor)
levels(livboz$fav_season_fct)
levels(livboz$fav_season_factorlvl)
levels(livboz$fav_season_fctlvl)

# also check this out: what do you think these numbers represent?
as.integer(livboz$fav_season_factor)
as.integer(livboz$fav_season_fct)
as.integer(livboz$fav_season_factorlvl)
as.integer(livboz$fav_season_fctlvl)





# Modifying factor order ---------------------

# To demonstrate this, we'll use the dataset gss_cat
#   - comes with the forcats package
#   - is a subset of data from the General Social Survey: https://gss.norc.org/

# to view:
gss_cat
# or if you want to view it in your environment:
data(gss_cat) # and then you probably have to click on it so that it displays as a dataset

# notice that a bunch of the variables are factor type already: <fct>
# A tibble: 21,483 × 9
#  year marital   age  race rincome partyid relig denom tvhours
# <int>   <fct> <int> <fct>   <fct>   <fct> <fct> <fct>   <int>

# count works pretty much the same for factors as we've seen before for other types
gss_cat |>
  count(race)

gss_cat |>
  count(year)

# but notice that it only shows you counts for the variables that appear in the 
# dataset, not for all possible levels:
levels(gss_cat$race)
# "Other", "Black", "White", "Not applicable"

# you can use .drop = FALSE to tell R not to drop levels that 
# don't appear in the dataset:
gss_cat |>
  count(race, .drop = FALSE)

# so now let's make a plot of average number of hours spent watching TV by religion
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n() # not strictly necessary, but often a good idea to take a look at
  )

# make the plot
ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_col()

# this probably isn't the most useful ordering to show for the plot
# how might you change it?
# one way is to reorder the religion categories based on the value of tvhours:
ggplot(
  relig_summary, 
  aes(x = tvhours, y = fct_reorder(relig, tvhours))
) +
  geom_col()

# an equivalent way to code this is to mutate before ggplot:
relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_col()

# a given approach to ordering factors isn't necessarily the best for all plots.
# for example, consider average age versus reported income level:
rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_col()

# since the factor levels here have an inherently meaningful order,
# we probably don't want to rearrange them

ggplot(rincome_summary, aes(x = age, y = rincome)) +
  geom_col()

# but it probably still makes sense to move "Not applicable" to the end:
ggplot(
  rincome_summary, 
  aes(x = age, y = fct_relevel(rincome, "Not applicable"))
) +
  geom_col()

# Question: Why do you think the average age for “Not applicable” is so high?

# another use for reordering factor levels: 
# fct_reorder2(f, x, y) reorders the factor f by the y values associated with 
# the largest x values. This makes the plot easier to read because the colors 
# of the line at the far right of the plot will line up with the legend.
by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )

# not as nice
ggplot(
  by_age, 
  aes(x = age, y = prop, color = marital)
) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

# nicer:
# fct_reorder2(marital, age, prop) looks at the prop value for the largest age 
# for each marital category. Widowed has the largest prop value for high age, 
# so that line will be on top at the right end of the plot; therefore, Widowed
# comes first in this reordering of the marital categories.
ggplot(
  by_age, 
  aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))
) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")


# if you want to sort the factors based on their frequencies in the dataset,
# not based on the value of one or more other variables,
# you can use fct_infreq()
# (if you want to reverse the order, use it with fct_rev)
gss_cat |>
  mutate(marital = marital |> fct_infreq()) |> # decreasing frequency
  ggplot(aes(x = marital)) +
  geom_bar()

gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |> # increasing freq.
  ggplot(aes(x = marital)) +
  geom_bar()

# Question: what's the difference between geom_col() and geom_bar()?

# When you want the height of the bar to be proportional to the number of cases use geom_col

# Practice with our own survey data ---------------------

# 1. make time_lived_in_BZN a factor

livboz = livboz |>
  mutate(time_lived_in_BZN = factor(
    time_lived_in_BZN, 
    levels = c("0-1 years", "2-5 years", "10+ years")
    ))

# 2. make a bar chart showing how often each level of time_lived_in_BZN appears in the dataset

livboz |> 
  ggplot(aes(x = time_lived_in_BZN)) +
  geom_bar()

time_summary <- livboz |>
  mutate(
    miles_to_campus = ifelse(miles_to_campus == "2 miles", "2", miles_to_campus, na.rm = TRUE)
  )


# 3. make a plot of average miles to campus by time lived in Bozeman
  





# Modifying factor levels ---------------------

# why might you want to change factor levels? some reasons:
#   -  make labels clearer or more formal for publication
#   -  make labels clearer or more formal for a plot or table
#   -  collapse levels for summary plots

# example: these labels don't have a consistent format
gss_cat |> count(partyid)

# let's modify them using fct_recode:
gss_cat |>
  mutate(
    partyid = fct_recode(
      partyid,
      # new level = old level
      # notice we don't have to mention how to handle every label,
      # only the ones we want to rename
      "Republican, strong"    = "Strong republican",
      "Republican, weak"      = "Not str republican",
      "Independent, near rep" = "Ind,near rep",
      "Independent, near dem" = "Ind,near dem",
      "Democrat, weak"        = "Not str democrat",
      "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)
# here, the counts are the same; all we did was relabel the factors

# alternatively, you can also collapse multiple levels into one 
# (see the Other category here):
gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  ) |>
  count(partyid)


# if you want to do a lot of collapsing, use fct_collapse():
gss_cat |>
  mutate(
    partyid = fct_collapse(
      partyid,
      "other" = c("No answer", "Don't know", "Other party"),
      "rep" = c("Strong republican", "Not str republican"),
      "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

# sometimes you want to lump together the small groups to simplify a plot/table 
#   - for this, use the fct_lump_*() family of functions
#   - fct_lump_lowfreq() is a simple starting point: it progressively lumps 
#     the smallest groups categories into “Other”, always keeping “Other” as 
#     the smallest category.
gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |>
  count(relig)
# here, one category represented more than half of the data, 
# so we end up with just that category and "Other"

# we can use fct_lump_n to specify how many categories to end up with:
gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)

# Practice with our own survey data ---------------------

# first, try adapting one of the plots we made above with the GSS data:
# remake the plot we made above of mean age by reported income level, except 
# combine the Refused, Don't Know, and No answer categories into one category
# (important: think about at what step in the data manipulation and plotting 
# you want to make that change)







# then you can experiment with one of the survey datasets we collected in class:
school <- read_csv("student_googleform_datasets/School_survey.csv")

# 1. what columns/variables of school do you think would make sense as factors?
# 2. choose one of those variables that has relatively more levels
# 3. make a plot of this variable, either a plot of how frequently it appears in 
# the data or a plot of some other variable against it (as we saw above with 
# mean miles from campus versus time lived in Bozeman, for example)
# 4. combine some levels of the variable that you think make sense to combine


