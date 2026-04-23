# the two following chunks of code are the same except for the first line.
#
# 1. what do you call the extra step that's in the first line of the second chunk that's not in the first chunk?
# 2. what is the difference in behavior when you run the first chunk vs the second?
# 3. when might you want one versus the other?
# 4. after running chunk 1, has the flights object changed? why/why not?
# 5. after running chunk 1, has the flights object changed? why/why not?
# 6. how might you run just the first three lines of chunk 1?
# 7. how could you step through this code to figure out what it's doing?

library(tidyverse)
library(nycflights13)

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

fast_flights = flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))


# now consider the second alternative to piping that the text mentions:

flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

# 8. what is the difference in behavior between the code chunks with piping and 
#    this non-piping chunk? what are some pros and cons to these two approaches?
# 9. what is the difference in behavior between the non-piping chunk above and
#    the non-piping chunk below?

flights1 <- filter(flights, dest == "IAH")
flights1 <- mutate(flights1, speed = distance / air_time * 60)
flights1 <- select(flights1, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

# 10. what will happen when you run the code below? why?
flights4 <- filter(flights, dest == "IAH")
flights1 <- mutate(flights, speed = distance / air_time * 60)
flights1 <- select(flights4, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

