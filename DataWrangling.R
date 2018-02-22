# data wrangling
install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
install.packages(c("dplyr","tidyr","ggvis","nycflights13"))

library(dplyr)
library(nycflights13)
library(EDAWR)
library(tidyr)

# check the content of a dataset
help(package = "nycflights13")
# display data as a table -  limited content
flights
# display data as a data frame - unfriendly display
as.data.frame(flights)
# display data as a spread sheet - best way
View(flights)
# show some structure of the data
glimpse(flights)

# use pipe operator %>% to pass output to input
dd <- flights$dep_delay
mean(dd, na.rm = TRUE)
dd %>% mean(na.rm = TRUE)

?tb
?population

?left_join
?inner_join
?semi_join
?anti_join
# extract existing variables
?select
# extract existing observations
?filter
# derive new variables from existing variables
?mutate
# derive new observations from existing observations
?summarise
?group_by

# select
# helper functions such as :, -, contains(), ends_with(), 
# everything(), matches(), num_range(), one_of(), starts_with()
select(flights, ends_with("time"))
select(flights, contains("_"))
select(flights, starts_with("dep"))
select(flights, ends_with("time")) %>% 
  select(-starts_with("sched"))
select(flights, contains("_")) %>% 
  select(-starts_with("sched")) %>%
  select(dep_time : arr_delay)
# use Ctrl + Shift + M to enter the pipe operator %>% with shortcut

# filter
storms
filter(storms, wind > 50)
filter(storms, storm %in% c("Alex", "Arlene"))
filter(storms, wind > 50, storm %in% c("Alex", "Arlene", "Allison"))
# logical operators in R include: 
# >, <, ==, >=, <=, !=, %in%, is.na, !is.na, &, |, xor, !, any, all
flights %>% filter(!is.na(arr_delay))
storms %>% 
  filter(wind > 50) %>% 
  select(storm, pressure)
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  select(carrier, arr_delay)

# mutate
storms %>%
  mutate(ratio = pressure / wind, inverse = ratio^-1)
flights %>% 
  mutate(speed = distance / air_time * 60) %>% 
  select(carrier, arr_delay, speed)

# summarize
pollution %>% summarise(median = median(amount), variance = var(amount),
                        mean = mean(amount), sum = sum(amount), n = n())
flights %>% 
  filter(!is.na(air_time) & !is.na(distance)) %>% 
  summarize(n = n(), n_carriers = n_distinct(carrier), 
            total_time = sum(air_time), total_dist = sum(distance))

# group by
pollution %>% 
  group_by(city) %>% 
  summarize(mean = mean(amount), sum = sum(amount), n = n())
delays <- flights %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(carrier) %>% 
  summarise(avg_delay = mean(arr_delay))
# group by combination of values and 
# summarize removes the rightmost grouping variable
toyb %>% group_by(sex, country) %>% 
  summarise(cases = sum(cases)) %>% # removes year variable
  summarise(cases = sum(cases)) # removes country variable
toyb %>% group_by(country, year) %>% 
  summarise(cases = sum(cases)) %>% # removes sex variable
  summarise(cases = sum(cases)) # removes year variable
flights %>% group_by(origin, dest) %>%
  summarise(n = n()) %>% 
  ungroup()

# arrange
storms %>% arrange(wind)
storms %>% arrange(desc(wind))
storms %>% arrange(wind, date)
flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>% 
  mutate(avg_delay = (dep_delay + arr_delay) / 2) %>% 
  select(carrier, avg_delay) %>% 
  group_by(carrier) %>%
  summarize(avg_delay = mean(avg_delay)) %>% 
  arrange(desc(avg_delay))

tb
tb %>% 
  filter(!is.na(child) & !is.na(adult) & !is.na(elderly)) %>% 
  mutate(cases = child + adult + elderly) %>%
  select(country, year, sex, cases) %>% 
  group_by(country, year) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup()

rawtb
# summarise at the country level
rawtb %>%
  group_by(country, year, sex, age) %>% 
  summarise(n = sum(n)) %>% 
  summarise(n = sum(n)) %>% 
  summarise(n = sum(n)) %>% 
  summarise(n = sum(n))
# this is equivalent to
rawtb %>% 
  group_by(country) %>% 
  summarise(n = sum(n))
# similarly on the year level
rawtb %>% 
  group_by(country, year) %>% 
  summarise(n = sum(n))

# tidy data
?gather
?spread

tb2 <- tb %>% 
  filter(!is.na(child) & !is.na(adult) & !is.na(elderly)) %>% 
  mutate(cases = child + adult + elderly) %>%
  select(country, year, sex, cases) %>% 
  group_by(country, year) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup()
tb2
population

# gather
cases
# gather collapses multiple columns into two columns
cases %>% 
  gather("year","n",2:4) %>% 
  glimpse()
# convert = TRUE convert year data type from chr to int
cases %>% 
  gather("year","n",2:4,convert=TRUE) %>% 
  glimpse()
tb %>% 
  gather("age","cases",4:6) %>% 
  arrange(country, year, sex, age)

# spread
pollution
pollution2 <- pollution %>% 
  spread(size, amount)
pollution2
pollution3 <- pollution2 %>% 
  gather("size","amount",2:3) %>% 
  arrange(desc(city))
pollution3

# till now we can use
# dplyr::mutate() to build variables from variables
# dplyr::summarise() to build observations from observations
# dplyr::gather() to build observations from variables
# dplyr::spread() to build variables from variables

