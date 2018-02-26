# data wrangling
install.packages("devtools")
devtools::install_github("rstudio/EDAWR")
install.packages(c("dplyr","tidyr","ggvis","nycflights13"))

library(dplyr)
library(nycflights13)
library(EDAWR)
library(tidyr)
library(ggvis)

######################################################################
# following section is about data manipulation
# which changes the variables, values, and units of analysis in dataset

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
delays <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>% 
  mutate(avg_delay = (dep_delay + arr_delay) / 2) %>% 
  select(carrier, avg_delay) %>% 
  group_by(carrier) %>%
  summarize(avg_delay = mean(avg_delay)) %>% 
  arrange(desc(avg_delay))
delays

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

######################################################################
# following section is about data tidying
# which changes the layout of tabular data suitable for some software

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

# further more we can use
# dplyr::separate() to split a column by a chr string seperator
# dplyr::unite() to unites columns into a single column

# separate
storms
storms2 <- storms %>% 
  separate(date, c("year","month","day"), sep="-")
storms2

# unite
storms2 %>% 
  unite("date", year, month, day, sep="-")

# combine the use of separate and unite to convert data format
storms %>%
  separate(date, c("year","month","day"), sep="-") %>% 
  unite("date", month, day, year, sep="/")

# for transform tabular data there are some functions
# the series of bind functions are very simple
# dplyr::bind_rows() binds observations with the same variables
# dplyr::bind_cols() binds variables of the same observations
# dplyr::union() binds observations and filter out identical ones
# dplyr::setdiff() filter out observations of the second dataframe

# there are some more powerful MATCH join functions
# dplyr::left_join() maintains all observations in the 1st dataframe
songs
artists
songs %>% left_join(artists, by="name")
# join on combined variables
songs2
artists2
songs2 %>% left_join(artists2, by=c("first","last"))
# dplyr::right_join() do the similar thing on opposite direction
songs %>% right_join(artists, by="name")
# dplyr::inner_join() join and maintain the observations in both dfs
songs %>% inner_join(artists, by="name")
# dplyr::full_join() join and maintain all info in both dfs
songs %>% full_join(artists, by="name")

# some FILTER join functions could be used for some special purpose
# dplyr::semi_join() returns observations that match the 2nd dataset
songs %>% semi_join(artists, by="name")
# dplyr::anti_join() returns observations that not match the 2nd dataset
songs %>% anti_join(artists, by="name")

# we use delays dataset from the previous practice
delays
airlines
delays %>% 
  left_join(airlines, by="carrier") %>% 
  arrange(avg_delay)

# a case study
# we use tb2 dataset from the previous practice
tb2
population
tb3 <- tb2 %>% 
  left_join(population, by = c("country","year")) %>% 
  mutate(rate = cases / population * 10000)
# we prepare a tb3 dataset for future use
tb3 %>% 
  select(country, year, rate)
tb3

# the population dataset could be not as tidy as required
# in case it is in a shape of population2 as follow, make it tidy
population
population2 <- population %>% 
  spread(year, population)
population2
population2 %>%
  # here, -1 means all cols except the 1st one
  # existing col names in function do not need to have double quotes
  # while newly generated col names need to have double quotes 
  gather("year","population",-1,convert=TRUE)

######################################################################
# following section is about data visualization
# which visualize data to reveal some visual patterns

# visualization is a collection of visual marks, for observatioins,
# that have visual properties, for variables. the structure of 
# datasets parallels the structure of data visualizations.

# there are many functions within ggvis for plotting data
?ggvis
china <- tb3 %>% filter(country == "China")
# ggvis use ~ as a shortcut for referring to a column in data frame
china %>% ggvis(x = ~year, y = ~rate) %>% layer_points()
china %>% ggvis(x = ~year, y = ~rate) %>% layer_lines()
china %>% ggvis(x = ~year, y = ~rate) %>% layer_bars()
china %>% ggvis(x = ~year, y = ~rate) %>% layer_smooths()
# layers of plotting graphs could be added with pipe joined
china %>% ggvis(x = ~year, y = ~rate) %>% 
  layer_points() %>% layer_lines()

# use group_by when plotting several sets of data with ggvis
indochina <- tb3 %>% filter(country == c("China", "India"))
indochina
# plotting two sets of data with same layout is confusing
indochina %>% ggvis(x = ~year, y = ~rate) %>% layer_points()
# sometimes plotting two sets of data may reveal error information
indochina %>% ggvis(x = ~year, y = ~rate) %>% layer_lines()
# ggvis supports grouped data with seperate mark for each data group 
indochina %>% group_by(country) %>% 
  ggvis(x = ~year, y = ~rate) %>% layer_lines()
# ggvis can group data by itself automatically
indochina %>% ggvis(x = ~year, y = ~rate, stroke = ~country) %>%
  layer_lines()

# especially when there are lots of datasets, they should be grouped
tb3 %>% 
  group_by(country) %>% 
  ggvis(x = ~year, y = ~cases) %>%
  layer_lines()

# use different variables of the same dataset can reveal different info
china %>% 
  ggvis(x = ~year, y = ~population, fill = ~rate) %>% 
  layer_points()

# size para in ggvis use size to distinguish data values
china %>% 
  ggvis(x = ~year, y = ~rate, size = ~rate) %>% 
  layer_points()

# := is used for passing value directly to the visual space
# while = is used for passing value to the data space
# for example, for plotting all points in red use := to pass red
china %>% 
  ggvis(x = ~year, y = ~rate, fill := "red") %>% 
  layer_points()
# while setting stroke = "green" will not get a green line in visual
indochina %>% group_by(country) %>% 
  ggvis(x = ~year, y = ~rate, stroke = "green") %>% 
  layer_lines()
# for setting the graph in a specific visual color use :=
indochina %>%
  ggvis(x = ~country, y = ~cases, fill := "orange") %>% 
  layer_boxplots()

######################################################################
# for loading and saving data, check the working dir first
getwd()
# use setwd() to set the working dir with a new dir value IN CONSOLE
setwd("/Users/user") # this line of code does NOT work in script

# import data could be done through wizard above Global Environment tab
# saving data could be done with write.csv(), for example
write.csv(df, file = "data/my-data.csv", row.names = FALSE)

# final project
flights
airlines
# ggvis can group data by itself and pay attention to fill = ~name para
flights %>% 
  left_join(airlines, by = "carrier") %>% 
  filter(!is.na(arr_delay)) %>% 
  ggvis(x= ~name, y = ~arr_delay, fill = ~name) %>% 
  layer_boxplots()
# use group by and summarize can filter out other variables
flights %>% 
  left_join(airlines, by = "carrier") %>% 
  filter(!is.na(arr_delay)) %>% 
  group_by(name) %>% 
  summarize(median = median(arr_delay)) %>% 
  arrange(desc(median))

######################################################################
# further reading
# hands-on programming with R