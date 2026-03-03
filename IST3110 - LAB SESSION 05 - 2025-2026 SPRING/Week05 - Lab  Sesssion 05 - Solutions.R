# IST3110 Week 05 - LAB SESSION 05-SOLUTIONS

# Import required packages
library(dplyr)
library(gapminder)

### 1) Load the dataset, look at the structure of the dataset,
### look at the first few lines. Look at the help file to better understand data

gapminder %>% head()
gapminder %>% str()
gapminder %>% summary()
?gapminder
help(gapminder)

# 2) How many observations (rows) are in the dataset?

nrow(gapminder)

# 3) Select population column from the dataset

gapminder %>% select(pop)

# 4)  Extract only the observations from the year 1957

gapminder %>% filter(year == 1957)


# 5) Filter the gapminder data to retrieve only the 
# observation from Turkey in the year 2002.

gapminder %>% filter(country == 'Turkey', year == 2002)
gapminder %>% filter(country == 'Turkey' & year == 2002)


# 6) Sort the population from the highest to lowest for the year 1957.

gapminder %>% filter (year == 1957) %>% arrange(desc(pop))


# 7) Find the country and year combination that has the 
# lowest life expectancy.

##### 1st solution
min_lifeExp <- gapminder %>% summarise(
  min_lifeExp = min(lifeExp)
) %>% pull

gapminder %>% filter(lifeExp == min_lifeExp) %>% 
  select(country, year, lifeExp)

####### 2nd solution

gapminder %>% arrange(lifeExp) %>% slice(1) %>% 
  select(country, year, lifeExp)

####### 3rd Solution

gapminder %>% slice_min(lifeExp, n=1) %>% 
  select(-continent, -gdpPercap, -pop)

# 8) Use mutate() to change the existing lifeExp column, 
# by multiplying it by 12: 12 * lifeExp.
gapminder %>% mutate(lifeExp = 12 * lifeExp)

# 9) Use mutate() to add a new column, called lifeExpMonths, 
# calculated as 12 * lifeExp
gapminder %>% mutate(lifeExpMonths = 12 * lifeExp)

# 10) In one sequence of pipes on the gapminder dataset:
# filter() for observations from the year 2007,
# mutate() to create a column lifeExpMonths, calculated as 12 * lifeExp, and
# arrange() in descending order of that new column

gapminder %>% filter(year == 2007) %>% 
  mutate(lifeExpMonths = 12 * lifeExp) %>% 
  arrange(desc(lifeExpMonths))


# 11) Calculate GDP for each country in million dollars

gapminder %>% mutate(GDP = gdpPercap * pop/1000000)

# 12) Investigate how GDP in million dollars change in Turkey over time


gapminder %>% filter(country == 'Turkey') %>% 
  mutate(GDP = gdpPercap * pop/1000000) %>% 
  select(year,GDP)
  
# 13) Calculate the percentage of change of GDP in million dollars 
# at Turkey over time.

gapminder %>% filter(country == 'Turkey') %>% 
  mutate(GDP = gdpPercap * pop/1000000) %>% 
  select(year,GDP) %>% 
  mutate(
    GDP_perc_change = (GDP-lag(GDP))/lag(GDP)*100
  )
  

# 14)  Find the median life expectancy.

gapminder %>%
  summarize(medianLifeExp = median(lifeExp))

# 15)  Find the median life expectancy for the year 1957.

gapminder %>% filter(year == 1957) %>% 
  summarize(medianLifeExp = median(lifeExp))

# 16)  Find the median life expectancy for both the years 1957 and 2007.

gapminder %>% filter(year == 1957 | year == 2007) %>% 
  summarize(medianLifeExp = median(lifeExp))

# 17)  Find the median life expectancy for the years 1957, 1967, 
# 1977, 1987, 1997 and 2007.

gapminder %>% filter(year %in% seq(1957,2007,10)) %>% 
  summarize(medianLifeExp = median(lifeExp))


# 18) Find both the median life expectancy (lifeExp) and 
# the maximum GDP per capita (gdpPercap) in the year 1957, 
# calling them medianLifeExp and maxGdpPercap respectively.

gapminder %>% filter(year == 1957) %>% 
  summarize(
    medianLifeExp = median(lifeExp),
    maxGdpPerCap = max(gdpPercap))

# 19) Find the median life expectancy (lifeExp) and 
# maximum GDP per capita (gdpPercap) within each year, 
# saving them into medianLifeExp and maxGdpPercap, 
# respectively.

gapminder %>% group_by(year) %>% 
  summarize(
    medianLifeExp = median(lifeExp),
    maxGdpPerCap = max(gdpPercap))

# 20) Filter the gapminder data for the year 1957. 
# Then find the median life expectancy (lifeExp) and 
# maximum GDP per capita (gdpPercap) within each continent, 
# saving them into medianLifeExp and maxGdpPercap, 
# respectively.

gapminder %>% filter(year == 1957) %>% 
  group_by(continent) %>% 
  summarize(
    medianLifeExp = median(lifeExp),
    maxGdpPerCap = max(gdpPercap))

# 21) Find median life expectancy and maximum GDP per capita in 
# each year/continent combination, 
# arrange the result by continent

gapminder %>% 
  group_by(year, continent) %>% 
  summarize(
    medianLifeExp = median(lifeExp),
    maxGdpPerCap = max(gdpPercap)) %>% 
  arrange(continent)


# 22) Find the top 3 countries with the highest life expectancy in each continent
# for the year 2007. Sort the result by life expectancy in descending order.

gapminder %>% filter(year == 2007) %>% 
  group_by(continent) %>% 
  slice_max(lifeExp, n=3) %>% 
  arrange(desc(lifeExp))
