# install.packages("babynames")
library(babynames)


library(tidyverse)

## Q1 Investigate the data and 
# try to understand what each variable indicates

babynames
head(babynames)
tail(babynames)
str(babynames)
sum(babynames$prop)

# Q2 Select names and prop columns.

babynames %>% select(name, prop)

# Q3 Select babies with name Adam

babynames %>% filter(name == "Adam")

# Q4 Find names whose proportion >= 0.07

babynames %>% filter(prop >= 0.07)

# Q5 Find the number and proportion 
# for the name Adam in Year 1995

babynames %>% filter(name == "Adam", year==1995)
babynames %>% filter(name == "Adam" & year==1995)

# Q6 Find the number and proportion 
# for the names Adams and Mary in Year 1995
babynames %>% filter((name == "Adam" | name == "Mary") 
                     & year==1995)

babynames %>% filter(name == "Adam" | name == "Mary",year==1995)

# Q7 Find the number and proportion 
# for the names Adams, Mary, John, Williams in Year 1995

babynames %>% filter(name %in% c("Adams", "Mary", "John", "Williams"),
                     year==1995)

# Q8 Find the names for males in 2010. Sort the result such that names with the 
# higher proportion appears top.

babynames %>% filter( sex == "M", year==2010) %>% arrange(desc(prop))


# Q9 
## Filter the name Joe for the males.
## Create a line graph with year is in the X axis and prop is at the Y axis
## Make comment on your graph

babynames %>% filter(name == "Joe", sex == "M") %>% 
  ggplot(aes(year, prop))+
  geom_line(col = "blue")+
  theme_minimal()

# Q10 We define a name as popular if it meets two conditions:
##    * Sums - a large number of children have the name when you sum across years
##    * Ranks - it consistently ranks among the top names from year to year.

### a) First find the total number of names, 
### find the first and last year in the database

babynames %>% summarise(
  total_babies = sum(n),
  first_year = min(year),
  last_year = max(year)
  )

### b) Extract the rows where name == "Khaleesi". 
### Then use summarise() and a summary functions to find:
#### * The total number of children named Khaleesi
#### * The first year Khaleesi appeared in the data

babynames %>% filter(name=="Khaleesi") %>%  summarise(
  total_babies = sum(n),
  first_year = min(year),
)

### c) Find number of rows in the data and number of distinct names in the data. 
### You can use n()  and n_distinct() to get this.

babynames %>% summarise(
  n = n(),
  distinct_names = n_distinct(name)
)

### d) Find how many males and females are born?

babynames %>% group_by(sex) %>% 
  summarise(
    sum_gender = sum(n)
  )

### e) Use group_by(), summarise(), and arrange() to display
### the ten most popular names. 
### Compute popularity as the total number of children 
### of a single gender given a name.

babynames %>% group_by(sex, name) %>% 
  summarise(
    total = sum(n)
  ) %>% arrange(desc(total))

### f) Create a bar graph for 10 most popular names,
### gender colors should be different in the graph

# A Simple Graph

babynames %>% group_by(sex, name) %>% 
  summarise(
    total = sum(n)
  ) %>% 
  arrange(desc(total)) %>% 
  ungroup %>%
  slice(1:10) %>%  
  ggplot()+
  geom_col(aes(x=name, 
               y = total, 
               fill = sex))+
  theme_bw()+
  xlab("Names")

# A more complicated graph visually

library(scales)
library(forcats)
babynames %>% group_by(sex, name) %>% 
  summarise(
    total = sum(n)
  ) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  slice(1:10) %>%  
  ggplot()+
  geom_col(aes(x=fct_reorder(name,desc(total)), 
               y = total, 
               fill = sex))+
  theme_bw()+
  scale_fill_brewer(palette = "Reds")+
  # scale_y_continuous(labels = comma)+
  scale_y_continuous(labels = paste0(seq(0,5000, 250),"K"), 
                     breaks = seq(0,5000000, 250000),
  )+
  xlab("Names")+
  theme(axis.text.x = element_text(angle = 90))
  

## g) Use grouping to calculate and then plot the number of 
## children born each year over time
library(scales)
babynames %>% group_by(year) %>% 
  summarise(
    n_year = sum(n)
  ) %>% 
  ggplot()+
  geom_line(aes(x=year, y= n_year))+
  # scale_y_continuous(labels = comma)+
  ylab("Number of babies")+
  theme_classic()

## h) Create new column named perc by multiplying prop by 100.
## Create another new column called nper by rounding perc to an integer

babynames %>% mutate(
  perc = prop * 100,
  nper = round(perc)
)

## i) rank each row in babynames from largest prop to lowest prop.
## You can use mutate() and min_rank() functions to do this.
babynames %>% mutate(
  rank = min_rank(desc(prop))
) %>% arrange(rank)

## j) Compute each name's rank within its year and sex.
## Then compute the median rank for each combination of
## name and sex, and arrange the results from highest
## median rank to lowest.
## Hint: You should use group_by 2 times


# FIRST START WITH THE FIRST GROUP BY
babynames %>% group_by(sex, year) %>% 
  mutate(
  rank = min_rank(desc(prop))
) %>% arrange(rank)


# EXTEND THE FIRST GROUP BY
babynames %>% group_by(sex, year) %>% 
  mutate(
  rank = min_rank(desc(prop))
  ) %>% 
    group_by(name, sex) %>% 
    summarize(
      median_rank = median(rank)
    ) %>% arrange(median_rank)

## k) Visualize the results for first 10 names with a bar graph.

baby_data <- babynames %>% 
group_by(sex, year) %>% 
  mutate(
    rank = min_rank(desc(prop))
  ) %>% 
  group_by(name, sex) %>% 
  summarize(
    median_rank = median(rank)
  ) %>% arrange(median_rank) %>% 
  ungroup() %>% 
  slice(1:10)

# A simple graph

baby_data %>% ggplot()+
  geom_col(aes(x=name, y = median_rank, fill= sex))+
  scale_fill_brewer(palette = "Reds")+
  ylab("Median Rank")+
  xlab("Names")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


# A more complex graph

baby_data %>% ggplot()+
  geom_col(aes(x=fct_reorder(name,median_rank), y = median_rank, fill= sex))+
  scale_fill_brewer(palette = "Reds")+
  ylab("Median Rank")+
  xlab("Names")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
