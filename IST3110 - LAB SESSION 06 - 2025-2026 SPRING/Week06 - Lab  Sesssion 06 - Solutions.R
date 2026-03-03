# IST3110 Week 06 - LAB SESSION 06-SOLUTIONS

# Import required packages
library(dplyr)
library(gapminder)
library(ggplot2)

# 1) Create a scatter plot so that population (pop) 
# is on the x-axis and life expactancy (lifeExp) is on the y-axis
# for the year 2007.


gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=pop, y=lifeExp))+
  geom_point()+
  theme_bw()


# 2) Change the scatter plot on the previous question such that 
# put the x-axis on a log scale by using 
# additional argument scale_x_log10() or by using 
# mutate() and log10().

# 1st Way

gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=pop, y=lifeExp))+
  geom_point()+
  scale_x_log10()+
  theme_bw()


# 2nd Way

gapminder %>% filter(year == 2007) %>% 
  mutate(pop = log10(pop)) %>% 
  ggplot(aes(x=pop, y=lifeExp))+
  geom_point()+
  theme_bw()


# 3) Suppose you want to create a scatter plot with population 
# on the x-axis and life expectancy on the y-axis for the year 2007. 
# Use log scales for both of the axis and color the points according to the continents.
# Use size argument to make points larger.


gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=pop, y=lifeExp, color=continent))+
  geom_point(size=3)+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()

# 4) Instead of giving a fixed point size like in the previous plot, 
# make point size appear proportional to GDP per Cap.

gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=pop, y=lifeExp, color=continent, size=gdpPercap))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()

# 5) Now instead of coloring the continent use facets to divide them into 
# separate panels.

gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=pop, y=lifeExp, size=gdpPercap, color=continent))+
  geom_point()+
  facet_wrap(~ continent)+
  scale_x_log10()+
  scale_y_log10()+
  theme_bw()

# 6) Now create another scatterplot for the whole gapminder data where
# gdpPercap is at the x axis and lifeExp at the y axis with color 
# representing continent and size representing population, faceted by year. 
# Only log correct gdpPercap.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, 
                      color = continent, size = pop)) + 
  geom_point() + 
  scale_x_log10() + 
  facet_wrap( ~ year)+
  theme_bw()

# 7) First find the median life expectancy for each year, then use this data
# to visualize how median life expectancy changes over years. Use additional 
# argument expand_limits(y=0) in the ggplot to guarantee point zero 
# is included in the graph.

gapminder %>% group_by(year) %>% 
  summarize(
    medianLifeExp = median(lifeExp, na.rm = T)
  ) %>% ggplot(aes(x=year, y =medianLifeExp))+
  geom_point()+
  theme_bw()+
  expand_limits(y=0)


# 8) Now use the same approach to visualize how median gdp per cap changes
# over years among each continent.

gapminder %>% group_by(continent,year) %>% 
  summarize(
    medianGdpPerCap = median(gdpPercap, na.rm = T)
  ) %>% ggplot(aes(x=year, y =medianGdpPerCap, color=continent))+
  geom_point(size=4)+
  theme_bw()+
  expand_limits(y=0)


# 9) Filter the gapminder dataset for the year 2007, then summarize the 
# median GDP per capita and the median life expectancy within each continent, 
# into columns called medianLifeExp and medianGdpPercap. 
# Save this as by_continent_2007.
# Use the by_continent_2007 data to create a scatterplot comparing these 
# summary statistics for continents in 2007, putting the median GDP per capita 
# on the x-axis to the median life expectancy on the y-axis. 
# Color the scatter plot by continent.

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year==2007)  %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp),
            medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x= medianGdpPercap, y= medianLifeExp,
                              colour=continent))+
  geom_point()+theme_bw()



# 10) Use a linegraph with points to see how median GDP per cap changes over 
# years according to continents.

gapminder %>% group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap)) %>% 
  ggplot(aes(x=year, y= medianGdpPercap,colour = continent)) +
  geom_line(size=2) + 
  geom_point(size=4)+
  theme_bw() +
  expand_limits(y = 0)
  

# 11) This time create a bar graph to visualize median gdp per cap 
# for the year 2007 among continents.

gapminder %>% filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(
    medianGdpPerCap = median(gdpPercap, na.rm = T)
  ) %>% ggplot(aes(x=continent, y=medianGdpPerCap, 
                   fill=continent))+
  geom_col()+
  theme_bw()



# 12) Create a bar graph to visualize gdp per cap 
# for each the country in the Oceania for the year 2007.

gapminder %>% 
  filter(continent == "Oceania", year == 2007) %>% 
  group_by(country) %>% 
  ggplot(aes(x=country, y=gdpPercap, 
                   fill=country))+
  geom_col()+
  theme_bw()


# 13) Create a histogram of country population in the year 2007.
# First, create a new variable at the gapminder data called pop_by_mil
# by dividing the population with 1 million. Then draw the histogram 
# with the number of bins set to 50.

gapminder %>% 
  filter(year == 2007) %>% 
  mutate(
    pop_by_mil = pop/1000000
  ) %>%
  ggplot(aes(x=pop))+
  geom_histogram(bins=50)


# 14) Did you able to understand anything from the previous plot? This
# time draw the same graph again by log transforming the x-axis.
# Then add density plot over the histogram by using after_stat(density)
# and geom_density()

gapminder %>% 
  filter(year == 2007) %>% 
  mutate(
    pop_by_mil = pop/1000000
  ) %>% 
  ggplot(aes(x=pop_by_mil))+
  geom_density(color='red',size=2)+
  geom_histogram(bins=50, col='black', fill='white',size=1.1)+
  scale_x_log10()+
  theme_bw()

gapminder %>% 
  filter(year == 2007) %>% 
  mutate(
    pop_by_mil = pop/1000000
  ) %>% 
  ggplot(aes(x=pop_by_mil))+
  geom_histogram(aes(y=after_stat(density)),bins=50, col='black', fill='white',size=1.1)+
  geom_density(color='blue',size=2, fill='blue', alpha=0.3)+
  scale_x_log10()+
  theme_bw()

# 15) Use the gapminder dataset to create a boxplot comparing GDP 
# per capita (gdpPercap) among continents for the year 2007. 
# Put the y-axis on a log scale with scale_y_log10().
# Add a title called 'Comparing GDP per capita across continents.'

gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot()+
  scale_y_log10()+
  ggtitle('Comparing GDP per capita across continents.')+
  xlab('Continent')+
  ylab('GDP per capita')+
  theme_bw()




# 16) Learn how to customize plots.

library(scales)


# Geom point for gdpPerCap vs LifeExp
gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  theme_bw()

# log on x axis

gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  scale_x_log10()+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  theme_bw()

# Alter number labels on axis

gapminder %>% filter(year == 2007) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  scale_x_log10(breaks = c(seq(0,3000,100),
                           seq(3000,7000,250),
                           seq(7000,16000,500),
                           seq(16000,30000,1000),                           
                           seq(30000,50000,2500)),
                labels = label_number_auto())+
  geom_point(color='blue')+
  geom_text(aes(label=country),size=3,check_overlap =T)+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  theme_bw()+
  scale_y_continuous(breaks = seq(40,100,5))+
  theme(axis.text.x = 
          element_text(angle = 90, 
                       vjust = 0.5, 
                       hjust=1))


gapminder %>% filter(year == 2007) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  scale_x_log10(breaks = c(seq(0,3000,100),
                           seq(3000,7000,250),
                           seq(7000,16000,500),
                           seq(16000,30000,1000),                           
                           seq(30000,50000,2500)),
                labels = label_number_auto())+
  geom_point(aes(color=continent))+
  geom_text(aes(label=country,color=continent), size=3,check_overlap =T)+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  theme_bw()+
  scale_y_continuous(breaks = seq(40,100,5))+
  theme(axis.text.x = 
          element_text(angle = 90, 
                       vjust = 0.5, 
                       hjust=1))




gapminder %>% filter(year == 2007) %>%
  mutate(lifeExptext = ifelse(lifeExp>70,as.character(country),'')) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  scale_x_log10(breaks = c(seq(0,3000,100),
                           seq(3000,7000,250),
                           seq(7000,16000,500),
                           seq(16000,30000,1000),                           
                           seq(30000,50000,2500)),
                labels = label_number_auto())+
  geom_point(aes(color=continent))+
  geom_text(aes(label=lifeExptext,colour = continent))+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  theme_bw()+
  scale_y_continuous(breaks = seq(40,100,5))+
  theme(axis.text.x = 
          element_text(angle = 90, 
                       vjust = 0.5, 
                       hjust=1))

# 17) Learn how to customize colors in plots.


colors_with_rgb <- c(
  rgb(0.20, 0.60, 0.86),  # sky blue
  rgb(0.96, 0.49, 0.19),  # orange
  rgb(0.47, 0.67, 0.19),  # green
  rgb(0.84, 0.15, 0.16),  # red
  rgb(0.58, 0.40, 0.74)   # purple
)


gapminder %>% filter(year == 2007) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  geom_point(aes(color=continent),size=3)+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  scale_x_log10()+
  # scale_color_brewer(palette='RdYlBu')+
  # scale_color_brewer(palette='Reds')+
  # scale_color_manual(values=colors_with_rgb)+
  # scale_color_hue(h=c(240,360),l=40, c=90)+
  theme_bw()



gapminder %>% filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(
    medianGdpPerCap = median(gdpPercap, na.rm = T)
  ) %>% ggplot(aes(x=continent, y=medianGdpPerCap, 
                   fill=continent))+
  geom_col(color='black',linewidth=1)+
  ggtitle('GDP per capita vs Life Expactancy (2007)')+
  xlab('GDP per Capita')+
  ylab('Life Expactancy')+
  # scale_fill_brewer(palette='RdYlBu')+
  # scale_fill_brewer(palette='Reds')+
  # scale_fill_manual(values=colors_with_rgb)+
  # scale_fill_hue(h=c(240,360),l=40, c=90)+
  theme_bw()



