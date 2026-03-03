
library(tidyverse)
library(rstatix)
library(ggpubr)


sales <- read.csv('sales.csv')


# Q1) Get summary stats and frequency tables for the sales dataset.
# Identify types of the data and number of choices in categorical variables.

str(sales)

sales %>% get_summary_stats(type = 'common')

# training level takes values 0 to 3 so it may be considered 
# as a categorical variable
# Also convert all other variables to categorical variables too.

sales <- sales %>% mutate(
  training.level = as.factor(training.level),
  across(where(is.character), as.factor)
)

sales %>% get_summary_stats(type = 'common')

sales %>% select(where(is.factor)) %>% apply(2,freq_table)


sales %>% select(where(is.numeric)) %>% 
  apply(2, shapiro_test)


# Q2) Test the null hypothesis average sales is equal to 335000 against
# the alternative hypothesis average sales is less than 335000.

sales %>% shapiro_test(sales)
# Normal One sample t-test

sales %>% t_test(sales ~ 1, mu = 335000, alternative = 'less')

# Reject null hypothesis, the average sale is less than 335000.


# Q3) Test the null hypothesis average salary is equal to 90000 against
# the alternative hypothesis average salary is higher than 90000.

sales %>% shapiro_test(salary)
# Not normal One sample Wilcoxon signed rank test

sales %>% wilcox_test(salary ~ 1, mu = 90000, alternative = 'greater')
# H0 can not be rejected the average salary is not greater than 90000.


# Q4) Find the average sales based on training level

sales %>% group_by(training.level) %>% 
  get_summary_stats(sales, type='mean_sd') %>% 
  arrange(variable)


# Q5) Test the null hypothesis average sales is equal to 375000 against
# the alternative hypothesis average sales is not 
# equal to 375000 for each training level

sales %>% group_by(training.level) %>%  shapiro_test(sales)

# Only group 3 is not normal

sales %>% filter(training.level != 3) %>% 
  group_by(training.level) %>% 
  t_test(sales~1, mu=375000)
# For training group 0 and training group 1 we reject the null hypothesis
# average sales is not equal to 375000
# whereas for training group 2 we accept the null hypothesis
# average sales is equal to 375000

sales %>% filter(training.level == 3) %>% 
  group_by(training.level) %>% 
  wilcox_test(sales~1, mu=375000)

# For training group 3  we reject the null hypothesis
# average sales is not equal to 375000


# Q6) Does computer software division make more sales than 
# office supplies division


sales %>% group_by(division) %>% 
  get_summary_stats(sales,salary, type='mean_sd') %>% 
  arrange(variable)


sales %>% filter(division %in% c('office supplies', 'computer software')) %>%
  mutate(division = droplevels(division)) %>% 
  ggboxplot(x='division', y='sales',
          color = 'division',
          palette = "jco")


sales %>% filter(division %in% c('office supplies', 'computer software')) %>% 
  group_by(division) %>% 
  shapiro_test(sales)

# Both of them are normal we can use independent samples t-test

sales %>% levene_test(sales ~ division)
# Variances are also equal


sales %>% filter(division %in% c('office supplies', 'computer software')) %>%
  mutate(division = droplevels(division)) %>% 
  t_test(sales~division, paired = F, var.equal = T, alternative = 'greater')

# p>0.05 The staff in the computer software division doesn't make more sale
# compared to the staff in the office supplies division


# Q7) Does computer software division make more salary than 
# office supplies division


sales %>% filter(division %in% c('office supplies', 'computer software')) %>%
  mutate(division = droplevels(division)) %>% 
  ggboxplot(x='division', y='salary',
            color = 'division',
            palette = "jco")


sales %>% filter(division %in% c('office supplies', 'computer software')) %>% 
  group_by(division) %>% 
  shapiro_test(salary)
# Both of them are normal we can use independent samples t-test

sales %>% levene_test(salary ~ division)
# Variances are also equal


sales %>% filter(division %in% c('office supplies', 'computer software')) %>%
  mutate(division = droplevels(division)) %>% 
  t_test(salary~division, paired = F, var.equal = T, alternative = 'greater')

# p<0.05, Reject null hypothesis. The staff in the computer software division 
# get more salary compared to the staff in the office supplies division.



# Q8) Does average sales and average salary differ among level of education


sales %>% ggboxplot(x='level.of.education', y='sales',
                    color = 'level.of.education',
                    palette = "jco")+
  theme(axis.text.x = element_text(angle = 90))



sales %>% ggboxplot(x='level.of.education', y='salary',
                    color = 'level.of.education',
                    palette = "jco")+
  theme(axis.text.x = element_text(angle = 90))



sales %>% group_by(level.of.education) %>% 
  shapiro_test(salary, sales) %>% 
  arrange(variable)

# All p values are greater than 0.05, the distribution is normal


sales %>% 
  levene_test(salary ~ level.of.education)

sales %>% 
  levene_test(sales ~ level.of.education)
# both p values are greater than 0.05 variances are homogeneous
# ANOVA can be applied

sales %>% anova_test(salary ~ level.of.education)
# at least one group has different average 

sales %>% tukey_hsd(salary ~ level.of.education) %>% 
  print(width=Inf)

# There are differences in salaries among
## associate degrees and high school
## bachelors degree and high school
## bachelors degree and some college


sales %>% anova_test(sales ~ level.of.education)
# p > 0.05 none of the group make higher sales 
sales %>% tukey_hsd(sales ~ level.of.education) %>% 
  print(width=Inf)
# All pairwise comparisons are non-significant


# Q9) Is there a relationship between salary and the sales

sales %>% cor_test(salary,sales)
# correlation is 0.9 

sales %>% ggscatter(x='salary', y='sales')

sales %>% ggscatter(x='salary', y='sales',
                    color = 'level.of.education')

sales %>% ggscatter(x='salary', y='sales',
                    color = 'division')

sales %>% ggscatter(x='salary', y='sales',
                    color = 'training.level')


sales %>% ggscatter(x='salary', y='sales',
                    color = 'division', shape = 'training.level')


sales %>% ggscatter(x='salary', y='sales',
                    color = 'division', shape = 'training.level',
  facet.by = "level.of.education")


# Q10) Does level of education and training level are related?


sales %>% select(level.of.education, training.level) %>% table


sales %>% select(level.of.education, training.level) %>% table %>% 
  chisq_test()
# There is no relation between level of education and training level


# Q11) Does division and training level are related?

sales %>% select(division, training.level) %>% table

sales %>% select(division, training.level) %>% table %>% 
  chisq_test()

# There is no relation between level of education and training level


################################################################################
################################################################################

tips <- read.csv('tips.csv')


# Q12) Does female waitresses are getting more tip than male waitresses?

tips %>% group_by(sex) %>% 
  shapiro_test(tip)

# tip is not distributed normal
# we should employ non-parametric test

tips %>% wilcox_test(tip~sex, alternative = "greater")
# p > 0.05, average tip is not higher for females

tips %>% group_by(sex) %>% 
  get_summary_stats(type='mean_sd') %>% 
  arrange(variable)


tips %>% wilcox_test(tip~sex, alternative = "less")
# p > 0.05, average tip is not less for females

# Q13) Does average tip higher on friday and saturday compared to other days


tips %>% freq_table(day)

tips %>% group_by(day) %>% 
  get_summary_stats(tip,type='mean_sd')


tips %>% group_by(day) %>% 
  shapiro_test(tip)
# Tip for Fri and Sunday is distributed normal (p> 0.05) but
# tip for Sat and Thu is not distributed normal.
# So we should employ non parametric tests

tips %>% kruskal_test(tip~day)
# p < 0.05 average tips for at least one day is different

tips %>% dunn_test(tip~day)
# average tips for sunday and Thursday is different


# Q14) Does smokers pay higher total bill

tips %>% group_by(smoker) %>% 
  get_summary_stats(total_bill,type='mean_sd')


tips %>% group_by(smoker) %>% 
  shapiro_test(total_bill)
# p<0.05 we should employ non-parametric test

tips %>% wilcox_test(total_bill~smoker)
# p>0.05 on average there is no difference on total bill
# that smokers and non smokers are paying

# Q15) Are total bill, tip and size related
str(tips)
tips %>% 
  mutate(size = as.numeric(size)) %>%
  select(where(is.numeric)) %>% 
  cor_test(method='pearson')
# There is moderate positive relationship


tips %>% ggscatter(x='total_bill', y='tip',
                   color = 'size')

# Q16) Does day and time of the day are related


tips %>% select(day, time) %>% table

tips %>% select(day, time) %>% table %>% chisq_test()
# p<0.05 day and time are related
# on thursday almost all of the meals are lunch and
# on saturday and sunday all of the meals are dinner.



