# IST3110 Week 2 - LAB SESSION 2-SOLUTIONS


## 1

vec <- runif(24, 1,5)
vec

## 2

mat <- matrix(vec, nrow = 6, byrow = TRUE)
mat

## 3

mat[c(3,5), c(2,4)]


## 4

mammo <- read.table(file = 'mammo.txt', sep=',', header = T)

student_performance <- read.table('student_performance.txt', 
                                  sep = '&', header = T)

## 5

mammo_small <- mammo[, c('Age', 'Severity')]
mammo_small <- mammo[, c(2, 6)]


## 6 

mean(mammo_small$Age)

mean(mammo_small[mammo_small$Severity == '1',]$Age)

mean(mammo_small[mammo_small$Severity == '0',]$Age)


## 7

student_performance_list <- list()

student_performance_list$gender <- student_performance$gender

student_performance_list$math.score <- student_performance$math.score

student_performance_list$gender_summary <- table(student_performance$gender)

student_performance_list$math_mean <- mean(student_performance$math.score,na.rm = T)

student_performance_list$hist_math_score <- hist(student_performance$math.score) 

str(student_performance_list)
student_performance_list

plot(student_performance_list$hist_math_score)
