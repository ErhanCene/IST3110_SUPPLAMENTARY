# IST3110 Week 03 - LAB SESSION 03-SOLUTIONS

## 1

Age_Group <- function(age){
  if(class(age)!= "numeric"){
    stop("Your age should be given in numeric format!!!")
  }
  
  if(age<18){
    print("Young")
  } else if(age<=65){
    print("Adult")
  }else print("Old")
}

Age_Group(5)
Age_Group(20)
Age_Group(66)
Age_Group("5")

Age_Group2 <- function(age){
  if(class(age)!= "numeric"){
    stop("Your age should be given in numeric format!!!")
  }
  
  ifelse(age <18, "Young", ifelse(
    age <=65, "Adult","Old")
  )
}

Age_Group2(5)
Age_Group2(20)
Age_Group2(66)
Age_Group2("5")

## 2 

Age_Gender_Group <- function(age, gender){
  if(class(age) %in% c("numeric","integer")==F){
    stop("Your age should be given in numeric format!!!")
  }
  
  if(gender %in% c("M","F") == F){
    stop("Your gender should be either 'F' or 'M'!!!")
  }
  
  
  if(age<18 & gender == "M"){
    print("son")
  } else if(age<18 & gender == "F"){
    print("daughter")
  } else if(age<=65 & gender == "M"){
    print("father")
  } else if(age<=65 & gender == "F"){
    print("mother")
  } else if(age>65 & gender == "M"){
    print("grandfather")
  } else print("grandmother")
}

Age_Gender_Group(5,"M")
Age_Gender_Group(5,"F")
Age_Gender_Group(20,"M")
Age_Gender_Group(20,"F")
Age_Gender_Group(70,"M")
Age_Gender_Group(70,"F")
Age_Gender_Group(70,"f")
Age_Gender_Group("70","F")

Age_Gender_Group2 <- function(age, gender){
  if(class(age) %in% c("numeric","integer")==F){
    stop("Your age should be given in numeric format!!!")
  }
  
  if(gender %in% c("M","F") == F){
    stop("Your gender should be either 'F' or 'M'!!!")
  }
  
  ifelse(age<18, ifelse(gender == "M", "son", "daughter"),
         ifelse(age <= 65, ifelse(gender == "M", "father", "mother"),
                ifelse(gender == "M", "grandfather", "grandmother")
         )
  )
}

Age_Gender_Group2(5,"M")
Age_Gender_Group2(5,"F")
Age_Gender_Group2(20,"M")
Age_Gender_Group2(20,"F")
Age_Gender_Group2(70,"M")
Age_Gender_Group2(70,"F")
Age_Gender_Group2(70,"f")
Age_Gender_Group2("70","F")




## 3

student <- read.table("student_performance.txt", sep="&", header = TRUE)
head(student)
str(student)
summary(student)

## 4

student$score <- apply(student[,7:9],1, mean, na.rm=T)

student$score <- round(student$score)
str(student)
head(student)

## 5

student_completed <- student[student$test.preparation.course=="completed",]
student_not_completed <- student[student$test.preparation.course=="none",]
head(student_completed)
head(student_not_completed)

## 6

table(student$parental.level.of.education)

student_completed$hsplus <- ifelse(
  student_completed$parental.level.of.education != "some high school",1,0)

student_completed$hsplus <- factor(student_completed$hsplus, levels= 0:1, 
                                   labels=c("Some High School", "High School or Above"))

head(student_completed)


#### WHAT WOULD WE DO IF WE SEPERATE AS "at least college student","at most high school graduate"


student$univ_stud <- ifelse( (student$parental.level.of.education == "master's degree") |
                               (student$parental.level.of.education == "bachelor's degree") |
                               (student$parental.level.of.education == "associate's degree") |
                               (student$parental.level.of.education == "some college"),
                             "at least college student","at most high school graduate")

table(student$univ_stud)



## 7

table(student_not_completed$gender, student_not_completed$race.ethnicity)

