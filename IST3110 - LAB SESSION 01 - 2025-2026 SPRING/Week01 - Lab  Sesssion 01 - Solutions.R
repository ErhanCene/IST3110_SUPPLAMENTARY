# IST3110 Week 1 - LAB SESSION 1-SOLUTIONS


## 1

log(12.43)              # natural log
log10(12.43)            # log to base 10
log2(12.43)             # log to base 2
log(12.43, base = 2)    # alternative log to base 2
sqrt(12.43)             # square root
exp(12.43)              # exponent




## 2

area_circle <- pi * (20/2)^2


## 3 

(14 * 0.51)^(1/3)

## 4

weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)

## 5

first_five <- weights[1:5]                  # extract first 5 weight values
first_five <- weights[c(1, 2, 3, 4, 5)]     # alternative method

## 6

height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)

## 7

shorter_child <- height[height <= 99]     # extract all heights less than or equal to 99

## 8 

shorter_child <- height[height <= 99]     # extract all heights less than or equal to 99

## 9 

seq1 <- seq(from = 0, to = 1, by = 0.1)

## 10

seq2 <- rev(seq(from = 1, to = 10, by = 0.5))

## 11

rep(1:3, times = 3)
rep(c("a", "c", "e", "g"), each = 3)
rep(c("a", "c", "e", "g"), times = 3)
rep(1:3, each = 3, times = 2)
rep(1:5, times = 5:1)
rep(c(7, 2, 8, 1), times = c(4, 3, 1, 5))



## 12

x <- c(2,5,3,1,2,1,2,2,4,1,1,3,1,2,1,3,4,5,5,2)


## 13

x==2

x[x==2] <- 0
x

## 14

y <- x[x>2]
y

## 15

z <- c(3, NA, 5, NA, 4, 1, 2, 3)

## 16

newVec1 <- y + z
newVec1
  
newVec2 <- y + z
newVec2

## 17

newVec1[is.na(newVec1)] <- mean(newVec1,na.rm = TRUE) 
newVec1

## 18
newVec2[is.na(newVec2)] <- median(newVec2,na.rm = TRUE) 
newVec2

## 19

# Median is 7.5 and mean is 7.3 median is slightly higher than the mean.

## 20

opinion <- c(3,1,0,4,2,2,1,5,2,3)

quality <- factor(opinion, levels = 0:5, 
                  labels = c("very bad", "bad", "not bad", "ok","good","very good"))
quality

