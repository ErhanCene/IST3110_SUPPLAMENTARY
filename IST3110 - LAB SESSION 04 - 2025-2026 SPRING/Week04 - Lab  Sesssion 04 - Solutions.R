# IST3110 Week 04 - LAB SESSION 04-SOLUTIONS



### 1

mammo <- read.table("mammo.txt", sep=",",header = TRUE)

head(mammo)
str(mammo)
summary(mammo)


### 2
apply(mammo,2,table)


### 3

# mammo$BI.RADS <-  ifelse(mammo$BI.RADS == -99, NA, mammo$BI.RADS)
# mammo$Age <-  ifelse(mammo$Age == -99, NA, mammo$Age)
# mammo$Shape <-  ifelse(mammo$Shape == -99, NA, mammo$Shape)
# mammo$Margin <-  ifelse(mammo$Margin == -99, NA, mammo$Margin)
# mammo$Density <-  ifelse(mammo$Density == -99, NA, mammo$Density)
# mammo$Severity <-  ifelse(mammo$Severity == -99, NA, mammo$Severity)

####################################################################################################

change99withNA <- function(x){ifelse(x==-99,NA, x)}
mammo <- apply(mammo,2,change99withNA)
mammo <- as.data.frame(mammo)


### 4

# mammo$BI.RADS <- ifelse(is.na(mammo$BI.RADS), median(mammo$BI.RADS, na.rm = TRUE),mammo$BI.RADS)
# mammo$Age <- ifelse(is.na(mammo$Age), mean(mammo$Age, na.rm = TRUE),mammo$Age)
# mammo$Shape <- ifelse(is.na(mammo$Shape), median(mammo$Shape, na.rm = TRUE),mammo$Shape)
# mammo$Margin <- ifelse(is.na(mammo$Margin), median(mammo$Margin, na.rm = TRUE),mammo$Margin)
# mammo$Density <- ifelse(is.na(mammo$Density), median(mammo$Density, na.rm = TRUE),mammo$Density)
# mammo$Severity <- ifelse(is.na(mammo$Severity), median(mammo$Severity, na.rm = TRUE),mammo$Severity)

changeWithMean <- function(x){
  ifelse(is.na(x), mean(x,na.rm = T), x)
}

mammo$Age <- changeWithMean(mammo$Age)

changeWithMedian <- function(x){
  ifelse(is.na(x), median(x,na.rm = T), x)
}

mammo[,c(1,3:6)] <- apply(mammo[,c(1,3:6)], 2,changeWithMedian)

str(mammo)

### 5

# 
# mammo$BI.RADS <- as.factor(mammo$BI.RADS)
# mammo$Shape <- as.factor(mammo$Shape)
# mammo$Margin <- as.factor(mammo$Margin)
# mammo$Density <- as.factor(mammo$Density)
# mammo$Severity <- as.factor(mammo$Severity)

# apply(mammo[,c(1,3:6)], 2, as.factor)

mammo[,c(1,3:6)] <- lapply(mammo[,c(1,3:6)], as.factor)


str(mammo)

#### 6

mammo$AgeGroup <- cut(mammo$Age, c(min(mammo$Age)-1,40,60, max(mammo$Age)))
table(mammo$AgeGroup)

#### 7

table(mammo$AgeGroup, mammo$Severity)
prop.table(table(mammo$AgeGroup, mammo$Severity),1)

