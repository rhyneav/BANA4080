data("iris")
class(iris)
dim(iris)
nrow(iris)
ncol(iris)
names(iris)
str(iris)
head(iris)

class(iris$Species)
class(iris$Sepal.Length)

summary(iris)
apply(iris[,-5], 2, sd) # 2 means by column; sd calculates standard dev

quantile(iris$Sepal.Length)

aggregate(.~Species, iris, mean)

## 09/05
mysummary <- rbind(
  apply(iris[,-5], 2, sd),
  apply(iris[,-5], 2, mean),
  apply(iris[,-5], 2, quantile)
)
mysummary

round(mysummary, 3)
#! Copy and paste this into excel and make pretty for report



### Summary by group

aggregate(Sepal.Length~Species, data = iris, mean)
mygroupsummary <- aggregate(.~Species, data=iris, sd) # . means everything except

# artificially create a new categorical variable
sepallength.cate <- cut(iris$Sepal.Length, breaks = quantile(iris$Sepal.Length), include.lowest = T)
sepallength.cate

iris1 <- iris

iris1$sepal.length.cate <- sepallength.cate # attach to the dataset
# use plus sign to add more groups
mygroup2summary <- aggregate(.~Species+sepal.length.cate, data = iris1, mean)
mygroup2summary

# pivot table
table(iris$Species)
table(iris1$Species, iris1$sepal.length.cate)


###! Exercise 

#2
nrow(CustomerData)
ncol(CustomerData)
#3
head(CustomerData)

#4
rbind(
  summary(CustomerData$Age),
  summary(CustomerData$EducationYears),
  summary(CustomerData$HHIncome),
  summary(CustomerData$CreditDebt)
)
## alt 
customersubdata <- CustomerData[c("Age", "EducationYears", "HHIncome", "CreditDebt")]
rbind(
  apply(customersubdata, 2, mean), # 2 means by column
  apply(customersubdata, 2, sd),
  apply(customersubdata, 2, quantile)
)

#5
aggregate(HHIncome~MaritalStatus, data=CustomerData, mean)

# 6
table.default.job <- table(CustomerData$LoanDefault, CustomerData$JobCategory)
table.default.job[1,]/table(CustomerData$JobCategory)
table.default.job

### Visualizations

hist(iris$Sepal.Length, col="blue", breaks=20)
hist(iris$Sepal.Length, col="green", breaks=20, probability=T, main="Histogram" ,xlab="Sepal Length", ylab="Dense")
plot(density(iris$Sepal.Length))
# add line on top of current plot
lines(density(iris$Sepal.Length), col="red")
# add average line
abline(v=mean(iris$Sepal.Length), col="blue", lty=2, lwd=1.5)

aveg<- apply(iris[,1:4], 2, mean)
barplot(aveg, ylab = "Average")

pie(table(iris$Species), col=rainbow(3))

boxplot(iris$Sepal.Length)

boxplot(iris[,1:4], notch=T, col=c("red", "blue", "yellow", "grey"))

boxplot(iris[,1]~iris[,5], notch=T, ylab="Sepal Length", col="blue")

# boxplot by groups
boxplot(Sepal.Length~Species, data = iris, ylab="Sepal Length")

# Scatter plot
plot(iris$Sepal.Length, iris$Sepal.Width, xlab = "Length", ylab = "Width", main = "Sepal", pch=19, col=4, cex=0.5, cex.lab=1.5, cex.main=2) # pch changes dot look, cex changes style

pairs(iris[,1:4], col=rainbow(200))

# Parallel coordinate plot
library(MASS)
parcoord(iris[,1:4],col=iris$Species)

## Graphics options
# Lets you layout multiple plots (2 rows, 3 columns) 6 plots total!
par(mfrow=c(2,3))
par(mar=c(4,4,2,2))

# Data manipulation
## for data exploration
install.packages("tidyverse")
library(tidyverse)

## dplyr: useful for data manipulation

# filter: filter out data based on condition
iris_filter <- filter(iris, Sepal.Length<5 | Sepal.Width>3)
iris_filter <- filter(iris, Species=="setosa")

iris_select <- select(iris, Sepal.Length, Sepal.Width)
iris_select <- select(iris, -Sepal.Length, -Sepal.Width)
names(iris_select)

# Reorder columns
iris_order <- select(iris, Species, everything())
names(iris_order)

# Rename Column
iris_rename <- rename(iris, SL=Sepal.Length)
names(iris_rename)

# Add new variable
iris_newvar <- mutate(iris, SL_SW.ratio=Sepal.Length/Sepal.Width)
names(iris_newvar)
head(iris_newvar)
## no mutuate
iris$SL_SW.ratio <- iris$Sepal.Length/iris$Sepal.Width
head(iris)

customer <- read.csv("http://homepages.uc.edu/~lis6/Teaching/DM2017Fall/Data/CustomerData.csv")
# How many missing values there are
sum(is.na(customer))

# Clean by removing missing values
customer_clean <- na.omit(customer)
nrow(customer_clean)

# Impute data: replace blanks with median value. Need to go column by column
medHS <- median(customer$HouseholdSize, na.rm=T)
customer$HouseholdSize[is.na(customer$HouseholdSize)==T] <- medHS
