x<- 33
y= 99
x*y+x/y

log(x); exp(x/y); sin(x); cos(y); sqrt(y)

D= 5000
K= 4
h= 0.5

sqrt((2*D*K)/h)

x == y
x < y

## Vector
z<- c(3, 5, 7, 9)
zz<- c("cup", "place", "pen", "paper")
mean(z)
sd(z)
median(z)
max(z)
min(z)
summary(z)
z[2]

summary(zz)

# Calculations
z1 <- c(2, 4, 6, 8)
z + z1
z * z1
sum(z * z1) # dot product

z+2 # adds 2 to each of the numbers (itemwise)

# Combine vectors
z2 <- c(z, z1)
z2[2]
z2[c(3,5)] # returns 3rd and 5th number
z2[z2>4] # gets all elements greater than 3
z2[order(z2, decreasing = TRUE)] # largest to smallest

# exercise
z2[z2>3 & z2<6]
z2[z2<3 | z2>6]

## Matrix
A <- matrix(data = z2, nrow = 4, ncol = 2)
A2 <- matrix(data = z2, nrow = 4, ncol = 2, byrow = T)
class(A)
A
A2

# Dimension
dim(A)
dim(A2)[1] # number of rows
nrow(A2) # same
ncol(A2)

# Transpose
B <- t(A)
B

# Calculation
A*A2

# Matrix multiplication
A%*%B # number of rows of a must match columns of B

# cbind(): column combine
A1 <- cbind(z, z1)
cbind(A, A1)
# rbind(): row combine
rbind(z, z1)

# indexing for matrix
A[1,2]
A[c(1,3), 1]

# Vector is one dimentional
# Matrix is two dimentional

# row sum -  press columns togther and sum them
# column sum - press rows togther and sum them

rowSums(A)
colSums(A)

## Data Frame
# - anything imported will be a data frame

dataA <- data.frame(A)

# importing
mydata.csv <- read.csv("./storks.csv", header=T)
mydata.txt <- read.table("./storks.txt", header=T, sep="\t")

# built in dataset
data(cars)
dim(cars)
# previiew
head(cars, n=5)
# variable (column) names
names(cars)

summary(cars)
str(cars)

## List
mylist <- list(myvector=z2, mymatrix=A, mydata=cars)

# accesing portion
mylist$myvector
mylist[['myvector']]
mylist[[2]]

## functions
mean(z)

# truncation function
mytrunc.func <- function(vector, lower, upper) {
  vector[which(vector<lower)] <- lower
  vector[which(vector>upper)] <- upper
  return(vector)
}

mytrunc.func(z2, 3, 7)

plot(cars$speed, cars$dist, xlab = 'speed', ylab = 'distance')

i <- 1
x <- 1
while(i < 100) {
  i <- i+1
  x <- x+1/i
}
x

x <- 1
for(i in 2:100) {
  x <- x+1/i
}
x

x <- 1
for(i in 2:100000) {
  x <- x+1/i^2
}
x

## Exercise
#1
v <- c(5, 2, 11, 19, 3, -9, 8, 20, 1)
sum(v)
mean(v)
sd(v)
#2
v2 <- sort(v, decreasing = T)
#3
vv <- matrix(v, nrow = 3, ncol = 3)
sum(vv[1,])
vv[3,2]
colSums(vv)
#4
dim(CustomerData)
summary(CustomerData)
names(CustomerData)
mean(CustomerData$DebtToIncomeRatio)

marriage.func <- function(statuses) {
  total = length(statuses)
  married_count = 0
  for (status in statuses) {
    if (status == "Married") {
      married_count = married_count + 1
    }
  }
  
  return(married_count/total)
}

marriage.func(CustomerData$MaritalStatus)
### same as above
mean(CustomerData$MaritalStatus=="Married")
