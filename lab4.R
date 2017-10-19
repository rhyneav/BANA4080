# Load data
library(MASS)
data("Boston")

# Split the Data
index <- sample(nrow(Boston), nrow(Boston)*.90)
Boston.train <- Boston[index,]
Boston.test <- Boston[-index,]

# Model
model0 <- lm(medv~lstat, data=Boston.train)
model1 <- lm(medv~., data=Boston.train)
model2 <- lm(medv~.-indus-age, data=Boston.train)

# Best Subset variable selection
## Install package
install.packages("leaps")
library(leaps)

## Do stuff
model.subset <- regsubsets(medv~., data=Boston.train, nbest=2, nvmax=14) 
### nbest=at each number of variables, find the best 2
### nvmax=maximum number of variables

summary(model.subset)
plot(model.subset, scale="bic")

# Model
nullmodel <- lm(medv~1, data=Boston.train)
fullmodel <- lm(medv~., data=Boston.train)

n <- nrow(Boston.train)

## Backward
### Use k=log(n) if you want to use BIC instead of AIS
model.step.b <- step(fullmodel, direction = "backward", k=log(n))
summary(model.step.b)

## Forward
model.step.f <- step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), direction = "forward")
summary(model.step.b)

## Step
model.step.s <- step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), direction = "both")
summary(model.step.s)


# Testing 
pred.step.f <- predict(model.step.f, newdata = Boston.test)
MSPE.step.f <- mean((Boston.test$medv-pred.step.f)^2)
MSPE.step.f

## LASSO
install.packages("glmnet")
library(glmnet)

### standardize data
Boston.std <- scale(Boston)
index <- sample(nrow(Boston), nrow(Boston)*.90)
Boston.train <- Boston.std[index,]
Boston.test <- Boston.std[-index,]

### fit and plot it
lasso.fit <- glmnet(x=as.matrix(Boston.train[,-14]), y=Boston.train[,14])
lasso.fit$lambda

plot(lasso.fit, xvar="lambda")

### cross validation
cv.lasso <- cv.glmnet(x=as.matrix(Boston.train[,-14]), y=Boston.train[,14])
plot(cv.lasso)

### prediction
as.vector(coef(lasso.fit, s=.1)) # s is tuning parameter (lamda)
as.vector(coef(lasso.fit, s=.5))
as.vector(coef(lasso.fit, s=cv.lasso$lambda.min)) # gets left line from plot
as.vector(coef(lasso.fit, s=cv.lasso$lambda.lse)) # gets right line from plot
