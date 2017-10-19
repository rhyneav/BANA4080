library(MASS)
data("Boston")
dim(Boston)

?Boston

# EDA
str(Boston)
summary(Boston)

## Plots for EDA
hist(Boston$medv)

## standardization
std.Boston <- scale(Boston) # rescale to mean of 0 and std of 1. Good when data has very different magnitude
apply(std.Boston, 2, mean)

# Modeling
## Random Sampling for training/test
index <- sample(1:nrow(Boston), 0.8*nrow(Boston), replace = F)
Boston.train <- Boston[index,]
Boston.test <- Boston[-index,]

## Simple linear regression
model0 <- lm(medv~lstat, data=Boston.train) #medv and lstat are  variables in Boston
model0

## Plot
plot(medv~lstat, data=Boston.train)
abline(model0, col="navy", lwd=2)

## More details
sum.model0 <- summary(model0) ## variables are signficant
sum.model0$coefficients
sum.model0$residuals
sum.model0$sigma
sum.model0$sigma^2 # model MSE
sum.model0$adj.r.squared # Adjusted R squares

## Confidence Interval (less reliable than Bootstrap)
confint(model0, level=0.95)

## Prediction
pred.model0 <- predict(model0, newdata = Boston.test)

## MSPE
MSPE.model0 <- mean((Boston.test$medv-pred.model0)^2)





# Multiple Regession
# model1 <- lm(medv~crim+zn+indus, data = Boston.train)
model1 <- lm(medv~., data = Boston.train) # "." uses all variables
model1.sum <- summary(model1)
model1.sum
model1.sum$sigma^2 # Lower, so is better than model0
model1.sum$adj.r.squared # Higher, so is a better fit than model 0

## Prediction
model1.pred <- predict(model1, newdata = Boston.test)

### Another prediction
predict(model0, newdata = data.frame(lstat=c(3, 3.5, 10)), interval="confidence")

## MSPE
model1.mspe <- mean((Boston.test$medv-model1.pred)^2)
model1.mspe

## Manually remove insignificant variables
model2 <- lm(medv~.-indus-age, data = Boston.train) # using "-" excludes variables
model2.sum <- summary(model2)
model2.sum
model2.sum$sigma^2 # Lower, so is better than model0
model2.sum$adj.r.squared # Higher, so is a better fit than model 0
### Use this model because it is more simple, even though it is very similary MSE than model1

model2.pred <- predict(model2, newdata = Boston.test)
model2.mspe <- mean((Boston.test$medv-model2.pred)^2)
model2.mspe


## model diagnostics
par(mfrow=c(2,2))
plot(model2)
#### of the four figures, residual plot is most important. Cook's shows influential points, which are bad cause they influence our data

### residual plot
plot(model2$fitted.values, model2$residuals)
#### We see quadratic, but it should be randomly distributed. BONUS ON HOMEWORK

### model assessment
#### MSE (for training error)
#### Adj.R square
#### MSPE (for testing error)
#### AIC -> Smaller is better
AIC(model2)
AIC(model1)
#### BIC -> Smaller is better. Penalizes complexity in model
BIC(model2)
BIC(model1)

# Cross Validation -> Assess testing error
library(boot)
model1 <- glm(medv~., data = Boston)
cv.score1 <- cv.glm(data = Boston, glmfit = model1, K=10)
cv.score1$delta[2] # Shows MSE

model2 <- glm(medv~.-indus-age, data = Boston)
cv.score2 <- cv.glm(data = Boston, glmfit = model2, K=10)
cv.score2$delta[2]

model0 <- glm(medv~lstat, data = Boston)
cv.score0 <- cv.glm(data = Boston, glmfit = model0, K=10)
cv.score0$delta[2]






# Exercise
## import nba data
library(readr)
nba <- read_csv("C:/Users/Rhyne/Google Drive/School/Classes/2017-f/DataMining/nba.csv")

## Split the data
index <- sample(1:nrow(nba), 0.9*nrow(nba), replace = F)
nba.train <- nba[index,]
nba.test <- nba[-index,]

## linear regression with all variables to predict points
library(MASS)
model1 <- lm(PTS~., data = nba.train) # "." uses all variables
summary(model1)
confint(model1, level = .95)

### GP, `3P%`, `FG%` and STL are not significant
model2 <- lm(PTS~.-GP-`3P%`-`FG%`-STL, data = nba.train)

## COMPARE

### R-Square
model1.sum <- summary(model1)
model2.sum <- summary(model2)
model1.sum$adj.r.squared
model2.sum$adj.r.squared

### AIC
AIC(model1)
AIC(model2)

### BIC
BIC(model1)
BIC(model2)

### MSE
model1.sum$sigma^2
model2.sum$sigma^2

### MSPE
model1.pred <- predict(model1, newdata = nba.test)
model2.pred <- predict(model2, newdata = nba.test)
mean((nba.test$PTS-model1.pred)^2)
mean((nba.test$PTS-model2.pred)^2)

## Cross Validation
library(boot)
model1.cv <- glm(PTS~., data = nba)
model1.cv.score1 <- cv.glm(data = nba, glmfit = model1.cv, K=10)
model1.cv.score1$delta[2] # Shows MSE

model2.cv <- glm(PTS~.-GP-`3P%`-`FG%`-STL, data = nba)
model2.cv.score1 <- cv.glm(data = nba, glmfit = model2.cv, K=10)
model2.cv.score1$delta[2] # Shows MSE
