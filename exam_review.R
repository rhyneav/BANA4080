# lm()

library(MASS)
data("Boston")

## training vs testing
index <- sample(nrow(Boston), 0.8*nrow(Boston))
Boston.train <- Boston[index,]
Boston.test <- Boston[-index,]

# lm(Y~X, data=Boston) # Y is what we want to predict... x is our different variables to help predict
model.lm <- lm(medv~lstat, data=Boston)

## Summary()
summary(model.lm) # Good for linear, but WONT work for LASSO

## Confidence interval of estimated coefficients
confint(model.lm)

## Prediction (important)
model.predict <- predict(model.lm, newdata = Boston)
# mean((Y - Yhat)^2) actual - predicted value
mean((Boston$medv - model.predict)^2)









#################################


# Variable selection

# best subset (leaps)
# stepwise, forward, backward

nullmodel <- lm(medv~1, data = Boston)
fullmodel <- lm(medv~., data=Boston)

# Forward
step.lm.f <- step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), direction = "forward")
summary(step.lm.f)

# Backward
step.lm.b <- step(fullmodel, direction = "backward")
summary(step.lm.b)

# stepwise
step.lm.s <- step(nullmodel, scope = list(lower=nullmodel, upper=fullmodel), direction = "both", k=log(nrow(Boston))) # log(n) uses BIC
summary(step.lm.s)


# LASSO
library(glmnet)

# separate y and x
y <- Boston$medv
y.test <- y[c(1,2,3)]
x <- as.matrix(Boston[,-14]) # -14 removes medv from the x matrix
x.test <- x[c(1,2,3),]

lasso.fit <- glmnet(x=x, y=y)
plot(lasso.fit, xvar = "lambda")
## each lambda associates with one model


cv.lasso=cv.glmnet(x=x, y=y)
plot(cv.lasso)
## vertical axis is MSE
### left line is our best one
### right line is more simple

coef(lasso.fit, s=exp(-.5)) # s is the lambda, need to convert to exponent for import
coef(lasso.fit, s=cv.lasso$lambda.min) # optimal left line
coef(lasso.fit, s=cv.lasso$lambda.1se) # right line

### Prediction
pred.lasso <- predict(lasso.fit, newx = x.test, s=cv.lasso$lambda.1se)
pred.lasso

### MSPE
mean((y.test - pred.lasso)^2)

########################33
# Cross Validation
library(boot)

glm.model <- glm(medv~lstat, data=Boston)
cvobject <- cv.glm(data=Boston, glmfit=glm.model, K=10)
# MSE/Score
cvobject$delta[2]

