credit.data <- read.csv("http://homepages.uc.edu/~lis6/DataMining/Data/credit_default.csv", header=T)
dim(credit.data)

colnames(credit.data)
mean(credit.data$default.payment.next.month)

library(dplyr)
credit.data<- rename(credit.data, default=default.payment.next.month)
str(credit.data)    # structure - see variable type
summary(credit.data) # summary statistics

# convert ints to categorical variables using as.factor
credit.data$SEX<- as.factor(credit.data$SEX)
credit.data$EDUCATION<- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE<- as.factor(credit.data$MARRIAGE)

# rows show education categories and columns show default or nahh
table.edu<- table(credit.data$EDUCATION, credit.data$default)
# testing null hypothesis. That they have a relationship
chisq.test(table.edu)

######################

# Logisitic Regression

index <- sample(nrow(credit.data),nrow(credit.data)*0.80)
credit.train = credit.data[index,]
credit.test = credit.data[-index,]

# family=binomial VERY IMPORTANT bc it specifies the type of regression
credit.glm0<- glm(default~., family=binomial, data=credit.train)

# Estimate column is very small because we need to fit huge numbers between 0 and 1 (sigmoid function?)
# Fisher scoring is how many steps it takes to converge
summary(credit.glm0)

credit.glm0$deviance

# Smaller is better
AIC(credit.glm0)
BIC(credit.glm0)

pred.glm0.train<- predict(credit.glm0, type="response")

# ROC Curve
install.packages('ROCR')
install.packages("gplots")

library(ROCR)
pred <- prediction(pred.glm0.train, credit.train$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

# AUC
unlist(slot(performance(pred, "auc"), "y.values"))

# Out of sample prediction (more important)
pred.glm0.test<- predict(credit.glm0, newdata = credit.test, type="response")

pred <- prediction(pred.glm0.test, credit.test$default)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

# Cut off probablities
table((pred.glm0.train > 0.9)*1)
table((pred.glm0.train > 0.5)*1)
table((pred.glm0.train > 0.2)*1)

# Naive cut off 
pcut1<- mean(credit.train$default)
# Binary prediction
class.glm0.train<- (pred.glm0.train>pcut1)*1
# Confustion Matrix
table(credit.train$default, class.glm0.train, dnn = c("True", "Predicted"))

# MR (misclassification rate)
mean(credit.train$default!=class.glm0.train)
# FPR (False Positive Rate)
sum(credit.train$default==0 & class.glm0.train==1)/sum(credit.train$default==0)
# TPR (True Positive Rate)