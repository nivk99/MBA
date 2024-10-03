setwd("C:/Users/user/Desktop/R")

## read the data
bank.df <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE)
head(bank.df)

## split the data
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

## build tree
library(party)
tr <- ctree(Personal.Loan ~ ., data = train.df)
plot(tr, type = "simple")
pred <- predict(tr, newdata = valid.df)

## confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(pred>=0.5, 1, 0)), as.factor(valid.df$Personal.Loan))

## ROC curve
library(AUC)
r <- roc(pred, as.factor(valid.df$Personal.Loan))
auc(r)
plot(r)

## Random Forest
rf <- cforest(Personal.Loan ~ ., data = train.df)
pred <- predict(rf, newdata = valid.df)
r <- roc(pred, as.factor(valid.df$Personal.Loan))
auc(r)
plot(r)

varImp(rf)


