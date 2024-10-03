setwd("C:/Users/user/Desktop/R")


## small example
df <- read.csv("example1.csv", stringsAsFactors = TRUE)

# library neuralnet
library(neuralnet)
df$Like <- df$Acceptance == "like"
set.seed(1)
nn <- neuralnet(Like ~ Salt + Fat,
                data = df, linear.output = F, 
                hidden = 3)

plot(nn, rep="best")
# predict small example
pred <- compute(nn, df[, 2:3])
pred.value <- pred$net.result
library(caret)
confusionMatrix(as.factor(ifelse(pred.value>0.5, "like", "dislike")), as.factor(df$Acceptance))

# library nnet
library(nnet)
nn <- nnet(as.factor(Acceptance) ~ Salt + Fat,
           data = df, linout = F, 
           size = 3, 
           decay = 0.01, maxit = 200)

summary(nn)
# predict small example
pred <- predict(nn, df)
library(caret)
confusionMatrix(as.factor(ifelse(pred>0.5, "like", "dislike")), as.factor(df$Acceptance))

## universal bank data
bank.df <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE)
head(bank.df)

set.seed(1)
train.index <- sample(1:dim(bank.df)[1], dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

## try 1: use Income and CCAvg, with 3 hidden nodes
set.seed(1)
nn1 <- nnet(Personal.Loan ~ Income + CCAvg, 
            data = train.df, size = 3, 
            decay = 0.01, maxit = 200)
pred1 <- predict(nn1, valid.df)

library(AUC)
r <- roc(pred1, as.factor(valid.df$Personal.Loan))
plot(r)
auc(r)


## try 2: use Income and CCAvg, normalize first
set.seed(1)
nn2 <- nnet(Personal.Loan ~ Income + CCAvg, 
            data = train.df, size = 10, 
            decay = 0.01, maxit = 200)
pred2 <- predict(nn2, valid.df)

library(AUC)
r <- roc(pred2, as.factor(valid.df$Personal.Loan))
plot(r)
auc(r)


## try 3: use all variables, 10 hidden nodes
set.seed(1)
nn3 <- nnet(Personal.Loan ~ ., 
            data = train.df, size = 10, 
            decay = 0.01, maxit = 500)
pred3 <- predict(nn3, valid.df)

library(AUC)
r <- roc(pred3, as.factor(valid.df$Personal.Loan))
plot(r)
auc(r)
