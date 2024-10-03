setwd("C:/Users/user/Desktop/R")

df <- read.csv("West Roxbury.csv", stringsAsFactors = TRUE)
head(df)

### split the data
set.seed(1)  
train.index <- sample(1:dim(df)[1], dim(df)[1]*0.6)  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

### plot
library(ggplot2)
ggplot(train.df, aes(y = TOTAL.VALUE, x = LOT.SQFT)) + 
  geom_point() + geom_smooth(method = lm)

### check heteroscedasticity
reg <- lm(TOTAL.VALUE ~ LOT.SQFT, data = train.df)
library(car)
ncvTest(reg)


### plot log-log
ggplot(train.df, aes(y = TOTAL.VALUE, x = LOT.SQFT)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_point() + geom_smooth(method = lm)


### compare models
library(forecast)

# OLS - Ordinary least squares
reg <- lm(TOTAL.VALUE ~ LOT.SQFT, data = train.df)
summary(reg)
pred <- predict(reg, newdata = valid.df)
accuracy(pred, valid.df$TOTAL.VALUE)

# log-log model
reg <- lm(log(TOTAL.VALUE) ~ log(LOT.SQFT), data = train.df)
summary(reg)
pred <- predict(reg, newdata = valid.df)
accuracy(exp(pred), valid.df$TOTAL.VALUE) #convert back from log to original scale, use exponent