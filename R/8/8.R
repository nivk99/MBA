# רגרסיה LOG LOG 

setwd("C:/Users/niv/Desktop/R/8")
df <- read.csv("West Roxbury.csv",stringsAsFactors = TRUE)
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

# מבחן שונות משתנה - אם הפי נמוך אז יש בעיה
reg <- lm(TOTAL.VALUE~LOT.SQFT,data=train.df)
library(car)
ncvTest(reg)

ggplot(train.df, aes(x = TOTAL.VALUE)) + geom_histogram()

# plot log - log

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