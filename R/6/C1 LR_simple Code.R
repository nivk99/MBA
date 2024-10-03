setwd("C:/Users/user/Desktop/R")
setwd("C:/Users/dror/Desktop/R/lesson3")
library(ggplot2)

toyota.df <- read.csv("toyotacorolla.csv", stringsAsFactors = TRUE)
head(toyota.df)

### split the data
set.seed(1)  
train.index <- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)  #dimention - number of rows, number of columns
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]

### the relationship between Price ~ Weight
# viz
p <- ggplot(train.df, aes(y = Price, x = Weight)) + 
  geom_point() 
p

# simple regression 
reg <- lm(Price ~ Weight, data = train.df)
summary(reg)

# viz 
p + geom_smooth(method = lm)

### predict
# fit on training
pred.train <- predict(reg)

# predict and evaluate
library(forecast)
pred.train <- predict(reg)
accuracy(pred.train, train.df$Price)

pred.valid <- predict(reg, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)