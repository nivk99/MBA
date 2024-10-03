setwd("C:/Users/user/Desktop/R")
library(ggplot2)
library(forecast) 

toyota.df <- read.csv("ToyotaCorolla.csv", stringsAsFactors = TRUE)
head(toyota.df)

toyota.df$Doors <- as.factor(toyota.df$Doors)


### split the data
set.seed(1)  
train.index <- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]


## multiple regression 
reg <- lm(Price ~ ., data = train.df[, -1]) 
summary(reg)

pred.train <- predict(reg)
accuracy(pred.train, train.df$Price)
pred.valid <- predict(reg, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)

## interactions
ggplot(train.df, aes(y = Price, x = Weight, color = Fuel_Type)) + 
  geom_point() + 
  xlim(c(1000, 1400))

reg2 <- lm(Price ~ .^2, data = train.df[, -1]) 
summary(reg2)

pred.train <- predict(reg2)
accuracy(pred.train, train.df$Price)
pred.valid <- predict(reg2, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)

### stepwise
step.reg <- step(reg2, direction = "backward")
summary(step.reg)  # Which variables did it drop?

pred.train <- predict(step.reg)
accuracy(pred.train, train.df$Price)
pred.valid <- predict(step.reg, newdata = valid.df)
accuracy(pred.valid, valid.df$Price)


## Quarterly_Tax
ggplot(train.df, aes(x = as.factor(Quarterly_Tax), y = Price)) + 
  geom_bar(stat = "summary", fun = "mean")

toyota.df$Quarterly_Tax <-  as.factor(toyota.df$Quarterly_Tax)