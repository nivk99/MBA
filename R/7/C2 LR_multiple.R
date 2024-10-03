setwd("C:/Users/user/Desktop/R")
library(ggplot2)
toyota.df <- read.csv("ToyotaCorolla.csv", stringsAsFactors = TRUE)
head(toyota.df)

## convert to factor
class(toyota.df$Fuel_Type)
ggplot(train.df, aes(x = Doors, y = Price)) + 
  geom_bar(stat = "summary", fun = "mean")


### split the data
set.seed(1)  
train.index <- sample(1:dim(toyota.df)[1], dim(toyota.df)[1]*0.6)  
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[-train.index, ]


## multiple regression 
reg <- lm(Price ~ ., data = train.df[, -1]) 
summary(reg)

## interactions
ggplot(train.df, aes(y = Price, x = Weight, color = Fuel_Type)) + 
  geom_point() + 
  xlim(c(1000, 1400))

reg2 <- lm(Price ~ .^2, data = train.df[, -1]) 
summary(reg2)

### stepwise
reg <- lm(Price ~ ., data = train.df[, -1]) 
summary(reg)

step.reg <- step(reg, direction = "backward")
summary(step.reg)  # Which variables did it drop?



