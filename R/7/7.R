# רגרסיה מרובה 

setwd("C:/Users/niv/Desktop/R/6")
toyoty.df <- read.csv("ToyotaCorolla.csv")
head(toyoty.df)

#convert to factor
class(toyoty.df$Fuel_Type)
toyoty.df$Fuel_Type <- as.factor(toyoty.df$Fuel_Type)
class(toyoty.df$Fuel_Type)

class(toyoty.df$Doors)
toyoty.df$Doors <- as.factor(toyoty.df$Doors)
class(toyoty.df$Doors)


#split the data
set.seed(1)
train.index <- sample(1:dim(toyoty.df)[1],dim(toyoty.df)[1]*0.6)
train.df <- toyoty.df[train.index,]
valid.df <- toyoty.df[-train.index,]

## רגרסיה מרובה
reg <- lm(Price~ ., data=train.df[,-1])
summary(reg)


# predict - train המחיר החזוי שך ה  
pred.train <- predict(reg)
library(forecast)
# בדיקת המחיר החזוי למול המחיר האמיתי
accuracy(pred.train,train.df$Price)



#  valid המחיר החזוי שך ה 
pred.valid <- predict(reg,newdata = valid.df)
accuracy(pred.valid,valid.df$Price)


#  (אינטרקציה)רגרסיה משולבת
reg <- lm(Price~ .^2 , data=train.df[,-1])
summary(reg)

# הורדת NA - מודל קטן יותר
reg.step<-step(reg)
summary(reg)

