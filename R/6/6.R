# רגרסיה פשוטה
setwd("C:/Users/niv/Desktop/R/6")
toyoty.df <- read.csv("ToyotaCorolla.csv")
head(toyoty.df)


#toyoty.df <-toyoty.df[toyoty.df$Weight<1400,]
#toyoty.df <-toyoty.df[toyoty.df$Weight<1100 & toyoty.df$Price>9000 ,]
## partition
set.seed(1)
train.index <- sample(1:dim(toyoty.df)[1],dim(toyoty.df)[1]*0.6)
train.df <- toyoty.df[train.index,]
valid.df <- toyoty.df[-train.index,]

# viz
library(ggplot2)
p <- ggplot(train.df,aes(x=Weight,y=Price))+geom_point()
p

# regression
reg <- lm(Price~Weight, data=train.df)
summary(reg)

# הוספת קו
p+geom_smooth(method = lm)



# predict - train המחיר החזוי שך ה  
pred.train <- predict(reg)
library(forecast)
# בדיקת המחיר החזוי למול המחיר האמיתי
accuracy(pred.train,train.df$Price)


#  valid המחיר החזוי שך ה 
pred.valid <- predict(reg,newdata = valid.df)
accuracy(pred.valid,valid.df$Price)







