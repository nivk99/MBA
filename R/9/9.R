setwd("C:/Users/niv/Desktop/R/9")
bank.df <- read.csv("UniversalBank.csv",stringsAsFactors = TRUE)
head(bank.df)

set.seed(1)  
train.index <- sample(1:dim(bank.df)[1], dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ] # training  - 60% 
valid.df <- bank.df[-train.index, ] # validation - 40%

# בניית העץ
library(party)
tr <- ctree(Personal.Loan~.,train.df)

# הדפסת העץ
plot(tr,type="simple") 

# חיזוי
pre <- predict(tr,newdata=valid.df)

# n=מטריצת הסיווג
library(caret)
confusionMatrix(as.factor(ifelse(pre>=0.5,1,0)),as.factor(valid.df$Personal.Loan))

# בנית ROC
library(AUC)
r <-roc(pre,as.factor(valid.df$Personal.Loan))
plot(r)
# ככל שה אי.יו.סי גדול יותר זה אומר שהמודל יותר טוב.
auc(r)

# random forest
rf <- cforest(Personal.Loan~.,train.df)

# חיזוי
pre <- predict(rf,newdata=valid.df)

# n=מטריצת הסיווג
library(caret)
confusionMatrix(as.factor(ifelse(pre>=0.5,1,0)),as.factor(valid.df$Personal.Loan))

# בנית ROC
library(AUC)
r <-roc(pre,as.factor(valid.df$Personal.Loan))
plot(r)
# ככל שה אי.יו.סי גדול יותר זה אומר שהמודל יותר טוב.
auc(r)

# חשיבות המתשנים
varImp(rf)
