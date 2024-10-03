# file: EX4
# Name: Niv Kotek
#ID: 208236315

# set working directory
setwd("C:/Users/niv/Desktop/R/11/מטלה 4")

# Load the data from a CSV file (accidents.csv)
accidents.df <- read.csv("accidents.csv",stringsAsFactors = TRUE)
head(accidents.df)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# create new variable
accidents.df$Casualties <- ifelse(accidents.df$MAX_SEV_IR>0 , 1, 0)
df_new <- accidents.df[,-5] # Without MAX_SEV_IR

# split the data
# Splits the dataset into 60% of training and 40% of validation
set.seed(1)  
train.index <- sample(1:dim(df_new)[1], dim(df_new)[1]*0.6)  
train.df <- df_new[train.index, ] # training  - 60% 
valid.df <- df_new[-train.index, ] # validation - 40%



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# classification tree
library(party)
tr <- ctree(Casualties~. ,train.df)
# Print the tree
plot(tr,type="simple") 
# predict
pre_tr <- predict(tr,newdata=valid.df)
# ROC
library(AUC)
r <-roc(pre_tr,as.factor(valid.df$Casualties))
plot(r)
auc(r)#0.9714815



# neural network
library(nnet)
set.seed(1)
nn <- nnet(Casualties~., 
           data = train.df, size = 3, 
           decay = 0.01, maxit = 400)
#predict
pred_nn <- predict(nn, valid.df)
#roc
r <- roc(pred_nn, as.factor(valid.df$Casualties))
auc(r)#0.972048



# logistic regression
library(glmnet)
reg <- glm(Casualties~., 
           data = train.df, 
           family = "binomial")
#predict
pred_reg <- predict(reg, valid.df, type = "response")
#roc
r <- roc(pred_reg, as.factor(valid.df$Casualties))
auc(r)#0.972048



# random forest
rf <- cforest(Casualties~.,train.df)
# predict
pre_rf <- predict(rf,newdata=valid.df)
library(AUC)
#roc
r <-roc(pre_rf,as.factor(valid.df$Casualties))
plot(r)
auc(r) #0.9719347



# Support Vector Machine (SVM)
library(e1071)
library(AUC)
svm_model <- svm(Casualties ~ ., data = train.df)
#predict
svm_pred <- predict(svm_model, valid.df)
#roc
r <-roc(svm_pred,as.factor(valid.df$Casualties))
auc(r)# 0.9641157

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# confusion Matrix ,cutoff= 0.5

library(caret)
# neural network
confusionMatrix(as.factor(ifelse(pred_nn>=0.5,1,0)),as.factor(valid.df$Casualties))
# logistic regression
confusionMatrix(as.factor(ifelse(pred_reg>=0.5,1,0)),as.factor(valid.df$Casualties))

# The results are equal:
# Accuracy : 0.945
# Sensitivity : 0.9585          
# Specificity : 0.9290
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# confusion Matrix ,cutoff= 0.6

library(caret)
# neural network
confusionMatrix(as.factor(ifelse(pred_nn>=0.6,1,0)),as.factor(valid.df$Casualties))
# logistic regression
confusionMatrix(as.factor(ifelse(pred_reg>=0.6,1,0)),as.factor(valid.df$Casualties))

# The results are equal:
# Accuracy : 0.9575
# Sensitivity : 1.0000          
# Specificity : 0.9071
