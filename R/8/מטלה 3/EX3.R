# file: EX3
# Name: Niv Kotek
#ID: 208236315

# set working directory
setwd("C:/Users/niv/Desktop/R/8/מטלה 3")

# Load the data from a CSV file (Catalogs.csv)
Catalogs.df <- read.csv("Catalogs.csv",stringsAsFactors = TRUE)
head(Catalogs.df)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# split the data
# Splits the dataset into 60% of training and 40% of validation
set.seed(1)  
train.index <- sample(1:dim(Catalogs.df)[1], dim(Catalogs.df)[1]*0.6)  
train.df <- Catalogs.df[train.index, ] # training  - 60% 
valid.df <- Catalogs.df[-train.index, ] # validation - 40%


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Checks whether the relationship is linear according to geom_bar of the variables age, children, catalogs

library(ggplot2)

# age
class(Catalogs.df$Age) #integer
ggplot(train.df, aes(y = AmountSpent, x = as.factor(Age))) + 
  geom_bar(stat = "summary", fun = "mean")+
  ggtitle("Average Amount Spent by Age")
#Non-linear relationship


# children
class(Catalogs.df$Children) #integer
ggplot(train.df, aes(y = AmountSpent, x =  as.factor(Children))) + 
  geom_bar(stat = "summary", fun = "mean")+
  ggtitle("Average Amount Spent by Children")
#Non-linear relationship

# catalogs
class(Catalogs.df$Catalogs) #integer
ggplot(train.df, aes(y = AmountSpent, x = Catalogs)) + 
  geom_bar(stat = "summary", fun = "mean")+
  ggtitle("Average Amount Spent by Catalogs")
#linear relationship


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checks the distribution of the variables Salary and Amount by test and by geom_histogram


# Salary
class(Catalogs.df$Salary) #integer
ggplot(train.df, aes(x = Salary)) + geom_histogram(bin=100)


# Amount Spent
class(Catalogs.df$AmountSpent) #integer
ggplot(train.df, aes(x = AmountSpent)) + geom_histogram(bin=100)


# plot
library(ggplot2)
ggplot(train.df, aes(y = AmountSpent, x = Salary)) + 
  geom_point() + geom_smooth(method = lm)

# NCV Test
reg <- lm(AmountSpent~Salary,data=train.df)
library(car)
ncvTest(reg)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task:
#I have a database with 'Catalogs.csv' containing the variables Age, Gender, Married, Location, Salary, Children, Catalogs, and AmountSpent.
# Build the best possible multiple regression model in R to predict 'AmountSpent', including all variables in the model.
# Optimize the model's performance and evaluate it on the training set and the validation set, with the training set being split into 60% for training and 40% for validation.
#Print the RMSE and MAPE metrics for both samples.




#convert to factor
class(Catalogs.df$Age)
Catalogs.df$Age <- as.factor(Catalogs.df$Age)
class(Catalogs.df$Age)

class(Catalogs.df$Children)
Catalogs.df$Children <- as.factor(Catalogs.df$Children)
class(Catalogs.df$Children)



# split the data
set.seed(1)  
train.index <- sample(1:dim(Catalogs.df)[1], dim(Catalogs.df)[1]*0.6)  
train.df <- Catalogs.df[train.index, ]
valid.df <- Catalogs.df[-train.index, ]



# multiple regression - log log
reg <- lm(log(AmountSpent) ~ log(Salary) + . - Salary, data = train.df)
summary(reg)



# predict and evaluate - training
library(forecast)
pred.train <- predict(reg)
accuracy(exp(pred.train), train.df$AmountSpent)

# predict and evaluate - validaion
pred.valid <- predict(reg, newdata = valid.df)
accuracy(exp(pred.valid), valid.df$AmountSpent)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ChatGPT

# Input: I have an R code for a multiple regression model. Write me code to calculate RMSE and MAPE to evaluate the model's performance.

#Output:

# Assuming 'actual' is the vector of actual values
# and 'predicted' is the vector of predicted values from the model

# Load necessary package
library(Metrics)

# train

# Calculate RMSE
rmse_value <- rmse(train.df$AmountSpent, exp(pred.train))
# Calculate MAPE
mape_value <- mape(train.df$AmountSpent, exp(pred.train))

# Print the results
cat("RMSE:", rmse_value, "\n")
cat("MAPE:", mape_value, "\n")

# valid

# Calculate RMSE
rmse_value <- rmse(valid.df$AmountSpent, exp(pred.valid))
# Calculate MAPE
mape_value <- mape(valid.df$AmountSpent,exp(pred.valid))

# Print the results
cat("RMSE:", rmse_value, "\n")
cat("MAPE:", mape_value, "\n")




