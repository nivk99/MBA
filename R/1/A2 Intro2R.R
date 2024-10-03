# textual variables
txt <- "Hi there"

# numeric variables
x <- 8
y <- 9
result <- 8+9

# vector
vec <- c(3, 4, 9)
vec*2
vec[2]

# conversions
var_c <- "1"
class(var_c)
var_n <- as.numeric(var_c)
class(var_n)

# from text to factor (nominal variable)
vec <- c("c", "a", "b", "a", "c")
class(vec)
fct <- as.factor(vec)
vec
class(fct)

# ifelse statements 
a <- 5
if (a<7) {
  a <- a*2
} else { # optional
  a <- a/2
}

# shorter:
a <- ifelse(a<7, a*2, a/2)

# for loop
for(i in 1:10) {
  print(i*i)
}

# while loop
i <- 1
while(i<=10){
  print(i*i)
  i <- i+1
}

# set working directory
setwd("C:/Users/user/Desktop/R")
setwd("C:\\Users\\user\\Desktop\\R")

# read file
file.df <- read.csv("file.csv", stringsAsFactors = TRUE)		

# alternative (no need to set working dir)
file.name <- file.choose()
file.df <- read.csv(file.name)

# print file to screen
file.df
head(file.df) 

# variable names
names(file.df)

# column selection
file.df[,1]
file.df[,1:2]
file.df[,c(1,3)]
file.df$X2

# row selection
file.df[2:4,]

# advanced
file.df[file.df$X3 == "Good",]

# functions 
computeSum <- function(a, b, c){
  sum <- a+ b+ c
  return(sum)
}

computeSum(10, 20, 30)

install.packages("car")
library(car)
Boxplot(file.df$X2)