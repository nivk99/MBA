setwd("C:/Users/niv/Desktop/R/1")

file.df <- read.csv("file1.csv")
head(file.df)

# variable names
names(file.df)

#colum
file.df[,1]
file.df[,1:2]

file.df[,1:3]
file.df[,c(1,3)]
file.df$X2

#row
file.df[2:4,]
file.df[file.df$X3=="Good",]

install.packages("car")
library(car)
Boxplot(file.df$X2)


#functions
computSum <- function(a,b,c){
  sum <-a+b+c
  return(sum)
}
computSum(10,20,30)
