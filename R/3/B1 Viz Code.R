setwd("C:/Users/niv/Desktop/R/3")
ebay.df <- read.csv("Cross Auctions.csv", stringsAsFactors = TRUE)
head(ebay.df) 

## histogram
hist(ebay.df$ClosePrice)

## nicer plot
hist(ebay.df$ClosePrice,  main = "Histogram", 
     xlab = "Close Price")

## change number of bins
hist(ebay.df$ClosePrice, br = 100,  main = "Histogram with 100 Breaks", 
     xlab = "Close Price")

hist(ebay.df$ClosePrice, br = 1000,  main = "Histogram with 1000 Breaks", 
     xlab = "Close Price")

## zoom
hist(ebay.df$ClosePrice[ebay.df$ClosePrice < 200],  
     main = "Histogram", xlab = "Close Price")

## boxplot
boxplot(ebay.df$ClosePrice, xlab = "Close Price")
boxplot(ebay.df$ClosePrice[ebay.df$ClosePrice < 200], xlab = "Close Price")

## log scale
boxplot(ebay.df$ClosePrice, xlab = "Close Price", log = 'y')

## scatter plot - משתנה כמותי למשתנה כמותי
plot(ClosePrice ~ BuyItNow, data = ebay.df)

plot(ClosePrice ~ BuyItNow, data = ebay.df, 
     col = ifelse(ebay.df$HasReservePrice=="FALSE", "blue", "red"), 
     pch = ifelse(ebay.df$HasReservePrice=="FALSE",1,2))
# מקרא
# topright - מיקום המקרא
# pch - צורת הייצוג

legend("topright", 
       legend = c("No reserve prices", "With reserved price"), 
       col = c("blue", "red"), 
       pch = c(1,2))

## barplot - גרף עמודות, הקשר בין משתנה קטגורלי למשתנה כמותי
#height - ציר ה y
# names.arg - ציר x 
meanClosePrice <- aggregate(ClosePrice ~ Duration, 
                            data = ebay.df, 
                            FUN = "mean")
barplot(height = meanClosePrice[,2], names.arg = meanClosePrice[,1], 
        xlab = "duration", ylab = "mean close price")
