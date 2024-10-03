setwd("C:/Users/niv/Desktop/R/3")
ebay.df <- read.csv("Cross Auctions.csv", stringsAsFactors = TRUE)
head(ebay.df) 


## example
plot(ClosePrice ~ BuyItNow, data = ebay.df)

library(ggplot2)
## geom_point = plot
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) + geom_point()

## separate layers

p <- ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) 
p + geom_point()

## axes names and limits
#  ylim - זום לציר ה ואיי
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) + 
  geom_point() + 
  xlab("x Title") + 
  ylim(c(0, 250))

## aes
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice, 
                    color = HasReservePrice)) + geom_point()

ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice, 
                    shape = SellerAboutMePage,color = SellerAboutMePage)) + geom_point()

ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice, 
                    size = BidCount)) + geom_point()


## histogram and density
ggplot(ebay.df, aes(x = ClosePrice)) + geom_histogram()
ggplot(ebay.df, aes(x = ClosePrice)) + geom_histogram(bins = 100)
ggplot(ebay.df, aes(x = ClosePrice)) + geom_density(fill = "red")

## boxplot
ggplot(ebay.df, aes(x ="CloseProce", y = ClosePrice)) + geom_boxplot()
# scale_y_log10 - שכבה של לוג
ggplot(ebay.df, aes(x ="CloseProce", y = ClosePrice)) + geom_boxplot() + scale_y_log10()
ggplot(ebay.df, aes(x = as.factor(Duration), y = ClosePrice)) + geom_boxplot()+ scale_y_log10()

## scatter and bar plots
ggplot(ebay.df, aes(x = BuyItNow, y = ClosePrice)) + geom_point()
ggplot(ebay.df, aes(x = as.factor(Duration), y = ClosePrice)) + geom_bar(stat = "summary", fun = "mean")




## close price as a function of feedback
p <- ggplot(ebay.df, aes(x = SellerFeedbackScore, y = ClosePrice))

## log scale
p + geom_point() + scale_x_log10() + scale_y_log10()

## jitter
# הפעלת רעש בשביל לראות את כל הנקודות
p + geom_jitter(width = 0.1, height = 0.1) + scale_x_log10() + scale_y_log10()

## close price as a function of bid count:
p <- ggplot(ebay.df, aes(x = BidCount, y = ClosePrice)) 

## smooth
#ylim - זום על ציר ה ואיי
#geom_smooth - הוספת קו
p + geom_point() + geom_smooth(method = lm) + ylim(c(0, 250))
p + geom_point() + geom_smooth(method = loess) + ylim(c(0, 250))

## facets - 
# facet_grid - חלוקה של שני גרפים 
p <- ggplot(ebay.df, aes(x = SellerFeedbackScore, y = ClosePrice)) + scale_x_log10() + scale_y_log10() + geom_jitter(width = 0.1, height = 0.1)
p + facet_grid(. ~ SellerAboutMePage)
p + facet_grid(SellerAboutMePage ~ .) +geom_smooth(method = lm)


