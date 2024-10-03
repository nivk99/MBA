# file: EX2
# Name: Niv Kotek
#ID: 208236315

# set working directory
setwd("C:/Users/niv/Desktop/R/5/מטלה 2")
# Load the data from a CSV file
boston.df <- read.csv("BostonHousing.csv", stringsAsFactors = TRUE)
head(boston.df) 


install.packages("ggplot2")
# Load the ggplot2 library
library(ggplot2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating a graph of type - geom_point
ggplot(boston.df, aes(x = NOX, y = MEDV))+ylim(c(0, 50))+
  geom_jitter(width = 0.1, height = 0.1) +geom_point() +
  geom_smooth(method = loess)  + # Create a line
  xlab("Concentration of nitrogen oxide")+ # Setting the x-axis title
  ylab("Median Home Vakue (in $1000s)")+ # Setting the y-axis title
  ggtitle("Relationship between NOX Concentration and Nedian Home Value") # Setting the graph title
  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Creating a graph of type - geom_bar
ggplot(boston.df, aes(x = as.factor(CHAS), y = MEDV,fill = as.factor(CHAS))) + geom_bar(stat = "summary", fun = "mean")+
  xlab("Is the house close to the Charles River ? (CHAS)")+ # Setting the x-axis title
  ylab("Average MEDV (in $1000s)")+ # Setting the y-axis title
  ggtitle("House prices by proximity to the Charles River") + # Setting the graph title
  scale_fill_manual(values=c("0"="blue", "1"="red"), 
                    labels=c("No - Charles", "Yes - Charles")) + # Color update
  scale_x_discrete(labels=c("No - Carles", "Yes - Carles"))   # Updating names
  
  
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating a graph of type - geom_histogram

ggplot(boston.df, aes(x = RM)) + geom_histogram(bins = 100,color = "black")+ # Color update
  xlab("Average number of rooms (RM)") + # Setting the x-axis title
  ylab("Frequency") + # Setting the y-axis title
  ggtitle("Histogram of average number of rooms per house (RM)") # Setting the graph title


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Creating a graph of type - geom_boxplot

  ggplot(boston.df, aes(x = factor(CAT..MEDV), y = CRIM)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +# Using a logarithmic scale with easy-to-read labels
  ggtitle("Distribution of CRIM according to MEDV.CA")+ # Setting the graph title
  xlab("Is MEDV greater than the overall house median?")+ # Setting the x-axis title
  ylab("The crime rate-Log Scale")+ # Setting the y-axis title
  scale_x_discrete(labels=c("No", "Yes")) # Updating names
  
  