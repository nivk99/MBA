# file: EX2
# Name: Niv Kotek, Genadi Birfir
# ID: 208236315, 312741622

# Using libraries
library(stringi)
library(textstem)
library(tm)
library(qdap)
library(sentimentr)
library(wordcloud)
library(ggthemes)
library(lexicon)



# set working directory
setwd("C:/Users/niv/Desktop/Text mining/EX2")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Options
options(stringsAsFactors=FALSE) #don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C')

# read csv
airbnb.df<-read.csv('bos_airbnb_1k.csv')
head(airbnb.df)


#text coloum into new data frame
comments.df<-airbnb.df[,'comments']
head(comments.df)


#Create corpus
comments.vec<- VectorSource(comments.df)
comments.cor<- VCorpus(comments.vec)
class(comments.cor)

# Cleaning test by function
check <- function(text.cor,name) {
  cat(name,"\n\n")
  for (i in 1:6) {
    print(text.cor[[i]][1])
  }
  cat("\n\n")
}


### Text cleaning

# transferring data to lowercase
comments.cor  <- tm_map(comments.cor,content_transformer(tolower))
check(comments.cor,"lowercase")

# remove numbers
comments.cor  <- tm_map(comments.cor , removeNumbers)
check(comments.cor,"remove numbers")

# remove white spaces
comments.cor <- tm_map(comments.cor ,stripWhitespace)
check(comments.cor,"remove white spaces")

comments.cor <- lemmatize_words(comments.cor)

# Convert cleaned corpus to text
cleaned_comments <- unlist(lapply(comments.cor, function(x) x$content))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sentiment analysis
airbnb.df$sentiment <- sentiment_by(cleaned_comments,question.weight=0)$ave_sentiment
head(airbnb.df$sentiment)
table(sign(airbnb.df$sentiment))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in 1:nrow(airbnb.df)) {
  # Check if the sentiment is within the specified range
  if (airbnb.df$sentiment[i] < 0 && airbnb.df$sentiment[i] > -0.1) {
    # Calculate the sentiment for the current comment
    senti <- sentiment(cleaned_comments[i],question.weight=0)$sentiment
    
    # Initialize positive and negative sentiment accumulators
    positive <- 0
    negative <- 0
    
    for (var in senti) {
      if (var > 0) {
        positive <- positive + var
      } else {
        negative <- negative + var
      }
    }
    
    # Check if the sum of positive and negative sentiments meets criteria
    if ((positive + negative >= -0.15) && (positive != 0)) {
      # Print the 'comments' column for the current row
      cat("Comment: ", airbnb.df$comments[i], "\n")
      
      # Print the calculated sentiment value
      cat("Calculated Sentiment: ", senti, "\n")
      
      # Print the average sentiment value from the data frame
      cat("Average Sentiment: ", airbnb.df$sentiment[i], "\n")
      
      # Add a blank line for better readability
      cat("\n")
    }
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ChatGPT:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#check if a word exists in the lexicon
lexicon::hash_sentiment_jockers_rinker["dirty "] #NA

lexicon::hash_sentiment_jockers_rinker["repair"] #NA

lexicon::hash_sentiment_jockers_rinker["be cleaner"] #NA

lexicon::hash_sentiment_jockers_rinker["kept"] #NA

lexicon::hash_sentiment_jockers_rinker["mold"] #NA


# add new words to the dictionary
library(lexicon)
updated_hash_sentiment <- sentimentr:::update_polarity_table(lexicon::hash_sentiment_jockers_rinker,
                                                             x = data.frame(
                                                               words = c('be cleaner', 'repairs','dirty','mold','kept'),
                                                               polarity = c(-1, -1,-1,-1,1),
                                                               stringsAsFactors = FALSE
                                                             )
)



airbnb.df$sentiment_updated <- sentiment_by(cleaned_comments,polarity_dt=updated_hash_sentiment,question.weight=0)$ave_sentiment
head(airbnb.df$sentiment_updated)
table(sign(airbnb.df$sentiment_updated))

# writhe to csv 
write.csv(airbnb.df,'bos_airbnb_1k_sentiment_new.csv')

new_words <- c('be cleaner', 'repairs', 'dirty', 'mold', 'kept')
num<-1
# Loop through each row in the DataFrame
for (i in 1:nrow(airbnb.df)) {
  # Extract comments for the current row
  comments <- cleaned_comments[i]
  
  # Loop through each comment if it's a list or vector of comments
  for (j in 1:length(comments)) {
    # Loop through the new_words
    bool<-TRUE
    for (k in 1:length(new_words)) {
      # Check if the new_word appears in the comment
      if (grepl(new_words[k], comments[j], ignore.case = TRUE)&&bool&&airbnb.df$sentiment[i]!= airbnb.df$sentiment_updated[i]) {
        bool<-FALSE
        cat("num:",num,"\n")
        num<-num+1
        cat("Comment: ", comments[j], "\n")
        cat("Average Sentiment: ", airbnb.df$sentiment[i], "\n")
        cat("Average Sentiment Updated: ", airbnb.df$sentiment_updated[i], "\n")
        cat("\n\n")
      }
    }
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.6~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#Dividing positive and negative comments
negative.df<-airbnb.df[airbnb.df$sentiment<0,'comments']
positive.df<-airbnb.df[airbnb.df$sentiment>0,'comments']

#cleaning
clean.vec<-function(text.vec){
  text.vec <- tolower(text.vec)
  text.vec<-removeWords(text.vec,stopwords('english'))
  text.vec <- removePunctuation(text.vec)
  text.vec <- removeNumbers(text.vec)
  text.vec <- stripWhitespace(text.vec)
}

positive.vec<-clean.vec(positive.df)
negative.vec<-clean.vec(negative.df)

#concatenate all the comments together
negative.vec<-paste(negative.vec,collapse = " ")
positive.vec<-paste(positive.vec,collapse = " ")

comparisonCloud.df<-c(positive.vec,negative.vec)
comparisonCloud.corpus<-VCorpus(VectorSource(comparisonCloud.df))
tdm<-TermDocumentMatrix(comparisonCloud.corpus)
tdm.matrix<- as.matrix(tdm)
colnames(tdm.matrix)= c("positive","negative")

comparison.cloud(tdm.matrix,max.words = 150,random.order = FALSE,title.size = 1,
                 colors = brewer.pal(ncol(tdm.matrix),"Dark2"))




