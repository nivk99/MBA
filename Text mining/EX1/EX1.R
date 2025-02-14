# file: EX1
# Name: Niv Kotek, Genadi Birfir
# ID: 208236315, 312741622

# Using libraries
library(stringi)
library(textstem)
library(tm)


# set working directory
setwd("C:/Users/niv/Desktop/Text mining/EX1")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set Options
options(stringsAsFactors=FALSE) #don't treat strings as factors (categories)
Sys.setlocale('LC_ALL','C')

# read csv
Sentiments.df<-read.csv('Sentiment180_2000.csv')
head(Sentiments.df$Text)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#text coloum into new data frame
text.df<-Sentiments.df[,'Text']
head(text.df)


# regular expression

# remove all the characters after the @
text.df <- gsub("@\\S*", "", text.df)
head(text.df)
# remove all the characters after the http
text.df <- gsub("http\\S*", "", text.df)
head(text.df)


#Create corpus
text.vec<- VectorSource(text.df)
text.cor<- VCorpus(text.vec)
class(text.cor)

# Cleaning test by function
check <- function(text.cor,name) {
  cat(name,"\n\n")
  for (i in 1:6) {
    print(text.cor[[i]][1])
  }
  cat("\n\n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Text cleaning

# transferring data to lowercase
text.cor <- tm_map(text.cor,content_transformer(tolower))
check(text.cor,"lowercase")

# remove punctuation
text.cor <- tm_map(text.cor, removePunctuation)
check(text.cor,"remove punctuation")

# remove numbers from twits
text.cor <- tm_map(text.cor, removeNumbers)
check(text.cor,"remove numbers")

# remove stopwords
text.cor<- tm_map(text.cor, removeWords,stopwords("english"))
check(text.cor,"remove stopwords")

# remove white spaces
text.cor<- tm_map(text.cor,stripWhitespace)
check(text.cor,"remove white spaces")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


############  Change order 1 ############ 

#text coloum into new data frame
text.df<-Sentiments.df[,'Text']
head(text.df)

# regular expression

# remove all the characters after the @
text.df <- gsub("@\\S*", "", text.df)
head(text.df)
# remove all the characters after the http
text.df <- gsub("http\\S*", "", text.df)
head(text.df)


#Create corpus
text.vec<- VectorSource(text.df)
text.cor<- VCorpus(text.vec)


# transferring data to lowercase
text.cor <- tm_map(text.cor,content_transformer(tolower))
check(text.cor,"lowercase")


# remove punctuation
text.cor <- tm_map(text.cor, removePunctuation)
check(text.cor,"remove punctuation")

# remove numbers from twits
text.cor <- tm_map(text.cor, removeNumbers)
check(text.cor,"remove numbers")

# remove white spaces
text.cor<- tm_map(text.cor,stripWhitespace)
check(text.cor,"remove white spaces")

# remove stopwords
text.cor<- tm_map(text.cor, removeWords,stopwords("english"))
check(text.cor,"remove stopwords")








############ Change order 2 ############ 

#text coloum into new data frame
text.df<-Sentiments.df[,'Text']
head(text.df)

# remove all the characters after the http
text.df <- gsub("http\\S*", "", text.df)
head(text.df)

#Create corpus
text.vec<- VectorSource(text.df)
text.cor<- VCorpus(text.vec)

# transferring data to lowercase
text.cor <- tm_map(text.cor,content_transformer(tolower))
check(text.cor,"lowercase")

# remove punctuation
text.cor <- tm_map(text.cor, removePunctuation)
check(text.cor,"remove punctuation")


# Remove mentions (e.g., @username)
text.cor <- tm_map(text.cor, content_transformer(function(x) gsub("@\\S+", "", x)))
check(text.cor, "remove mentions (@)")


# remove numbers from twits
text.cor <- tm_map(text.cor, removeNumbers)
check(text.cor,"remove numbers")

# remove stopwords
text.cor<- tm_map(text.cor, removeWords,stopwords("english"))
check(text.cor,"remove stopwords")

# remove white spaces
text.cor<- tm_map(text.cor,stripWhitespace)
check(text.cor,"remove white spaces")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Term Document Matrix 
text.tdm <- TermDocumentMatrix(text.cor,control=list(wordLengths=c(1,Inf)))
# convert to a matrix
text.mat <- as.matrix(text.tdm)
# print
inspect(text.tdm)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.6~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Terms that appear in two or fewer documents are deleted
text.tdm <- TermDocumentMatrix(text.cor,control=list(wordLengths=c(1,Inf),bounds = list(global=c(3,Inf))))
# convert to a matrix
text.mat <- as.matrix(text.tdm)
# print
inspect(text.tdm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.7~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#In this part I used chat gpt

# Create the Term-Document Matrix
text.tdm <- TermDocumentMatrix(text.cor,control=list(wordLengths=c(1,Inf)))

# Convert TDM to a matrix
text.mat <- as.matrix(text.tdm)

# Calculate term frequencies
term_frequency <- rowSums(text.mat)

# Sort terms by frequency in descending order
sorted_terms <- sort(term_frequency, decreasing = TRUE)

# Create a data frame of the top 20 terms
top_terms <- data.frame(
  Term = names(sorted_terms)[1:20],
  Frequency = sorted_terms[1:20]
)

# Display the table - 6
head(top_terms)
