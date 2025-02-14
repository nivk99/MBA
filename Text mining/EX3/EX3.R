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
library(topicmodels)
# set working directory
setwd("C:/Users/niv/Desktop/Text mining/EX3")

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

# Sentiment analysis
airbnb.df$sentiment <- sentiment_by(comments.df,question.weight=0)$ave_sentiment
head(airbnb.df$sentiment)
table(sign(airbnb.df$sentiment))

#Dividing positive and negative comments
negative.df<-airbnb.df[airbnb.df$sentiment<0,'comments']
positive.df<-airbnb.df[airbnb.df$sentiment>0,'comments']

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#clean.data function

negative.df<- gsub("\n"," ",negative.df)
positive.df<-gsub("\n"," ",positive.df)

negative.df<- textclean::replace_non_ascii(negative.df,replacement = "")
positive.df<- textclean::replace_non_ascii(positive.df,replacement = "")


clean.data <- function(text){
  myCorpus <- VCorpus(VectorSource(text))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))# convert to lower case
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english")) #remove stopwords
  myCorpus <- tm_map(myCorpus, removeNumbers)# remove numbers
  myCorpus <- tm_map(myCorpus, removePunctuation)# remove punctuation
  myCorpus <- tm_map(myCorpus, stripWhitespace)# remove extra whitespace
  myCorpus <- tm_map(myCorpus, content_transformer(lemmatize_strings)) 
  myCorpus <- tm_map(myCorpus, stemDocument)
  return(myCorpus)
}

positive.Corpus<-clean.data(positive.df)
negative.Corpus<-clean.data(negative.df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dtm_positive <- DocumentTermMatrix(positive.Corpus)
dtm_negative <- DocumentTermMatrix(negative.Corpus)

# Choose number of topics
library(ldatuning)

# positive
result_positive <- FindTopicsNumber(
  dtm_positive,
  topics = seq(from = 1, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", #VEM/Gibbs
  control = list(seed = 10,alpha=0.1),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result_positive)

# negative
result_negative <- FindTopicsNumber(
  dtm_negative,
  topics = seq(from = 1, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", #VEM/Gibbs
  control = list(seed = 10,alpha=0.1),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result_negative)


##run LDA with 8 topics (positive)
lda_positive <- LDA(dtm_positive, k = 8, control=list(seed=10,alpha=0.1),method = "Gibbs") #lower alpha, we assume documents contain fewer topics
termsByTopic_positive <- terms(lda_positive, 10)
termsByTopic_positive

##run LDA with 5 topics (negative)
lda_negative <- LDA(dtm_negative, k = 5, control=list(seed=10,alpha=0.1),method = "Gibbs") #lower alpha, we assume documents contain fewer topics
termsByTopic_negative <- terms(lda_negative, 10)
termsByTopic_negative


topics_positive <- topics(lda_positive)
topics_positive  #assignment of term to a topic
table(topics_positive) #number of terms in each topic

topics_negative <- topics(lda_negative)
topics_negative  #assignment of term to a topic
table(topics_negative) #number of terms in each topic



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The most common words in each group

# positive

# Define custom titles for each topic
topic_titles <- c(
  "1. Living Experience and Accessibility",
  "2. Availability and Timing",
  "3. Comfort and Quality of Stay",
  "4. Experiences and Hosting in Boston",
  "5. Location and Recommended Hosting",
  "6. Positive Atmosphere Stay",
  "7. Service and Social Interactions",
  "8. Apartments and Clean Environment"
)

# Print topic titles with terms
for (i in 1:ncol(termsByTopic_positive)) {
  cat(topic_titles[i], ":\n", termsByTopic_positive[, i], "\n\n")
}


# negative

# Define custom titles for each topic
topic_titles <- c(
  "1. Room and Accommodation Quality",
  "2. Location and Accessibility",
  "3. Negative Experiences and Complaints",
  "4. General Stay and Host Feedback",
  "5. Arrival and Communication Issues"
)

# Print topic titles with terms
for (i in 1:ncol(termsByTopic_negative)) {
  cat(topic_titles[i], ":\n", termsByTopic_negative[, i], "\n\n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# interactive visualization
library(LDAvis)
library(servr)
# Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
# for use with LDAvis

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

# Apply the function 
#positive
Jason_positive <- topicmodels2LDAvis(lda_positive)
serVis(Jason_positive)
#negative
Jason_negative <- topicmodels2LDAvis(lda_negative)
serVis(Jason_negative)


