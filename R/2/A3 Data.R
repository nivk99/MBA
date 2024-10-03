setwd("C:/Users/user/Desktop/R")


## read the data
imdb.movies <- read.csv("IMDB_movies.csv", stringsAsFactors = TRUE)
head(imdb.movies)

players.df <- read.csv("IMDB_players.csv")
head(players.df)

## summary
summary(imdb.movies)

## remove rows with missing values
imdb.movies.nomissing <- 
  imdb.movies[is.na(imdb.movies$total.gross) == FALSE, ]

## data imputation
summary(imdb.movies$total.gross)
mean(imdb.movies$total.gross, na.rm = TRUE) 

imdb.movies$total.gross[is.na(imdb.movies$total.gross)] <- 
  mean(imdb.movies$total.gross, na.rm = TRUE) 
summary(imdb.movies$total.gross)

# budget class
summary(imdb.movies$budget)
class(imdb.movies$budget)
imdb.movies$budget <- as.numeric(as.character(imdb.movies$budget))

## create new variable
imdb.movies$budgetAvil <- is.na(imdb.movies$budget)

## aggreagtion
table(imdb.movies$genre)
aggregate(total.gross ~ genre, data = imdb.movies, FUN = "mean")

# class exercise
numDirectors <- table(players.df[players.df$role == "Director",]$id)
numDirectors.df <- as.data.frame(numDirectors)
names(numDirectors.df) <- c("id", "numDir")

## merge files
merged.data <- merge(imdb.movies, numDirectors.df, 
      by.x = "id", 
      by.y = "id", 
      all = T)


## data partitioning
set.seed(1)

train.rows <- sample(1:dim(imdb.movies)[1], dim(imdb.movies)[1]*0.6)
train.data <- imdb.movies[train.rows, ]
valid.data <- imdb.movies[-train.rows, ]

