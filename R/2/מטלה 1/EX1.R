# file: EX1
# Name: Niv Kotek
#ID: 208236315


# set working directory
setwd("C:/Users/niv/Desktop/R/2")


# Load the data from a CSV file (IMDB_movies.csv and IMDB_players.csv)
imdb.movies <- read.csv("IMDB_movies.csv", stringsAsFactors = TRUE)
head(imdb.movies)
players.df <- read.csv("IMDB_players.csv", stringsAsFactors = TRUE)
head(players.df)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
imdb.movies$budget <- as.numeric(as.character(imdb.movies$budget))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a new column containing TRUE if the budget is greater than or equal to a million, otherwise FALSE
imdb.movies$is.over.1m <- imdb.movies$budget >= 1000000
print(imdb.movies$is.over.1m)

# This function creates a frequency table of the values in the is.over.1m column,
# including a count of NA values if they exist in the column.
table(imdb.movies$is.over.1m, useNA = "ifany")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate using the table command the number of officials of each type by movie (ID)
n.actors.table <- table(players.df$id, players.df$role)


# Convert the frequency table into a data frame and matrix
n.actors <- as.data.frame.matrix(n.actors.table)

# Checking a type variable data.frame
class(n.actors)

# Displays the first rows of the data frame
head(n.actors)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filter rows where both budget and total gross are reported
movies <- imdb.movies[is.na(imdb.movies$budget) == FALSE & is.na(imdb.movies$total.gross) == FALSE, ]

# Compute the correlation between budget and total gross
correlation <- cor(movies$budget, movies$total.gross)

# Print the correlation coefficient
print(correlation)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Q.5~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Calculates the average gross income per MPAA rating and genre, only for genres classified as "crime comedy". Uses the aggregation function.
MPAA.rating.genre.total.gross.mean <- aggregate(total.gross ~ MPAA.rating+ genre, data = imdb.movies[imdb.movies$genre == "Crime Comedy", ], FUN = "mean")
# Print 
head(MPAA.rating.genre.total.gross.mean)



#chat GPT

crime_comedy_movies <- subset(imdb.movies, grepl("Crime Comedy", imdb.movies$genre, ignore.case = TRUE))

# Now, use the aggregate function to calculate the average gross revenue per MPAA rating and genre
average_gross <- aggregate(total.gross ~ MPAA.rating + genre, data = crime_comedy_movies, FUN = mean, na.rm = TRUE)

# Print the result
print(average_gross)








