set.seed(1)
train.rows<-sample(1:dim(imdb.movies)[1],dim(imdb.movies)[1]*0.6)
train.data <-imdb.movies[train.rows,]
valid.data <- imdb.movies[-train.rows,]


