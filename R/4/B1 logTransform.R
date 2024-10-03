## observe function
x <- c(-1:100)
log_x <- log(x)

plot(log_x ~ x)

## observe distribution
x <- rlnorm(1000, 0, 1)
hist(x)
hist(log(x))

boxplot(x)
boxplot(x, log = 'y')
