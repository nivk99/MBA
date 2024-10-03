# textual variable
txt <- "Hi there"

# numeric variables
x <- 8
y <- 9

result <- x+y

# vector
vec <- c(3,4,9)
vec*2
vec[1]
vec[2]

#conversion
var_c <-"1"
class(var_c)
var_n <- as.numeric(var_c)
class(var_n)

vec <- c("c","a","b","a","c")
class (vec)
fct <-as.factor(vec)



#if else

a <-5
if (a<7){
  a<-a*2
}else{
  a<-a/2
}

a<-ifelse(a<7,a*2,a/2)


# loops
for (i in 1:10){
  print(i*i)
}


