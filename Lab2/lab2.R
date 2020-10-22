## Exercice 2####
f1 <- function(n) {
  res <- 0
  for(i in 1:n)
    res <- res+1
  return(res)
}

#debug(f1)
f1(5)


## Exercice 3####
f2 <- function(n){
  res <- sum(seq(1,n,by=1))
  return(res)
}

f3 <- function(n){
  res <- n*(n+1)/2
  return(res)
}

print(system.time(f1(1000000)))
print(system.time(f2(10000000)))
print(system.time(f3(1000000000000)))

## Exercice 4####
f_tri <- function(x){
  if (x <= 0){
    res <- 0
  }
  else if (x >= 0 & x <= 1){
    res <- x
  }
  else if (x >= 1 & x <= 2){
    res <- 2 - x
  }
  else{
    res <- 0
  }
  return(res)
}
f_tri <- Vectorize(f_tri, vectorize.args = "x")
#curve(f_tri, from = 0, to = 20, lwd = 2)
x = seq(-6,4, by = 0.1)
plot(x,f_tri(x), type = 'l')

library(microbenchmark)
xs <- seq(-3,3, lengh.out = 1e5)
microbenchmark(f_tri(xs))

rejection <- function(f, a, b, M){
  x = runif(1, min=a, max=b)
  y = runif(1, min=0, max=M)
  while (y > f(x)){
    x = runif(1, min=a, max=b)
    y = runif(1, min=0, max=M)
  }
  return(x)
}

Rcpp::cppFunction("double rejection(double a, double b, double M) {
                  }")

v = numeric()
for (k in 1:1000){
  v <- append(v, rejection(f_tri, 0, 2, 1))
}

library(ggplot2)
df <- data.frame(val = v)
p <- ggplot(df, aes(x=val)) + geom_density()