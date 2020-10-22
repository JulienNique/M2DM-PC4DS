A <- function(h){
  if (h >= 0 & h <= 2) return(100*h^2)
  else return(400*(h-1))
}

v = read.delim('data/catchment.txt')
v = v[,1]
hmax = 10
alpha = 1
beta = 0.05

h = 2
level <- tracking(h, hmax, A, alpha, beta, v)
day <- seq(length(level))
plot(day, level, col="blue", type="o", pch="+", lty=1)

h = 5
level <- tracking(h, hmax, A, alpha, beta, v)
day <- seq(length(level))
plot(day, level, col="dark green", pch="+", lty=1)
