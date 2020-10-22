## Exercice 1 ####

is.integer(2)

0.1 + 0.2 == 0.3
#False car le résultat de l'addtion n'est pas égal à 0.3

## Exercice 2 ####
#1
f <- function(x) {
    sin(x)^2+sqrt(abs(x-3))
}
#2
x = seq(-6,4, by = 0.1)
#plot(x,f(x), type = 'l')
curve(f, from = -6, to = 4, lwd = 2)
grid(lty = 1)
#4
optimise(f, interval = c(-6,4), maximum = FALSE)

## Exercice 3 ####
simuData <- function(n) {
  runif(n, min=0, max=20)
}

perte <- function(s, y, p) {
  sum(abs(y-s)^p)^(1/p)
}

data = simuData(100)
mean(data)
p = 2
f <- function(s) {
  sum(abs(data-s)^p)^(1/p)
}
res <- optimize(f, lower = -10, upper = 10)
res$objective
curve(f, from = 0, to = 20, lwd = 2)

## Exercice 4 ####