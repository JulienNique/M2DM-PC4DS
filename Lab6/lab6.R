library(bigmemory)

if (!file.exists("big.desc")) {
  
  # 10 000 lignes et 5 colonnes =
  x <- big.matrix(nrow = 1e4,
                  ncol = 5,
                  init = 0, # Optionnel mais souvent une bonne idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")

#Important de faire une "boucle" pour gérer la mémoire !
x[,1] <- runif(1e4, 0, 1)
x[,2] <- runif(1e4, 0, 1)
x[,3] <- runif(1e4, 0, 1)
x[,4] <- runif(1e4, 0, 1)
x[,5] <- runif(1e4, 0, 1)
#autre solution avec foreach
head(x)

for (i in 1:ncol(x)){
  x[,i] <- scale(x[,i], center = TRUE, scale = TRUE)
}

matriceCor <- matrix(1, ncol= ncol(x), nrow = ncol(x))

for (i in 1:ncol(x)-1){
  for (j in (i+1):ncol(x)){
    matriceCor[j,i] <- matriceCor[i,j] <- cor(x[,i],x[,j])
  }
}

decomp <- eigen(matriceCor)

# Rcpp
f <- function(n) {
  if (n < 2) return(n)
  return(f(n - 1) + f(n - 2))
}

Rcpp::cppFunction("int g(int n) {
  if (n < 2) return(n);
                  return(g(n-1) + g(n-2)); }")
sapply(0:11, g)
