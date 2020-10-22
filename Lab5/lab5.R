## Fichier: demo-bigmemory.r
## Date: Nov 2018 by jc (using code by Jay Emerson)

library(bigmemory)

if (!file.exists("big.desc")) {
  
  # 100 000 000 lignes et 8 colonnes =
  #           800 M d'entrées * 8 bytes ~ 6 GB
  # ceci prends du temps :
  x <- big.matrix(nrow = 1e8,
                  ncol = 8,
                  init = 0, # Optionel mais souvent une bonné idée
                  backingfile    = "big.bin",
                  descriptorfile = "big.desc")
  
} else x <- attach.big.matrix("big.desc")

# Attention : x à le comportement d'une matrice, mais ce n'est pas une matrice

x
# x[,]       # A NE PAS FAIRE !!!

dim(x)

x[1,1] <- 0.1234
x[nrow(x), 1] <- 0.5678
head(x)
tail(x)

system.time(x[,1] <- 1.234)


#######################################################################
# Faire attention aux dimensions, adapter en fonction de votre machine 

gc(reset=TRUE) # Utilisation de la mémoire
system.time(x[,3] <- runif(nrow(x)))
gc()           # Encore la mémoire (~.75 GB de mémoire R réelle)
gc(reset=TRUE) # La vision de la RAM selon R


######################################################
##
## Recommencer une nouvelle session R
##

library(bigmemory)
x <- attach.big.matrix("big.desc")
dim(x) 
head(x)
tail(x)


######################################################
##
## est-ce safe de faire tourner ces lignes ?
##

if (FALSE) { # fool proof =)
  
  x[,1] <- x[,1] + 1 #* Cas 0: Attention, mais besoin de 
  #*        2 * object.size(x[,1]) de RAM
  hist(x[,3])        #* Cas 1: Danger, besoin d'environ
  #*        3 * object.size(x[,4]) RAM
  cor(x)             #* Cas 2: erreur
  as.data.frame(x)   #* Cas 3: error
  y <- x[]           #* Cas 4: Catastrophe !
}


##
## Moving on... column minima?
##

x[1:5,] <- rnorm(40)
x[6,1] <- NA
x[6,2] <- NaN
x[6,3] <- -Inf
x[6,4] <- Inf
x[nrow(x),] <- runif(8)
head(x)
tail(x)

####################################################
# Observer le temps et la conso de mémoire.

system.time({
  ans <- sapply(1:ncol(x), function(i) min(x[,i]))
})
ans
