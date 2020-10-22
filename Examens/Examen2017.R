## Fichier: 2017pc4ds-trump.r
## Etudian : D. Trump
## Description : Rendu de l'examen du PC4DS 2017
## Date : 26 janvier 2017

rm(list = ls())

################################################################################
####                                                                        ####
####                       E X E R C I C E   1                              ####
####                                                                        ####
################################################################################
## Un collègue qui travaille sur le test d'indépedance Chi-2 vous transmet le 
## code R ci dessous :
myfun<-function(a){da <- dim(a)
n  <- da[1]; m  <- da[2]; res <- c()
for(i in seq(n)){res<-c(res,0) 
for(j in seq(m)){if(is.na(a[i,j])) res[i]<-res[i]+1}}
return(res)}

# test
nas <- matrix(ifelse(runif(5e5) > 0.2, NA, 1), 1e4) 
a <- matrix(rnorm(5e5), 1e4) * nas
myfun(a) 

# Il vous demande de l'aide pour améliorer le temps de calcul. 

# __ 1. Rajoutez une description du code ####
# en suivant le canevas ci dessous

# Description : ...
# Entrée : ...
# Sortie : ...

# __ 2. Écrivez une version plus facile à lire de myfun #### 
# Corrigez la mise en format du code, le noms des objets intermédiaires 
# et commentez-le si besoin.

# __ 3. Écrivez une version vectorisée de la fonction dans 2. ####

# __ 4. Écrivez une première version en parallèle de la fonction dans 2. ####
# en utilisant la librarie foreach (parallélisme implicite) et 2 noeuds de 
# calcul.

# __ 5. Écrivez une deuxièeme version en parallèle de myfun2 ####
# en utilisant la librarie parallel (parallélisme explicite) et 2 noeuds de 
# calcul.

# __ 6. Obtenez les temps d'exécution ####
# de toutes les versions de la fonction myfun que vous avez écrit.
# Quelle est la plus performant?



################################################################################
####                                                                        ####
####                       E X E R C I C E   2                              ####
####                                                                        ####
################################################################################

# Obtenir une version plus performante del fonction main (cf. fichier 
# simulate_multivariate.r)

# Vous serez notés en fonction du gain obtenu. Ne modifiez pas les valeurs
# du point 1, UNIQUEMENT la fonction main.
# Astuce : n'essayez pas de rentrer dans le détail du code (assez long et )

#library(CDVine)
library(energy)
library(mvtnorm)
source('simulate_exam.r')

## 1. Choix pour les simulations ####
corel   <- seq(0.5, 0.95, by = 0.15)    # Corrélations
nb_data <- seq(20, 200, length.out = 4) # Taille des données réelles
nb_var  <- 4                            # Nombre de variables
n_simu  <- nb_data                      # Taille de données simulées
nb.iter <- 2                            # Nombre d'itérations
nb.test <- 5                            # Nombre de tests
method.vect <- c("indep", "indepPCA")   # Méthodes de simulation

## 2. Simulations ####
system.time(
  multitest <- main(method.vect, corel, nb_data, nb_var, 
                    n_simu, nb.iter, nb.test)
)
