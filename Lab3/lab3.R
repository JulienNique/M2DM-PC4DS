# nbproc <- 4
# nb <- 1e6
# y_vec <- rnorm(nbproc * nb)
# sum1 <- sum(y_vec)
# 
# y_list <- split(y_vec, rep(1:nbproc, each = nb))
# s_list <- lapply(y_list, sum)
# 
# sum2 <- Reduce('+', s_list)
# 
# library(parallel)
# detectCores()
# cl <- makeCluster(2)
# 
# system.time(s_parlist <- clusterApply(cl, y_list, sum), sum3 <- Reduce('+', s_parlist))
# stopCluster(cl); rm(cl);
# 
# print(identical(sum1, sum3))

##Exercice 2####
library(microbenchmark)

f1 <- function(data){
  y_list <- split(data, rep(1:20))
  s_list <- lapply(y_list, sum)
  sum <- Reduce('+', s_list)
  return(sum)
}

library(parallel)
detectCores()
cl <- makeCluster(4)
f2 <- function(data){
  y_list <- split(data, rep(1:20))
  s_parlist <- clusterApply(cl, y_list, sum)
  sum <- Reduce('+', s_parlist)
  return(sum)
}

y_vec <- rnorm(1e5)
m <- microbenchmark(f1(y_vec), f2(y_vec), times=100L)
autoplot(m)
stopCluster(cl); rm(cl);

##Exercice 3####
leave.one.out <- function(i){
  fit <- lm(Petal.Length ~ Petal.Width, data = iris[-i,])
  ypred <- predict(fit, newdata = iris[i,])
  err = (ypred - iris$Petal.Length[i])^2
  return(err)
}

err = 0
for (k in 1:150){
  err = err + leave.one.out(k)
}
sum1 <- err / 150

sum2 <- Reduce('+', lapply(1:150, leave.one.out)) / 150

library(parallel)
cl <- makeCluster(2)
s_parlist <- clusterApply(cl,1:150, leave.one.out)
sum3 <- Reduce('+', s_parlist) / 150

library(foreach)
foreach(i = 1:150, .combine = sum) %do% leave.one.out(i) / 150

library(doParallel)
foreach(i = 1:150, .combine = sum) %dopar% leave.one.out(i) / 150