tracking <- function(h1, hmax, ftn, alpha, beta, v){
  h <- c(h1)
  for (t in 1:99){
    if (h[t] != 'stop'){
      h[t+1] = height(h[t], hmax, v[t] - alpha - beta*ftn(h[t]), ftn)
    }
    else break
  }
  return (h)
}
