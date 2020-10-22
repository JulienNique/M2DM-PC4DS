height <- function(h, hmax, v, ftn){
  u <- Newton1d(h, function(u){
    volume(u, hmax, ftn) - volume(h, hmax, ftn) - v}, 1e-6)
  return(u)
}

height <- Vectorize(height, vectorize.args = c("h","v"))
