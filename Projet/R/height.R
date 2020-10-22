height <- function(h, hmax, v, ftn){
  if (volume(h, hmax, ftn) + v <= 0){
    return(0)
  }
  else if (volume(h, hmax, ftn) + v > volume(hmax, hmax, ftn)){
    return(hmax)
  }
  else{
  u <- Newton1d(h, function(u){
    volume(u, hmax, ftn) - volume(h, hmax, ftn) - v}, 1e-6)
  return(u)
  }
}

height <- Vectorize(height, vectorize.args = c("h","v"))
