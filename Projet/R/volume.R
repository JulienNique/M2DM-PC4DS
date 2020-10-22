volume <- function(h, hmax, ftn, cl){
  if (h < 0){
    v <- 0
  }
  else if (h <= hmax){
    n <- 100  #nombre de rectangles utilisés pour l'intégration
    if (missing(cl)){
      a <- lapply(0:(n-1)*h/n, ftn)
    }
    else{    #on effectue le calcul en parallèle avec les clusters cl
      a <- parLapply(cl, 0:(n-1)*h/n, ftn)
    }
    v <- Reduce('sum',a)*h/n
  }
  else {
    v <- volume(hmax, hmax, ftn)
  }
  return(v)
}

volume <- Vectorize(volume, vectorize.args = "h")
