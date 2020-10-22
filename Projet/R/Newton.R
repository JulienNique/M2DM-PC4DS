Newton1d <- function(x0, f, epsilon){
  x <- as.numeric(x0)
  while (abs(f(x)) > epsilon){
    ff <- numericDeriv(expr = quote(f(x)), theta = 'x')
    if (attributes(ff)$gradient[1] == 0) {
      x <- 'stop'
      break
    }
    else {
      x <- x - f(x)/attributes(ff)$gradient[1]
    }
  }
  return(x)
}
