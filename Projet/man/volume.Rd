\name{volume}
\alias{volume}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volume of water contained by the dam
}
\description{
Computes the volume of water contained by the dam
}
\usage{
volume(h, hmax, ftn, [cl])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
a scalar or a numeric vector
}
  \item{hmax}{
a scalar
}
  \item{ftn}{
a function which returns the cross-sectionnal aera of the dam for a given height
}
  \item{cl}{
an object of class "cluster"
}

}
\details{
For each height h[i] in vector h, the function volume computes the volume of water contained by the dam when it is filled to level h[i] <= hmax. It returns 0 if h[i] < 0 and Vmax = volume(hmax, hmax, ftn) otherwise.
}
\value{
A numeric vector of the same size as h
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
J. Nique
}

\seealso{
height, tracking
}

\examples{
A <- function(h){
  return(h)
}

hmax <- 10
h <- seq(10)
volume(h,hmax,A)

#parallel computing
library(parallel)
cl = makeCluster(2)
volume(h, hmax, A, cl)
stopCluster(cl)

}
