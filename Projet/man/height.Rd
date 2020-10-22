\name{height}
\alias{height}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Height of water contained by the dam
}
\description{
Computes the height of water contained by the dam
}
\usage{
height(h, hmax, v, ftn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
a scalar or a numeric vector
}
  \item{hmax}{
a scalar
}
  \item{v}{
a scalar or a numeric vector of the same size as h
}
  \item{ftn}{
a function which returns the cross-sectionnal aera of the dam for a given height
}
}
\details{
Computes the height of water contained by the dam from a given current level of water h and an amount of change of the volume of water v.
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
volume, tracking
}
\examples{
A <- function(h){
  return(h)
}

hmax <- 10
h <- seq(5)
v <- 1
height(h, hmax, v, A)
}
