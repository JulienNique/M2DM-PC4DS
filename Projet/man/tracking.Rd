\name{tracking}
\alias{tracking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tracking the level of water contained by the dam over time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tracking(h1, hmax, ftn, alpha, beta, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h1}{
a scalar representing the initial level of the dam
}
  \item{hmax}{
a scalar
}
  \item{ftn}{
a function which returns the cross-sectionnal aera of the dam for a given height
}
  \item{alpha}{
a scalar representing the volume of water taken from the dam for use per day
}
  \item{beta}{
a positive scalar representing a coefficient such that beta*ftn(t) is the volume of water lost due to evaporation during day t
}
  \item{v}{
a numeric vector (of size 99) representing the volume of rain falling into the catchment for day 1 to 99
}
}
\details{

}
\value{
Tracking returns a numeric vector h of size 100 where h[t] is the level of water contained by the dam at day t.

}
\references{
%% ~put references to the literature/web site here ~
}

\author{
J. Nique
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
A <- function(h){
  if (h >= 0 & h <= 2) return(100*h^2)
  else return(400*(h-1))
}

v = 0
hmax = 10
alpha = 1
beta = 0.05
h = 2
tracking(h, hmax, A, alpha, beta, v)
}
