\name{Newton1d}
\alias{Newton1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Newton method in one dimension
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Newton1d(x0, f, epsilon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x0}{
a scalar
}
  \item{f}{
a function
}
  \item{epsilon}{
a scalar
}
}
\details{
Newton1d implements the Newton method for finding a root of f ;
x0 is the initial value of the algorithm which computes iteratively a sequence that can converge to a solution of the equation f(x) = 0
}
\value{
'Stop' if the algorithm doesn't converge or a scalar x such that f(x) is bounded by epsilon
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
J. Nique
}

\examples{
f <- function(u){
  exp(u) - 2
}

Newton1d(0, f, 1e-5)
