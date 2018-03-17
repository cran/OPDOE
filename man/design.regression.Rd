\name{design.regression}
\alias{design.regression}
\title{Regression Design Object}
\arguments{
  A \code{triangular.test} object is a list of
  \item{model}{character, currently only \code{"polynomial"} is implemented
    }
  \item{locations}{choosen locations}
  \item{replications}{choosen replications per location}
  \item{interval}{vector of size 2 storing the given interval}
}
\description{
  An \code{design.regression} object is created with
  \code{\link{design.regression.polynom}}
}
\seealso{\code{\link{design.regression.polynom}}
}

\references{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt:
  Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}   
\author{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt
}
\keyword{lm}
