\name{update.triangular.test}
\alias{update.triangular.test}
\alias{update.triangular.test.norm}
\alias{update.triangular.test.prop}
\title{Print method for Triangular Test Objects}
\usage{
\method{update}{triangular.test}(object, x=NULL, y=NULL, initial=FALSE,
plot="last", recursive=FALSE, ...)
}
\arguments{
  \item{object}{\code{\link{triangular.test}} object}
  \item{x}{data for group 1}
  \item{y}{data for group 2}
  \item{initial}{logical, used internally for creating a
    \code{\link{triangular.test}} object}
  \item{plot}{character, \code{"all"}: plot all intermediate steps,
    \code{"last"}: plot only the last state}
  \item{recursive}{logical, used internally to decide wether a plot
    should be generated (will be omitted if recursively called)}
  \item{...}{additional parameters for \code{update}}
  }
\description{
  Updates a \code{triangular.test} object and executes one or more steps
  in the sequence of tests.
}
\seealso{\code{\link{triangular.test.norm}}, \code{\link{triangular.test.prop}}
}

\references{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt:
  Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}   
\author{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt
}
\keyword{test}
