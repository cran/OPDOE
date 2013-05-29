\name{triangular.test}
\alias{triangular.test}
\title{Triangular Test Object}
\arguments{
  A \code{triangular.test} object is a list of
  \item{x}{data for group 1}
  \item{y}{data for group 2}
  \item{n}{size of group 1}
  \item{m}{size of group 2}
  \item{alpha}{risk of 1st kind}
  \item{beta}{risk of 2nd kind}
  \item{dist}{character, either \code{"normal"} or \code{"bernoulli"}, describing the
    type of triangiular test}
  \item{sample}{character, \code{"one"} or \code{"two"}}
  \item{kind}{character, \code{"one-sided"} or \code{"two-sided"}}
  \item{p0}{parameter describing the Null hypothesis, see  \code{\link{triangular.test.prop}}}
  \item{p1}{...}
  \item{p2}{...}
  \item{mu0}{parameter describing the Null hypothesis, see \code{\link{triangular.test.norm}}}
  \item{mu1}{...}
  \item{mu2}{...}
  \item{result}{character, outcome of the test, \code{"H0"} or \code{"H1"}}
  \item{step}{total number of steps}
  and some more components for internal use.
}
\description{
  An \code{triangular.test} object is created with
  \code{\link{triangular.test.norm}} or \code{\link{triangular.test.prop}} 
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
