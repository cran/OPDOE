\name{design.reg.polynom}
\alias{design.reg.polynom}
\alias{design.regression.polynom}

\title{
  Design for Polynomial Regression
}
\description{
  Determines locations and number of replications for a polynomial
  regression design.
  
  Needs specification of order of polynom, borders of intervall and total
  number of measurements as input.
}
\usage{
design.regression.polynom(a, b, k, n)
design.reg.polynom(...)
}
\arguments{
  \item{a}{
    lower bound of interval
  }
  \item{b}{
    upper bound of interval
  }
  \item{k}{
    order of polynom
  }
  \item{n}{
    total number of planned measurements
  }
  \item{...}{only used for call wrapper \code{design.reg.polynom}}
}
\details{
  Uses Legendre Polynomials to determine the support points for the
  design:
  
  If \eqn{a=-1}, \eqn{b=1}: places \eqn{k +1} support points in
  \eqn{[-1,1]}, located at the roots of \eqn{(1-x^{2})\frac{dP_{k}(x)}{dx}}
  where \eqn{P_{k}(x)} is the Legendre polynomial of degree \eqn{k}).
  
  Distributes the \code{n} measurements almost equally over the
  support points. 
}
  
\value{
  Object of class \code{\link{design.regression}}
}
\note{
\code{design.reg.polynom} is a call wrapper for backward compatibility for
\code{design.regression.polynom}
}
\references{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt:
  Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}   
\author{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt
}
\keyword{lm}
\examples{
x <- design.reg.polynom(10, 100, 3, 45)
x
}