\name{heights}
\alias{heights}
\docType{data}
\title{
male / female heights data
}
\description{
Body heights of male and female students
collected in a classroom experiment.
}
\usage{data(heights)}
\format{
  A data frame with 7 observations on the following 2 variables.
  \describe{
    \item{\code{female}}{a numeric vector}
    \item{\code{male}}{a numeric vector}
  }
}
\references{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt: Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}
\author{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt, Minghui Wang
}
\examples{
data(heights)
attach(heights)
tt <- triangular.test.norm(x=female[1:3],
   y=male[1:3], mu1=170,mu2=176,mu0=164,
   alpha=0.05, beta=0.2,sigma=7)
# Test is yet unfinished, add the remaining values:
tt <- update(tt,x=female[4:7], y=male[4:7])
# Test is finished now
}
\keyword{datasets}
