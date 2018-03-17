\name{triangular.test.norm}
\alias{triangular.test.norm}
\title{
Triangular Test for Normal Data
}
\description{
  Performs a sequential test, compares means of two normally distributed
  groups.
}
\usage{
triangular.test.norm(x, y = NULL, mu0 = NULL, mu1, mu2 = NULL,
                     delta = NULL, sigma = NULL, sigma2 = NULL,
                     alpha = 0.05, beta = 0.1, plot = TRUE)
}
\arguments{
  \item{x}{
    initial data for group \code{x}, at least 1 entry.
  }
  \item{y}{
    initial data for group \code{y}, at least 1 entry for a two sample
  test, otherwise omitted.
  }
  \item{mu0}{
    specifies Null and alternative hypothesis, see Details below. 
  }
  \item{mu1}{
    specifies Null and alternative hypothesis, see Details below. 
  }
  \item{mu2}{
    specifies Null and alternative hypothesis, see Details below. 
  }
  \item{delta}{
    The minimum difference to be detected, alternative way to specify \code{mu2=m1+delta}, see above, use either this or \code{mu2}.
  }
\item{sigma}{
  prior sigma.
}
\item{sigma2}{
    prior sigma for group 2 if different than for grouop 1.
}
\item{alpha}{
  Risk of 1st kind
}
\item{beta}{
  Risk of 2nd kind
}
  \item{plot}{
logical, indicates whether a initial plot should be generated.
}
}
\details{

  One-sample:

  This function performs a one- or two-sided sequential Test for 
 \eqn{\mu=\code{mu1}} versus

  \eqn{\mu>\code{mu2}}, if \code{mu2} > \code{mu1} (one-sided)
  
  \eqn{\mu<\code{mu2}}, if \code{mu2} < \code{mu1} (one-sided)
  
  \eqn{\mu<\code{mu0}} or \eqn{\mu>\code{mu2}},
  if \code{mu2} > \code{mu1} and \code{mu0} <
  \code{mu1} (two-sided, possibly unsymmetric)
  
  
  
  Two-sample:
  
  This function performs a one- or two-sided sequential Test for equal
  means \eqn{\mu_1=\code{mu1}} \eqn{\mu_2=\code{mu1}} in both groups versus

  \eqn{\mu_2>\code{mu2}}, if \code{mu2} > \code{mu1} (one-sided)
  
  \eqn{\mu_2<\code{mu2}}, if \code{mu2} < \code{mu1} (one-sided)
  
  \eqn{\mu_2<\code{mu0}} or \eqn{\mu_2>\code{mu2}},
  if \code{mu2} > \code{mu1} and \code{mu0} <
  \code{mu1} (two-sided, possibly unsymmetric)
  


  
}
\value{
  An object of class  \code{\link{triangular.test}}, to be used for
  later update steps.
}
\note{
  A two-sided test may be specified by supplying both \code{mu1} and
  \code{mu2}, even unsymmetric if needed.
  }
\references{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt:
  Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}   
\author{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt
}
\seealso{
  \code{\link{triangular.test}}, \code{\link{triangular.test.prop}}, \code{\link{update.triangular.test}}
}
\examples{
data(heights)
attach(heights)
# a symmetric two sided alternative:
tt <- triangular.test.norm(x=female[1:3],
   y=male[1:3], mu1=170,mu2=176,mu0=164,
   alpha=0.05, beta=0.2,sigma=7)
# Test is yet unfinished, add the remaining values step by step:
tt <- update(tt,x=female[4])
tt <- update(tt,y=male[4])
tt <- update(tt,x=female[5])
tt <- update(tt,y=male[5])
tt <- update(tt,x=female[6])
tt <- update(tt,y=male[6])
tt <- update(tt,x=female[7])
tt <- update(tt,y=male[7])
# Test is finished now
# an unsymmetric two sided alternative:
tt2 <- triangular.test.norm(x=female[1:3],
   y=male[1:3], mu1=170,mu2=180,mu0=162,
   alpha=0.05, beta=0.2,sigma=7)
tt2 <- update(tt2,x=female[4])
}
\keyword{ test }

