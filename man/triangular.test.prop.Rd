\name{triangular.test.prop}
\alias{triangular.test.prop}

\title{
Triangular Test for Bernoulli Data
}
\description{
  Performs a sequential test, compares probabilities in two groups.
}
\usage{
triangular.test.prop(x, y = NULL, p0 = NULL, p1 = NULL, p2 = NULL, alpha
= 0.05, beta = 0.1, delta = NULL, plot = TRUE)
}
\arguments{
  \item{x}{
initial data for group \code{x}, at least 1 entry, values restricted to
0 and 1.
}
  \item{y}{
  initial data for group \code{y}, at least 1 entry for a two sample
  test, otherwise omitted, values restricted to
0 and 1.
}
\item{p0}{
    specifies Null and alternative hypothesis, see Details below. 
}
\item{p1}{
  specifies Null and alternative hypothesis, see Details below. 
}
\item{p2}{
  specifies Null and alternative hypothesis, see Details below. 
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
\item{delta}{
    The minimum difference to be detected, alternative way to specify \code{p2=p1+delta}, see above, use either this or \code{p2}.
  }
}
\details{

  One-sample:

  This function performs a one- or two-sided sequential Test for equal
  proportions \eqn{p=\code{p1}} \eqn{p=\code{p1}} versus

  \eqn{p>\code{p2}}, if \code{p2} > \code{p1} (one-sided)
  
  \eqn{p<\code{p2}}, if \code{p2} < \code{p1} (one-sided)
  
  \eqn{p<\code{p0}} or \eqn{p>\code{p2}},
  if \code{p2} > \code{p1} and \code{p0} <
  \code{p1} (two-sided, possibly unsymmetric)


  Two-sample:

  This function performs a one- or two-sided sequential Test for equal
  proportions \eqn{p_1=\code{p1}} \eqn{p_2=\code{p1}} versus

  \eqn{p_2>\code{p2}}, if \code{p2} > \code{p1} (one-sided)
  
  \eqn{p_2<\code{p2}}, if \code{p2} < \code{p1} (one-sided)
  
  \eqn{p_2<\code{p0}} or \eqn{p_2>\code{p2}},
  if \code{p2} > \code{p1} and \code{p0} <
  \code{p1} (two-sided, possibly unsymmetric)
}
\value{
  An object of class  \code{\link{triangular.test}}, to be used for
  later update steps.
}
\note{
  A two-sided test may be specified by supplying both \code{p1} and
  \code{p2}, even unsymmetric if needed.
  }
\references{
  Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt:
  Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}   
\author{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt
}
\seealso{
\code{\link{triangular.test}}, \code{\link{triangular.test.norm}}, \code{\link{update.triangular.test}}
}
\examples{
data(heights)
attach(heights)
male180 <- as.integer(male>180) 
female164 <- as.integer(female>164)
sum(male180)/length(male180) 
tt <- triangular.test.prop(x=female164[1:3],
   y=male180[1:3], p1=0.4,p2=0.8,p0=0.1,
   alpha=0.05, beta=0.2)
tt <- update(tt,x=female164[4])
tt <- update(tt,y=male180[4])
tt <- update(tt,x=female164[5])
sum(female164)/length(female164)
}

\keyword{ test }