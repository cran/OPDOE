\name{size_n.one_way.model_1}
\alias{size_n.one_way.model_1}
\title{
Design for One-Way ANOVA
}
\description{
Returns the optimal number of obervations per level of factor A.
}
\usage{
size_n.one_way.model_1(alpha, beta, delta, a, cases)
}
\arguments{
  \item{alpha}{
Risk of 1st kind
}
  \item{beta}{
Risk of 2nd kind
}
  \item{delta}{
The minimum difference to be detected
}
  \item{a}{
Number of levels of fixed factor A
}
  \item{cases}{
Specifies whether the \code{"maximin"} or \code{"maximin"} sizes are to be determined
}
}
\details{
see chapter 3 in the referenced book
}
\value{
Integer giving the size.
}
\references{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt: Optimal Experimental Design with R, Chapman and Hall/CRC, 2011  
}
\author{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt, Minghui Wang
}
\note{
Better use \code{\link{size.anova}} which allows a cleaner notation.
}
\seealso{
\code{\link{size.anova}}  
}
\examples{
size_n.one_way.model_1(0.05,0.1, 2, 4, "maximin")
size_n.one_way.model_1(0.05,0.1, 2, 4, "minimin")
}

\keyword{ anova }

