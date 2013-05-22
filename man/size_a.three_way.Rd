\name{size_a.three_way}
\alias{size_a.three_way_mixed_cxbina.model_3_c}
\alias{size_a.three_way_mixed_cxbina.model_7_c}
\alias{size_ab.three_way_mixed_cxbina.model_7_c}
\title{
Three-way analysis of variance -- mixed  classification \eqn{(A\times B)\succ  C}{(A in B) x C}  model III and VII
}
\description{
Returns the optimal number of levels for factor A (and B).
}
\usage{
size_a.three_way_mixed_cxbina.model_3_c(alpha, beta, delta, b, c, n, cases)
size_a.three_way_mixed_cxbina.model_7_c(alpha, beta, delta, b, c, n, cases)
size_ab.three_way_mixed_cxbina.model_7_c(alpha, beta, delta, c, n, cases)
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
  \item{b}{
Number of levels of fixed factor B
}
  \item{c}{
Number of levels of fixed factor C
}
  \item{n}{
Number of replications
}
  \item{cases}{
Specifies whether the \code{"maximin"} or \code{"maximin"} sizes are to be determined
}
}
\details{
see chapter 3 in the referenced book
}
\value{
  Integer(s) giving the size(s).
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
size_a.three_way_mixed_cxbina.model_3_c(0.05, 0.1, 0.5, 5, 4, 1, "maximin")
size_a.three_way_mixed_cxbina.model_3_c(0.05, 0.1, 0.5, 5, 4, 1, "minimin")
size_a.three_way_mixed_cxbina.model_7_c(0.05, 0.1, 0.5, 5, 4, 1, "maximin")
size_a.three_way_mixed_cxbina.model_7_c(0.05, 0.1, 0.5, 5, 4, 1, "minimin")
size_ab.three_way_mixed_cxbina.model_7_c(0.05,0.1,0.50, 5,2,  "maximin")
size_ab.three_way_mixed_cxbina.model_7_c(0.05,0.1,0.50, 5,2,  "minimin")
}

\keyword{ anova }
