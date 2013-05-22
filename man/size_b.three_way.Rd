\name{size_b.three_way}
\alias{size_b.three_way_mixed_ab_in_c.model_3_a}
\alias{size_b.three_way_mixed_cxbina.model_4_a}
\alias{size_b.three_way_mixed_cxbina.model_4_axc}
\alias{size_b.three_way_mixed_cxbina.model_4_c}
\alias{size_b.three_way_mixed_cxbina.model_7_c}
\alias{size_b.three_way_nested.model_6_a}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Three-way analysis of variance -- nested and mixed classification  \eqn{A\succ B \succ  C}{A in B in C} and \eqn{(A\times B)\succ  C}{(A in B) x C} model III, IV and VII
}
\description{
Returns the optimal number of levels for factor B.
}
\usage{
size_b.three_way_mixed_ab_in_c.model_3_a(alpha, beta, delta, a, c, n, cases)
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
size_b.three_way_mixed_ab_in_c.model_3_a(0.05, 0.1, 0.5, 6, 5, 1, "maximin")
size_b.three_way_mixed_ab_in_c.model_3_a(0.05, 0.1, 0.5, 6, 5, 1, "minimin")
size_b.three_way_mixed_cxbina.model_4_a(0.05, 0.1, 0.5, 6, 4, 1, "maximin")
size_b.three_way_mixed_cxbina.model_4_a(0.05, 0.1, 0.5, 6, 4, 1, "minimin")
size_b.three_way_mixed_cxbina.model_4_c(0.05, 0.1, 0.5, 6, 4, 1, "maximin")
size_b.three_way_mixed_cxbina.model_4_c(0.05, 0.1, 0.5, 6, 4, 1, "minimin")
size_b.three_way_mixed_cxbina.model_4_axc(0.05, 0.1, 0.5, 6, 4, 1, "maximin")
size_b.three_way_mixed_cxbina.model_4_axc(0.05, 0.1, 0.5, 6, 4, 1, "minimin")
size_b.three_way_nested.model_6_a(0.05, 0.1, 0.5, 6, 4, 2, "maximin")
size_b.three_way_nested.model_6_a(0.05, 0.1, 0.5, 6, 4, 2, "minimin")
}
\keyword{ anova }
