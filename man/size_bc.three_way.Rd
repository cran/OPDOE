\name{size_bc.three_way}
\alias{size_bc.three_way_cross.model_4_a_case1}
\alias{size_bc.three_way_cross.model_4_a_case2}
\alias{size_bc.three_way_mixed_cxbina.model_6_a_case1}
\alias{size_bc.three_way_mixed_cxbina.model_6_a_case2}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Three-way analysis of variance -- cross classification  (A in B) x C -- model IV,
Three-way analysis of variance -- mixed  classification (A in B) x C  model VI
}
\description{
Returns the optimal number of levels for factor B and C.
}
\usage{
size_bc.three_way_cross.model_4_a_case1(alpha, beta, delta, a, n, cases)
size_bc.three_way_cross.model_4_a_case2(alpha, beta, delta, a, n, cases)
size_bc.three_way_mixed_cxbina.model_6_a_case1(alpha, beta, delta, a, n, cases)
size_bc.three_way_mixed_cxbina.model_6_a_case2(alpha, beta, delta, a, n, cases)
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
  Integers giving the sizes.
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
size_bc.three_way_cross.model_4_a_case1(0.05, 0.1, 0.5, 6, 2, "maximin")
size_bc.three_way_cross.model_4_a_case1(0.05, 0.1, 0.5, 6, 2, "minimin")
size_bc.three_way_cross.model_4_a_case1(0.05, 0.1, 1, 6, 2, "maximin")
size_bc.three_way_cross.model_4_a_case1(0.05, 0.1, 1, 6, 2, "minimin")
size_bc.three_way_cross.model_4_a_case2(0.05, 0.1, 0.5, 6, 2, "maximin")
size_bc.three_way_cross.model_4_a_case2(0.05, 0.1, 0.5, 6, 2, "minimin")
size_bc.three_way_cross.model_4_a_case2(0.05, 0.1, 1, 6, 2, "maximin")
size_bc.three_way_cross.model_4_a_case2(0.05, 0.1, 1, 6, 2, "minimin")
size_bc.three_way_mixed_cxbina.model_6_a_case1(0.05, 0.1, 0.5, 6, 2, "maximin")
size_bc.three_way_mixed_cxbina.model_6_a_case1(0.05, 0.1, 0.5, 6, 2, "minimin")
size_bc.three_way_mixed_cxbina.model_6_a_case2(0.05, 0.1, 0.5, 6,  2, "maximin")
size_bc.three_way_mixed_cxbina.model_6_a_case2(0.05, 0.1, 0.5, 6,  2, "minimin")

}
\keyword{ anova }
