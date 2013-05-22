\name{size_n.two_way}
\alias{size_n.two_way_cross.model_1_a}
\alias{size_n.two_way_cross.model_1_axb}
\alias{size_n.two_way_nested.model_1_test_factor_a}
\alias{size_n.two_way_nested.model_1_test_factor_b}
\alias{size_n.two_way_nested.a_random_b_fixed_b}
\title{
Design for Two-Way ANOVA
}
\description{
Returns the optimal number of obervations per level of factor A.
}
\usage{
size_n.two_way_cross.model_1_a(alpha, beta, delta, a, b, cases)
size_n.two_way_cross.model_1_axb(alpha, beta, delta, a, b, cases)
size_n.two_way_nested.model_1_test_factor_a(alpha, beta, delta, a, b, cases)
size_n.two_way_nested.model_1_test_factor_b(alpha, beta, delta, a, b, cases)
size_n.two_way_nested.a_random_b_fixed_b(alpha, beta, delta, a, b, cases)
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
  \item{b}{
Number of levels of fixed factor B
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
size_n.two_way_cross.model_1_a(0.05,0.1, 1, 6, 4, "maximin")
size_n.two_way_cross.model_1_a(0.05,0.1, 1, 6, 4, "minimin")
size_n.two_way_cross.model_1_axb(0.05,0.1, 1, 6, 4, "maximin")
size_n.two_way_cross.model_1_axb(0.05,0.1, 1, 6, 4, "minimin")
size_n.two_way_nested.model_1_test_factor_a(0.05, 0.1, 1, 6, 4, "maximin")
size_n.two_way_nested.model_1_test_factor_a(0.05, 0.1, 1, 6, 4, "minimin")
size_n.two_way_nested.a_random_b_fixed_b(0.05, 0.1, 1, 2, 10, "maximin")
size_n.two_way_nested.a_random_b_fixed_b(0.05, 0.1, 1, 2, 10, "minimin")
size_n.two_way_nested.a_random_b_fixed_b(0.05, 0.1, 1, 3, 10, "maximin")
size_n.two_way_nested.a_random_b_fixed_b(0.05, 0.1, 1, 3, 10, "minimin")
size_n.two_way_nested.a_random_b_fixed_b(0.05, 0.1, 1, 10, 10, "maximin")
size_n.two_way_nested.a_random_b_fixed_b(0.05, 0.1, 1, 10, 10, "minimin")
}
\keyword{ anova }
