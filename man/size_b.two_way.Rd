\name{size_b.two_way}
\alias{size_b.two_way_cross.mixed_model_a_fixed_a}
\alias{size_b.two_way_nested.b_random_a_fixed_a}
\title{
Design for Two-Way ANOVA
}
\description{
Returns the optimal number of obervations per level of factor B.
}
\usage{
size_b.two_way_cross.mixed_model_a_fixed_a(alpha, beta, delta, a, n, cases)
size_b.two_way_nested.b_random_a_fixed_a(alpha, beta, delta, a, cases)
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
size_b.two_way_cross.mixed_model_a_fixed_a(0.05,0.1, 1, 6, 1, "maximin")
size_b.two_way_cross.mixed_model_a_fixed_a(0.05,0.1, 1, 6, 1, "minimin")
size_b.two_way_cross.mixed_model_a_fixed_a(0.05,0.1, 1, 6, 2, "maximin")
size_b.two_way_cross.mixed_model_a_fixed_a(0.05,0.1, 1, 6, 2, "minimin")
size_b.two_way_nested.b_random_a_fixed_a(0.05, 0.1, 1, 6, "maximin")
size_b.two_way_nested.b_random_a_fixed_a(0.05, 0.1, 1, 6, "minimin")
}
\keyword{ anova }
