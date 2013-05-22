\name{cattle}
\alias{cattle}
\docType{data}
\title{
Cattle data
}
\description{
milk fat performance (in kg per lactation) of heifers of three
sires from Holstein Frisian cattle to select the sire with the highest
breeding value for milk fat performance.
}
\usage{data(cattle)}
\format{
  The format is:
 num [1:5, 1:3] 132 128 135 121 138 173 166 172 176 169 ...
}
\references{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt: Optimal Experimental Design with R, Chapman and Hall/CRC, 2011
}
\author{
Dieter Rasch, Juergen Pilz, L.R. Verdooren, Albrecht Gebhardt
}
\examples{
data(cattle)
size.seq_select.mean(data=cattle,delta=10, P=0.95)
}
\keyword{datasets}
