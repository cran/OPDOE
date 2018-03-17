\name{size_n.three_way}

\alias{size_n.three_way_cross.model_1_a          }
\alias{size_n.three_way_cross.model_1_axb        }
\alias{size_n.three_way_cross.model_1_axbxc      }
\alias{size_n.three_way_mixed_ab_in_c.model_1_a  }
\alias{size_n.three_way_mixed_ab_in_c.model_1_axb}
\alias{size_n.three_way_mixed_ab_in_c.model_1_b  }
\alias{size_n.three_way_mixed_ab_in_c.model_1_c  }
\alias{size_n.three_way_mixed_ab_in_c.model_3_c  }
\alias{size_n.three_way_mixed_ab_in_c.model_4_c  }
\alias{size_n.three_way_mixed_cxbina.model_1_a   }
\alias{size_n.three_way_mixed_cxbina.model_1_axc }
\alias{size_n.three_way_mixed_cxbina.model_1_b   }
\alias{size_n.three_way_mixed_cxbina.model_1_bxc }
\alias{size_n.three_way_mixed_cxbina.model_1_c   }
\alias{size_n.three_way_mixed_cxbina.model_3_b   }
\alias{size_n.three_way_mixed_cxbina.model_3_bxc }
\alias{size_n.three_way_nested.model_1_a         }
\alias{size_n.three_way_nested.model_1_b         }
\alias{size_n.three_way_nested.model_1_c         }
\alias{size_n.three_way_nested.model_3_b         }
\alias{size_n.three_way_nested.model_3_c         }
\alias{size_n.three_way_nested.model_4_a         }
\alias{size_n.three_way_nested.model_8_c         }

\title{
Design for Three-Way ANOVA
}
\description{
Returns the optimal number of obervations per level of each factor.
}
\usage{
size_n.three_way_cross.model_1_a          (alpha, beta, delta, a, b, c, cases)
size_n.three_way_cross.model_1_axb        (alpha, beta, delta, a, b, c, cases)
size_n.three_way_cross.model_1_axbxc      (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_ab_in_c.model_1_a  (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_ab_in_c.model_1_b  (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_ab_in_c.model_1_c  (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_ab_in_c.model_3_c  (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_ab_in_c.model_4_c  (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_1_a   (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_1_axc (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_1_b   (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_1_bxc (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_1_c   (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_3_b   (alpha, beta, delta, a, b, c, cases)
size_n.three_way_mixed_cxbina.model_3_bxc (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_1_a         (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_1_b         (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_1_c         (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_3_b         (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_3_c         (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_4_a         (alpha, beta, delta, a, b, c, cases)
size_n.three_way_nested.model_8_c         (alpha, beta, delta, a, b, c, cases)
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
  \item{c}{
Number of levels of fixed factor C
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
size_n.three_way_cross.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_cross.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_cross.model_1_axb(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_cross.model_1_axb(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_cross.model_1_axbxc(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_cross.model_1_axbxc(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_ab_in_c.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_ab_in_c.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_ab_in_c.model_1_axb(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_ab_in_c.model_1_axb(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_ab_in_c.model_1_b(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_ab_in_c.model_1_b(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_ab_in_c.model_1_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_ab_in_c.model_1_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_ab_in_c.model_3_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_ab_in_c.model_3_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_ab_in_c.model_4_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_ab_in_c.model_4_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_1_axc(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_1_axc(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_1_b(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_1_b(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_1_bxc(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_1_bxc(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_1_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_1_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_3_b(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_3_b(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_mixed_cxbina.model_3_bxc (0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_mixed_cxbina.model_3_bxc (0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_nested.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_nested.model_1_a(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_nested.model_1_b(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_nested.model_1_b(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_nested.model_1_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_nested.model_1_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_nested.model_3_b(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_nested.model_3_b(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_nested.model_3_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_nested.model_3_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")
size_n.three_way_nested.model_4_c(0.05, 0.1, 0.5, 6, NA, 4, "maximin")
size_n.three_way_nested.model_4_c(0.05, 0.1, 0.5, 6, NA, 4, "minimin")
size_n.three_way_nested.model_8_c(0.05, 0.1, 0.5, 6, 5, 4, "maximin")
size_n.three_way_nested.model_8_c(0.05, 0.1, 0.5, 6, 5, 4, "minimin")

}

\keyword{ anova }
