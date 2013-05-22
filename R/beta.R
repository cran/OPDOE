# starting program beta

beta <- function (alpha, dfn, dfd, ncp)
{
  qF <- qf(1-alpha, dfn, dfd)
  beta.calculated <- pf(qF, dfn, dfd, ncp)
  return (beta.calculated)
}

