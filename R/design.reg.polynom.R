design.reg.polynom <- function(a, b, k, n){
  require(orthopolynom)
  p <- n%/%(k+1)
  rem <- n%%(k+1)
  d <- matrix(0,nrow=2,ncol=k+1)
  d[1,] <- (a+b)/2+(b-a)/2*
    solve(deriv(legendre.polynomials(k)[[k+1]])*polynomial(c(1,0,-1)))
  d[2,] <- rep(p,k+1)+c(rep(1,rem),rep(0,k+1-rem))
  d
}

