### Function to calculate an effective range for the distribution function
### or for the density function
### DJS 19/12/06
ghypCalcRange <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                          Theta = c(mu, delta, alpha, beta, lambda),
                          tol = 10^(-5), density = TRUE, ...) {

  Theta <- as.numeric(Theta)

  if (length(Theta) == 4)
    Theta <- c(Theta, 1)

  mu <- Theta[1]
  delta <- Theta[2]
  alpha <- Theta[3]
  beta <- Theta[4]
  lambda <- Theta[5]

  if (density == FALSE){
    ## bounds are for distribution function
    stop("Distribution function bounds not yet implemented")
  }else{
    ## bounds are for the density function
    mode <- ghypMode(Theta = Theta)
    xHigh <- mode + sqrt(ghypVar(Theta = Theta))
    while (dghyp(xHigh, Theta = Theta) > tol){
      xHigh <- xHigh + sqrt(ghypVar(Theta = Theta))
    }
    zeroFun<-function(x){ 
       dghyp(x, Theta = Theta) - tol 
     } 
    xUpper <- uniroot(zeroFun, interval = c(mode, xHigh), ...)$root
    xLow <- mode - sqrt(ghypVar(Theta = Theta))
    while (dghyp(xLow, Theta = Theta) > tol){
      xLow <- xLow - sqrt(ghypVar(Theta = Theta))
    }
    zeroFun<-function(x){ 
       dghyp(x, Theta = Theta) - tol 
     } 
    xLower <- uniroot(zeroFun, interval = c(xLow, mode), ...)$root
    range <- c(xLower, xUpper)
  }
  return(range)
} ## End of hyperbCalcRange()
