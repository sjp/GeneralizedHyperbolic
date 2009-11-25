### Function to calculate an effective range for the distribution function
### or for the density function
### DJS 10/01/07
gigCalcRange <- function(chi = 1, psi = 1, lambda = 1,
                         Theta = c(chi, psi, lambda),
                         tol = 10^(-5), density = TRUE, ...) {

  Theta <- as.numeric(Theta)

  ## check parameters
  parResult <- gigCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  chi <- Theta[1]
  psi <- Theta[2]
  lambda <- Theta[3]
  KLambda <- besselK(x = sqrt(chi * psi), nu = lambda)

  if (!density) {
    ## bounds are for distribution function
    stop("Distribution function bounds not yet implemented")
  } else {
    ## bounds are for the density function
    mode <- gigMode(Theta = Theta)
    xHigh <- mode + sqrt(gigVar(Theta = Theta))

    while (dgig(xHigh, Theta = Theta) > tol) {
      xHigh <- xHigh + sqrt(gigVar(Theta = Theta))
    }

    zeroFun <- function(x) {
       dgig(x, Theta = Theta) - tol
     }

    xUpper <- uniroot(zeroFun, interval = c(mode, xHigh), ...)$root
    xLow <- 0
    xLower <- uniroot(zeroFun, interval = c(xLow, mode), ...)$root
    range <- c(xLower, xUpper)
  }

  return(range)
} ## End of gigCalcRange()
