### Function to calculate an effective range for the distribution function
### or for the density function of the Hyperbolic Distribution
### DJS 8/09/06
hyperbCalcRange <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                            Theta = c(mu, delta, alpha, beta),
                            tol = 10^(-5), density = FALSE) {

  Theta <- as.numeric(Theta)

  ghypCalcRange(Theta = c(Theta, 1), tol = tol, density = density)
} ## End of hyperbCalcRange()
