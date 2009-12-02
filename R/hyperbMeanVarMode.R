### Function to calculate the theoretical mean of a 
### hyperbolic distribution given its parameters.
hyperbMean <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                       Theta = c(mu, delta, alpha, beta)) {

  Theta <- as.numeric(Theta)
  ghypMean(Theta = c(Theta, 1))
} ## End of hyperbMean() 

### Function to calculate the theoretical variance of a 
### hyperbolic distribution given its parameters.
hyperbVar <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                      Theta = c(mu, delta, alpha, beta)) {

  Theta <- as.numeric(Theta)
  ghypVar(Theta = c(Theta, 1))
} ## End of hyperbVar()

### Function to calculate the theoretical skewness of a 
### hyperbolic distribution given its parameters.
hyperbSkew <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                       Theta = c(mu, delta, alpha, beta)) {

  Theta <- as.numeric(Theta)
  ghypSkew(Theta = c(Theta, 1))
} ## End of hyperbSkew()

### Function to calculate the theoretical kurtosis of a 
### hyperbolic distribution given its parameters.
hyperbKurt <- function(mu = 0, delta = 1, alpha = 1, beta = 0,  
                       Theta = c(mu, delta, alpha, beta)) {

  Theta <- as.numeric(Theta)
  ghypKurt(Theta = c(Theta, 1))
} ## End of hyperbKurt()


### Function to calculate the theoretical mode point of a 
### hyperbolic distribution given its parameters.
hyperbMode <- function(mu = 0, delta = 1, alpha = 1, beta = 0,
                       Theta = c(mu, delta, alpha, beta)) {

  Theta <- as.numeric(Theta)
  ghypMode(Theta = c(Theta, 1))
} ## End of hyperbMode()
