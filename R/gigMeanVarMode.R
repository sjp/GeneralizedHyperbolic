### Function to calculate the theoretical mean of a
### generalized inverse Gaussian distribution given its parameters.
gigMean <- function(chi = 1, psi = 1, lambda = 1,
                    Theta = c(chi, psi, lambda)) {

  ## check parameters
  parResult <- gigCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  Theta <- as.numeric(Theta)
  chi <- Theta[1]
  psi <- Theta[2]
  lambda <- Theta[3]
  omega <- sqrt(chi * psi)
  eta <- sqrt(chi / psi)
  eta * besselRatio(omega, lambda, 1)
} ## End of gigMean()

### Function to calculate the theoretical variance of a
### generalized inverse Gaussian distribution given its parameters.
gigVar <- function(chi = 1, psi = 1, lambda = 1,
                   Theta = c(chi, psi, lambda)) {

  ## check parameters
  parResult <- gigCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  m1 <- gigMean(Theta = Theta)
  var <- gigMom(2, Theta = Theta, about = m1)
  return(var)
} ## End of gigVar()

### Function to calculate the theoretical skewness of a
### generalized inverse Gaussian distribution given its parameters.
gigSkew <- function(chi = 1, psi = 1, lambda = 1,
                    Theta = c(chi, psi, lambda)) {

  ## check parameters
  parResult <- gigCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  m1 <- gigMean(Theta = Theta)
  skew <- gigMom(3, Theta = Theta, about = m1) / (gigVar(Theta = Theta)^(3 / 2))
  return(skew)
} ## End of gigSkew()

### Function to calculate the theoretical kurtosis of a
### generalized inverse Gaussian distribution given its parameters.
gigKurt <- function(chi = 1, psi = 1, lambda = 1,
                    Theta = c(chi, psi, lambda)) {

  ## check parameters
  parResult <- gigCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  m1 <- gigMean(Theta = Theta)
  kurt <- gigMom(4, Theta = Theta, about = m1) / (gigVar(Theta = Theta)^2) - 3
  return(kurt)
} ## End of gigKurt()


### Function to calculate the theoretical mode point of a
### generalized inverse Gaussian distribution given its parameters.
gigMode <- function(chi = 1, psi = 1, lambda = 1,
                    Theta = c(chi, psi, lambda)) {

  ## check parameters
  parResult <- gigCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  Theta <- as.numeric(Theta)
  chi <- Theta[1]
  psi <- Theta[2]
  lambda <- Theta[3]
  (lambda - 1 + sqrt((lambda - 1)^2 + chi * psi)) / psi
} ## End of gigMode()
