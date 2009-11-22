### Function to calculate the theoretical mean of a 
### generalized hyperbolic distribution given its parameters.
ghypMean <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     Theta = c(mu, delta, alpha, beta, lambda)){

  Theta <- as.numeric(Theta)

  if (length(Theta) == 4)
    Theta <- c(Theta, 1)

  ## check parameters
  parResult <- ghypCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  mu <- Theta[1]
  delta <- Theta[2]
  alpha <- Theta[3]
  beta <- Theta[4]
  lambda <- Theta[5]

  gamma <- sqrt(alpha^2 - beta^2)
  
  mu + delta*beta*besselRatio(delta*gamma, lambda, 1)/gamma
} ## End of ghypMean() 

### Function to calculate the theoretical variance of a 
### generalized hyperbolic distribution given its parameters.
ghypVar <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                    Theta = c(mu, delta, alpha, beta, lambda)){

  Theta <- as.numeric(Theta)

  if (length(Theta) == 4)
    Theta <- c(Theta, 1)

  ## check parameters
  parResult <- ghypCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  var <- ghypMom(2, Theta = Theta, momType = "central")
  return(var)
} ## End of ghypVar()

### Function to calculate the theoretical skewness of a 
### generalized hyperbolic distribution given its parameters.
ghypSkew <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     Theta = c(mu, delta, alpha, beta, lambda)){

  Theta <- as.numeric(Theta)

  if (length(Theta) == 4)
    Theta <- c(Theta, 1)

  ## check parameters
  parResult <- ghypCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  skew <- ghypMom(3, Theta = Theta, momType = "central")/(ghypVar(Theta = Theta)^(3/2))
  return(skew)
} ## End of ghypSkew()

### Function to calculate the theoretical kurtosis of a 
### generalized hyperbolic distribution given its parameters.
ghypKurt <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     Theta = c(mu, delta, alpha, beta, lambda)){

  Theta <- as.numeric(Theta)

  if (length(Theta) == 4)
    Theta <- c(Theta, 1)

  ## check parameters
  parResult <- ghypCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  kurt <- ghypMom(4, Theta = Theta, momType = "central")/(ghypVar(Theta = Theta)^2) - 3
  return(kurt)
} ## End of ghypKurt()

### Function to calculate the theoretical mode point of a 
### generalized hyperbolic distribution given its parameters.
ghypMode <- function(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
                     Theta = c(mu, delta, alpha, beta, lambda)){

  Theta <- as.numeric(Theta)

  if (length(Theta) == 4)
    Theta <- c(1, Theta)

  ## check parameters
  parResult <- ghypCheckPars(Theta)
  case <- parResult$case
  errMessage <- parResult$errMessage

  if (case == "error")
    stop(errMessage)

  modeFun <- function(x){
    log(dghyp(x, Theta = Theta))
  }
  start <- ghypMean(Theta = Theta)
  optResult <- optim(start, modeFun,
                     control = list(fnscale = -1, maxit = 1000),
                     method = "BFGS")
  if (optResult$convergence == 0){
    mode <- optResult$par
  }else{
    mode <- NA
  }
  mode
} ## End of ghypMode()

