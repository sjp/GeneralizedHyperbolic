### Check for boundary and error cases
### Return case normal, or error
### In case of error, include appropriate message
ghypCheckPars <- function(Theta, ...) {

  Theta <- as.numeric(Theta)
  mu <- Theta[1]
  delta <- Theta[2]
  alpha <- Theta[3]
  beta <- Theta[4]
  lambda <- Theta[5]

  case <- "normal"
  errMessage <- ""

  if (length(Theta) != 5) {
    case <- "error"
    errMessage <- "Theta vector must contain 5 values"
  } else {
    if (lambda == 0) {

      if (abs(beta) >= alpha) {
        case <- "error"
        errMessage <- "absolute value of beta must be less than alpha when lambda = 0"
      }

      if (delta <= 0) {
          case <- "error"
          errMessage <- "delta must be greater than zero when lambda = 0"
      }

      if (abs(beta) >= alpha & delta <= 0) {
          case <- "error"
          errMessage <- "absolute value of beta must be less than alpha and delta must be greater than zero when lambda = 0"
      }
    }

    if (lambda > 0) {

      if (abs(beta) >= alpha) {
        case <- "error"
        errMessage <- "absolute value of beta must be less than alpha when lambda > 0"
      }

      if (delta < 0) {
        case <- "error"
        errMessage <- "delta must be less than zero when lambda > 0"
      }

      if (abs(beta) >= alpha & delta < 0) {
        case <- "error"
        errMessage <- "absolute value of beta must be less than alpha and delta must be less than zero when lambda > 0"
      }
    }

    if (lambda < 0) {

      if (abs(beta) != alpha) {
        case <- "error"
        errMessage <- "absolute value of beta must be equal to alpha when lambda < 0"
      }

      if (delta <= 0) {
        case <- "error"
        errMessage <- "delta must be greater than zero when lambda < 0"
      }

      if (abs(beta) ! alpha & delta <= 0) {
        case <- "error"
        errMessage <- "absolute value of beta must be equal to alpha and delta must be greater than zero when lambda < 0"
      }
    }
  }

  result <- list(case = case, errMessage = errMessage)
  return(result)
}
