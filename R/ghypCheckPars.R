### Check for boundary and error cases
### Return case normal, or error
### In case of error, include appropriate message
ghypCheckPars <- function(param) {

  param <- as.numeric(param)
  mu <- param[1]
  delta <- param[2]
  alpha <- param[3]
  beta <- param[4]
  lambda <- param[5]

  case <- ""
  errMessage <- ""

  if (length(param) != 5) {
    case <- "error"
    errMessage <- "param vector must contain 5 values"
  } else {
    if (alpha < 0) {
      case <- "error"
      errMessage <- "alpha must not be less than zero"
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

        if (abs(beta) != alpha & delta <= 0) {
          case <- "error"
          errMessage <- "absolute value of beta must be equal to alpha and delta must be greater than zero when lambda < 0"
        }
      }
    }
  }

  result <- list(case = case, errMessage = errMessage)
  return(result)
}
