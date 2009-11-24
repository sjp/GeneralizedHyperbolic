### Change parameterizations of the Generalized Hyperbolic Distribution
ghypChangePars <- function(from, to, Theta, noNames = FALSE) {

  Theta <- as.numeric(Theta)

  # If lambda is omitted from the Theta vector it defaults to 1
  if (length(Theta) == 4)
    Theta <- c(Theta, 1)

  if (length(Theta) != 5)
    stop("Theta vector must contain 4 or 5 values")

  if ((from != 1) & (from != 2) & (from != 3) & (from != 4))
    stop("the argument 'from' must be either 1, 2, 3 or 4")

  if ((to != 1) & (to != 2) & (to != 3) & (to != 4))
    stop("the argument 'to' must be either 1, 2, 3 or 4")

  mu <- Theta[1]
  delta <- Theta[2]
  lambda <- Theta[5]

  if (delta <= 0)
    stop("delta must be greater than zero")

  if (from == 1) {
    alpha <- Theta[3]
    beta <- Theta[4]

    if (alpha <= 0)
      stop("alpha must be greater than zero")

    if (abs(beta) >= alpha)
      stop("absolute value of beta must be less than alpha")
  }

  if (from == 2) {
    zeta <- Theta[3]
    rho <- Theta[4]

    if (zeta <= 0)
      stop("zeta must be greater than zero")
  }

  if (from == 3) {
    xi <- Theta[3]
    chi <- Theta[4]

    if (xi <= 0 | xi > 1)
      stop("xi must be between zero and one")

    if (abs(chi) > xi)
      stop("absolute value of chi must be less than xi")
  }

  if (from == 4) {
    alphaBar <- Theta[3]
    betaBar <- Theta[4]

    if (alphaBar <= 0)
      stop("alpha bar must be greater than zero")
    if (abs(betaBar) >= alphaBar)
      stop("absolute value of beta bar must be less than alpha bar")
  }

  if (from == 1 & to == 2) {
    zeta <- delta * sqrt(alpha^2 - beta^2)
    rho <- beta / alpha
    output <- c(mu = mu, delta = delta, zeta = zeta,
                rho = rho, lambda = lambda)
  }

  if (from == 1 & to == 3) {
    xi <- 1 / sqrt(1 + delta * sqrt(alpha^2 - beta^2))
    chi <- beta / (alpha * sqrt(1 + delta * sqrt(alpha^2 - beta^2)))
    output <- c(mu = mu, delta = delta, xi = xi,
                chi = chi, lambda = lambda)
  }

  if (from == 1 & to == 4) {
    alphaBar <- delta * alpha
    betaBar <- delta * beta
    output <- c(mu = mu, delta = delta, alphaBar = alphaBar,
                betaBar = betaBar, lambda = lambda)
  }

  if (from == 2 & to == 1) {
    alpha <- zeta / (delta * sqrt(1 - rho^2))
    beta <- rho * alpha
    output <- c(mu = mu, delta = delta, alpha = alpha,
                beta = beta, lambda = lambda)
  }

  if (from == 2 & to == 3) {
    xi <- 1 / sqrt(1 + zeta)
    chi <- xi * rho
    output <- c(mu = mu, delta = delta, xi = xi,
                chi = chi, lambda = lambda)
  }

  if (from == 2 & to == 4) {
    alphaBar <- zeta / sqrt(1 - rho^2)
    betaBar <- rho * alphaBar
    output <- c(mu = mu, delta = delta, alphaBar = alphaBar,
                betaBar = betaBar, lambda = lambda)
  }

  if (from == 3 & to == 1) {
    alpha <- (1 - xi^2) / (delta * xi * sqrt(xi^2 - chi^2))
    beta <- alpha * chi / xi
    output <- c(mu = mu, delta = delta, alpha = alpha,
                beta = beta, lambda = lambda)
  }

  if (from == 3 & to == 2) {
    zeta <- (1 / xi^2) - 1
    rho <- chi / xi
    output <- c(mu = mu, delta = delta, zeta = zeta,
                rho = rho, lambda = lambda)
  }

  if (from == 3 & to == 4) {
    alphaBar <- (1 - xi^2) / (xi * sqrt(xi^2 - chi^2))
    betaBar <- alphaBar * chi / xi
    output <- c(mu = mu, delta = delta, alphaBar = alphaBar,
                betaBar = betaBar, lambda = lambda)
  }

  if (from == 4 & to == 1) {
    alpha <- alphaBar / delta
    beta <- betaBar / delta
    output <- c(mu = mu, delta = delta, alpha = alpha,
                beta = beta, lambda = lambda)
  }

  if (from == 4 & to == 2) {
    zeta <- sqrt(alphaBar^2 - betaBar^2)
    rho <- betaBar / alphaBar
    output <- c(mu = mu, delta = delta, zeta = zeta,
                rho = rho, lambda = lambda)
  }

  if (from == 4 & to == 3) {
    xi <- 1 / sqrt(1 + sqrt(alphaBar^2 - betaBar^2))
    chi <- betaBar / (alphaBar * sqrt(1 + sqrt(alphaBar^2 - betaBar^2)))
    output <- c(mu = mu, delta = delta, xi = xi,
                chi = chi, lambda = lambda)
  }

  if (from == to) {
    if (from == 1)
      output <- c(mu = mu, delta = delta, alpha = alpha,
                  beta = beta, lambda = lambda)

    if (from == 2)
      output <- c(mu = mu, delta = delta, zeta = zeta,
                  rho = rho, lambda = lambda)

    if (from == 3)
      output <- c(mu = mu, delta = delta, xi = xi,
                  chi = chi, lambda = lambda)

    if (from == 4)
      output <- c(mu = mu, delta = delta, alphaBar = alphaBar,
                  betaBar = betaBar, lambda = lambda)
  }

  if (noNames == TRUE)
      names(output) <- NULL

  output
} ## End of hyperbChangePars()
