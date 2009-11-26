### Change parameterizations of the Hyperbolic Distribution
hyperbChangePars <- function (from, to, Theta, noNames = FALSE) {

  if (length(Theta) != 4)
    stop("parameter vector must contain 4 values")

  if ((from != 1) & (from != 2) & (from != 3) & (from != 4))
    stop("the argument 'from' must be either 1, 2, 3 or 4")

  if ((to != 1) & (to != 2) & (to != 3) & (to != 4))
    stop("the argument 'to' must be either 1, 2, 3 or 4")

  mu <- Theta[1]
  delta <- Theta[2]

  if (delta <= 0) 
    stop("delta must be greater than zero")

  if (from == 1) {
    hyperbPi <- Theta[3]
    zeta <- Theta[4]

    if (zeta <= 0) 
      stop("zeta must be greater than zero")
  }

  if (from == 2) {
    alpha <- Theta[3]
    beta <- Theta[4]

    if (alpha <= 0) 
      stop("alpha must be greater than zero")

    if (abs(beta) >= alpha) 
      stop("absolute value of beta must be less than alpha")
  }

  if (from == 3) {
    phi <- Theta[3]
    gamma <- Theta[4]

    if (phi <= 0) 
      stop("phi must be greater than zero")

    if (gamma <= 0) 
      stop("gamma must be greater than zero")
  }

  if (from == 4) {
    xi <- Theta[3]
    chi <- Theta[4]

    if ((xi <= 0) | (xi > 1)) 
      stop("xi must be between zero and one")

    if (abs(chi) > xi) 
      stop("absolute value of chi must be less than xi")
  }

  if (from == 1 & to == 2) {
    alpha <- zeta * sqrt(1 + hyperbPi^2) / delta
    beta <- zeta * hyperbPi / delta
    output <- c(mu = mu, delta = delta, alpha = alpha, beta = beta)
  }

  if (from == 1 & to == 3) {
    phi <- zeta / delta * (sqrt(1 + hyperbPi^2) + hyperbPi)
    gamma <- zeta / delta * (sqrt(1 + hyperbPi^2) - hyperbPi)
    output <- c(mu = mu, delta = delta, phi = phi, gamma = gamma)
  }

  if (from == 1 & to == 4) {
    xi <- 1 / sqrt(1 + zeta)
    chi <- hyperbPi / sqrt((1 + zeta) * (1 + hyperbPi^2))
    output <- c(mu = mu, delta = delta, xi = xi, chi = chi)
  }

  if (from == 2 & to == 1) {
    hyperbPi <- beta / sqrt(alpha^2 - beta^2)
    zeta <- delta * sqrt(alpha^2 - beta^2)
    output <- c(mu = mu, delta = delta, hyperbPi = hyperbPi, zeta = zeta)
  }

  if (from == 2 & to == 3) {
    phi <- alpha + beta
    gamma <- alpha - beta
    output <- c(mu = mu, delta = delta, phi = phi, gamma = gamma)
  }

  if (from == 2 & to == 4) {
    xi <- 1 / sqrt(1 + delta * sqrt(alpha^2 - beta^2))
    chi <- beta / (alpha * sqrt(1 + delta * sqrt(alpha^2 - beta^2)))
    output <- c(mu = mu, delta = delta, xi = xi, chi = chi)
  }

  if (from == 3 & to == 1) {
    hyperbPi <- (phi - gamma) / (2 * sqrt(phi * gamma))
    zeta <- delta * sqrt(phi * gamma)
    output <- c(mu = mu, delta = delta, hyperbPi = hyperbPi, zeta = zeta)
  }

  if (from == 3 & to == 2) {
    alpha <- (phi + gamma) / 2
    beta <- (phi - gamma) / 2
    output <- c(mu = mu, delta = delta, alpha = alpha, beta = beta) 
  }

  if (from == 3 & to == 4) {
    xi <- 1 / sqrt(1 + delta * sqrt(phi * gamma))
    chi <- ((phi - gamma) / (phi + gamma)) / sqrt(1 + delta * sqrt(phi * gamma))
    output <- c(mu = mu, delta = delta, xi = xi, chi = chi)
  }

  if (from == 4 & to == 1) {
    hyperbPi <- chi / sqrt(xi^2 - chi^2)
    zeta <- (1 - xi^2) / xi^2
    output <- c(mu = mu, delta = delta, hyperbPi = hyperbPi, zeta = zeta)
  }

  if (from == 4 & to == 2) {
    alpha <- (1 - xi^2) / (delta * xi * sqrt(xi^2 - chi^2))
    beta <- chi * (1 - xi^2) / (delta * xi^2 * sqrt(xi^2 - chi^2))
    output <- c(mu = mu, delta = delta, alpha = alpha, beta = beta)
  }

  if (from == 4 & to == 3) {
    phi <- (1 - xi^2) * (xi + chi) / (delta * xi^2 * sqrt(xi^2 - chi^2))
    gamma <- (1 - xi^2) * (xi - chi) / (delta * xi^2 * sqrt(xi^2 - chi^2))
    output <- c(mu = mu, delta = delta, phi = phi, gamma = gamma)
  }

  if (from == to) {
    if (from == 1) 
      output <- c(mu = mu, delta = delta, hyperbPi = hyperbPi, zeta = zeta) 

    if (from == 2) 
      output <- c(mu = mu, delta = delta, alpha = alpha, beta = beta) 

      if (from == 3) 
        output <- c(mu = mu, delta = delta, phi = phi, gamma = gamma)

      if (from == 4) 
        output <- c(mu = mu, delta = delta, xi = xi, chi = chi)
  }

  if (noNames == TRUE) 
    names(output) <- NULL

  output
} ## End of hyperbChangePars()
