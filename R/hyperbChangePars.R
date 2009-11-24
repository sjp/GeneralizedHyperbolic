### Change parameterizations of the Hyperbolic Distribution
hyperbChangePars <- function (from, to, Theta, noNames = FALSE) {

  Theta <- as.numeric(Theta)

  ghypResult <- ghypChangePars(from = from, to = to,
                               Theta = c(Theta, 1), noNames = noNames)

  # Removing lambda from the result of the change of parameterization.
  # This is possible because lambda is always the last parameter
  # regardless of the chosen form of parameterization.
  ghypResult[-5]
} ## End of hyperbChangePars()
