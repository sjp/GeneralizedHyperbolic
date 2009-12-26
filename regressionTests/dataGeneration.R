library(GeneralizedHyperbolic)
#smallShape

for (delta in c(0, 0.5, 1)) {
  for (alpha in c(0, 0.5, 1)) {
    for (beta in c(0, 0.5, 1)) {
      for (lambda in c(-1, -0.5, 0, 0.5, 1, 1.5)) {
        ghypTestOutput <- ghypCheckPars(c(0, delta, alpha, beta, lambda))
        chi <- delta^2
        psi <- alpha^2 - beta^2
        gigTestOutput <- gigCheckPars(c(chi, psi, lambda))
		newalpha <- sqrt(psi / chi)
        newbeta <- sqrt(psi * chi)
        lambdaCheck <- (lambda - 1 + sqrt((lambda - 1)^2 + newbeta^2))/newbeta
        bool <- is.na(newalpha) | is.na(newbeta) | is.infinite(alpha) | is.infinite(newbeta) | is.na(lambdaCheck) | is.infinite(lambdaCheck)
        if (ghypTestOutput$case != "error" & gigTestOutput$case != "error" & ! bool)
          cat("   ", paste(0, delta, alpha, beta, paste(lambda, ",", sep = ""), sep = ", "), "\n", file = "/home/simon/Desktop/smallShape.txt", append = TRUE)
      }
    }
  }
}
 
 
#largeShape

for (delta in c(0, 0.5, 1, 1.5, 2)) {
  for (alpha in c(0, 0.5, 1, 1.5, 2)) {
    for (beta in c(0, 0.5, 1, 1.5, 2)) {
      for (lambda in c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) {
        ghypTestOutput <- ghypCheckPars(c(0, delta, alpha, beta, lambda))
        chi <- delta^2
        psi <- alpha^2 - beta^2
        gigTestOutput <- gigCheckPars(c(chi, psi, lambda))
		newalpha <- sqrt(psi / chi)
        newbeta <- sqrt(psi * chi)
        lambdaCheck <- (lambda - 1 + sqrt((lambda - 1)^2 + newbeta^2))/newbeta
        bool <- is.na(newalpha) | is.na(newbeta) | is.infinite(alpha) | is.infinite(newbeta) | is.na(lambdaCheck) | is.infinite(lambdaCheck)
        if (ghypTestOutput$case != "error" & gigTestOutput$case != "error" & ! bool)
          cat("   ", paste(0, delta, alpha, beta, paste(lambda, ",", sep = ""), sep = ", "), "\n", file = "/home/simon/Desktop/largeShape.txt", append = TRUE)
      }
    }
  }
}
 
 
#smallParam
 
for (mu in c(-2, 0, 2)) {
  for (delta in c(0, 0.5, 1)) {
    for (alpha in c(0, 0.5, 1)) {
      for (beta in c(0, 0.5, 1)) {
        for (lambda in c(-1, -0.5, 0, 0.5, 1, 1.5)) {
          ghypTestOutput <- ghypCheckPars(c(mu, delta, alpha, beta, lambda))
          chi <- delta^2
          psi <- alpha^2 - beta^2
          gigTestOutput <- gigCheckPars(c(chi, psi, lambda))
		  newalpha <- sqrt(psi / chi)
          newbeta <- sqrt(psi * chi)
          lambdaCheck <- (lambda - 1 + sqrt((lambda - 1)^2 + newbeta^2))/newbeta
          bool <- is.na(newalpha) | is.na(newbeta) | is.infinite(alpha) | is.infinite(newbeta) | is.na(lambdaCheck) | is.infinite(lambdaCheck)
          if (ghypTestOutput$case != "error" & gigTestOutput$case != "error" & ! bool)
            cat("   ", paste(mu, delta, alpha, beta, paste(lambda, ",", sep = ""), sep = ", "), "\n", file = "/home/simon/Desktop/smallParam.txt", append = TRUE)
        }
      }
    }
  }
}
 
 
#largeParam
 
for (mu in c(-2, 0, 2)) {
  for (delta in c(0, 0.5, 1, 1.5, 2)) {
    for (alpha in c(0, 0.5, 1, 1.5, 2)) {
      for (beta in c(0, 0.5, 1, 1.5, 2)) {
        for (lambda in c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) {
          ghypTestOutput <- ghypCheckPars(c(mu, delta, alpha, beta, lambda))
          chi <- delta^2
          psi <- alpha^2 - beta^2
          gigTestOutput <- gigCheckPars(c(chi, psi, lambda))
		  newalpha <- sqrt(psi / chi)
          newbeta <- sqrt(psi * chi)
          lambdaCheck <- (lambda - 1 + sqrt((lambda - 1)^2 + newbeta^2))/newbeta
          bool <- is.na(newalpha) | is.na(newbeta) | is.infinite(alpha) | is.infinite(newbeta) | is.na(lambdaCheck) | is.infinite(lambdaCheck)
          if (ghypTestOutput$case != "error" & gigTestOutput$case != "error" & ! bool)
            cat("   ", paste(mu, delta, alpha, beta, paste(lambda, ",", sep = ""), sep = ", "), "\n", file = "/home/simon/Desktop/largeParam.txt", append = TRUE)
        }
      }
    }
  }
}
