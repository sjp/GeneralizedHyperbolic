detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(0, 1, 3, 1, 0.5)
hTheta <- c(0.5, 3, 1, 1, 0)
sampleInput <- rghyp(100, Theta = gTheta)
gdResult <- dghyp(sampleInput, Theta = gTheta)
gpResult <- pghyp(seq(-2, 5, 0.2), Theta = gTheta)
gqResult <- qghyp(seq(0.1, 0.9, 0.1), Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hdResult <- dghyp(sampleInput, Theta = hTheta)
hpResult <- pghyp(seq(-2, 5, 0.2), Theta = hTheta)
hqResult <- qghyp(seq(0.1, 0.9, 0.1), Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gdResult == hdResult)) {
  dResult <- "PASS: dghyp"
} else {
  dResult <- "FAIL: dghyp"
}

if (all(gpResult == hpResult)) {
  pResult <- "PASS: pghyp"
} else {
  pResult <- "FAIL: pghyp"
}

if (all(gqResult == hqResult)) {
  qResult <- "PASS: qghyp"
} else {
  qResult <- "FAIL: qghyp"
}

dResult; pResult; qResult;
