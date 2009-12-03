detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(1, 2, 1.5, 1)
hTheta <- hyperbChangePars(2, 1, gTheta, noNames = TRUE)
hTheta <- c(hTheta[3], hTheta[4], hTheta[2], hTheta[1])
gmResult <- hyperbMean(Theta = gTheta)
gvResult <- hyperbVar(Theta = gTheta)
gsResult <- hyperbMode(Theta = gTheta)
gkResult <- hyperbKurt(Theta = gTheta)
gmoResult <- hyperbMode(Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hmResult <- hyperbMean(Theta = hTheta)
hvResult <- hyperbVar(Theta = hTheta)
hsResult <- hyperbSkew(Theta = hTheta)
hkResult <- hyperbKurt(Theta = hTheta)
hmoResult <- hyperbMode(Theta = hTheta)
detach("package:HyperbolicDist")

if ((gmResult - hmResult) < 0.00001) {
  mResult <- "PASS: hyperbMean"
} else {
  mResult <- "FAIL: hyperbMean"
}

if ((gvResult - hvResult) < 0.00001) {
  vResult <- "PASS: hyperbVar"
} else {
  vResult <- "FAIL: hyperbVar"
}

if ((gsResult - hsResult) < 0.00001) {
  sResult <- "PASS: hyperbSkew"
} else {
  sResult <- "FAIL: hyperbSkew"
}

if ((gkResult - hkResult) < 0.00001) {
  kResult <- "PASS: hyperbKurt"
} else {
  kResult <- "FAIL: hyperbKurt"
}

if ((gmoResult - hmoResult) < 0.00001) {
  moResult <- "PASS: hyperbMode"
} else {
  moResult <- "FAIL: hyperbMode"
}

mResult; vResult; sResult; kResult; moResult;
