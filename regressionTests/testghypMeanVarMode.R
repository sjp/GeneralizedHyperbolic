detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(0, 1, 3, 1, 0.5)
hTheta <- c(0.5, 3, 1, 1, 0)
gmResult <- ghypMean(Theta = gTheta)
gvResult <- ghypVar(Theta = gTheta)
gsResult <- ghypSkew(Theta = gTheta)
gkResult <- ghypKurt(Theta = gTheta)
gmResult <- ghypMode(Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hmResult <- ghypMean(Theta = hTheta)
hvResult <- ghypVar(Theta = hTheta)
hsResult <- ghypSkew(Theta = hTheta)
hkResult <- ghypKurt(Theta = hTheta)
hmResult <- ghypMode(Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gmResult == hmResult)) {
  mResult <- "PASS: ghypMean"
} else {
  mResult <- "FAIL: ghypMean"
}

if (all(gvResult == hvResult)) {
  vResult <- "PASS: ghypVar"
} else {
  vResult <- "FAIL: ghypVar"
}

if (all(gsResult == hsResult)) {
  sResult <- "PASS: ghypSkew"
} else {
  sResult <- "FAIL: ghypSkew"
}

if (all(gkResult == hkResult)) {
  kResult <- "PASS: ghypKurt"
} else {
  kResult <- "FAIL: ghypKurt"
}

if (all(gmResult == hmResult)) {
  moResult <- "PASS: ghypMode"
} else {
  moResult <- "FAIL: ghypMode"
}

mResult; vResult; sResult; kResult; moResult;
