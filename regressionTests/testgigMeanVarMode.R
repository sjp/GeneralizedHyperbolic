detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(2, 1, 1)
hTheta <- c(1, 2, 1)
gmResult <- gigMean(Theta = gTheta)
gvResult <- gigVar(Theta = gTheta)
gsResult <- gigSkew(Theta = gTheta)
gkResult <- gigKurt(Theta = gTheta)
gmResult <- gigMode(Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hmResult <- gigMean(Theta = hTheta)
hvResult <- gigVar(Theta = hTheta)
hsResult <- gigSkew(Theta = hTheta)
hkResult <- gigKurt(Theta = hTheta)
hmResult <- gigMode(Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gmResult == hmResult)) {
  mResult <- "PASS: gigMean"
} else {
  mResult <- "FAIL: gigMean"
}

if (all(gvResult == hvResult)) {
  vResult <- "PASS: gigVar"
} else {
  vResult <- "FAIL: gigVar"
}

if (all(gsResult == hsResult)) {
  sResult <- "PASS: gigSkew"
} else {
  sResult <- "FAIL: gigSkew"
}

if (all(gkResult == hkResult)) {
  kResult <- "PASS: gigKurt"
} else {
  kResult <- "FAIL: gigKurt"
}

if (all(gmResult == hmResult)) {
  moResult <- "PASS: gigMode"
} else {
  moResult <- "FAIL: gigMode"
}

mResult; vResult; sResult; kResult; moResult;
