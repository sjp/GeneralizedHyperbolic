detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(2, 1, 1)
hTheta <- c(1, 2, 1)
sampleInput <- rgig(100, Theta = gTheta)
gdResult <- dgig(sampleInput, Theta = gTheta)
gpResult <- pgig(seq(-2, 5, 0.2), Theta = gTheta)
gqResult <- qgig(seq(0.1, 0.9, 0.1), Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hdResult <- dgig(sampleInput, Theta = hTheta)
hpResult <- pgig(seq(-2, 5, 0.2), Theta = hTheta)
hqResult <- qgig(seq(0.1, 0.9, 0.1), Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gdResult == hdResult)) {
  dResult <- "PASS: dgig"
} else {
  dResult <- "FAIL: dgig"
}

if (all(gpResult == hpResult)) {
  pResult <- "PASS: pgig"
} else {
  pResult <- "FAIL: pgig"
}

if (all(gqResult == hqResult)) {
  qResult <- "PASS: qgig"
} else {
  qResult <- "FAIL: qgig"
}

dResult; pResult; qResult;
