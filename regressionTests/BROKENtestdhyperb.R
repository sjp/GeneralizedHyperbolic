detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c()
hTheta <- c()
sampleInput <- rhyperb(100, Theta = gTheta)
gdResult <- dhyperb(sampleInput, Theta = gTheta)
gpResult <- phyperb(seq(-2, 5, 0.2), Theta = gTheta)
gqResult <- qhyperb(seq(0.1, 0.9, 0.1), Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hdResult <- dhyperb(sampleInput, Theta = hTheta)
hpResult <- phyperb(seq(-2, 5, 0.2), Theta = hTheta)
hqResult <- qhyperb(seq(0.1, 0.9, 0.1), Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gdResult == hdResult)) {
  dResult <- "PASS: dhyperb"
} else {
  dResult <- "FAIL: dhyperb"
}

if (all(gpResult == hpResult)) {
  pResult <- "PASS: phyperb"
} else {
  pResult <- "FAIL: phyperb"
}

if (all(gqResult == hqResult)) {
  qResult <- "PASS: qhyperb"
} else {
  qResult <- "FAIL: qhyperb"
}

dResult; pResult; qResult;
