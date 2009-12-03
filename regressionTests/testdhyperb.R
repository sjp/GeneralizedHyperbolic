detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(1, 2, 1.5, 1)
hTheta <- hyperbChangePars(2, 1, gTheta, noNames = TRUE)
hTheta <- c(hTheta[3], hTheta[4], hTheta[2], hTheta[1])
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

if (all(abs(gdResult - hdResult) < 0.00001)) {
  dResult <- "PASS: dhyperb"
} else {
  dResult <- paste("FAIL: dhyperb", "gdResult:", gdResult, "hdResult:", hdResult)
}

if (all(abs(gpResult - hpResult) < 0.00001)) {
  pResult <- "PASS: phyperb"
} else {
  pResult <- paste("FAIL: phyperb", "gpResult:", gpResult, "pdResult:", hpResult)
}

if (all(abs(gqResult - hqResult) < 0.00001)) {
  qResult <- "PASS: qhyperb"
} else {
  qResult <- paste("FAIL: qhyperb", "gqResult:", gqResult, "hqResult:", hqResult)
}

dResult; pResult; qResult;
