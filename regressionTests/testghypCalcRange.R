detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(0, 1, 3, 1, 0.5)
hTheta <- c(0.5, 3, 1, 1, 0)
gResult <- ghypCalcRange(Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hResult <- ghypCalcRange(Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gResult == hResult)) {
  print("PASS: ghypCalcRange")
} else {
  print("FAIL: ghypCalcRange")
}
