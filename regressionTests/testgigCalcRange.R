detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(2, 1, 1)
hTheta <- c(1, 2, 1)
gResult <- gigCalcRange(Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
hResult <- gigCalcRange(Theta = hTheta)
detach("package:HyperbolicDist")

if (all(gResult == hResult)) {
  print("PASS: ghypCalcRange")
} else {
  print("FAIL: ghypCalcRange")
}
