detach("package:GeneralizedHyperbolic")
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
gTheta <- c(2, 1, 1)
hTheta <- c(1, 2, 1)
g0Result <- gigMom(0, Theta = gTheta)
g1Result <- gigMom(1, Theta = gTheta)
g2Result <- gigMom(2, Theta = gTheta)
detach("package:GeneralizedHyperbolic")
library(HyperbolicDist)
h0Result <- gigMom(0, Theta = hTheta)
h1Result <- gigMom(1, Theta = hTheta)
h2Result <- gigMom(2, Theta = hTheta)
detach("package:HyperbolicDist")

#gResult <- c(g0Result, g1Result, g2Result)
gResult <- c(g1Result, g2Result)
#hResult <- c(h0Result, h1Result, h2Result)
hResult <- c(h1Result, h2Result)

if (all(gResult == hResult)) {
  momResult <- "PASS: gigMom"
} else {
  momResult <- "FAIL: gigMom"
}

momResult
