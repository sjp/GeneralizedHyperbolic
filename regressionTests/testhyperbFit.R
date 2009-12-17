detach("package:HyperbolicDist")
detach("package:GeneralizedHyperbolic")

library(HyperbolicDist)
# pi, zeta, delta, mu
hypDat <- rhyperb(1000, c(0, 1, 1, 0))
hResultList <- hyperbFit(hypDat)

# printing Theta to be sure it works
print(hResultList$Theta)

hResult <- hResultList$Theta
hResultStart <- hResultList$ThetaStart

hResult <- c(hResult[1], exp(hResult[2]), exp(hResult[3]), hResult[4])
hResultStart <- c(hResultStart[1], exp(hResultStart[2]),
                  exp(hResultStart[3]), hResultStart[4])

hResult <- hyperbChangePars(1, 2, hResult)
hResultStart <- hyperbChangePars(1, 2, hResultStart)

hResult <- c(hResult[4], hResult[3], hResult[1], hResult[2])
hResultStart <- c(hResultStart[4], hResultStart[3],
                  hResultStart[1], hResultStart[2])

names(hResult) <- c("mu", "delta", "alpha", "beta")
names(hResultStart) <- c("mu", "delta", "alpha", "beta")

hResultList$Theta <- hResult
hResultList$ThetaStart <- hResultStart

hResult <- hResultList
detach("package:HyperbolicDist")
library(GeneralizedHyperbolic)
# should be close to c(0, 1, 0, 1)
gResult <- hyperbFit(hypDat)


cat("Test results:\n\n", "HyperbolicDist result:\n")
print(hResult)
cat("GeneralizedHyperbolic result:\n")
print(gResult)

