detach("package:HyperbolicDist")
detach("package:GeneralizedHyperbolic")
library(GeneralizedHyperbolic)
hypDat <- rhyperb(1000, param = c(2, 1, 1, 0))
gResult <- hyperbFit(hypDat, hessian = TRUE)

cat("GeneralizedHyperbolic result:\n")
summary(gResult)

