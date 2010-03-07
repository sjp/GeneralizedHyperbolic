# GeneralizedHyperbolic

This package for [R](http://www.r-project.org/) allows the easy use of functions for the generalized hyperbolic distribution and its related distributions.

__NOTE__: GeneralizedHyperbolic is now available for download on CRAN. CRAN is the preferred method of installing the GeneralizedHyperbolic package.

The main differences between this package and [HyperbolicDist](http://cran.stat.auckland.ac.nz/web/packages/HyperbolicDist/index.html) are the following:

* Each distribution parameter now has a default value.
* Hyperbolic distribution functions now use the (alpha, beta) form of parameterization instead of (pi, zeta).
* The distribution parameter vector `Theta` has been renamed to `param` and the order in which individual parameters appear within it has been rearranged. The `param` vector is now more consistent across the various distributions.
* The hessian matrix produced when fitting data to the hyperbolic distribution has been improved. The output no longer comes from the numeric optimizer. More accurate standard deviations on parameters can be produced.
* The generalized hyperbolic functions now test for the validity of the parameters and can identify a specialised case of the distribution (e.g. hyperbolic, asymmetric laplace).
