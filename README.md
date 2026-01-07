
# MomX <a href="https://momx.github.io/MomX/"><img src="man/figures/logo.png" align="right" height="139" alt="MomX website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/MomX/MomX/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MomX/MomX/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/MomX)](https://CRAN.R-project.org/package=MomX)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

MomX is an ecosystem of R packages for everything 2D morphometrics, that
is the statistical description of shape and its (co)variation.

MomX is intended to provide a complete, comfortable, powerful, and -
last but not least - open-source workflow for morphometrics.

MomX packages share common principles and work together well. This
architecture is largely inspired by the
[tidyverse](https://tidyverse.org).

## Installation

You can install the development version of MomX from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak") # once on CRAN
pak::pak("MomX/MomX")     # so far
```

Then you can install all MomX packages from GitHub with:

``` r
momx_install_all_github()
```
