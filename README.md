
<!-- README.md is generated from README.Rmd. Please edit that file -->

## MomX <sup>*Part of [MomX](https://momx.github.io/MomX/)*</sup>

<!--
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/MomX/MomX.svg?branch=master)](https://travis-ci.org/MomX/MomX)
[![CRAN status](https://www.r-pkg.org/badges/version/MomX)](https://cran.r-project.org/package=MomX)
-->

MomX is an ecosystem of R packages for everything 2D morphometrics, that
is the statistical description of shape and its (co)variation.

It is intended to provide a complete, comfortable, powerful, and - last
but not least - open-source workflow for morphometrics in R.

MomX packages share common principles and work together well. This
eponymous package is designed to make it easy to install and load core
MomX packages in a single step.

Besides MomX itself, these packages are currently in development:

  - **[Momocs](http://momx.github.io/Momocs/)**: the mothership of MomX,
    complete 2D morphometrics toolbox from shapes and collections of
    shapes.
  - **[Momacs](https://github.com/Falindir/Momacs)**: acquisition of
    morphometrics data.
  - **[Momoshop](https://github.com/vbonhomme/Momoshop)**: image
    post-production on top of `magick`.
  - **[Momit](http://momx.github.io/Momit/)**: morphometrics data
    conversion and exchange.
  - **[Momecs](http://momx.github.io/Momecs/)**: multivariate analyses
    for multivariate data, notably morphometrics.
  - **[Momfarm](http://momx.github.io/Momfarm/)**: breeding shapes on
    top of Momocs. Mostly used, so far, to prototype ideas.

This is (very largely) inspired by the
[tidyverse](https://tidyverse.org).

### Core MomX packages

| Package | Lifecycle                                                                                                                         | Travis                                                                                                              | CRAN                                                                                                         | Website                                               |
| ------- | --------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------- |
| MomX    | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/MomX.svg?branch=master)](https://travis-ci.org/MomX/MomX)        | [![CRAN status](https://www.r-pkg.org/badges/version/MomX)](https://cran.r-project.org/package=MomX)         | [MomX.github.io/MomX](http://momx.github.io/MomX)     |
| Momacs  | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momacs.svg?branch=master)](https://travis-ci.org/MomX/Momacs)    | [![CRAN status](https://www.r-pkg.org/badges/version/Momacs)](https://cran.r-project.org/package=Momacs)     | soon                                                  |
| Momit   | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momit.svg?branch=master)](https://travis-ci.org/MomX/Momit)      | [![CRAN status](https://www.r-pkg.org/badges/version/Momit)](https://cran.r-project.org/package=Momit)       | [MomX.github.io/Momit](http://momx.github.io/Momit)   |
| Momocs  | [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)           | [![Travis-CI Build Status](https://travis-ci.org/MomX/Momocs.svg?branch=master)](https://travis-ci.org/MomX/Momocs) | [![CRAN Status Badge](http://www.r-pkg.org/badges/version/Momocs)](http://cran.r-project.org/package=Momocs) | [MomX.github.io/Momocs](http://momx.github.io/Momocs) |
| Momecs  | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momecs.svg?branch=master)](https://travis-ci.org/MomX/Momecs)    | [![CRAN status](https://www.r-pkg.org/badges/version/Momecs)](https://cran.r-project.org/package=Momecs)     | [MomX.github.io/Momecs](http://momx.github.io/Momecs) |

### Protypic MomX packages

| Package  | Lifecycle                                                                                                                         | Travis                                                                                                             | CRAN                                                                                                       | Website                                                 |
| -------- | --------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| Momfarm  | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momfarm.svg?branch=master)](https://travis-ci.org/MomX/Momfarm) | [![CRAN status](https://www.r-pkg.org/badges/version/Momfarm)](https://cran.r-project.org/package=Momfarm) | [MomX.github.io/Momfarm](http://momx.github.io/Momfarm) |
| Momoshop | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | soon                                                                                                               | soon                                                                                                       | soon                                                    |

### Installation

The (future) released version will be installable from
[CRAN](https://CRAN.R-project.org/package=MomX) with:

``` r
install.packages("MomX")
```

But, so far, and for all MomX packages, I strongly recommend (and
typically only support) using the development version that can be
installed from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("MomX/MomX")
```

Then, all MomX packages (only Momocs so far) will be loadable with a
single call to:

``` r
library(MomX)
#> ───────────────────  Attaching MomX packages  ────────────────── 
#> → Momocs     1.2.9.1      
#> → Momecs     0.1.0    
#> ─────────────────────────────── ✔ ──────────────────────────────
```

MomX packages can be attached, detached, updated from CRAN, updated from
[GitHub](http://github.com/MomX) with:

``` r
MomX_attach()        # already called by library(MomX)
MomX_detach()
MomX_update_cran()
MomX_update_github()
```

<!--
### Example

This is a basic example which shows you how to solve a common problem:


```r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:


```r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
-->
