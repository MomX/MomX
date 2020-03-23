
<!-- README.md is generated from README.Rmd. Please edit that file -->

## MomX

*Part of [MomX](https://momx.github.io/MomX/)*

<!--
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build status](https://travis-ci.org/MomX/MomX.svg?branch=master)](https://travis-ci.org/MomX/MomX)
[![CRAN status](https://www.r-pkg.org/badges/version/MomX)](https://cran.r-project.org/package=MomX)
-->

### Rationale
MomX is an ecosystem of R packages for everything 2D morphometrics, that
is the statistical description of shape and its (co)variation. It is
intended to provide a complete, comfortable, powerful, and - last but
not least - open-source workflow for morphometrics.

MomX packages share common principles and work together well. This
architecture is largely inspired by the
[tidyverse](https://tidyverse.org).

### Advances

This repos will report advances on MomX development.

| Package                                                      | Family        | Description                                  | Lifecycle                                                                                                                         | Passing                                                                                                              | CRAN                                                                                                         |
|--------------------------------------------------------------|---------------|----------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------|
| :camera:[Momoshop](https://github.com/MomX/Momoshop)         | acquisition   | images post-processing                       | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momacs.svg?branch=master)](https://travis-ci.org/MomX/Momacs)     | [![CRAN status](https://www.r-pkg.org/badges/version/Momoshop)](https://cran.r-project.org/package=Momoshop) |
| :round_pushpin:[Momacs](https://github.com/MomX/Momoshop)    | acquisition   | interactive acquisition from images          | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momoshop.svg?branch=master)](https://travis-ci.org/MomX/Momoshop) | [![CRAN status](https://www.r-pkg.org/badges/version/Momacs)](https://cran.r-project.org/package=Momacs)     |
| :vulcan_salute:[Momit](https://github.com/MomX/Momit)        | acquisition   | import from images and foreign file format   | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momit.svg?branch=master)](https://travis-ci.org/MomX/Momit)       | [![CRAN status](https://www.r-pkg.org/badges/version/Momit)](https://cran.r-project.org/package=Momit)       |
| :dove:[Momocs](https://github.com/MomX/Momocs)               | morphometrics | shape manipulation and morphometrics         | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momocs.svg?branch=master)](https://travis-ci.org/MomX/Momocs)     | [![CRAN status](https://www.r-pkg.org/badges/version/Momocs)](https://cran.r-project.org/package=Momocs)     |
| :eagle:[Momstats](https://github.com/MomX/Momstats)          | morphometrics | statistical analyses of morphometric data    | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momstats.svg?branch=master)](https://travis-ci.org/MomX/Momstats) | [![CRAN status](https://www.r-pkg.org/badges/version/Momstats)](https://cran.r-project.org/package=Momstats) |
| :hatching_chick:[Momclick](https://github.com/MomX/Momclick) | morphometrics | the two previous, made interactive           | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momclick.svg?branch=master)](https://travis-ci.org/MomX/Momclick) | [![CRAN status](https://www.r-pkg.org/badges/version/Momclick)](https://cran.r-project.org/package=Momclick) |
| :ring:[MomX](https://github.com/MomX/MomX)                   | meta          | to install, reinstall, load some or them all | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/MomX.svg?branch=master)](https://travis-ci.org/MomX/MomX)         | [![CRAN status](https://www.r-pkg.org/badges/version/MomX)](https://cran.r-project.org/package=MomX)         |
| :blue_book:[Mombook](https://github.com/MomX/Mombook)        | meta          | a book collating all vignettes               | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | _no CI_                                                                                                              | _not for CRAN_                                                                                               |
| :package:[Momdata](https://github.com/MomX/Momdata)          | meta          | a data package                               | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momdata.svg?branch=master)](https://travis-ci.org/MomX/Momdata)   | _not for CRAN_                                                                                               |
|                                                              |               |                                              |                                                                                                                                   |                                                                                                                      |                                                                                                              |



### General philosophy
* MomX aims to be a complete and user-friendly ecosystem for morphometrics
* Compartiment key processes into well-defined, interoperable packages that go together well.
* Each package should solve a single task, and do it well. Make it work, document, release, improve, repeat.

MomX embraces and owns much to the [tidy manifesto](https://cran.r-project.org/web/packages/tidyverse/vignettes/manifesto.html). Kind of theorise the programming side of morphometrics, or at least itself (yes, infinite loop). Tibbles are the answer.

MomX packages share :
* Companion website built by [pkg::pkgdown]
* Continous integration with [Travis](travis-ci.org/)
* Continuous testing with [pkg::testthat] and [codecov](https://codecov.io)




### How to help
I'm much than welcoming contributions to MomX ! Good news is that whatever your skills, you can contribute a bit.

You can :

 * Signal a bug 
 * Discuss or request a feature
 * Code, improve, develop new features
 * Improve examples, and my English
 * Share datasets

### How to get helped
* Vignettes are a good place to start
* Read the reference manual, understand examples
* Go to walk for 2 hours in the forest

Then, if you think something :

* is wrong with Momocs, fill an issue with __bug__
* is wrong with you, your data, or something local, fill an issue tagged with __help__
* would be fantastic, fill an issue with __request__

...


<!-- Core MomX currently includes:

  - **[MomX](https://momx.github.io/MomX/)**: make it easy to
    install/load/update all MomX packages
  - **[Momacs](https://github.com/MomX/Momacs)**: acquisition of
    morphometrics data
  - **[Momit](http://momx.github.io/Momit/)**: morphometrics data
    conversion and exchange
  - **[Momocs](http://momx.github.io/Momocs/)**: the mothership of MomX,
    complete 2D morphometrics toolbox from shapes and collections of
    shapes
  - **[Momecs](http://momx.github.io/Momecs/)**: multivariate analyses
    for multivariate data, notably morphometrics

You can follow their development/status
below:

| Package | Lifecycle                                                                                                                         | Travis                                                                                                              | CRAN                                                                                                         | Website                                               |
| ------- | --------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------- |
| MomX    | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/MomX.svg?branch=master)](https://travis-ci.org/MomX/MomX)        | [![CRAN status](https://www.r-pkg.org/badges/version/MomX)](https://cran.r-project.org/package=MomX)         | [MomX.github.io/MomX](http://momx.github.io/MomX)     |
| Momacs  | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momacs.svg?branch=master)](https://travis-ci.org/MomX/Momacs)    | [![CRAN status](https://www.r-pkg.org/badges/version/Momacs)](https://cran.r-project.org/package=Momacs)     | <https://github.com/MomX/Momacs>                      |
| Momit   | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momit.svg?branch=master)](https://travis-ci.org/MomX/Momit)      | [![CRAN status](https://www.r-pkg.org/badges/version/Momit)](https://cran.r-project.org/package=Momit)       | [MomX.github.io/Momit](http://momx.github.io/Momit)   |
| Momocs  | [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)           | [![Travis-CI Build Status](https://travis-ci.org/MomX/Momocs.svg?branch=master)](https://travis-ci.org/MomX/Momocs) | [![CRAN Status Badge](http://www.r-pkg.org/badges/version/Momocs)](http://cran.r-project.org/package=Momocs) | [MomX.github.io/Momocs](http://momx.github.io/Momocs) |
| Momecs  | [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) | [![Travis build status](https://travis-ci.org/MomX/Momecs.svg?branch=master)](https://travis-ci.org/MomX/Momecs)    | [![CRAN status](https://www.r-pkg.org/badges/version/Momecs)](https://cran.r-project.org/package=Momecs)     | [MomX.github.io/Momecs](http://momx.github.io/Momecs) |

Other protypic MomX packages
follows:

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
#> ────────────────────────────────────────────  Attaching MomX packages  ─────────────────────────────────────────── 
#> → Momocs     1.2.9.1      
#> → Momecs     0.1.0    
#> ──────────────────────────────────────────────────────── ✔ ───────────────────────────────────────────────────────
```

MomX packages can be attached, detached, updated from CRAN, updated from
[GitHub](http://github.com/MomX) with:

``` r
MomX_attach()        # equivalent to: library(MomX)
MomX_detach()
MomX_update_cran()
MomX_update_github()
```

-->

