
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MomX

*Part of [MomX](https://momx.github.io/MomX/)*

MomX is an ecosystem of R packages for everything 2D morphometrics, that
is the statistical description of shape and its (co)variation.

MomX is intended to provide a complete, comfortable, powerful, and -
last but not least - open-source workflow for morphometrics.

MomX packages share common principles and work together well. This
architecture is largely inspired by the
[tidyverse](https://tidyverse.org).

### Planned schedule

1.  Finish rewriting Momocs, release (April 2020)
2.  Port old Momocs stats to Momcalc, release (April 2020)
3.  Finish Momit, release (May 2020)
4.  Finish MomX, release (May 2020)
5.  Finish Momacs and Momoshop, release (June 2020)
6.  Submit to [rOpenSci](https://ropensci.org/)

## Packages

### Acquisition

#### 🖖 Momit

Import from various image and foreing file format and defines a portable
format for morphometrics
data

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/Momit?color=green)](https://cran.r-project.org/package=Momit)
[![](https://travis-ci.org/MomX/Momit.svg?branch=master)](https://travis-ci.org/MomX/Momit)
[![](https://codecov.io/gh/MomX/Momit/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/Momit)

#### 🛠 Momacs

Interactively acquire morphometrics data from images, eg defining
landmarks, curves, and other regions of
interest.

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/Momacs?color=green)](https://cran.r-project.org/package=Momacs)
[![](https://travis-ci.org/MomX/Momacs.svg?branch=master)](https://travis-ci.org/MomX/Momacs)
[![](https://codecov.io/gh/MomX/Momacs/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/Momacs)

#### 📷 Momoshop

Post-processes images, eg adjust contrast, pick channels, remove
background,
etc.

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/Momoshop?color=green)](https://cran.r-project.org/package=Momoshop)
[![](https://travis-ci.org/MomX/Momoshop.svg?branch=master)](https://travis-ci.org/MomX/Momoshop)
[![](https://codecov.io/gh/MomX/Momoshop/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/Momoshop)

### Morphometrics

#### 🕊 [Momocs](https://github.com/MomX/Momocs)

Manipulating shapes and turning them into coefficients using
morphometrics
methods.

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/Momocs?color=green)](https://cran.r-project.org/package=Momocs)
[![](https://travis-ci.org/MomX/Momocs.svg?branch=master)](https://travis-ci.org/MomX/Momocs)
[![](https://codecov.io/gh/MomX/Momocs/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/Momocs)

#### 🧮 Momcalc

Wraps sensible statistics methods for morphometrics in a consistent
grammar

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/Momcalc?color=green)](https://cran.r-project.org/package=Momcalc)
[![](https://travis-ci.org/MomX/Momcalc.svg?branch=master)](https://travis-ci.org/MomX/Momcalc)
[![](https://codecov.io/gh/MomX/Momcalc/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/Momcalc)

#### 🔮 Momex

Is the shiny child of Momocs +
Momecs

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/Momex?color=green)](https://cran.r-project.org/package=Momex)
[![](https://travis-ci.org/MomX/Momex.svg?branch=master)](https://travis-ci.org/MomX/Momex)
[![](https://codecov.io/gh/MomX/Momex/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/Momex)

### Meta

#### 💍 **MomX** helps install and update MomX packages

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![](https://www.r-pkg.org/badges/version/MomX?color=green)](https://cran.r-project.org/package=MomX)
[![](https://travis-ci.org/MomX/MomX.svg?branch=master)](https://travis-ci.org/MomX/MomX)
[![](https://codecov.io/gh/MomX/MomX/branch/master/graph/badge.svg)](https://codecov.io/gh/MomX/MomX)

#### 📚 **Mombook** compiles all package documentation into a practical book for MomX

#### 🚯 **MomXiv** aims at archiving and share morphometrics data

## General philosophy

  - MomX aims to be a complete and user-friendly ecosystem for
    morphometrics
  - Compartiment key processes into well-defined, interoperable packages
    that go together well.
  - Each package should solve a single task, and do it well. Make it
    work, document, release, improve, repeat.

MomX embraces and owns much to the [tidy
manifesto](https://cran.r-project.org/web/packages/tidyverse/vignettes/manifesto.html).
MomX kind of theorise the programming side of morphometrics, or at least
itself (yes, infinite loop). Tibbles are the answer.

MomX packages share :

  - Continous integration with [Travis](travis-ci.org/)
  - Continuous testing with \[pkg::testthat\] and
    [codecov](https://codecov.io)
  - Companion website built by \[pkg::pkgdown\], at url:
    `MomX.github.io/packagename`
  - Cheatsheets to decorate your bathroom walls

## Installation

**Will come on top when ready**

Once on CRAN, install the last released version of a package, say “MomX”
with:

``` r
install.packages("MomX")
```

I will typically only support the very last development versions that
you can get from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MomX/MomX")
```

Then you will need to laod it with :

``` r
library(MomX)
```

You can install and use packages separetely (eg with
`install.packages("Momocs")`. `MomX` package will install all MomX
packages, and help update them.

## How to help

I’m much than welcoming contributions to MomX \! Good news is that
whatever your skills, you can contribute a bit.

You can :

  - Signal a bug : fill an issue with **bug** label
  - Code, improve, develop, discuss, request new features : fill an
    issue with **discuss**
  - Improve examples, and my English : *edit pages*

## How to get help

I do not have time either, so before asking for help, please :

  - Make sure you have the very last versions of packages :
    `MomX::MomX_update`
  - Start reading the vignettes, they are a good place to start
  - Read the reference manual, understand examples
  - Go to walk for 2 hours in the forest
  - Show us what you have tried and make a [reproducible
    example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example)

Then, if you think something is wrong with :

  - is wrong with MomX : fill an issue with **bug**
  - is wrong with you, your data, or something local, fill an issue
    tagged with **help**

Feel free to ring my bell `<bonhomme.vincent@gmail.com>` if :

  - You would like to collaborate with me
  - You would like to get trained in R and/or MomX
  - Any other sensible reason

Happy MomX-ing \!

<!--


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

-->

\<\!– \#\#\#
Packages

#### :dove: [Momocs](https://github.com/MomX/Momocs): shape manipulation and morphometrics

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/Momocs)](https://cran.r-project.org/package=Momocs)

[![Travis build
status](https://travis-ci.org/MomX/Momocs.svg?branch=master)](https://travis-ci.org/MomX/Momocs)

#### :camera: [Momoshop](https://github.com/MomX/Momoshop) : images post-processing

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/Momoshop)](https://cran.r-project.org/package=Momoshop)

[![Travis build
status](https://travis-ci.org/MomX/Momacs.svg?branch=master)](https://travis-ci.org/MomX/Momacs)

#### :round\_pushpin: [Momacs](https://github.com/MomX/Momoshop) : interactive acquisition from images

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/Momacs)](https://cran.r-project.org/package=Momacs)

[![Travis build
status](https://travis-ci.org/MomX/Momoshop.svg?branch=master)](https://travis-ci.org/MomX/Momoshop)

### General philosophy

  - MomX aims to be a complete and user-friendly ecosystem for
    morphometrics
  - Compartiment key processes into well-defined, interoperable packages
    that go together well.
  - Each package should solve a single task, and do it well. Make it
    work, document, release, improve, repeat.

MomX embraces and owns much to the [tidy
manifesto](https://cran.r-project.org/web/packages/tidyverse/vignettes/manifesto.html).
MomX kind of theorise the programming side of morphometrics, or at least
itself (yes, infinite loop). Tibbles are the answer.

MomX packages share : \* Companion website built by \[pkg::pkgdown\] \*
Continous integration with [Travis](travis-ci.org/) \* Continuous
testing with \[pkg::testthat\] and [codecov](https://codecov.io)

### 

### How to help

I’m much than welcoming contributions to MomX \! Good news is that
whatever your skills, you can contribute a bit.

You can :

  - Signal a bug
  - Discuss or request a feature
  - Code, improve, develop new features
  - Improve examples, and my English
  - Share datasets

### How to get helped

  - Vignettes are a good place to start
  - Read the reference manual, understand examples
  - Go to walk for 2 hours in the forest

Then, if you think something :

  - is wrong with Momocs, fill an issue with **bug**
  - is wrong with you, your data, or something local, fill an issue
    tagged with **help**
  - would be fantastic, fill an issue with
**request**

…

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
