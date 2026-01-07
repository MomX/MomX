# readme

# # =======================================================
# # FILE: README.md
# # =======================================================
# cat('
# # MomX <img src="man/figures/logo.png" align="right" height="139" />
#
# <!-- badges: start -->
# [![CRAN status](https://www.r-pkg.org/badges/version/MomX)](https://CRAN.R-project.org/package=MomX)
# [![R-CMD-check](https://github.com/MomX/MomX/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MomX/MomX/actions/workflows/R-CMD-check.yaml)
# [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
# <!-- badges: end -->
#
# ## Overview
#
# MomX is an ecosystem of R packages for morphometrics that work together naturally. The goal of MomX is to provide a comprehensive, modern, and user-friendly toolkit for morphometric analysis that follows tidyverse principles.
#
# The MomX ecosystem includes:
#
# ### Core packages (loaded with `library(MomX)`)
#
# * **Momocs2**: Core morphometric methods and data structures for outline and landmark analysis
# * **Momacs**: Interactive digitization tools for extracting coordinates from images
# * **Momoshop**: Image processing pipeline builder for reproducible preprocessing
#
# ### Extended packages (install separately)
#
# * **MomData**: Curated example datasets for teaching and testing
# * **MomStats**: Advanced statistical methods for shape analysis
# * **MomViz**: Specialized visualization tools and themes
# * **MomBot**: Domain-specific tools for botanical morphometrics
# * **MomArch**: Archaeological shape analysis methods
#
# ## Installation
#
# Install the complete MomX ecosystem from CRAN:
#
# ```r
# # Install core packages
# install.packages("MomX")
#
# # Or install all packages at once
# install_momx("all")
# ```
#
# Install development versions from GitHub:
#
# ```r
# # Install remotes if needed
# install.packages("remotes")
#
# # Install core packages
# remotes::install_github("MomX/MomX")
#
# # Or use the helper function
# library(MomX)
# install_momx("all", from = "github")
# ```
#
# ## Usage
#
# Load all core MomX packages:
#
# ```r
# library(MomX)
# #> ✓ Momocs2   v2.0.0
# #> ✓ Momacs    v1.0.0
# #> ✓ Momoshop  v1.0.0
# ```
#
# Check package status:
#
# ```r
# momx_status()
# ```
#
# Update packages:
#
# ```r
# # Update from CRAN
# momx_update()
#
# # Update from GitHub
# momx_update(from = "github")
# ```
#
# ## Getting Help
#
# * Main website: https://momx.github.io/MomX/
# * Package documentation: `momx_website("Momocs2")`
# * GitHub repository: `momx_repo("Momocs2")`
# * Report bugs: https://github.com/MomX/MomX/issues
#
# ## Philosophy
#
# MomX follows these key principles:
#
# * **Easy to enter, easy to escape**: Intuitive for beginners, but you can always access the underlying data
# * **Tidyverse compatible**: Works seamlessly with dplyr, ggplot2, and other modern R packages
# * **Reasonable defaults**: Works out of the box, but fully customizable when needed
# * **Documentation first**: Every function is well-documented with practical examples
# * **Reproducible**: All analyses can be scripted and version-controlled
#
# ## Code of Conduct
#
# Please note that the MomX project is released with a [Contributor Code of Conduct](https://momx.github.io/MomX/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
#
# ## Citation
#
# If you use MomX in your research, please cite:
#
# ```r
# citation("MomX")
# ```
#
# ---
#
# Learn more about the MomX philosophy and see examples at <https://momx.github.io/MomX/>
#


# vignette ?

# # Introduction
#
# MomX is an ecosystem of R packages for morphometric analysis. This vignette will get you up and running with the basics.
#
# ## Installation
#
# Install MomX from CRAN:
#
#   ```{r eval = FALSE}
# install.packages("MomX")
# ```
#
# Or install the development version from GitHub:
#
#   ```{r eval = FALSE}
# # install.packages("remotes")
# remotes::install_github("MomX/MomX")
# ```
#
# ## Loading packages
#
# The MomX meta-package loads all core packages:
#
#   ```{r}
# library(MomX)
# ```
#
# This loads:
#
#   - **Momocs2**: Core morphometric methods
# - **Momacs**: Interactive digitization
# - **Momoshop**: Image processing pipelines
#
# ## Package management
#
# ### Check package status
#
# See which packages are installed and loaded:
#
#   ```{r eval = FALSE}
# momx_status()
# ```
#
# ### Install additional packages
#
# Install extended packages:
#
#   ```{r eval = FALSE}
# # Install all extended packages
# install_momx("extended")
#
# # Or install everything
# install_momx("all")
# ```
#
# ### Update packages
#
# Keep your packages up to date:
#
#   ```{r eval = FALSE}
# # Update from CRAN
# momx_update()
#
# # Update from GitHub (development versions)
# momx_update(from = "github")
# ```
#
# ## Getting help
#
# ### Package websites
#
# Open package documentation sites:
#
#   ```{r eval = FALSE}
# # Main MomX site
# momx_website()
#
# # Individual package sites
# momx_website("Momocs2")
# momx_website("Momacs")
# ```
#
# ### GitHub repositories
#
# Open package repositories:
#
#   ```{r eval = FALSE}
# momx_repo()           # Main repo
# momx_repo("Momocs2")  # Package repo
# ```
#
# ## Next steps
#
# - Learn about the [MomX philosophy](philosophy.html)
# - Explore [core packages](core-packages.html)
# - Read about [extending MomX](extending-momx.html)
