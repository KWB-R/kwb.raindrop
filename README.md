[![R-CMD-check](https://github.com/KWB-R/kwb.raindrop/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.raindrop/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.raindrop/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.raindrop/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.raindrop/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.raindrop)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.raindrop)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.raindrop)](https://kwb-r.r-universe.dev/)

# kwb.raindrop

R Package for Optimisation Simulations for Rainwater
Management Simulations Performed With Calculation Engine Provided by
Tandler.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.raindrop' from GitHub
remotes::install_github("KWB-R/kwb.raindrop")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.raindrop](https://kwb-r.github.io/kwb.raindrop)

Development: [https://kwb-r.github.io/kwb.raindrop/dev](https://kwb-r.github.io/kwb.raindrop/dev)
