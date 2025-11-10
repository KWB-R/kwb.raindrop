R Package for Optimisation Simulations for Rainwater Management
Simulations Performed With Calculation Engine Provided by Tandler.

## Installation

For details on how to install KWB-R packages checkout our [installation
tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

``` r
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
