
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulater

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/StevenGolovkine/simulater.svg?branch=main)](https://travis-ci.com/StevenGolovkine/simulater)
[![Codacy
Badge](https://app.codacy.com/project/badge/Grade/bcb0d2da16ff4e6e8b8f7d09f7eb7739)](https://www.codacy.com/gh/StevenGolovkine/simulater/dashboard?utm_source=github.com&utm_medium=referral&utm_content=StevenGolovkine/simulater&utm_campaign=Badge_Grade)
[![DOI](https://zenodo.org/badge/386667875.svg)](https://zenodo.org/badge/latestdoi/386667875)
<!-- badges: end -->

`simulater` is an `R`-package that allows users to generated realistic
functional dataset. The simulated data can be irregularly sampled. It is
based on an estimation of the mean, covariance and noise functions of
the real data.

## Installation

To install the latest version directly from
[GitHub](https://github.com/), please use

``` r
# install.packages("devtools")
devtools::install_github("StevenGolovkine/simulater")
```

To build the vignette as well, please use

``` r
# install.packages("devtools")
devtools::install_github("StevenGolovkine/simulater", build_vignettes = TRUE)
```

## Dependencies

The `simulater` package depends on the `R`-packages
[`fdapace`](https://CRAN.R-project.org/package=fdapace),
[`glmnet`](https://CRAN.R-project.org/package=glmnet),
[`magrittr`](https://CRAN.R-project.org/package=magrittr),
[`mgcv`](https://CRAN.R-project.org/package=mgcv),
[`MASS`](https://CRAN.R-project.org/package=MASS) and
[`purrr`](https://CRAN.R-project.org/package=purrr).

## Bug reports

Please use [GitHub
issues](https://github.com/StevenGolovkine/simulater/issues) for
reporting bugs or issues.
