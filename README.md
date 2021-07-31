
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulater

<!-- badges: start -->
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
