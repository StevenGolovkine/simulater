# -----
# Generate some data
# -----

#' @title Generate realistic datasets
#' @description This function generates realistic irregularly sampled functional
#'  dataset given mean and covariance functions.
#'
#' @param n Number of curves to generate.
#' @param m Mean number of observation points per curve.
#' @param model_mean \code{\link[glmnet]{glmnet}} model for the mean curve.
#' @param covariance Matrix for the covariance surface.
#' @param model_noise Object of class 'gam' from the function `learn_noise`.
#' @param lambda Value of the penalty parameter for the mean curve.
#' @param p Uncertainty for the number of observation per curve, default=0.2.
#' @param k Multiplicative factor for the noise variance, default=1.
#'
#' @return List containing \code{n} entries. Each of the entry represents a
#'  simulated curve as another list with three entries:
#'  \itemize{
#'   \item \strong{$t} the sampling points.
#'   \item \strong{$x} the observed points.
#'   \item \strong{$x_true} the observed points without noise.
#'  }
#'
#' @details The data are generated as
#'
#' \deqn{X = \mu + \Sigma u + \epsilon,}
#'
#' where \eqn{\mu} is the mean function, \eqn{\Sigma} is the square-root of the
#' covariance matrix, \eqn{u} and \eqn{\epsilon} are random normal variables.
#' Heteroscedasticity is allowed using the \code{coefs} parameter.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'  mod <- learn_mean(df = powerconsumption, k = 50)
#'  cov <- learn_covariance(powerconsumption, 'lm')
#'  coefs <- learn_noise(df = powerconsumption)
#'  df <- generate_data(n = 10, m = 40, model_mean = mod, covariance = cov,
#'                      coefs = coefs, lambda = exp(-3.5), p = 0.2)
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[stats]{Uniform}},\code{\link[stats]{Normal}}
#'  \code{\link[MASS]{mvrnorm}}
#' @rdname generate_data
#' @export
#' @importFrom purrr map pmap
#' @importFrom stats runif rnorm
#' @importFrom MASS mvrnorm
#' @importFrom magrittr %>%
generate_data <- function(n, m, model_mean, covariance, model_noise, lambda,
                          p = 0.2, k = 1){

  mi <- sample(floor((1 - p) * m):floor((1 + p) * m), n, replace = TRUE)
  ti <- mi %>% purrr::map(~ sort(stats::runif(.x)))

  mui <- ti %>% purrr::map(~ predict_mean(.x, model_mean, lambda, k = 50))
  covi <- ti %>% purrr::map(~ predict_covariance(.x, covariance))

  list(ti, mui, covi) %>%
    purrr::pmap(function(tt, m, c) {
      list(t = tt, x = MASS::mvrnorm(1, m, c))
    }) %>%
    purrr::map(function(x) {
      noise <- sqrt(k * predict_noise(x$t, x$x, model_noise))
      list(t = x$t, x = x$x + noise * stats::rnorm(length(x$t)), x_true = x$x)
    })
}
