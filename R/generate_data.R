# -----
# Generate some data
# -----

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#'
#' @param n PARAM_DESCRIPTION
#' @param m PARAM_DESCRIPTION
#' @param model_mean PARAM_DESCRIPTION
#' @param covariance PARAM_DESCRIPTION
#' @param coefs PARAM_DESCRIPTION
#' @param lambda PARAM_DESCRIPTION
#' @param p PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
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
generate_data <- function(n, m, model_mean, covariance, coefs, lambda, p) {

  mi <- sample(floor((1 - p) * m):floor((1 + p) * m), n, replace = TRUE)
  ti <- mi %>% purrr::map(~ sort(stats::runif(.x)))

  mui <- ti %>% purrr::map(~ predict_mean(.x, model_mean, lambda, k = 50))
  covi <- ti %>% map(~ predict_covariance(.x, covariance))

  list(ti, mui, covi) %>%
    purrr::pmap(function(tt, m, c) {
      list(t = tt, x = MASS::mvrnorm(1, m, c))
    }) %>%
    purrr::map(function(x) {
      noise <- sqrt(predict_noise(x$t, x$x, coefs))
      list(t = x$t, x = x$x + noise * stats::rnorm(length(x$t)), x_true = x$x)
    })
}
