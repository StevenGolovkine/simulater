# -----
# Estimate the mean
# -----

#' @title Create a model for the estimation of the mean
#' @description This function creates a \code{\link[glmnet]{glmnet}} model for
#'  the mean curve of a real dataset.
#'
#' @param df Dataframe containing the real dataset.
#' @param k Number of function to be used to fit the data, default=50.
#'
#' @return A \code{\link[glmnet]{glmnet}} model for the mean curve.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'  mod <- learn_mean(df = powerconsumption, k = 50)
#'  }
#' }
#' @seealso
#'  \code{\link[glmnet]{glmnet}}
#' @rdname learn_mean
#' @export
#' @importFrom glmnet glmnet
learn_mean <- function(df, k = 50) {
  # Compute true mean
  true_mu <- unname(colMeans(df, na.rm = TRUE))

  # Smooth the mean
  m <- ncol(df)
  t <- seq(0, 1, length.out = m)
  tfeatures <- matrix(NA, ncol = 2 * k + 1, nrow = length(t))
  tfeatures[,1] <- t
  for (j in 1:k){
    tfeatures[,j + 1] <- sqrt(2) * cos(2 * j * pi * t)
    tfeatures[,2 * k + 2 - j] <- sqrt(2) * sin(2 * j * pi * t)
  }

  glmnet::glmnet(x = tfeatures, y = true_mu, alpha = 1)
}

#' @title Estimate the mean function given some model
#' @description This function estimates the mean curve of a functional dataset
#'  given a vector of sampling points and a \code{\link[glmnet]{glmnet}} model
#'  for the mean curve.
#'
#' @param u Vector of sampling points.
#' @param model \code{\link[glmnet]{glmnet}} model for the mean curve.
#' @param lambda Value of the penalty parameter.
#' @param k Number of function to be used to fit the data, default=50.
#'
#' @return A numeric vector representing the estimation of the mean curve.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'  mod <- learn_mean(df = powerconsumption, K = 50)
#'  m <- predict_mean(u = seq(0, 1, length.out = 101), model = mod,
#'                    lambda = exp(-3.5), k = 50)
#'  }
#' }
#' @seealso
#'  \code{\link[glmnet]{coef.glmnet}}
#' @rdname predict_mean
#' @export
#' @importFrom glmnet predict.glmnet
predict_mean <- function(u, model, lambda, k = 50){
  features <- matrix(NA, ncol = 2 * k + 1, nrow = length(u))
  features[, 1] <- u
  for(j in 1:k){
    features[, j + 1] <- sqrt(2) * cos(2 * j * pi * u)
    features[, 2 * k + 2 - j] <- sqrt(2) * sin(2 * j * pi * u)
  }
  glmnet::predict.glmnet(model, newx = features, s = lambda)[, 1]
}
