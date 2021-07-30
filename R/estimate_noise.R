# -----
# Estimate the noise
# -----

#' @title Estimate a set of coefficients for the noise variance
#' @description This function estimates the noise variance by fitting a linear
#'  model on the squared difference between the observed curves and their
#'  smoothed version.
#'
#' @param df Dataframe containing the real dataset.
#'
#' @return An object of class 'gam' from the function `mgcv::gam`.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'  coefs <- learn_noise(df = powerconsumption)
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{smooth.spline}},
#'  \code{\link[stats]{predict}},
#'  \code{\link[stats]{lm}},
#'  \code{\link[stats]{coef}}
#' @rdname learn_noise
#' @export
#' @importFrom stats smooth.spline predict lm coefficients
learn_noise <- function(df) {
  m <- ncol(df)
  grid <- 1:m
  grid_1 <- grid[as.logical(grid %% 2)]
  grid_2 <- grid[!(grid %% 2)]

  tt <- seq(0, 1, length.out = m)
  tt_1 <- tt[grid_1]
  tt_2 <- tt[grid_2]

  # Smooth the curves
  res_xs2 <- matrix(NA, nrow = nrow(df), ncol = length(tt_2))
  res_ec2 <- matrix(NA, nrow = nrow(df), ncol = length(tt_2))
  for(i in 1:nrow(df)){
    curve <- unname(unlist(df[i,]))
    if(sum(is.na(curve)) != 0) next

    xs <- stats::smooth.spline(tt_1, curve[grid_1], cv = TRUE)
    xs2 <- stats::predict(xs, tt_2)$y
    res_xs2[i, ] <- xs2
    res_ec2[i, ] <- (curve[grid_2] - xs2)**2
  }

  dd <- data.frame(ec = as.vector(res_ec2),
                   xx = as.vector(res_xs2),
                   tt = rep(seq(0, 1, length.out = length(tt_2)), nrow(df)))
  mgcv::gam(ec ~ te(xx, tt), data = dd)
}

#' @title Estimate the variance of the noise given a set of coefficients
#' @description This function estimates the noise variance given vectors of
#'  sampling and observed points and a vector of coefficients.
#'
#' @param t Vector of sampling points.
#' @param x Vector of observed points.
#' @param model Object of class 'gam' from the function `learn_noise`.
#'
#' @return A numeric vector representing the estimation of the noise variance.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'  coefs <- learn_noise(df = powerconsumption)
#'  noise <- predict_noise(t = seq(0, 1, length.out = 101),
#'                         x = seq(min(powerconsumption, na.rm = T),
#'                                 max(powerconsumption, na.rm = T),
#'                                 length.out = 101),
#'                         coefs = coefs)
#'  }
#' }
#' @rdname predict_noise
#' @export
predict_noise <- function(t, x, model){
  dd <- data.frame(xx = x, tt = t)
  mgcv::predict.gam(model, newdata = dd)
}
