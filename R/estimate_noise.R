# -----
# Estimate the noise
# -----

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#'
#' @param df PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
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

    xs <- stats::smooth.spline(tt_1, curve[grid_1])
    xs2 <- stats::predict(xs, tt_2)$y
    res_xs2[i, ] <- xs2
    res_ec2[i, ] <- (curve[grid_2] - xs2)**2
  }

  # Fit linear model
  ec2 <- colMeans(res_ec2, na.rm = TRUE)
  xs2 <- colMeans(res_xs2, na.rm = TRUE)
  model_matrix <- data.frame(ec2, tt_2, xs2, tt_2**2, tt_2 * xs2, xs2**2)
  model <- stats::lm(ec2 ~ ., data = model_matrix)

  stats::coefficients(model)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#'
#' @param t PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param coefs PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
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
predict_noise <- function(t, x, coefs){
  mat <- data.frame(1, t, x, t**2, t * x, x**2)
  as.vector(abs(as.matrix(mat) %*% coefs))
}
