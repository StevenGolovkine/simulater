# -----
# Estimate the covariance
# -----

#' @title Create a matrix of covariance
#' @description This function estimates the covariance matrix of a real dataset.
#'
#' @param df Dataframe containing the real dataset.
#' @param method Method to used to estimate the eigenvalues of the matrix,
#'  default='lm'. See details.
#'
#' @return A matrix with the estimated covariance.
#'
#' @details Two methods are available for the estimation of the eigenvalues of
#'  the matrix. For `lm`, we fit a linear model on the log of the first
#'  eigenvalues on the log of their rank. For `min`, we add the minimum of the
#'  estimated eigenvalues to the complete set of eigenvalues to ensure that they
#'  are all positives.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'
#'  # Using 'lm'
#'  cov <- learn_covariance(powerconsumption, 'lm')
#'
#'  # Using 'min'
#'  cov <- learn_covariance(powerconsumption, 'min')
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{smooth.spline}},
#'  \code{\link[stats]{predict}},
#'  \code{\link[fdapace]{Lwls2D}}
#' @rdname learn_covariance
#' @export
#' @importFrom stats smooth.spline predict var coef
#' @importFrom fdapace Lwls2D
learn_covariance <- function(df, method = 'lm') {
  m <- ncol(df)
  tt <- seq(0, 1, length.out = m)

  # Smooth the curves
  df_smooth <- matrix(NA, nrow = nrow(df), ncol = m)
  for(i in 1:nrow(df)){
    curve <- unname(unlist(df[i,]))
    if(sum(is.na(curve)) != 0) next

    xs <- stats::smooth.spline(tt, curve)
    df_smooth[i, ] <- stats::predict(xs, tt)$y
  }

  # Compute the empirical covariance
  cov_emp <- stats::var(df_smooth, na.rm = TRUE)

  # Smooth the covariance
  g <- expand.grid(t1 = tt, t2 = tt)
  cov_smooth <- fdapace::Lwls2D(bw = 0.01,
                                xin = as.matrix(g),
                                yin = as.vector(cov_emp),
                                xout1 = seq(0, 1, length.out = 2 * m),
                                xout2 = seq(0, 1, length.out = 2 * m))

  cov_smooth <- 0.5 * (cov_smooth + t(cov_smooth))
  if(method == 'lm') {
    e_val <- eigen(cov_smooth, only.values = FALSE)
    e_val_trunc <- e_val$values[6:50]
    model <- lm(log(e_val_trunc) ~ log(seq(6, 50, by = 1)))

    new_e_val <- rep(0, 2 * m)
    new_e_val[1:(2 * m)] <- c(e_val$values[1:5],
         exp(stats::coef(model)[1]) * seq(6, 2 * m, by = 1)**stats::coef(model)[2])

    lambda <- diag(new_e_val, nrow = 2 * m, ncol = 2 * m)
    cov <- e_val$vectors %*% lambda %*% t(e_val$vectors)
  } else if(method == 'min') {
      e_val <- eigen(cov_smooth, only.values = TRUE)
      cov <- cov_smooth + diag(-min(e_val$values), nrow = 2 * m, ncol = 2 * m)
  } else {
    stop("method not 'lm' or 'min'")
  }
  return(cov)
}

#' @title Estimate the covariance given a matrix
#' @description This function estimates the covariance matrix on a vector of
#'  sampling points. The \code{covariance} matrix parameter has to be larger
#'  than the vector of sampling points.
#'
#' @param t Vector of sampling points.
#' @param covariance Matrix of covariance.
#'
#' @return A matrix of covariance estimated on a vector of sampling points.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  attach(powerconsumption)
#'  covariance <- learn_covariance(powerconsumption, 'lm')
#'  cov <- predict_covariance(t = seq(0, 1, length.out = 101),
#'                            covariance = covariance)
#'  }
#' }
#' @rdname predict_covariance
#' @export
predict_covariance <- function(t, covariance) {
  idx <- ceiling(t * nrow(covariance))
  covariance[idx, idx]
}
