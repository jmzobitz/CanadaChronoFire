#' Function that just the AIC for the model data fits
#'
#' \code{compute_aic} computes the AIC values
#' @param observations Vector of measured values
#' @param model Vector of model values
#' @param n_params number of parameters in model

#' @return The computed AIC

#' @export


compute_aic <- function(observations,model,n_params) {
  n_obs <- length(observations)
  ll <- -n_obs*(log(2*pi)+1+log((sum((model-observations)^2)/n_obs)))/2
  AIC <- -2*ll + 2*n_params

  return(AIC)
}
