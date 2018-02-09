#' Bayesian meta-analysis with Stan
#'
#' @param stanDat Data
#' @return an object of class `stanfit` returned by `rstan::sampling`
#' @export
meta_stan = function(stanDat) {
  out = rstan::sampling(stanmodels$BNHM, data = stanDat)
  return(out)
}

