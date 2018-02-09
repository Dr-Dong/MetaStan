#' Fitting a meta-analysis model using Stan
#'
#' \code{meta_stan} fits a meta-analysis model using Stan.
#' 
#' @param ntrt Number of subjects in treatment arm
#' @param nctrl Number of subjects in control arm
#' @param rtrt Number of events in treatment arm
#' @param rctrl Number of events in contrl arm
#' @return an object of class `stanfit` returned by `rstan::sampling`
#' @examples
#' \dontrun{
#' data('dat.Crins2014', package = "MetaStan")
#' ## Subset of dataset where PTLD outcomes available
#' dat.Crins2014.PTLD = subset(dat.Crins2014, is.na(exp.PTLD.events) == FALSE)
#' ## Fitting a Binomial-Normal Hierarchial model
#' bnhm.vague.PTLD.stan  <- meta_stan(nt = dat.Crins2014.PTLD$exp.total, 
#'                                    nc = dat.Crins2014.PTLD$cont.total, 
#'                                    rt = dat.Crins2014.PTLD$exp.PTLD.events,
#'                                    rc = dat.Crins2014.PTLD$cont.PTLD.events)
#' }
#'
#' @export
meta_stan = function(nt, nc, rt, rc) {
  ## Create a list to be used with Stan
  stanDat <- list(N = length(nt),
                  nt = nt,
                  nc = nc,
                  rt = rt,
                  rc = rc)
  ## Ftiing the model
  out = rstan::sampling(stanmodels$BNHM, data = stanDat)
  return(out)
}

