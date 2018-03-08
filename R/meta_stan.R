#' Fitting a meta-analysis model using Stan
#'
#' \code{meta_stan} fits a meta-analysis model using Stan.
#' 
#' @param nt Number of subjects in treatment arm
#' @param nc Number of subjects in control arm
#' @param rt Number of events in treatment arm
#' @param rc Number of events in control arm
#' @param mu_par A numerical vector specifying the parameter of the normal prior
#' density for baseline risks, first value is parameter for mean (default 0), 
#' second is for standard deviation (default 10).
#' @param theta_par A numerical vector specifying the parameter of the normal prior
#' density for mean treatmen effect, first value is parameter for mean(default 0), 
#' second is for standard deviation (default 2.81).
#' @param tau_par A numerical vector specifying the parameter of the half normal prior
#' density for heterogenety stdev, first value is parameter for mean (default 0), 
#' second is for standard deviation (default 1).
#' @param OR_apriori A numerical value used to calculate the standard deviation of
#' the nomral prior for treatment effect parameter. 95% prior interval of the underlying 
#' odds ratio lies between 1/\code{OR_apriori} and OR_apriori. Default is NULL.
#' When used, \code{theta_par} should not be specified.
#' @param iter A positive integer specifying the number of iterations for 
#' each chain (including warmup). The default is 4000.
#' @param warmup A positive integer specifying the number of warmup (aka burnin) 
#' iterations per chain. The default is 2000.
#' @param chains A positive integer specifying the number of Markov chains. 
#' The default is 4.
#' @return an object of class `stanfit` returned by `rstan::sampling`
#' @examples
#' \dontrun{
#' data('dat.Crins2014', package = "MetaStan")
#' ## Subset of dataset where PTLD outcomes available
#' dat.Crins2014.PTLD = subset(dat.Crins2014, is.na(exp.PTLD.events) == FALSE)
#' ## Fitting a Binomial-Normal Hierarchial model using WIP priors
#' bnhm.vague.PTLD.stan  <- meta_stan(nt = dat.Crins2014.PTLD$exp.total, 
#'                                    nc = dat.Crins2014.PTLD$cont.total, 
#'                                    rt = dat.Crins2014.PTLD$exp.PTLD.events,
#'                                    rc = dat.Crins2014.PTLD$cont.PTLD.events, 
#'                                    mu_par = c(0, 10),
#'                                    theta_par = c(0, 2.81), 
#'                                    tau_par = c(0, 1))
#' }
#'
#' @export
meta_stan = function(nt, nc, rt, rc, 
                     mu_par = c(0, 10), 
                     theta_par = NULL, 
                     tau_par = c(0, 1),
                     OR_apriori = NULL,
                     iter = 4000,
                     warmup = 2000,
                     chains = 4) {
  call <- match.call()
  
  theta_par[1] = 0
  if(!is.null(OR_apriori)){
    theta_par[2] = (log(OR_apriori) - log(1 / OR_apriori)) / (2 * 1.96)
  } else {
    theta_par[2] = 2.81
  }
  
  
  ## Create a list to be used with Stan
  stanDat <- list(N = length(nt),
                  nt = nt,
                  nc = nc,
                  rt = rt,
                  rc = rc,
                  mu_par = mu_par,
                  theta_par = theta_par,
                  tau_par = tau_par)
  
  control <- list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
  
  ## MODEL RUN
  stan_msg <- capture.output(fit <- rstan::sampling(object = stanmodels$BNHM,
                                                    data = stanDat,
                                                    warmup = warmup,
                                                    iter = iter,
                                                    refresh = 0,
                                                    algorithm = "NUTS",
                                                    include = TRUE,
                                                    control = control))
  
  if(attributes(fit)$mode != 0)
    stop("Stan sampler did not run successfully!")
  
  ## MODEL FINISHED
  fit_sum <- rstan::summary(fit)$summary
  
  vars <- rownames(fit_sum)
  
  theta_ind <- grep("^theta", vars)
  tau_ind  <- grep("^tau", vars)
  lp_ind <- grep("^lp__", vars)
  
  theta <- c(fit_sum[theta_ind, "2.5%"], fit_sum[theta_ind, "50%"], fit_sum[theta_ind, "97.5%"])
  tau  <- c(fit_sum[tau_ind, "2.5%"], fit_sum[tau_ind, "50%"], fit_sum[tau_ind, "97.5%"])
  
  Rhat.max <- max(fit_sum[,"Rhat"], na.rm=TRUE)
  
  if(Rhat.max > 1.1)
    warning("Maximal Rhat > 1.1. Consider increasing warmup MCMC parameters.")
  
  Neff.min <- min(fit_sum[c(theta_ind, tau_ind, lp_ind),"n_eff"], na.rm=TRUE)
  
  if(Neff.min < 1e3)
    message("Final MCMC sample equivalent to less than 1000 independent draws.\nPlease consider increasing the MCMC simulation size.")
  
  ## finally include a check if the Stan NuTS sample had any
  ## divergence in the sampling phase, these are not supposed to
  ## happen and can often be avoided by increasing adapt_delta
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  n_divergent <- sum(sapply(sampler_params, function(x) sum(x[,'divergent__'])) )
  if(n_divergent > 0) {
    warning(paste("In total", n_divergent, "divergent transitions occured during the sampling phase.\nPlease consider increasing adapt_delta closer to 1."))
  }
  
  Out <- list(tau = tau,
              theta = theta,
              Rhat.max = Rhat.max,
              call = call,
              fit = fit,
              fit.data = stanDat)
  
  structure(Out, class=c("MetaStan"))
}

