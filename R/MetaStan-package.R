#' Bayesian Meta-Analysis via Stan
#' 
#' @description
#' Conducting Bayesian meta-analysis using \pkg{rstan}.
#' @name MetaStan-package
#' @docType package
#' @useDynLib MetaStan, .registration = TRUE 
#' @import methods
#' @importFrom rstan sampling extract get_sampler_params summary stanc_builder
#' @import stats
#' @importFrom utils capture.output modifyList
#' @import ggplot2
#' @import Rcpp
NULL
