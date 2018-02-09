#' Convert contrast-based dataset to arm-based dataset
#'
#' \code{convert_data_arm} creates a dataframe to fit a Binomial-Normal
#' Hierarchical model using \code{glmer} function.
#'
#'
#'
#' @param ntrt Number of subjects in treatment arm
#' @param nctrl Number of subjects in control arm
#' @param ptrt Number of events in treatment arm
#' @param pctrl Number of events in treatment arm
#' @return A dataframe object
#' @examples
#' data('dat.Crins2014', package = "MetaStan")
#' ## Subset of dataset where PTLD outcomes available
#' dat.Crins2014.PTLD = subset(dat.Crins2014, is.na(exp.PTLD.events) == FALSE)
#' ## Create arm-based dataset
#' dat.Crins2014.PTLD.arm <- convert_data_arm(dat.Crins2014.PTLD$exp.total,
#' dat.Crins2014.PTLD$cont.total,
#' dat.Crins2014.PTLD$exp.PTLD.events, dat.Crins2014.PTLD$cont.PTLD.events)
#' @export
convert_data_arm <- function(ntrt, nctrl, ptrt, pctrl) {
    data <- NULL
    data$ptrt <- ptrt
    data$ntrt <- ntrt
    data$pctrl <- pctrl
    data$nctrl <- nctrl
    data <- data.frame(data)
    N <- nrow(data)
    Y <- as.vector(rbind(data$pctrl, data$ptrt))  # number of events
    sampleSize <- as.vector(rbind(data$nctrl, data$ntrt))  # number of all patients
    d <- rep(0:1, times = N)
    het <- as.vector(rbind(rep(NA, times = N), 1:N))  # ID for random effects
    # Dataset for arm-level meta-analysis
    data.arm <- data.frame(cbind(Y, sampleSize, d, het))
    data.arm$mu <- as.factor(as.numeric(gl(n = N, k = 2)))
    return(data.arm)
}
