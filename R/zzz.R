.onLoad <- function(libname, pkgname) {
    if (!("methods" %in% .packages())) attachNamespace("methods")
    modules <- paste0("stan_fit4", names(stanmodels), "_mod")
    for (m in modules) loadModule(m, what = TRUE)
}

.onAttach <- function(...) {
    ver <- utils::packageVersion("MetaStan")
    packageStartupMessage("This is MetaStan version ", ver)
    ggplot2::theme_set(bayesplot::theme_default())
}

