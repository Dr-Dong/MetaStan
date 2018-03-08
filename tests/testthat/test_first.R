
context("Checking meta-analysis example: Berkey (1995) Tuberclosis dataset")

## Fitting a pairwise random effects meta-analysis model
test_that("Results are correct for fitting binomial normal hierarchical model using WIP priors.", {
  skip_on_cran()
  ## Load the dataset
  data('dat.Berkey1995', package = "MetaStan")
  ## Fitting a Binomial-Normal Hierarchial model using WIP priors
  bnhm.wip.stan  <- meta_stan(nt = dat.Berkey1995$nt, 
                              nc = dat.Berkey1995$nc, 
                              rt = dat.Berkey1995$rt,
                              rc = dat.Berkey1995$rc, 
                              mu_par = c(0, 10),
                              theta_par = c(0, 2.81), 
                              tau_par = c(0, 1))
  ### compare with results
  res1 = round(bnhm.wip.stan$theta[2], 2) - -0.76
  expect_true(all(res1 < 0.2))
  res2 = round(bnhm.wip.stan$theta[2], 2) - 0.62
  expect_true(all(res2 < 0.2))
  
})

