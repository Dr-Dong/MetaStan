/* A Binomial-Normal hierarchical model for
   pairwise meta-analysis */


data {
  int<lower=1> N;                        // num studies
  int<lower=0> rc[N];                    // num events, control
  int<lower=1> nc[N];                    // num patients, control
  int<lower=0> rt[N];                    // num events, treatment
  int<lower=1> nt[N];                    // num patients, treatment
  vector[2] mu_prior;                    // prior for baseline risks
  vector[2] theta_prior;                 // prior for mean treatment effect
  vector[2] tau_prior;                   // prior for heterogeneity stdev.
  int<lower=0,upper=7> tau_prior_dist;  // prior distribution for heterogeneity stdev.
//  int<lower= 0,upper=1> re_dist;         // random effects distribution
//  real<lower=0> re_dist_t_df;            // degrees of freedom

}

transformed data{
  
//  if(tau_prior_dist == -1) print("tau distrib.:    Fixed");
  if(tau_prior_dist ==  0) print("tau distrib.:    HalfNormal");
  if(tau_prior_dist ==  1) print("tau distrib.:    TruncNormal");
  if(tau_prior_dist ==  2) print("tau distrib.:    Uniform");
  if(tau_prior_dist ==  3) print("tau distrib.:    Gamma");
  if(tau_prior_dist ==  4) print("tau distrib.:    InvGamma");
  if(tau_prior_dist ==  5) print("tau distrib.:    LogNormal");
  if(tau_prior_dist ==  6) print("tau distrib.:    TruncCauchy");
  if(tau_prior_dist == 7) print("tau distrib.:    Exponential");
  
//  if(re_dist == 0)         print("random effects:  Normal");
//  if(re_dist == 1)         print("random effects:  Student-t, df = ", re_dist_t_df);

}

parameters {
  vector[N] mu;                             // baseline risks
  real theta;                               // mean treatment effect
  vector[N] zeta;                           // individual treatment effects
  real<lower=0> tau;                        // heterogeneity stdev.
}

transformed parameters {
  real pc[N];
  real pt[N];

  for(i in 1:N) {
    pc[i] = inv_logit(mu[i]);
    pt[i] = inv_logit(mu[i] + theta + zeta[i] * tau);
  }
}

model {
  // latent variable (random effects)
  zeta ~ normal(0, 1);
  // prior distributions
  mu ~ normal(mu_prior[1], mu_prior[2]);
  theta ~ normal(theta_prior[1], theta_prior[2]);
// deprecated  
//  tau ~ normal(tau_prior[1], tau_prior[2])T[0,];
  
  // normal
  if(tau_prior_dist ==  0) tau ~ normal(tau_prior[1], tau_prior[2]);
  // halfnormal
  if(tau_prior_dist ==  1) tau ~ normal(tau_prior[1], tau_prior[2])T[0,];
  if(tau_prior_dist ==  2) tau ~ uniform(tau_prior[1], tau_prior[2]);
  if(tau_prior_dist ==  3) tau ~ gamma(tau_prior[1], tau_prior[2]);
  if(tau_prior_dist ==  4) tau ~ inv_gamma(tau_prior[1], tau_prior[2]);
  if(tau_prior_dist ==  5) tau ~ lognormal(tau_prior[1], tau_prior[2]);
  if(tau_prior_dist ==  6) tau ~ cauchy(tau_prior[1], tau_prior[2]);
  if(tau_prior_dist ==  7) tau ~ exponential(tau_prior[1]);

  // likelihood
  rc ~ binomial(nc, pc);                   // cntrl
  rt ~ binomial(nt, pt);                   // trt
}
