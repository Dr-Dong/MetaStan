/* A Binomial-Normal hierarchical model for
   pairwise meta-analysis */


data {
  int<lower=1> N;                           // num studies
  int<lower=0> rc[N];                       // num events, treatment
  int<lower=1> nc[N];                       // num patients, treatment
  int<lower=0> rt[N];                       // num events, control
  int<lower=1> nt[N];                       // num patients, control
}

parameters {
  vector[N] mu;                             // baseline risks
  real d;                                   // relative treatment effect
  vector[N] zeta;                          // individual treatment effects
  real<lower=0> tau;                        // heterogeneity stdev.
}

transformed parameters {
  real pc[N];
  real pt[N];

  for(i in 1:N) {
    pc[i] = inv_logit(mu[i]);
    pt[i] = inv_logit(mu[i] + d + zeta[i] * tau);
  }
}

model {
  // latent variable (random effects)
  zeta ~ normal(0, 1);
  // prior distributions
  mu ~ normal(0, 100);
  d ~ normal(0, 100);
  tau ~ normal(0, 1)T[0,];
  // likelihood
  rc ~ binomial(nc, pc);                   // cntrl
  rt ~ binomial(nt, pt);                   // trt
}
