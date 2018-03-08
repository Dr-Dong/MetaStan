/* A Binomial-Normal hierarchical model for
   pairwise meta-analysis */


data {
  int<lower=1> N;                        // num studies
  int<lower=0> rc[N];                    // num events, control
  int<lower=1> nc[N];                    // num patients, control
  int<lower=0> rt[N];                    // num events, treatment
  int<lower=1> nt[N];                    // num patients, treatment
  vector[2] mu_par;                    // prior for baseline risks
  vector[2] theta_par;                 // prior for mean treatment effect
  vector[2] tau_par;                   // prior for heterogeneity stdev.
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
  mu ~ normal(mu_par[1], mu_par[2]);
  theta ~ normal(theta_par[1], theta_par[2]);
  tau ~ normal(tau_par[1], tau_par[2])T[0,];
  // likelihood
  rc ~ binomial(nc, pc);                   // cntrl
  rt ~ binomial(nt, pt);                   // trt
}
