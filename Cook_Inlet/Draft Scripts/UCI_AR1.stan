// Aaron Lambert - NOAA
// 9-25-2024
// Upper Cook Inlet Tier 1 Bayesian Stock  Analysis




data {
  int<lower=0> n_years;
  vector[n_years] F_state_hist;
  vector<lower=0>[n_years] run_hist;
}

transformed data{
  vector [n_years]ln_run_hist;
  
  ln_run_hist = log(run_hist);
  
  
}

parameters {
  real alpha_F;
  real beta_F;
  real alpha_R;
  real beta_R;
  real sigma;
  real <lower=0>phi;
  // real<lower=0> sigma;
}

transformed parameters{
  
  // Fstate parameters
  vector[n_years]predFstate;
  real curr_predFstate;
  vector [n_years]A;
  vector [n_years]B;

  // PF run size parameters
  vector[n_years]ln_predRunsize;
  real ln_curr_predRunsize;
  
  
  // Fit the AR 1 model for Fstate
  for (n in 2:n_years) {
    predFstate[n] = alpha_F + beta_F * F_state_hist[n-1];
  }
  
  //
  A[2:n_years] = predFstate[2:n_years]*phi;
  B[2:n_years] = (1-predFstate[2:n_years])*phi;
  
  
  // Predict Fstate for myYear
  curr_predFstate = alpha_F + beta_F * F_state_hist[n_years];
  
  // Fit the AR1 model for PF run size
  for (n in 2:n_years) {
    ln_predRunsize[n] = alpha_R + beta_R * ln_run_hist[n-1];
  }
  
  // Predict run size for myYear
  ln_curr_predRunsize = alpha_R + beta_R * ln_run_hist[n_years];
  
}

model {
  // Priors
  alpha_F ~ normal(0, 1e10);
  beta_F ~ normal(0, 1e10);
  phi ~ normal(0,5);
  
  alpha_R ~ normal(0, 1e10);
  beta_R ~ normal(0, 1e10);
  sigma ~ normal(0,5);
  
  // Fstate likelihood
  for (n in 2:n_years) {
    (F_state_hist[n]) ~ beta(A[n], B[n]);
  }
  
  // Run size likelihood
  for (n in 2:n_years) {
    (ln_run_hist[n]) ~ normal(ln_predRunsize[n], sigma);
  }
  
  
}

generated quantities{
  real post_curr_predFstate;
  real ln_post_curr_pred;
  real post_curr_predRunsize;
  
  
  post_curr_predFstate = beta_rng(A[n_years],B[n_years]);
  
  ln_post_curr_pred = normal_rng(ln_curr_predRunsize,sigma);
  
  post_curr_predRunsize = exp(ln_post_curr_pred);
  
}



