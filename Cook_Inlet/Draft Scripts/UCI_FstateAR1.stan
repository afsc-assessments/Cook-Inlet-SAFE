




data {
  int<lower=0> n_years;
  vector[n_years] F_state_hist;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
transformed parameters{
  vector[n_years]predFstate;
  real curr_predFstate;
  
  // Fit the AR 1 model
  for (n in 2:n_years) {
    predFstate[n] = alpha + beta * F_state_hist[n-1];
  }
  
  // Predict myYear
  curr_predFstate = alpha + beta * F_state_hist[n_years];
  
}
model {
  // Priors
  alpha ~ normal(0, 1e10);
  beta ~ normal(0, 1e10);
  sigma ~ normal(0,5);
  
  for (n in 2:n_years) {
    (F_state_hist[n]) ~ normal(predFstate[n], sigma);
  }
}

generated quantities{
  real post_curr_predFstate;
  
  post_curr_predFstate = beta_rng(A,B);
}




