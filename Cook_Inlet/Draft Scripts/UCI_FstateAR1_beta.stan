




data {
  int<lower=0> n_years;
  vector[n_years] F_state_hist;
}
parameters {
  real alpha;
  real beta;
  real <lower=0>phi;
  // real<lower=0> sigma;
}
transformed parameters{
  vector[n_years]predFstate;
  real curr_predFstate;
  vector [n_years]A;
  vector [n_years]B;

  
  // Fit the AR 1 model
  for (n in 2:n_years) {
    predFstate[n] = alpha + beta * F_state_hist[n-1];
  }
  
  //
  A[2:n_years] = predFstate[2:n_years]*phi;
  B[2:n_years] = (1-predFstate[2:n_years])*phi;
  
  // Predict myYear
  curr_predFstate = alpha + beta * F_state_hist[n_years];
  
}
model {
  // Priors
  alpha ~ normal(0, 1e10);
  beta ~ normal(0, 1e10);
  phi ~ normal(0,5);
  
  for (n in 2:n_years) {
    (F_state_hist[n]) ~ beta(A[n], B[n]);
  }
}

generated quantities{
  real post_curr_predFstate;
  
  post_curr_predFstate = beta_rng(A[n_years],B[n_years]);
}

