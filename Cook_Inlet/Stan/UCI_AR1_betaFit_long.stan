// Aaron Lambert - NOAA
// 9-25-2024
// Upper Cook Inlet Tier 1 Bayesian Stock  Analysis
// This version is to run a longer timeseries for the preseason forecast and
// estiamate state harvest by drawing from a distribution paramterized by the mean and
// sd of observed state harvest rates.




data {
  // int<lower=0> n_years_F;
  int<lower=0> n_years;
  int<lower=0> n_years_F;
  vector<lower=0>[n_years]run_hist;
  vector<lower=0>[n_years_F]Fstate;
  // real<lower=0> A;
  // real<lower=0> B;
}

transformed data{
  vector [n_years]ln_run_hist;
  
  ln_run_hist = log(run_hist);

 // print(logit_F_state_hist); 
}

parameters {
  real<lower=0> A;
  real<lower=0> B;
  
  real alpha_R;
  real beta_R;
  real<lower=0> sigma;

  
}

transformed parameters{
  
  // PF run size parameters
  vector[n_years]ln_predRunsize;
  real ln_curr_predRunsize;
  
  
  // Fit the AR1 model for PF run size
  for (n in 2:n_years) {
    ln_predRunsize[n] = alpha_R + beta_R * ln_run_hist[n-1];
  }
  
  // Predict run size for myYear
  ln_curr_predRunsize = alpha_R + beta_R * ln_run_hist[n_years];
  
  
  
 
  
}

model {
  // Priors

  A ~ uniform(1,100);
  B ~ uniform(1,100);
  
  alpha_R ~ normal(0, 100);
  beta_R ~ normal(0, 10);
  sigma ~ normal(0,5);
  
  // State harvest likelihood
  Fstate ~ beta(A, B);
  
  // Run size likelihood
  for (n in 2:n_years) {
    (ln_run_hist[n]) ~ normal(ln_predRunsize[n], sigma);
  }
  
  
}

generated quantities{
  real post_curr_predFstate;
  real ln_post_curr_pred;
  real post_curr_predRunsize;
  
  
  
  post_curr_predFstate = beta_rng(A,B);
  
  ln_post_curr_pred = normal_rng(ln_curr_predRunsize,sigma);
  
  post_curr_predRunsize = exp(ln_post_curr_pred);
  
}




