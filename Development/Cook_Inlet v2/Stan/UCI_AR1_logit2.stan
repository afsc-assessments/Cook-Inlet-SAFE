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
  vector [n_years]logit_F_state_hist;
  
  ln_run_hist = log(run_hist);
  
  logit_F_state_hist = logit(F_state_hist);
 
 // print(logit_F_state_hist); 
}

parameters {
  real beta_F;
  real alpha_F;
  real alpha_R;
  real beta_R;
  real<lower=0> sigma;
  real<lower=0> sigma_F;
  
}

transformed parameters{
  
  vector[n_years]logit_pred_Fstate;
  vector[n_years]pred_Fstate;
  real logit_curr_predFstate;
  real curr_predFstate;

  // PF run size parameters
  vector[n_years]ln_predRunsize;
  real ln_curr_predRunsize;
  

  // Fit the AR 1 model for Fstate
  for (n in 2:n_years) {
  logit_pred_Fstate[n] = alpha_F + beta_F*logit_F_state_hist[n-1];
  }
  
  // for (n in 2:n_years) {
  //   nu[n] = logit_F_state_hist[n-1] + theta*err[n-1];
  //   
  //   err[n] = logit_F_state_hist[n] - nu[n];
  // }
  // The current years Fstate
  logit_curr_predFstate =  alpha_F + beta_F*logit_F_state_hist[n_years] ;
  
  // Fit the AR1 model for PF run size
  for (n in 2:n_years) {
    ln_predRunsize[n] = alpha_R + beta_R * ln_run_hist[n-1];
  }
  
  // Predict run size for myYear
  ln_curr_predRunsize = alpha_R + beta_R * ln_run_hist[n_years];
  
  pred_Fstate = inv_logit(logit_pred_Fstate);
  
  curr_predFstate = inv_logit(logit_curr_predFstate);
  
}

model {
  // Priors
  alpha_F ~ normal(0,1e10); //10
  beta_F ~ normal(0,1e10); //2
  sigma_F ~ normal(0,5); //5
  
  
  alpha_R ~ normal(0, 1e10);
  beta_R ~ normal(0, 1e10);
  sigma ~ normal(0,5);
  
  // State harvest likelihood
  for (n in 2:n_years) {
    (logit_F_state_hist[n]) ~ normal(logit_pred_Fstate[n], sigma_F);
  };
  
  // Run size likelihood
  for (n in 2:n_years) {
    (ln_run_hist[n]) ~ normal(ln_predRunsize[n], sigma);
  }
  
  
}

generated quantities{
  real logit_post_curr_predFstate;
  real post_curr_predFstate;
  real ln_post_curr_pred;
  real post_curr_predRunsize;
  
  
  
  logit_post_curr_predFstate =  normal_rng(logit_curr_predFstate, sigma_F);
  
  post_curr_predFstate = inv_logit(logit_post_curr_predFstate);
  
  ln_post_curr_pred = normal_rng(ln_curr_predRunsize,sigma);
  
  post_curr_predRunsize = exp(ln_post_curr_pred);
  
}



