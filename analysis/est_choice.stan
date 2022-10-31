data {
  int B;                                 // number of brands
  int n_par_choice;                      // number of predictors that are not brand intercepts
  
  int est_N;                             // number of consumers
  int est_y[est_N];                      // number of observed choices
  matrix[B, n_par_choice] est_X[est_N];  // predictors
}

parameters {
  vector[n_par_choice]   beta;
}

transformed parameters {
  vector[B] est_utility[est_N];

  for(i in 1:est_N) {
    est_utility[i] = est_X[i]*beta;  
  }
}

model {
  // prior
  beta ~ normal(0, 10);
  
  // likelihood
  for(i in 1:est_N) {
      target += categorical_logit_lpmf(est_y[i] | est_utility[i]); 
  }
}
