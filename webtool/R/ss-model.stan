//
// This Stan program defines a state space model for healing and damage data
//

//
// Author: Trent Henderson, 12 August 2020
//

data {
  int n_eps;             // number of episodes
  real y_values[n_eps];  // actual values in episodes
  real mu_start;         // first value in series
  real sigma;            // standard deviation of series
}

parameters {
  real<lower=0> mu[n_eps]; // don't define an upper limit as this can throw off modelling
}


model {
  
// state model

mu[1] ~ normal(mu_start, sigma);

for (i in 2:n_eps) 
mu[i] ~ normal(mu[i - 1], sigma);

// measurement model

for(t in 1:n_eps)
y_values[t] ~ normal(mu[t], sigma);

}
