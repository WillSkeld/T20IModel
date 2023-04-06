//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> K; //observatios
  int<lower=1> J; //striker levels
  matrix[K,J] X;
  int<lower=0> Y[K];
  vector[K] faced;
}
    
parameters {
  real<lower=0> phi;// neg. binomial dispersion parameter
  vector[J] beta;
  //vector[O] gamestate;
}
model {
  // priors
  beta[] ~ gamma(2.5,0.5);
  phi ~ gamma(2.5,0.5);
  // data model:
  Y ~ neg_binomial_2( X*beta + log(faced), phi);
}

