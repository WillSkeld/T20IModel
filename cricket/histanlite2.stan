/*A simple example of an hierarchical model*/
data {
  int<lower=1> K; //the number of observations
  int<lower=1> P; //number of players
 // int<lower=1> S; //number of seasons
  
  matrix<lower=0, upper=1>[K, P] Player;
 //matrix<lower=0, upper=1>[K, S] Season;
  
  int<lower=0> Runs[K]; //the response variable
  vector[K] faced;
}
parameters {
  //matrix of group-level regression coefficients
  vector[P] beta1;
 // vector[S] beta3;
  real<lower=0> phi; //standard deviation of the individual observations
  real mu;
  real xi;
}

model {
  //priors
  vector[K] alpha;
  
  mu ~ gamma(2,2);
  xi~ gamma(0.5,0.5);
  beta1 ~ gamma(mu,xi);
  //likelihood
  for(i in 1:K){
    alpha[i] =(Player[i]*beta1) + log(faced[i]);
  }
  Runs ~ neg_binomial_2(alpha,phi);
}


