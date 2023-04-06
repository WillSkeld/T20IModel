/*A simple example of an hierarchical model*/
data {
  int<lower=1> K; //the number of observations
  int<lower=1> P; //number of players
  int<lower=1> G; //number of gamestates
 // int<lower=1> S; //number of seasons
  
  matrix<lower=0, upper=1>[K, P] Player;
  matrix<lower=0, upper=1>[K, G] Gamestate;
 //matrix<lower=0, upper=1>[K, S] Season;
  
  int<lower=0> Runs[K]; //the response variable
  vector[K] faced;
}
parameters {
  //matrix of group-level regression coefficients
  vector[P] beta1;
  vector[G] beta2;
 // vector[S] beta3;
  real<lower=0> phi; //standard deviation of the individual observations
  /*real mu;
  real xi;*/
}

model {
  //priors
  beta1[P] ~ gamma(2.5,0.5);
  beta2[G] ~ gamma(1.5,0.5);
  phi ~ gamma(1,1);
  
  /*mu ~ gamma(2,2);
  xi~ gamma(0.5,0.5);*/
  
  /*for(i in 1:P){
    beta1[i] ~ gamma(mu,xi);
  }*/
  //likelihood
  Runs ~ neg_binomial_2((Player*beta1) + (Gamestate*beta2) + log(faced),phi);
}

