/*A simple example of an hierarchical model*/
data {
  int<lower=1> K; //the number of observations
  int<lower=1> P; //number of players
  int<lower=1> G; //number of gamestates
  int<lower=1> S; //number of seasons
  int<lower=1> O; //number of owlig teams
  int<lower=1> W; //number of owlig teams
  
  matrix<lower=0, upper=1>[K, P] Player;
  matrix<lower=0, upper=1>[K, G] Gamestate;
  matrix<lower=0, upper=1>[K, S] Season;
  matrix<lower=0, upper=1>[K, O] Opposition;
  vector<lower=0>[W] WicketsLost;
  
  
  int<lower=0> Runs[K]; //the response variable
  vector[K] faced;
}
parameters {
  vector[P] PLY_raw;//matrix of group-level regression coefficients
  vector[G] GME;
  vector[S] SEA;
  vector[O] OPP;
  real LST;
  real<lower=0> phi; //standard deviation of the individual observations
  real mu;
  real sigma;
  
}

transformed parameters{
  vector[P] PLY_adj;
  PLY_adj = mu + 3*PLY_raw;
  
}



model {
  //priors
  mu ~ normal(log(6),0.5);
  //sigma ~ inv_gamma(3,1);
  PLY_raw[P] ~ normal(mu,3);
  GME[G] ~ normal(0,10);
  SEA[S] ~ normal(0,10);
  OPP[O] ~ normal(0,10);
  LST ~ normal(0,10);
  phi ~ gamma(1,1);
  

  //likelihood
  Runs ~ neg_binomial_2_log((Player*PLY_adj) + (Gamestate*GME) +
    (Season*SEA) + (Opposition*OPP) + (WicketsLost*LST) + log(faced),phi);
}
