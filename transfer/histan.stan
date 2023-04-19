/*A simple example of an hierarchical model*/
data {
  int<lower=1> K; //the number of observations
  int<lower=1> P; //number of players
  int<lower=1> G; //number of gamestates
  int<lower=1> S; //number of seasons
  int<lower=1> O; //number of owlig teams
  int<lower=1> W; //number of owlig teams
  int<lower=1> I;
  
  matrix<lower=0, upper=1>[K, P] Player;
  matrix<lower=0, upper=1>[K, G] Gamestate;
  matrix<lower=0, upper=1>[K, S] Season;
  matrix<lower=0, upper=1>[K, O] Opposition;
  vector<lower=0>[K] Innings;
  vector<lower=0>[K] WicketsLost;
  
  
  int<lower=0> Runs[K]; //the response variable
  vector[K] faced;
}
parameters {
  vector[P] PLY_raw;//matrix of group-level regression coefficients
  vector[G] GME_raw;
  vector[S] SEA_raw;
  vector[O] OPP_raw;
  real IGS;
  real LST;
  real<lower=0> phi; //standard deviation of the individual observations
  real mu_p;
  real<lower=0> sigma_p;
  real mu_g;
  real<lower=0> sigma_g;
  real mu_o;
  real<lower=0> sigma_o;
  real mu_s;
  real<lower=0> sigma_s;

}

transformed parameters{
  vector[P] PLY_adj;
  PLY_adj = mu_p + sigma_p*PLY_raw;
  vector[G] GME_adj;
  GME_adj = mu_g + sigma_g*GME_raw;
  vector[O] OPP_adj;
  OPP_adj = mu_o + sigma_o*OPP_raw;
  vector[S] SEA_adj;
  SEA_adj = mu_s + sigma_s*SEA_raw;
}


model {
  //priors
  mu_p ~ normal(log(7.5/6),0.5);
  sigma_p ~ inv_gamma(3,1);
  PLY_raw[P] ~ normal(0,1);
  mu_g ~ normal(0,1);
  sigma_g ~ inv_gamma(3,1);
  GME_raw[G] ~ normal(0,1);
  mu_s ~ normal(0,1);
  sigma_s ~ inv_gamma(3,1);
  SEA_raw[S] ~ normal(0,1);
  mu_o ~ normal(0,1);
  sigma_o ~ inv_gamma(3,1);
  OPP_raw[O] ~ normal(0,1);
  LST ~ normal(-0.5,1);
  phi ~ gamma(1,1);
  IGS ~ normal(0,1);
  

  //likelihood
  Runs ~ neg_binomial_2_log((Player*PLY_adj) + (Gamestate*GME_adj) +
  (WicketsLost*LST) + (Opposition*OPP_adj) + (Innings*IGS) + (Season*SEA_adj) + log(faced),phi);
}

generated quantities {
  array[K] real log_lik;
  array[K] int y_rep;
  for (k in 1 : K) {
    y_rep[k] = neg_binomial_2_log_rng((Player[k]*PLY_adj) + (Gamestate[k]*GME_adj) +
  (WicketsLost[k]*LST) + (Opposition[k]*OPP_adj) + (Innings[k]*IGS) + (Season[k]*SEA_adj) + log(faced[k]),phi);
    log_lik[k] = neg_binomial_2_log_lpmf(Runs[k] | (Player*PLY_adj) + (Gamestate*GME_adj) +
  (WicketsLost*LST) + (Opposition*OPP_adj) + (Innings*IGS) + (Season*SEA_adj) + log(faced),phi);
}
}

