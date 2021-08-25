// ideal point model
// identification:
// - xi ~ hierarchical
// - except fixed senators
data {
  // number of individuals
  int N;
  // number of items
  int M;
  // observed votes
  int Y_obs;
  int y_idx_leg[Y_obs];
  int y_idx_vote[Y_obs];
  int y[Y_obs];
  // priors
  // on items
  real mu_loc;
  real mu_scale;
  real alpha_loc;
  real alpha_scale;
  // on legislators
  int N_xi_obs;
  int idx_xi_obs[N_xi_obs];
  vector[N_xi_obs] xi_obs;
  int N_xi_param;
  int idx_xi_param[N_xi_param];
  // prior on ideal points
  real beta_loc;
  real beta_scale;
}
parameters {
  // item difficulties
  vector[M] mu;
  // item discrimination
  vector[M] alpha;
  // unknown ideal points
  vector[N_xi_param] xi_param;
}
transformed parameters {
  // create xi from observed and parameter ideal points
  vector[Y_obs] eta;
  vector[N] xi;
  xi[idx_xi_param] = xi_param;
  xi[idx_xi_obs] = xi_obs;
  for (i in 1:Y_obs) {
    eta[i] = mu[y_idx_vote[i]] + alpha[y_idx_vote[i]] * xi[y_idx_leg[i]];
  }
}
model {
  mu ~ normal(mu_loc, mu_scale);
  alpha ~ normal(alpha_loc, alpha_scale);
  xi_param ~ normal(beta_loc, beta_scale);
  y ~ bernoulli_logit(eta);
}
