// Two-source d15N mixing model with a regression for the waste-associated fraction.
// Likelihood and literature priors are the same as isotope_two_source_mixing.stan. The sample fractions
// are logit-normal with a mean that depends on covariates (non-centered), plus an ahupua'a random
// intercept. This is the joint model used to test whether land use predicts the waste-associated fraction:
// the posterior uncertainty in the isotope-derived fractions is carried into the regression coefficients.
data {
  int<lower=1> N;
  vector[N] y;
  int<lower=1> K;
  matrix[N, K] X;                  // standardized covariates and island dummies (no intercept column)
  int<lower=1> A;
  array[N] int<lower=1, upper=A> ah;
  real m_w;  real<lower=0> se_w;  real<lower=0> sd_w;
  real m_o;  real<lower=0> se_o;  real<lower=0> sd_o;
  real<lower=0> sd_logsd;
  real<lower=0> sigma_a;
  real enr_mean;
  real<lower=0> enr_sd;
  real<lower=0> prior_beta_sd;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real mu_w;
  real mu_o;
  real<lower=0> s_w;
  real<lower=0> s_o;
  real alpha0;
  vector[K] beta;
  real<lower=0> sigma_f;           // residual spread of logit fractions between samples
  real<lower=0> sigma_ah;          // spread between ahupua'a
  vector[A] z_ah;
  vector[N] z_f;
}
transformed parameters {
  vector[N] eta = alpha0 + X * beta + sigma_ah * z_ah[ah] + sigma_f * z_f;
  vector<lower=0, upper=1>[N] f = inv_logit(eta);
}
model {
  mu_w ~ normal(m_w, se_w);
  mu_o ~ normal(m_o, se_o);
  s_w ~ lognormal(log(sd_w), sd_logsd);
  s_o ~ lognormal(log(sd_o), sd_logsd);
  alpha0 ~ normal(0, 1.5);
  beta ~ normal(0, prior_beta_sd);
  sigma_f ~ normal(0, 1.5);
  sigma_ah ~ normal(0, 1);
  z_ah ~ std_normal();
  z_f ~ std_normal();
  if (!prior_only) {
    vector[N] mu = f * mu_w + (1 - f) * mu_o + enr_mean;
    vector[N] sg = sqrt(square(f * s_w) + square((1 - f) * s_o) + square(sigma_a) + square(enr_sd));
    y ~ normal(mu, sg);
  }
}
generated quantities {
  vector[N] y_rep;
  for (i in 1:N) {
    real mu_i = f[i] * mu_w + (1 - f[i]) * mu_o + enr_mean;
    real sg_i = sqrt(square(f[i] * s_w) + square((1 - f[i]) * s_o) + square(sigma_a) + square(enr_sd));
    y_rep[i] = normal_rng(mu_i, sg_i);
  }
}
