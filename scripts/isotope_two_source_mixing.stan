// Two-source d15N mixing model: waste-associated N (W) vs other N (O).
// One isotope, source variability, shared source means, optional enrichment allowance.
// Fractions use only the isotope data and literature priors (no land-use, NOx or GAM inputs).
data {
  int<lower=1> N;
  vector[N] y;                    // observed d15N of NOx (per mil vs air)
  real m_w;  real<lower=0> se_w;  real<lower=0> sd_w;   // literature prior: mean, SE of mean, source SD
  real m_o;  real<lower=0> se_o;  real<lower=0> sd_o;
  real<lower=0> sd_logsd;         // spread of the lognormal prior on each source SD
  real<lower=0> sigma_a;          // analytical SD (fixed; lab precision not yet known)
  real enr_mean;                  // enrichment allowance: mean shift (0 = none)
  real<lower=0> enr_sd;           // enrichment allowance: SD of the shift across samples
  int<lower=0, upper=1> prior_only;
}
parameters {
  real mu_w;
  real mu_o;
  real<lower=0> s_w;
  real<lower=0> s_o;
  vector<lower=0, upper=1>[N] f;  // waste-associated fraction; uniform prior
}
model {
  mu_w ~ normal(m_w, se_w);
  mu_o ~ normal(m_o, se_o);
  s_w ~ lognormal(log(sd_w), sd_logsd);
  s_o ~ lognormal(log(sd_o), sd_logsd);
  if (!prior_only) {
    vector[N] mu = f * mu_w + (1 - f) * mu_o + enr_mean;
    vector[N] sg = sqrt(square(f * s_w) + square((1 - f) * s_o) + square(sigma_a) + square(enr_sd));
    y ~ normal(mu, sg);
  }
}
generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  for (i in 1:N) {
    real mu_i = f[i] * mu_w + (1 - f[i]) * mu_o + enr_mean;
    real sg_i = sqrt(square(f[i] * s_w) + square((1 - f[i]) * s_o) + square(sigma_a) + square(enr_sd));
    y_rep[i] = normal_rng(mu_i, sg_i);
    log_lik[i] = normal_lpdf(y[i] | mu_i, sg_i);
  }
}
