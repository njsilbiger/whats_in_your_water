// Two-source d15N mixing model with a population (Beta) distribution for the sample fractions.
// Same likelihood and literature priors as isotope_two_source_mixing.stan; the only addition is partial
// pooling of the fractions through the population mean pi and concentration kappa. Uses only d15N.
data {
  int<lower=1> N;
  vector[N] y;
  real m_w;  real<lower=0> se_w;  real<lower=0> sd_w;
  real m_o;  real<lower=0> se_o;  real<lower=0> sd_o;
  real<lower=0> sd_logsd;
  real<lower=0> sigma_a;
  real enr_mean;
  real<lower=0> enr_sd;
}
parameters {
  real mu_w;
  real mu_o;
  real<lower=0> s_w;
  real<lower=0> s_o;
  real<lower=0, upper=1> pi_w;       // population mean waste-associated fraction
  real<lower=0> kappa;               // concentration (2 = uniform when pi_w = 0.5)
  vector<lower=0, upper=1>[N] f;
}
model {
  mu_w ~ normal(m_w, se_w);
  mu_o ~ normal(m_o, se_o);
  s_w ~ lognormal(log(sd_w), sd_logsd);
  s_o ~ lognormal(log(sd_o), sd_logsd);
  pi_w ~ beta(1, 1);
  kappa ~ lognormal(log(2), 1);
  f ~ beta(pi_w * kappa, (1 - pi_w) * kappa);
  {
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
