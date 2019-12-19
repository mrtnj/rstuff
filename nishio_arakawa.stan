data {
  int<lower=1>    J; // number of fixed effects
  int<lower=1>    K; // number of all animals
  int<lower=1>    N; // number of observations
  matrix[N,J]     X; // Fixed effects design matrix
  matrix[N,K]     Z; // Random effects design matrix
  vector[N]       Y; // response variable
  matrix[K,K]     A; // relationship matrix
}
transformed data{
  matrix[K,K] LA;
  LA = cholesky_decompose(A);
}
parameters {
  vector[K]  a_decompose; // breeding values
  vector[J] b; // fixed effects
  real<lower=0> sigma_G; // genetic standard deviation
  real<lower=0> sigma_R; // residual standard deviation
}
model {
    vector[N] mu;
    vector[K] a;
    a_decompose ~ normal(0, 1);
    a = sigma_G * (LA * a_decompose);
    mu = X * b + Z * a;
    Y ~ normal(mu, sigma_R);
    to_vector(b) ~ normal(0, 1);
    sigma_G ~ student_t(4, 0, 1);
    sigma_R ~ student_t(4, 0, 1);
}
generated quantities{
  real sigma_U;
  real sigma_E;
  real h2;
  sigma_U = sigma_G * sigma_G; // genetic variance
  sigma_E = sigma_R * sigma_R; // residual variance
  h2 = sigma_U / (sigma_E + sigma_U);
}

