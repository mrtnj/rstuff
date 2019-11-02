data {
    int n;
    int n_slices;
    int <lower = 0> amyloplasts[n];
    real <lower = 0> day[n];
    int <lower = 1, upper = n_slices> slice[n];
    real prior_phi_scale;
}
parameters {
    real initial_amyloplasts[n_slices];
    real decline[n_slices];
    real < lower = 0> phi_rec;
}
model {
//    initial_amyloplasts ~ student_t(nu_amy, mu_amy, sigma_amy);
//    decline ~ student_t(nu_amy, mu_dec, sigma_dec);

    phi_rec ~ normal(0, prior_phi_scale);
    for (i in 1:n) {
        amyloplasts[i] ~ neg_binomial_2_log(initial_amyloplasts[slice[i]] +
		                            day[i] * decline[slice[i]],
					    (1/phi_rec)^2);
    }
}
generated quantities {
    vector[n] y_rep;
    for (i in 1:n) {
        y_rep[i] = neg_binomial_2_rng(exp(initial_amyloplasts[slice[i]] +
		                          day[i] * decline[slice[i]]),
				      	  (1/phi_rec)^2);
    }
}
