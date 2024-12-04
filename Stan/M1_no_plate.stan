data {
	int N_st_q; // Total number of observation in qPCR standard samples
	int N_st_qp; // Total number of observation in qPCR standard samples for only detected samples
	array[N_st_q] int Z_qst; // Presence/Absence response of qPCR standard data
	array[N_st_q] real S_q; // Known concentration (log10) in qPCR data
	array[N_st_qp] real R_qst; // Ct values of qPCR standard data for only detected samples
	array[N_st_qp] real S_q_p; // Known concentration (log10) in qPCR data for only detected samples
}
parameters {
	real alpha_0;
	real alpha_1;
	real eta_0;
	real eta_1;
	real gamma_0;
	real<upper=0> gamma_1;
}
transformed parameters{
	vector[N_st_q] theta_st;
	vector[N_st_qp] mu_st;
	vector[N_st_qp] sigma_st;
	for (i in 1:N_st_q){
		theta_st[i] = alpha_0 + (alpha_1 * S_q[i]);
	}
	for (i in 1:N_st_qp){
		mu_st[i] = eta_0 + (eta_1 * S_q_p[i]);
		sigma_st[i] = exp(gamma_0+(gamma_1 * S_q_p[i]));
	}
}
model {
	Z_qst ~ bernoulli(inv_logit(theta_st));
	R_qst ~ normal(mu_st,sigma_st);
  alpha_0 ~ normal(0, 1);
  alpha_1 ~ normal(0, 1);
  // // // Continious model
  eta_0 ~ normal(40, 10);
  eta_1 ~ normal(-3, 1);
  gamma_0 ~ normal(1, 3);
  gamma_1 ~ normal(0, 3);
}
generated quantities{}