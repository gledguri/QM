data {
	int N_st_q; // Total number of observation in qPCR standard samples
  int N_en_q; // Total number of observation in qPCR environmental samples
	int N_st_qp; // Total number of observation in qPCR standard samples for positive qPCR reaction (Z = 1)
  int N_en_qp; // Total number of observation in qPCR environmental samples for only detected samples
  int N_plate; // Total number of plates used
  int N_j; // Number of samples in environmental data
  //
  //Indexes
  // // // Binomial model
  array[N_en_q] int j_qen_idx; // Species and standard index for qPCR environmental samples
  array[N_en_qp] int j_qen_p_idx; // Species and standard index for qPCR environmental samples
  // // // Continious model
  array[N_st_qp] int plate_st_idx; // Plate index
  array[N_en_qp] int plate_en_idx; // Plate index
  // Data
  // // // Binomial model
	array[N_st_q] int Z_qst; // qPCR reaction (yes = 1 | no = 0) of environmental samples
  array[N_en_q] int Z_qen; // qPCR reaction (yes = 1 | no = 0) of environmental samples
  // // // Continious model
	vector[N_st_qp] R_qst; // Ct values of qPCR standard samples only for positive qPCR reaction (Z = 1)
  vector[N_en_qp] R_qen; // Ct values of qPCR environmental samples only for positive qPCR reaction (Z = 1)
  // 
	vector[N_st_q] S_q; // Known concentration (ln) in qPCR data
	vector[N_st_qp] S_q_p; // Known concentration (ln) in qPCR data for only detected samples
}
parameters {
	real logit_phi;
  vector[N_plate] beta_0;
	real beta_1;
	real gamma_0;
	real<upper=0> gamma_1;
  vector[N_j] C_q;
}
transformed parameters{
	vector[N_st_q] p_tmp_st = exp(S_q) * -inv_logit(logit_phi);
  vector[N_st_q] theta_st = log1m_exp(p_tmp_st) - p_tmp_st;
	vector[N_st_qp] mu_st = beta_0[plate_st_idx] + (beta_1 * S_q_p);
	vector[N_st_qp] sigma_st = exp(gamma_0+(gamma_1 * S_q_p));
	// 
	vector[N_en_q] p_tmp_en = exp(C_q[j_qen_idx]) * -inv_logit(logit_phi);
  vector[N_en_q] theta_un = log1m_exp(p_tmp_en) - p_tmp_en;
	vector[N_en_qp] mu_en = beta_0[plate_en_idx] + (beta_1 * C_q[j_qen_p_idx]);
	vector[N_en_qp] sigma_en = exp(gamma_0+(gamma_1 * C_q[j_qen_p_idx]));
}
model {
	Z_qst ~ bernoulli_logit(theta_st);;
	Z_qen ~ bernoulli_logit(theta_un);;
	R_qst ~ normal(mu_st,sigma_st);
  R_qen ~ normal(mu_en,sigma_en);
  // Priors
	logit_phi ~ normal(3,1);
	beta_0 ~ normal(40, 1);
	beta_1 ~ normal(-3, 1);
	gamma_0 ~ normal(1, 0.1);
	gamma_1 ~ normal(0, 0.1);
  C_q ~ normal(0,3);
}
