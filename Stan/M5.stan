data {
  //////////////////////////////////////////// Intigers
  int N_sp_i_MC2; // Number of species Model Compartment 2
  int N_obs_Y_M5; // Number of observation in Model 5
  int N_obs_Y_M6; // Number of observation in Model 6
  int N_st_M6; // Number of stations in Model 6
  //
  //////////////////////////////////////////// Data
  int NPCR; //Number of PCR cycles in Model 5 and 6
  vector[N_sp_i_MC2] alr_M5; // Additive log ratio of mock initial concentration in Model 5
  array[N_sp_i_MC2,N_obs_Y_M5] int Y_M5; // Metabarcoding reads in Model 5
  array[N_sp_i_MC2,N_obs_Y_M6] int Y_M6; // Metabarcoding reads in Model 6
  //////////////////////////////////////////// Idx
  array[N_obs_Y_M6] int st_idx_M6; // Station index
  ////////////////////////////////////////////  Parameters
  //
  real alpha_magnitude; // dispersion parameter of eta
  real ini_prop_mu;
  real ini_prop_sd;
  // 
  //Numbers of dimentions
	int N_st_q; // Total number of observation in qPCR standard samples
  int N_en_q; // Total number of observation in qPCR environmental samples
	int N_st_qp; // Total number of observation in qPCR standard samples for positive qPCR reaction (Z = 1)
  int N_en_qp; // Total number of observation in qPCR environmental samples for only detected samples
  int N_plate; // Total number of plates used
  int N_j; // Number of samples in environmental data
  //
  //Indexes
  array[N_en_q] int j_qen_idx; // Species and standard index for qPCR environmental samples
  array[N_en_qp] int j_qen_p_idx; // Species and standard index for qPCR environmental samples
  array[N_st_qp] int plate_st_idx; // Plate index
  array[N_en_qp] int plate_en_idx; // Plate index
  // Data
  array[N_st_q] int Z_qst; // qPCR reaction (yes = 1 | no = 0) of environmental samples
  array[N_en_q] int Z_qen; // qPCR reaction (yes = 1 | no = 0) of environmental samples
  // // // Continious model
	vector[N_st_qp] R_qst; // Ct values of qPCR standard samples only for positive qPCR reaction (Z = 1)
  vector[N_en_qp] R_qen; // Ct values of qPCR environmental samples only for positive qPCR reaction (Z = 1)
  //
	vector[N_st_q] S_q; // Known concentration (ln) in qPCR data
	vector[N_st_qp] S_q_p; // Known concentration (ln) in qPCR data for only detected samples
  // 
}
parameters {
  //////////////////////////////////////////// Model Compartment 2
  vector[N_sp_i_MC2-1] alpha_raw;
  matrix[N_sp_i_MC2-1,N_st_M6] alr_2_raw;
  //
  real logit_phi;
  vector[N_plate] beta_0;
	real beta_1;
	real gamma_0;
	real<upper=0> gamma_1;
  vector[N_j] C_q;
}
transformed parameters{
  // Parameters
  // // qPCR
	vector[N_st_q] p_tmp_st = exp(S_q) * -inv_logit(logit_phi);
  vector[N_st_q] theta_st = log1m_exp(p_tmp_st) - p_tmp_st;
	vector[N_st_qp] mu_st = beta_0[plate_st_idx] + (beta_1 * S_q_p);
	vector[N_st_qp] sigma_st = exp(gamma_0+(gamma_1 * S_q_p));
	// 
	vector[N_en_q] p_tmp_en = exp(C_q[j_qen_idx]) * -inv_logit(logit_phi);
  vector[N_en_q] theta_un = log1m_exp(p_tmp_en) - p_tmp_en;
	vector[N_en_qp] mu_en = beta_0[plate_en_idx] + (beta_1 * C_q[j_qen_p_idx]);
	vector[N_en_qp] sigma_en = exp(gamma_0+(gamma_1 * C_q[j_qen_p_idx]));
  //////////////////////////////////////////// Declaration of transformed parameters
  matrix[N_sp_i_MC2,N_obs_Y_M6] gamma_M6;
  gamma_M6[N_sp_i_MC2,] = to_row_vector(rep_vector(0.0,N_obs_Y_M6)); //This states that the last row of gamma_M6 is 0
  matrix[N_sp_i_MC2,N_obs_Y_M5] gamma_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] psi_M6;
  matrix[N_sp_i_MC2,N_obs_Y_M5] psi_M5;
  vector[N_sp_i_MC2] alpha;
  matrix<lower=-30>[N_sp_i_MC2-1,N_st_M6] alr_2;
  //////////////////////////////////////////// Adjusting priors
  // Adjusting Alpha prior
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * alpha_magnitude;
  alpha[N_sp_i_MC2] = 0;
  // Adjusting alr_2 priors for env samp
  for (i in 1:(N_sp_i_MC2-1)) {
    alr_2[i,] = ini_prop_mu + alr_2_raw[i,] * ini_prop_sd;
  }
  //////////////////////////////////////////// Model Compartment 2
  // Model 5 (Gamma)
  for (i in 1:N_obs_Y_M5){
    for(j in 1:N_sp_i_MC2){
      gamma_M5[j,i] = alr_M5[j]+(NPCR*(alpha[j]));
    }
  }
  // Model 6 (Gamma)
  for (i in 1:N_obs_Y_M6){
    for(j in 1:N_sp_i_MC2-1){
      // gamma_M6[j,i] = alr_2[j,st_idx_M6[i]]+(NPCR*(alpha[j]));
      gamma_M6[j,i] = (alr_2[j,st_idx_M6[i]]-C_q[i])+(NPCR*(alpha[j]));
    }
  }
  // Model 5 (Psi)
  for (i in 1:N_obs_Y_M5){
    psi_M5[,i] = softmax(gamma_M5[,i]);
  }
  // Model 6 (Psi)
  for (i in 1:N_obs_Y_M6){
    psi_M6[,i] = softmax(gamma_M6[,i]);
  }
}
model {
  // Bernoulli model
	Z_qst ~ bernoulli_logit(theta_st);
	Z_qen ~ bernoulli_logit(theta_un);
  // Continuous (Ct) model compartment
  R_qst ~ normal(mu_st,sigma_st);//Standards
  R_qen ~ normal(mu_en,sigma_en);//Field samples
  // Model 5
  for (i in 1:N_obs_Y_M5){
    Y_M5[,i] ~ multinomial(psi_M5[,i]);
  }
  // Model 6
  for (i in 1:N_obs_Y_M6){
    Y_M6[,i] ~ multinomial(psi_M6[,i]);
  }
  // Priors
  alpha_raw ~ std_normal();
  for(i in 1:(N_sp_i_MC2-1)){
    alr_2_raw[i] ~ std_normal();
  }
	//
	logit_phi ~ normal(3,1);
	beta_0 ~ normal(40, 1);
	beta_1 ~ normal(-3, 1);
	gamma_0 ~ normal(1, 0.1);
	gamma_1 ~ normal(0, 0.1);
  C_q ~ normal(0,3);
}
