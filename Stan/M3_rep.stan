data {
	// Intigers
  int N_sp_i_MC2; // Number of species
  int N_obs_Y_M5; // Number of samples included in mock community
  //
  // Data
  vector[N_sp_i_MC2] alr_M5; // Additive log(e)-ratio of initial concentration of mock community
  int Y_M5[N_sp_i_MC2,N_obs_Y_M5]; // Sequence reads of mock community samples
  int NPCR;  // Number of PCR reactions
  //
  // Parameters
  real tau_eta_mock_mu; // dispersion parameter of eta
  real tau_eta_mock_sd; // dispersion parameter of eta
}
parameters {
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M5;
  vector[N_sp_i_MC2-1] alpha_raw;
  matrix[N_sp_i_MC2-1,N_obs_Y_M5] eta_raw_M5;
}
transformed parameters{
  matrix[N_sp_i_MC2,N_obs_Y_M5] gamma_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M5] psi_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M5] eta_M5 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M5);
  vector[N_sp_i_MC2] alpha;
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * 0.01;
  alpha[N_sp_i_MC2] = 0;
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M5[i,] = eta_raw_M5[i,] * tau_eta_M5[i];
  }
  for (i in 1:N_obs_Y_M5){
    for(j in 1:N_sp_i_MC2){
      gamma_M5[j,i] = alr_M5[j]+(NPCR*(alpha[j]))+eta_M5[j,i];
    }
  }
  for (i in 1:N_obs_Y_M5){
    psi_M5[,i] = softmax(gamma_M5[,i]);
  }
}
model {
  for (i in 1:N_obs_Y_M5){
    Y_M5[,i] ~ multinomial(psi_M5[,i]);
  }
  alpha_raw ~ std_normal();
  for(i in 1:(N_sp_i_MC2-1)){
    eta_raw_M5[i,] ~ std_normal();
  }
  tau_eta_M5 ~ gamma(tau_eta_mock_mu,tau_eta_mock_sd);
}
