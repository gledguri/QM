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
  real ini_prop_mu;
  real ini_prop_sd;
}
parameters {
  //////////////////////////////////////////// Model Compartment 2
  vector[N_sp_i_MC2-1] alpha_raw;
  matrix[N_sp_i_MC2-1,N_st_M6] alr_2_raw;
}
transformed parameters{
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
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * 0.01;
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
      gamma_M6[j,i] = alr_2[j,st_idx_M6[i]]+(NPCR*(alpha[j]));
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
  //////////////////////////////////////////// Model compartment 2
  // Model 5
  for (i in 1:N_obs_Y_M5){
    Y_M5[,i] ~ multinomial(psi_M5[,i]);
  }
  // Model 6
  for (i in 1:N_obs_Y_M6){
    Y_M6[,i] ~ multinomial(psi_M6[,i]);
  }
  //////////////////////////////////////////// Priors
  // Alpha Prior
  alpha_raw ~ std_normal();
  // Eta Prior
  for(i in 1:(N_sp_i_MC2-1)){
    alr_2_raw[i] ~ std_normal();
  }
}