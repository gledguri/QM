data {
  //Numbers of dimentions
  int N_st_q; // Total number of observation in qPCR standard samples
  int N_en_q; // Total number of observation in qPCR environmental samples
  int N_st_qp; // Total number of observation in qPCR standard samples for only detected samples
  int N_en_qp; // Total number of observation in qPCR environmental samples for only detected samples
  int N_j; // Number of samples in environmental data
  //
  //Indexes
  // // // Binomial model
  array[N_en_q] int j_qen_idx; // Species and standard index for qPCR environmental samples
  // // // Continious model
  array[N_en_qp] int j_qen_p_idx; // Species and standard index for qPCR environmental samples
  // Data
  // // // Binomial model
  array[N_st_q] int Z_qst; // Presence/Absence response of qPCR standard data
  array[N_en_q] int Z_qen; // Presence/Absence response of qPCR environmental data
  array[N_st_q] real S_q; // Known concentration (log10) in qPCR data
  // // // Continious model
  array[N_st_qp] real R_qst; // Ct values of qPCR standard data for only detected samples
  array[N_en_qp] real R_qen; // Ct values of qPCR environmental data for only detected samples
  array[N_st_qp] real S_q_p; // Known concentration (log10) in qPCR data for only detected samples
}
parameters {
  // Parameters
  // // qPCR
  // // // Bernoulli model
  real alpha_0;
  real alpha_1;
  // // // Continous model
  real eta_0;
  real eta_1;
  real gamma_0;
  real<upper=0> gamma_1;
  vector[N_j] C_q;
}
transformed parameters{
  // Parameters
  // // qPCR
  // // // Bernoulli model
  vector[N_st_q] theta_st;
  vector[N_en_q] theta_un;
  // // // Continious model
  vector[N_st_qp] mu_st;
  vector[N_en_qp] mu_en;
  vector[N_st_qp] sigma_st;
  vector[N_en_qp] sigma_en;
  //
  // Model TP
  // // qPCR model
  // // // Bernuli module model compartment
  // // // // // Standard
  for (i in 1:N_st_q){
    theta_st[i] = alpha_0 + (alpha_1 * S_q[i]);
  }
  // // // // // Unknown
  for (i in 1:N_en_q){
    theta_un[i] = alpha_0 + (alpha_1 * C_q[j_qen_idx[i]]);
  }
  // // // Continious model compartment
  // // // // Standard
  for (i in 1:N_st_qp){
    mu_st[i] = eta_0 + (eta_1 * S_q_p[i]);
    sigma_st[i] = exp(gamma_0+(gamma_1 * S_q_p[i]));
  }
  // // // // Unknown
  for (i in 1:N_en_qp){
    mu_en[i] = eta_0 + (eta_1 * C_q[j_qen_p_idx[i]]);
    sigma_en[i] = exp(gamma_0+(gamma_1 * C_q[j_qen_p_idx[i]]));
  }
}
model {
  // Model
  // // qPCR
  // // // Bernoulli model model compartment
  Z_qst ~ bernoulli(inv_logit(theta_st)); //Standards
  Z_qen ~ bernoulli(inv_logit(theta_un)); //Environmental samples
  // // // Continuous (Ct) model compartment
  R_qst ~ normal(mu_st,sigma_st);//Standards
  R_qen ~ normal(mu_en,sigma_en);//Field samples
  //
  // Priors
  // // qPCR
  // // // Bernoulli model
  alpha_0 ~ normal(0, 1);
  alpha_1 ~ normal(0, 1);
  // // // Continious model
  eta_0 ~ normal(40, 10);
  eta_1 ~ normal(-3, 1);
  gamma_0 ~ normal(1, 3);
  gamma_1 ~ normal(0, 3);
  C_q ~ normal(0,3);
}
