devtools::install_github("gledguri/QM",dependencies = TRUE, force = T );library(QM)
load_QM_packages()
library(here)
# Load data -----------------------------------------------------------------------------------
data(qpcr);force(qpcr)
data(metabarcoding);force(metabarcoding)

# Model 1 -----------------------------------------------------------------
M1 <- load_model('M1')

# Prepare the data for going into the model
stan_data_M1 <- prep_stan_M1(qpcr_data = qpcr %>% filter(Sample_type=="STANDARD"),
														 Ct = "Ct",
														 standard_concentration = "Std_concentration",
														 plate_index = 'Plate')

# Run the model
M1_output <- Run_Model(stan_object = M1, stan_data = stan_data_M1)

# diagnose_model(M1_output)
extract_qpcr_param(M1_output)
plot_qpcr_prob_det(M1_output)
plot_qpcr_cont_mod(M1_output)
plot_qpcr_curves(M1_output)
plot_qpcr_cont_mod_plate_specific(M1_output)

# Model 2 -----------------------------------------------------------------
M2 <- load_model('M2')

# Prepare the data for going into the model
stan_data_M2 <- prep_stan_M2(qpcr_data = qpcr,
														 sample_type = "Sample_type",
														 Ct = "Ct",
														 sample_name_column = "Sample_name",
														 standard_concentration = "Std_concentration",
														 plate_index = 'Plate')

# Run the model
M2_output <- Run_Model(stan_object = M2, stan_data = stan_data_M2)

# diagnose_model(M2_output)
extract_est_conc(M2_output)
plot_qpcr_curves(M2_output)
plot_qpcr_prob_det(M2_output)
plot_qpcr_cont_mod(M2_output)
plot_qpcr_cont_mod_plate_specific(M2_output)
plot_est_conc(M2_output)

# Model 3 -----------------------------------------------------------------
M3 <- load_model('M3')

# Trim metabarcoding data only for mock samples
moc_dat <- metabarcoding %>% select(Species,sp_idx,ini_conc,Mock_1:Mock_6)

# Prepare the data for going into the model
stan_data_M3 <- prep_stan_M3(metabarcoding_data = moc_dat,
														 mock_sequencing_columns = c('Mock_1','Mock_2','Mock_3','Mock_4','Mock_5','Mock_6'),
														 mock_initial_concentration = 'ini_conc',
														 species_index = 'sp_idx',
														 species_names = 'Species',
														 number_of_PCR = 43,
														 alpha_magnitude = 0.1)


# Run the model
M3_output <- Run_Model(stan_object = M3, stan_data = stan_data_M3)

# diagnose_model(M3_output)
extract_amp_efficiecy(M3_output)
amp_eff_output_extract(M3_output)
plot_amp_eff(M3_output)

# Model 4 -----------------------------------------------------------------
M4 <- load_model('M4')

# Get column names for mock samples and environmental samples
mock_columns <- metabarcoding %>% select(Mock_1:Mock_6) %>% names()
sample_columns <- metabarcoding %>% select(-mock_columns,-Species,-sp_idx,-ini_conc) %>% names()

# Prepare the data for going into the model
stan_data_M4 <- prep_stan_M4(metabarcoding_data = metabarcoding,
														 mock_sequencing_columns = mock_columns,
														 sample_sequencing_columns = sample_columns,
														 mock_initial_concentration = 'ini_conc',
														 species_index = 'sp_idx',
														 species_names = 'Species',
														 number_of_PCR = 43,
														 alpha_magnitude = 0.1);str(stan_data_M4)


# stanMod_4 <- Run_Model(stan_object = M4, stan_data = stan_data_M4)
M4_output <- Run_Model(stan_object = M4, stan_data = stan_data_M4)

# diagnose_model(M4_output)
extract_amp_efficiecy(M4_output)
amp_eff_output_extract(M4_output)
plot_amp_eff(M4_output)
extract_ini_prop(M4_output)
bar_plot_est_ini_prop(M4_output)
heatmap_plot_est_ini_prop(M4_output)

# Model 5 -------------------------------------------------------------------------------------
M5 <- load_model('M5')

mock_columns <- metabarcoding %>% select(Mock_1:Mock_6) %>% names()
sample_columns <- metabarcoding %>% select(-mock_columns,-Species,-sp_idx,-ini_conc) %>% names()


stan_data_M5 <- prep_stan_M5(qpcr_data = qpcr,
														 sample_type = "SampleType",
														 Ct = "Ct",
														 sample_name_column = "Sample_name",
														 standard_concentration = "int_concentation",
														 plate_index = 'plate',
														 metabarcoding_data = metabarcoding,
														 mock_sequencing_columns = mock_columns,
														 sample_sequencing_columns = sample_columns,
														 mock_initial_concentration = 'ini_conc',
														 species_index = 'sp_idx',
														 species_names = 'Species',
														 number_of_PCR = 43,
														 alpha_magnitude = 0.1)

M5_output <- Run_Model(stan_object = M5, stan_data = stan_data_M5,treedepth = 12,iterations = 2000,warmup = 1000)

# diagnose_model(M5_output)
extract_amp_efficiecy(M5_output)
amp_eff_output_extract(M5_output)
plot_amp_eff(M5_output)
extract_ini_prop(M5_output)
bar_plot_est_ini_prop(M5_output)
bar_plot_est_ini_prop(M4_output)
heatmap_plot_est_ini_prop(M5_output)
plot_est_ini_conc(M5_output)
