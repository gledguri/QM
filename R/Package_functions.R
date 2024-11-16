#' Extract model parameters
#'
#' This function extracts and summarizes parameters from a fitted Stan model.
#' The function returns a data frame containing the mean, standard error, quantiles,
#' number of effective samples, and Rhat value for the specified parameter(s).
#'
#' @param model A `stanfit` object containing the results of the model fitting
#' @param parmeter A `character` string specifying the name of the parameter to be extracted
#'   from the model. Defaults to `"alpha"`.
#'
#' @return A `data.frame` containing the summarized statistics for the specified
#'   parameter(s), including mean, standard error, quantiles (2.5%, 25%, 50%, 75%, 97.5%),
#'   effective sample size (n_eff), and Rhat.
#' @export
#'
#' @examples
#' # Extract summary statistics for the parameter "alpha" from a fitted Stan model (e.g., `stanMod_1`)
#' param_summary <- extract_param(model = stanMod_1, parmeter = "alpha")
#'
#' # Extract summary statistics for a different parameter, "beta"
#' param_summary <- extract_param(model = stanMod_1, parmeter = "beta")
extract_param <- function(model,par){
	fit <- methods::selectMethod("summary", signature = "stanfit")(object = model, par = par)
	fit <- fit$summary
	return(fit %>% unlist()%>%as.data.frame%>%round(.,3))
}

#' Extract 2-Dimensional Model parameters as a matrix
#'
#' This function extracts and organizes 2-dimensional parameters (e.g., `alpha[m,n]`)
#' from a fitted Stan model into a matrix format. The user can specify which summary statistic
#' (e.g., mean, standard error) to extract.
#'
#' @param stanMod A `stanfit` object containing the results of the model fitting.
#' @param param A `character` string specifying the name of the 2-dimensional parameter to be extracted
#'   from the model (e.g., `"alpha"` for `alpha[m,n]`).
#' @param vector A `character` string specifying the summary statistic to be extracted for each element
#'   of the matrix. Options might include `"mean"`, `"se_mean"`, `"sd"`, or quantiles (e.g., `"2.5%"`).
#'   Defaults to `"mean"`.
#'
#' @return A `data.frame` representing the 2-dimensional parameter as a matrix, with rows and columns
#'   corresponding to the indices of the parameter.
#' @export
#'
#' @examples
#' # Extract the mean values of the 2-dimensional parameter "alpha[m,n]" as a matrix
#' alpha_matrix <- extract_matrix(stanMod = stanMod_1, param = "alpha", vector = "mean")
#'
#' # Extract the standard deviations of the 2-dimensional parameter "beta[m,n]" as a matrix
#' beta_matrix <- extract_matrix(stanMod = stanMod_1, param = "beta", vector = "sd")
extract_matrix <- function(stanMod,param,vector="mean"){
	dd <- extract_param(stanMod,param)
	nrrow <- str_split(dd %>% rownames(),"\\[") %>% as.data.frame() %>% t() %>% as.data.frame() %>% pull(V2) %>% gsub("\\,\\d+\\]","",.) %>% as.numeric() %>% max()
	nccol <- str_split(dd %>% rownames(),"\\[\\d+\\,") %>% as.data.frame() %>% t() %>% as.data.frame() %>% pull(V2) %>% gsub("\\]","",.) %>% as.numeric() %>% max()
	ret <- as.data.frame(matrix(NA,nrrow,nccol)) %>% setNames(c(1:nccol))
	for (i in 1:nrrow) {
		for (j in 1:nccol) {
			ret[i,j] <- extract_param(stanMod,param) %>% filter(rownames(.)==str_subset(rownames(.),paste0("\\[",i,",",j,"\\]"))) %>% pull(vector)
		}
	}
	return(ret)
}

data_example <- function(data="M1") {
	if(data=="M1"){
	cat("    Sample_name Species SampleType           Ct standard_concentation
1         Std-1     Cod   STANDARD  17.17366409          1000000
2         Std-2     Cod   STANDARD  20.32516289           100000
3         Std-3     Cod   STANDARD  24.26965332            10000
4         Std-4     Cod   STANDARD  26.89413261             1000
5         Std-5     Cod   STANDARD  30.73742294              100
6         Std-6     Cod   STANDARD   33.8976326               10
7         Std-7     Cod   STANDARD  36.95103073                1
9         Std-1     Cod   STANDARD  17.41136169          1000000
10        Std-3     Cod   STANDARD   24.2088623            10000
11        Std-4     Cod   STANDARD  27.22113228             1000
12        Std-5     Cod   STANDARD  30.60840034              100
13        Std-6     Cod   STANDARD  34.95129395               10
14        Std-7     Cod   STANDARD Undetermined                1
...")
}
	if(data=="M2"){
	cat("    Sample_name Species SampleType           Ct standard_concentation
1         Std-1     Cod   STANDARD  17.17366409          1000000
2         Std-2     Cod   STANDARD  20.32516289           100000
3         Std-3     Cod   STANDARD  24.26965332            10000
4         Std-4     Cod   STANDARD  26.89413261             1000
5         Std-5     Cod   STANDARD  30.73742294              100
6         Std-6     Cod   STANDARD   33.8976326               10
7         Std-7     Cod   STANDARD  36.95103073                1
9         Std-1     Cod   STANDARD  17.41136169          1000000
10        Std-3     Cod   STANDARD   24.2088623            10000
11        Std-4     Cod   STANDARD  27.22113228             1000
12        Std-5     Cod   STANDARD  30.60840034              100
13        Std-6     Cod   STANDARD  34.95129395               10
14        Std-7     Cod   STANDARD Undetermined                1
...
40    Sample_10     Cod    UNKNOWN  43.85659409               NA
41    Sample_10     Cod    UNKNOWN  46.21499252               NA
43    Sample_10     Cod    UNKNOWN Undetermined               NA
44    Sample_10     Cod    UNKNOWN Undetermined               NA
...
158  Sample_250     Cod    UNKNOWN  30.05127716               NA
159  Sample_250     Cod    UNKNOWN  30.39950752               NA
160  Sample_250     Cod    UNKNOWN  33.72243118               NA
161  Sample_250     Cod    UNKNOWN  33.49518585               NA
...")
}
	if(data=="M3"){
	cat(
"                        Species   Mock_r1   Mock_r2   Mock_r3  ...   Mock_r9  initial_conc species_index
1                 Brosme brosme     26537     26282     55800          55800          6022             1
2            Cyclopterus lumpus     63611     38494     80634          80634         12061             2
3  Hippoglossoides platessoides    103953     53527     96043          96043          6812             3
4         Leptoclinus maculatus    228228    124488    237519         237519          3725             4
5             Mallotus villosus     72751     35851     94465          94465          9816             5
6           Maurolicus muelleri     99815     29082    130790         130790          7087             6
7        Myoxocephalus scorpius     75120     44139     77521          77521          8908             7
8              Pholis gunnellus     66110     36235     58501          58501          4477             8
9         Pleuronectes platessa     41550     19983     44716          44716          2637             9
10                 Gadus morhua     72763     56460    111345         111345          5942            10")
	}
	if(data=="M4"){
		cat(
			"                        Species    Sample_1    Sample_2    Sample_3 ... Sample_12
1                 Brosme brosme           0           0           0             0
2            Cyclopterus lumpus           2           0           0             2
3  Hippoglossoides platessoides        8214           0           0          6890
4         Leptoclinus maculatus         160           0           0          6800
5             Mallotus villosus       70304        3100           6             4
6           Maurolicus muelleri           0           0           0             0
7        Myoxocephalus scorpius           0           0           0             2
8              Pholis gunnellus         100         694           0          2936
9         Pleuronectes platessa           0           0           0             0
10                 Gadus morhua        8514           8        1752          4462")
	}
}
#' Scientific notation labels for log-space ggplots
#'
#' This function creates axis labels in scientific notation for ggplot2 plots when
#' working with logarithmic or exponential scales. The labels are formatted as powers of 10
#' (e.g., `10^1`, `10^2`, `10^3`), making them more readable and appropriate for log-scaled axes.
#'
#' @param x A numeric vector of values that need to be formatted in scientific notation.
#'
#' @return A vector of expressions formatted in scientific notation (e.g., `10^1`, `10^2`),
#'   suitable for use as axis labels in ggplot2.
#' @export
#'
#' @examples
#' # Example of using scientific_10 for y-axis labels in a ggplot2 plot
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_y_continuous(trans = 'log10', labels = scientific_10)
#'
#' # Example of using scientific_10 for x-axis labels in a ggplot2 plot
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_x_continuous(trans = 'log10', labels = scientific_10)
scientific_10 <- function(x) {
	c <- scales::scientific_format()(x)
	# t <- gsub("1e", "10^", c)
	t <- gsub("[0-9]e", "10^", c)
	t <- gsub("10\\^\\+00", "10^0", t)
	t2 <- gsub("10\\^\\+", "10\\^", t)
	str2expression(t2)}

mutate_indices <- function(data, index_column) {
	data <- data %>%
		ungroup() %>%
		group_by(.data[[index_column]]) %>%
		mutate(index = cur_group_id()) %>%
		ungroup()

	return(data)
}
#' Extract Model Parameters from Stan Output (M1 & M2)
#'
#' This function extracts specific model parameters from the Stan model M1 and M2 output.
#'
#' @param stanMod A `stanfit` object containing the results of the model fitting,
#'   typically produced by `run_M1`.
#'
#' @return A `data.frame` containing the extracted parameters: `alpha_0`, `alpha_1`, `eta_0`, `eta_1`, `gamma_0`, and `gamma_1`.
#' @export
#'
#' @examples
#' # Assuming you have run the model and have a stanfit object called stanMod_1
#' ss_param <- ss_param_extract(stanMod = stanMod_1)
ss_param_extract <- function(stanMod){
	l <- c('alpha_0','alpha_1','eta_0','eta_1','gamma_0','gamma_1')
	output <- extract_param(stanMod,l)
	return(output)
}

#' Extract estimated eDNA qPCR concentrations from model M2
#'
#' This function extracts the estimated quantities (e.g., DNA concentrations) from the fitted Stan Model 2.
#' It processes the model output to return a data frame containing the sample indices, sample names, and
#' the estimated log10-transformed DNA concentrations along with their associated standard errors and standard deviations.
#'
#' @param stanMod A `stanfit` object containing the results of fitting Stan Model 2, typically produced by `run_M2`.
#'
#' @return A `data.frame` (tibble) with the following columns:
#'   - `sample_index`: The index of the sample as used in the Stan model.
#'   - `Sample_name`: The name or identifier of the sample which was inputed in Stan model.
#'   - `C_est_log`: The mean of the log10-transformed estimated DNA concentrations.
#'   - `C_est_log_se`: The standard error of the log10-transformed estimated DNA concentrations.
#'   - `C_est_log_sd`: The standard deviation of the log10-transformed estimated DNA concentrations.
#' @export
#'
#' @examples
#' # Extract the estimated quantities from the fitted Stan Model 2
#' est_ss_quant <- est_ss_quant_extract(stanMod = stanMod_2)
est_ss_quant_extract <- function(stanMod){
	output <- extract_param(stanMod,"C_q") %>%
		tibble::rownames_to_column("sample_index") %>%
		mutate(sample_index = stringr::str_extract(sample_index, "\\d+")) %>%
		mutate(sample_index=as.numeric(sample_index)) %>%
		left_join(.,stan_data_M2$label,by="sample_index") %>%
		rename(C_est_log="mean") %>%
		rename(C_est_log_se="se_mean") %>%
		rename(C_est_log_sd="sd") %>%
		select(sample_index,Sample_name,C_est_log,C_est_log_se,C_est_log_sd)
	return(output)
}
amp_eff_param_extract <- function(stanMod){
	output <- methods::selectMethod("summary", signature = "stanfit")(object = stanMod, par = 'alpha')
	output <- output$summary %>% as.data.frame() %>% as.data.frame()
	return(output)
}
#' Extract amplification efficiencies from Stan Model 3
#'
#' This function extracts the amplification efficiencies calculated by Stan Model 3
#' and formats the results for further analysis. The output includes the pre-PCR and
#' post-PCR values, as well as the estimated amplification efficiencies with their
#' associated confidence intervals.
#'
#' @param stan_data A `list` of data formatted as required by Stan Model 3, typically
#'   produced by the `prep_stan_M3` function.
#' @param amp_eff_param A `list` or `data.frame` containing the amplification efficiency
#'   parameters estimated by Stan Model 3. Typically, this includes the mean and standard
#'   deviation (`sd`) of the amplification efficiency estimates.
#'
#' @return A `data.frame` containing the following columns:
#'   - `Pre-PCR`: The pre-PCR relative abundances of each species in the mock community.
#'   - `Post-PCR`: The post-PCR mean sequencing counts for each species.
#'   - `Post-PCR_est`: The estimated post-PCR values based on the calculated amplification efficiency.
#'   - `amp_eff`: The amplification efficiency for each species, calculated as `exp(40 * mean(amp_eff_param))`.
#'   - `amp_eff_lo`: The lower bound of the amplification efficiency, based on the standard deviation.
#'   - `amp_eff_up`: The upper bound of the amplification efficiency, based on the standard deviation.
#'
#' @export
#'
#' @examples
#' # Extract amplification efficiencies from the Stan Model 3 output
#' amp_eff_output <- amp_eff_output_extract(stan_data = stan_data_M3, amp_eff_param = amp_eff_param)
amp_eff_output_extract <- function(stan_data,amp_eff_param){
	output <- stan_data$alr_M5 %>% as.data.frame() %>% exp() %>%
		cbind(.,rowMeans(stan_data$Y_M5),stan_data$Species) %>%
		as.data.frame() %>%
		setNames(c("Pre-PCR","Post-PCR","Species")) %>% rownames_to_column("x") %>% select(-x) %>% column_to_rownames("Species") %>%
		mutate(`Post-PCR_est`=exp(galr(`Pre-PCR`)+(40*amp_eff_param$mean)))%>% rel.ab() %>%
		mutate(amp_eff=exp(40*amp_eff_param$mean)) %>% round(.,2) %>%
		mutate(amp_eff_lo=exp(40*(amp_eff_param$mean-amp_eff_param$sd))) %>% round(.,2) %>%
		mutate(amp_eff_up=exp(40*(amp_eff_param$mean+amp_eff_param$sd))) %>% round(.,2)
	return(output)
}

mock_variance_extract <- function(stan_data,stanMod,amp_eff_param){
	modeled_prop <- (stan_data$alr_M5+(amp_eff_param$mean*stan_data$NPCR)) %>% as.data.frame()
	error_term <- extract_matrix(stanMod,"eta_M5")
	right_term <- sweep(t(error_term), 2, t(modeled_prop), "+") %>% exp() %>% t() %>% rel.ab()
	obs_prop <- stan_data$Y_M5 %>% rel.ab()
	diff_prop <- sweep(t(obs_prop), 2, t(exp(modeled_prop) %>% rel.ab()), "-") %>% t()
	diff_prop <- diff_prop %>% cbind(.,stan_data$Species) %>% rownames_to_column("x") %>% select(-x) %>%
		column_to_rownames("Species")
	return(diff_prop)
}
est_ini_prop_extract <- function(stanMod){
	output <- extract_matrix(stanMod,"alr_2") %>% rbind(.,setNames(data.frame(matrix(0,1,ncol(.))),colnames(.)))
	return(output)
}
sample_variance_extract <- function(est_ini_prop,amp_eff_param,stan_data,stanMod){
	modeled_prop <- est_ini_prop+make_matrix(amp_eff_param$mean*stan_data$NPCR,ncol(est_ini_prop)) %>% as.data.frame()
	modeled_prop <- expand_matrix_by_idx(modeled_prop,stan_data$st_idx_M6 %>% sort())

	error_term <- extract_matrix(stanMod,"eta_M6")
	obs_prop <- modeled_prop+error_term
	diff_prop <- rel.ab(exp(obs_prop)) - rel.ab(exp(modeled_prop))
	colnames(diff_prop) <- colnames(stan_data$Y_M6)
	return(diff_prop)
}
est_ini_conc_extract_5_1 <- function(stanMod,stan_data){
	mat <- extract_matrix(stanMod,"alr_2")
	last_r <- mean_by_idx(stan_data$ss_conc,stan_data$st_idx_M6) %>% pull(mean)
	return(rbind(mat,last_r))
}
est_ini_conc_sd_extract_5_1 <- function(stanMod){
	mat <- extract_matrix(stanMod,"alr_2",vector = "sd")
	return(mat)
}
est_ini_conc_extract_5_2 <- function(stanMod,stan_data){
	mat <- extract_matrix(stanMod,"alr_2")
	last_r <- extract_param(stanMod,"C_q") %>% pull(mean) %>% mean_by_idx(.,stan_data$st_idx_M6) %>% pull(mean)
	return(rbind(mat,last_r))
}
est_ini_conc_sd_extract_5_2 <- function(stanMod,stan_data){
	mat <- extract_matrix(stanMod,"alr_2",vector = "sd")
	last_r <- extract_param(stanMod,"C_q") %>% pull(sd) %>% mean_by_idx(.,stan_data$st_idx_M6) %>% pull(mean)
	return(rbind(mat,last_r))
}


# Stan Models -------------------------------------------------------------

#' Prepare data for Stan Model 1
#'
#' This function prepares the data required to run Stan Model 1 by filtering
#' the input data and selecting the necessary columns.
#'
#' @param data A `data.frame` containing the qPCR data. It should include the
#'   Ct values and standard (initial) concentrations.
#' @param Ct A `character` string specifying the name of the column in `data`
#'   that contains the Ct values. qPCR non-detects should be defined as 'Undetermined' and not NA or any other value
#' @param standard_concentration A `character` string specifying the name of the
#'   column in `data` that contains the standard concentrations.
#'
#' @return A list formatted as required by the Stan model.
#' @export
#'
#' @examples
#' stan_data_M1 <- prep_stan_M1(data = qpcr %>% filter(task == "STANDARD"),
#'                              Ct = "Ct",
#'                              standard_concentration = "st_conc")
prep_stan_M1 <- function(data, Ct, standard_concentration) {
	qpcr_g <- data %>%
		rename(Ct = .data[[Ct]]) %>%
		rename(st_conc = .data[[standard_concentration]])

	if (sum(qpcr_g$Ct=="Undetermined")==0) {
		cat("\n");questions("Are the undetermined CT values called \"Undetermined\"?",col = 31)
	}
	qpcr_g <- qpcr_g %>%
		mutate(Ct = replace(Ct, Ct == "Undetermined", NA)) %>%
		mutate(Ct = as.numeric(Ct)) %>%
		mutate(pres = 1, pres = replace(pres, is.na(Ct), 0)) %>%
		mutate(st_conc=as.numeric(st_conc))

	st_qpcr <- qpcr_g

	st_qpcr_cm <- st_qpcr %>% filter(pres==1)

	stan_data <- list(
		N_st_q = nrow(st_qpcr),
		N_st_qp = nrow(st_qpcr_cm),
		#
		Z_qst = as.integer(st_qpcr$pres),
		S_q = log10(st_qpcr$st_conc),
		#
		R_qst = as.numeric(st_qpcr_cm$Ct),
		S_q_p = log10(st_qpcr_cm$st_conc)
	)
	cat(str(stan_data))
	return(stan_data)
}

#' Run Stan Model 1
#'
#' This function runs Stan Model 1 using the prepared data and optionally
#' generates plots of the model's outputs.
#'
#' @param stan_object A `stanmodel` object representing the compiled Stan model (M1).
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M1`.
#' @param plot_fig A `logical` value indicating whether to generate plots of the
#'   model's outputs after fitting the model. Defaults to `TRUE`. If `TRUE`, the
#'   function will generate and display the output plots; if `FALSE`, no plots
#'   will be produced.
#'
#' @return An object of class `stanfit` containing the results of the model fitting.
#' The model runs 4 chains, 5000 iterations and burns 2000 for warm-up. For changing these values run the analog version of this function
#' @export
#'
#' @examples
#' stanMod_1 <- run_M1(stan_object = M1, stan_data = stan_data_M1)
run_M1 <- function(stan_object=M1,stan_data=stan_data_M1,plot_fig=T){
	if(!inherits(stan_object, "stanmodel")){
		stop('\nThe provided stan_object is not a stanmodel. \nPlease load the correct model.')
	}
	xmin_log <- floor(min(stan_data_M1$S_q))
	xmax_log <- round(max(stan_data_M1$S_q))
	stanMod_1 <<- sampling(
		object = stan_object,
		chains = 4,
		iter = 5000,
		warmup = 2000,
		data = stan_data
	)
	ss_param <<- ss_param_extract(stanMod = stanMod_1)
	if (plot_fig==T) {
	plot_ss_param(stan_data=stan_data,ss_param=ss_param,xmin_log,xmax_log)
	}
}
#' Plot Probability of Detection
#'
#' This function creates a plot of the probability of detection along the DNA concentration range,
#' based on the Stan model's output.
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M1`.
#' @param ss_param A vector of output (unknown) parameters produced by fitting
#'   the Stan model through `run_M1`. It can also be retrieved by `ss_param_extract`.
#' @param xmin_log A `numeric` value specifying the lower bound of the DNA concentration
#'   range to be plotted on a logarithmic scale. If not provided, it defaults to
#'   the floor of the minimum DNA concentration in `stan_data`.
#' @param xmax_log A `numeric` value specifying the upper bound of the DNA concentration
#'   range to be plotted on a logarithmic scale. If not provided, it defaults to
#'   the rounded maximum DNA concentration in `stan_data`.
#'
#' @return A `ggplot` object showing the probability of detection along the DNA concentration.
#' @export
#'
#' @examples
#' plot_ss_param_pd(stan_data = stan_data_M1, ss_param = ss_param)
plot_ss_param_pd <- function(stan_data,ss_param,xmin_log,xmax_log){
	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	samp_point <- cbind(stan_data$S_q,stan_data$Z_qst) %>% as.data.frame() %>% setNames(c("x","y"))
	pp <- data.frame(x = seq(xmin_log, xmax_log, by = 0.1)) %>% mutate(prob_d_logit=ss_param$mean[1]+(ss_param$mean[2]*x)) %>%
		mutate(prob_d=inverselogit(prob_d_logit)) %>%
		ggplot(.,aes(x=10^x,y=prob_d))+
		geom_line()+
		geom_jitter(data=samp_point,aes(x = 10^x,y=y),width = 0.09, height = 0.03,color="red")+
		ylab("Probability of detection")+
		theme_bw()+
		xlab("DNA concentration")+
		scale_x_log10(labels=scientific_10,breaks=c(10^seq(xmin_log,xmax_log,by=1)),lim=c(10^(xmin_log-0.1),10^(xmax_log+0.1)))
	return(pp)
}

#' Plot Continuous Model (Ct Values)
#'
#' This function creates a plot of the continuous model, showing the predicted cycle threshold (Ct) values
#' along the DNA concentration range, based on the Stan model's output.
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M1`.
#' @param ss_param A vector of output (unknown) parameters produced by fitting
#'   the Stan model through `run_M1`. It can also be retrieved by `ss_param_extract`.
#' @param xmin_log A `numeric` value specifying the lower bound of the DNA concentration
#'   range to be plotted on a logarithmic scale. If not provided, it defaults to
#'   the floor of the minimum DNA concentration in `stan_data`.
#' @param xmax_log A `numeric` value specifying the upper bound of the DNA concentration
#'   range to be plotted on a logarithmic scale. If not provided, it defaults to
#'   the rounded maximum DNA concentration in `stan_data`.
#'
#' @return A `ggplot` object showing the continuous model predictions and observed Ct values
#'   along the DNA concentration.
#' @export
#'
#' @examples
#' plot_ss_param_cm(stan_data = stan_data_M1, ss_param = ss_param)
plot_ss_param_cm <- function(stan_data,ss_param,xmin_log,xmax_log){
	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	samp_point <- cbind(stan_data$S_q_p,stan_data$R_qst) %>% as.data.frame() %>% setNames(c("x","y"))
	pp <- data.frame(x = seq(xmin_log, xmax_log, by = 0.1)) %>% mutate(ct=ss_param$mean[3]+(ss_param$mean[4]*x)) %>%
		mutate(sigma=exp(ss_param$mean[5]+(ss_param$mean[6] * x))) %>%
		mutate(ct_max=ct+sigma) %>%
		mutate(ct_min=ct-sigma) %>%
		ggplot()+
		geom_line(aes(x=10^x,y=ct))+
		geom_line(aes(x=10^x,y=ct_max),lty=2)+
		geom_line(aes(x=10^x,y=ct_min),lty=2)+
		geom_point(data=samp_point,aes(x = 10^x,y=y),color="red")+
		ylab("Cycle threshold (Ct)")+
		theme_bw()+
		xlab("DNA concentration")+
		scale_x_log10(labels=scientific_10,breaks=c(10^seq(xmin_log,xmax_log,by=1)),lim=c(10^(xmin_log-0.1),10^(xmax_log+0.1)))
	return(pp)
}

#' Plot Model Parameters
#'
#' This function creates a plot of the model parameters based on the Stan output,
#' including both the probability of detection and the continuous model along
#' the DNA concentration range.
#'
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M1`.
#' @param ss_param A vector of output (unknown) parameters produced by fitting
#'   the Stan model through `run_M1`. It can also be retrieved by `ss_param_extract`.
#' @param xmin_log A `numeric` value specifying the lower bound of the DNA concentration
#'   range to be plotted on a logarithmic scale. If not provided, it defaults to
#'   the floor of the minimum DNA concentration in `stan_data`.
#' @param xmax_log A `numeric` value specifying the upper bound of the DNA concentration
#'   range to be plotted on a logarithmic scale. If not provided, it defaults to
#'   the rounded maximum DNA concentration in `stan_data`.
#'
#' @return A `ggplot` object showing the probability of detection (left) and
#'   the continuous model (right) along the DNA concentration (x-axis).
#' @export
#'
#' @examples
#' # Plot with automatically determined DNA concentration range
#' plot_ss_param(stan_data = stan_data_M1, ss_param = ss_param)
#'
#' # Plot with a custom DNA concentration range
#' plot_ss_param(stan_data = stan_data_M1, ss_param = ss_param, xmin_log = -2, xmax_log = 5)
plot_ss_param <- function(stan_data,ss_param,xmin_log,xmax_log){
	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	cowplot::plot_grid(plot_ss_param_pd(stan_data,ss_param,xmin_log,xmax_log),
										 plot_ss_param_cm(stan_data,ss_param,xmin_log,xmax_log))
}

#' Prepare data for Stan Model 2
#'
#' This function prepares the data required to run Stan Model 2 by filtering
#' the input data, selecting the necessary columns, and formatting the data
#' to include both standard and unknown (environmental) qPCR samples.
#'
#' @param data A `data.frame` containing the qPCR data. It should include the
#'   Ct values, sample types (e.g., "STANDARD", "UNKNOWN"), sample names, and
#'   standard (initial) concentrations.
#' @param sample_type A `character` string specifying the name of the column in `data`
#'   that indicates the type of sample (e.g., "STANDARD" for standard samples
#'   and "UNKNOWN" for environmental samples).
#' @param Ct A `character` string specifying the name of the column in `data`
#'   that contains the Ct values. qPCR non-detects should be defined as 'Undetermined'
#'   and not NA or any other value.
#' @param sample_name_column A `character` string specifying the name of the column
#'   in `data` that contains the unique sample names or identifiers (index).
#' @param standard_concentration A `character` string specifying the name of the
#'   column in `data` that contains the standard concentrations for the standard samples.
#'   This column should be numeric and have NA values for environmental samples. If not it will be converted to numeric within the function.
#'
#' @return A list formatted as required by the Stan model.
#' @export
#'
#' @examples
#' stan_data_M2 <- prep_stan_M2(data = qpcr,
#'                              sample_type = "task",
#'                              Ct = "Ct",
#'                              sample_name_column = "sample_name",
#'                              standard_concentration = "standard_concentrations")
prep_stan_M2 <- function(data, sample_type, Ct, sample_name_column, standard_concentration) {
	qpcr_g <- data %>%
		rename(sample_type = .data[[sample_type]]) %>%
		rename(Ct = .data[[Ct]]) %>%
		rename(Sample_name = .data[[sample_name_column]]) %>%
		rename(st_conc = .data[[standard_concentration]])


	if (sum(qpcr_g$Ct=="Undetermined")==0) {
		cat("\n");questions("Are the undetermined CT values called \"Undetermined\"?",col = 31);cat("\n")
	}
	qpcr_g <- qpcr_g %>%
		mutate(Ct = replace(Ct, Ct == "Undetermined", NA)) %>%
		mutate(Ct = as.numeric(Ct)) %>%
		mutate(pres = 1, pres = replace(pres, is.na(Ct), 0))

	if (sum(grepl("STANDARD",qpcr_g$sample_type))==0) {
		cat("\n");questions("Are the standard samples included and called \"STANDARD\"?",col = 31);cat("\n")
	}

	if (sum(grepl("UNKNOWN",qpcr_g$sample_type))==0) {
		cat("\n");questions("Are the environmental samples included and called \"UNKNOWN\"?",col = 31);cat("\n")
	}

	e_qpcr <-  qpcr_g %>% filter(sample_type=="UNKNOWN")
	st_qpcr <- qpcr_g %>% filter(sample_type=="STANDARD") %>%
		mutate(st_conc=as.numeric(st_conc))

	e_qpcr <- mutate_indices(data = e_qpcr,
													 index_column = "Sample_name") %>%
		rename(sample_index="index")

	# st_qpcr <- mutate_indices(data = st_qpcr,
	# 													index_column = "Sample_name") %>%
	# 	rename(sample_index="index")

	e_qpcr_cm <- e_qpcr %>% filter(pres==1)
	st_qpcr_cm <- st_qpcr %>% filter(pres==1)

	label <- e_qpcr %>% distinct(sample_index,Sample_name) %>% arrange(sample_index) %>% as.data.frame()

	stan_data <- list(
		N_st_q = nrow(st_qpcr),
		N_en_q = nrow(e_qpcr),
		N_st_qp = nrow(st_qpcr_cm),
		N_en_qp = nrow(e_qpcr_cm),
		#
		N_j = le(e_qpcr$sample_index),
		#
		j_qen_idx = e_qpcr$sample_index,
		#
		j_qen_p_idx = e_qpcr_cm$sample_index,
		#
		Z_qst = as.integer(st_qpcr$pres),
		Z_qen = as.integer(e_qpcr$pres),
		S_q = log10(st_qpcr$st_conc),
		#
		R_qst = as.numeric(st_qpcr_cm$Ct),
		R_qen = as.numeric(e_qpcr_cm$Ct),
		S_q_p = log10(st_qpcr_cm$st_conc),
		#
		label_M2 = label
	)
	return(stan_data)
}

#' Plot estimated eDNA qPCR concentrations from model M2
#'
#' This function creates a plot of estimated DNA concentrations for single-species qPCR,
#' including error bars representing the uncertainty (standard deviation) around each estimate.
#' The y-axis can be displayed on a logarithmic scale if specified.
#'
#' @param est_ss_quant A `data.frame` containing the estimated DNA concentrations and their standard deviations derived from `est_ss_quant_extract` function.
#' @param scale A `character` string specifying whether the y-axis should be on a logarithmic scale.
#'   Defaults to `"log"`. If `scale = "log"`, the y-axis will be displayed on a logarithmic scale;
#'   otherwise, a linear scale will be used.
#'
#' @return A `ggplot` object showing the estimated DNA concentrations with error bars.
#'   If `scale = "log"`, the y-axis is scaled logarithmically and labeled using scientific notation.
#' @export
#'
#' @examples
#' # Plot the estimated DNA concentrations with a logarithmic y-axis
#' plot_est_ss_quant(est_ss_quant = est_ss_quant, scale = "log")
#'
#' # Plot the estimated DNA concentrations with a linear y-axis
#' plot_est_ss_quant(est_ss_quant = est_ss_quant)
plot_est_ss_quant <- function(est_ss_quant,scale="log"){
	ymin_log <- floor(min(est_ss_quant$C_est_log-est_ss_quant$C_est_log_sd))
	ymax_log <- round(max(est_ss_quant$C_est_log+est_ss_quant$C_est_log_sd))
	pp <- est_ss_quant %>%
		ggplot()+
		geom_point(aes(x=Sample_name,y=10^C_est_log))+
		geom_errorbar(aes(x=Sample_name,ymin=10^(C_est_log-C_est_log_sd),ymax=10^(C_est_log+C_est_log_sd)))+
		theme_bw()+
		ylab("DNA concentration")+
		xlab("Sample name")+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
	if(missing(scale)) {
		return(pp)
	} else if(scale=="log"){
		return(pp + scale_y_log10(labels=scientific_10,breaks=c(10^seq(ymin_log,ymax_log,by=1))))
	}
}

#' Run Stan Model 2
#'
#' This function runs Stan Model 2 using the prepared data to estimate the environmental DNA (eDNA) concentrations
#' of unknown environmental samples that have been processed through qPCR alongside standard samples of known concentrations.
#'
#' @param stan_object A `stanmodel` object representing the compiled Stan model (M2).
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M2`.
#' @param plot_fig A `logical` value indicating whether to generate plots of the
#'   model's outputs after fitting the model. Defaults to `TRUE`. If `TRUE`, the
#'   function will generate and display the output plots; if `FALSE`, no plots
#'   will be produced.
#'
#' @return An object of class `stanfit` containing the results of the model fitting.
#' The model runs 4 chains, 5000 iterations and burns 2000 for warm-up. For changing these values run the analog version of this function
#' @export
#'
#' @examples
#' stanMod_2 <- run_M2(stan_object = M2, stan_data = stan_data_M2)
run_M2 <- function(stan_object=M2, stan_data=stan_data_M2,plot_fig=T){
	if(!inherits(stan_object, "stanmodel")){
		stop('\nThe provided stan_object is not a stanmodel. \nPlease load the correct model.')
	}
	xmin_log <- floor(min(stan_data_M2$S_q))
	xmax_log <- round(max(stan_data_M2$S_q))
	stanMod_2 <<- sampling(
		object = stan_object,
		chains = 4,
		iter = 5000,
		warmup = 2000,
		data = stan_data
	)

	ss_param <<- ss_param_extract(stanMod = stanMod_2)

	est_ss_quant <<- est_ss_quant_extract(stanMod_2)

	if (plot_fig==T) {
	cowplot::plot_grid(plot_ss_param(stan_data_M2,ss_param,xmin_log,xmax_log),
										 plot_est_ss_quant(est_ss_quant),nrow=2)
	}
}

#' Prepare Data for Stan Model 3
#'
#' This function prepares the data required to run Stan Model 3 by selecting and formatting
#' the necessary columns. Model 3 (M3) uses ONLY mock community sequencing data and their initial
#' concentrations to estimate the amplification efficiency (α) relative to a reference species.
#'
#' @param data A `data.frame` containing the mock community data, including columns for
#'   sequencing counts, initial concentrations, species indices, and species names. See more in 'data_example(data = M3)'.
#' @param mock_species_names A `character` string specifying the name of the column(s) in `data`
#'   that contains the species names in the mock community.
#' @param mock_sequencing_columns A `character` string or a `vector` of characters specifying the
#'   columns in `data` that contain the sequencing counts for the mock community species.
#' @param mock_initial_concentration A `character` string specifying the name of the column in `data`
#'   that contains the initial concentrations of the mock community species. This column should be numeric.
#'   Note that this model only runs with ONE mock mix (one set of initial proportions for the mock cocktail mix).
#' @param species_index A `character` string specifying the name of the column in `data`
#'   that contains the indices or identifiers for the species in the mock community.
#' @param number_of_PCR A `numeric` value specifying the number of PCR cycles conducted during the experiment.
#'   Defaults to `40`.
#' @param tau_mu A numeric value specifying the mean of the prior distribution for the
#'   error term (η) in the model. Defaults to `1000`. It is recommended to keep `tau_mu` and `tau_sd` in a 1:10 ratio, but you may adjust these values as needed.
#' @param tau_sd A numeric value specifying the standard deviation of the prior distribution
#'   for the error term (η) in the model. Defaults to `10000`.
#'
#' @return A list formatted as required by Stan Model 3, ready for input into the model.
#' The computations in this model run in natural log space (ln).
#' @export
#'
#' @examples
#' stan_data_M3 <- prep_stan_M3(data = mock_data,
#'                              mock_species_names = "species_names",
#'                              mock_sequencing_columns = c("mock_samp_1", "mock_samp_2", "mock_samp_3"),
#'                              mock_initial_concentration = "initial_concentration",
#'                              species_index = "species_index",
#'                              number_of_PCR = 40,
#'                              tau_mu = 1000,
#'                              tau_sd = 10000)
prep_stan_M3 <- function(data,
												 mock_species_names,
												 mock_sequencing_columns,
												 mock_initial_concentration,
												 species_index,
												 number_of_PCR=40,
												 tau_mu=1000,
												 tau_sd=10000) {
	mock <- data %>%
		select({{ mock_sequencing_columns }})
	ini_mock <- data %>%
		select({{ mock_initial_concentration }}) %>% setNames("i_c")
	species_idx <- data %>%
		select({{ species_index }})
	mock_sp_names <- data %>%
		select({{ mock_species_names }})

	ini_mock$prop <- galr(ini_mock[,1],log="e")

	stan_data <- list(
		############## Integers
		N_sp_i_MC2 = nrow(species_idx),
		N_obs_Y_M5 = ncol(mock),
		############## Data
		alr_M5 = ini_mock$prop,
		Y_M5 = mock,
		NPCR = number_of_PCR,
		############## Parameters
		tau_eta_mock_mu = tau_mu, #increase or decrease these values for better convergance (relaxes/tighten the error term η)
		tau_eta_mock_sd = tau_sd, #increase or decrease these values for better convergance (relaxes/tighten the error term η)
		############## Other parameters
		Species = mock_sp_names,
		mock_initial_conc = ini_mock,
		species_idx = species_idx
	);str(stan_data)
	return(stan_data)
}

#' Prepare Data for Stan Model 3_2
#'
#' This function prepares the data required to run Stan Model 3 by selecting and formatting
#' the necessary columns. Model 3 (M3) uses ONLY mock community sequencing data and their initial
#' concentrations to estimate the amplification efficiency (α) relative to a reference species.
#'
#' @param data A `data.frame` containing the mock community data, including columns for
#'   sequencing counts, initial concentrations, species indices, and species names. See more in 'data_example(data = M3)'.
#' @param mock_species_names A `character` string specifying the name of the column(s) in `data`
#'   that contains the species names in the mock community.
#' @param mock_sequencing_columns A `character` string or a `vector` of characters specifying the
#'   columns in `data` that contain the sequencing counts for the mock community species.
#' @param mock_initial_concentration A `character` string specifying the name of the column in `data`
#'   that contains the initial concentrations of the mock community species. This column should be numeric.
#'   Note that this model only runs with ONE mock mix (one set of initial proportions for the mock cocktail mix).
#' @param species_index A `character` string specifying the name of the column in `data`
#'   that contains the indices or identifiers for the species in the mock community.
#' @param number_of_PCR A `numeric` value specifying the number of PCR cycles conducted during the experiment.
#'   Defaults to `40`.
#' @param tau_mu A numeric value specifying the mean of the prior distribution for the
#'   error term (η) in the model. Defaults to `1000`. It is recommended to keep `tau_mu` and `tau_sd` in a 1:10 ratio, but you may adjust these values as needed.
#' @param tau_sd A numeric value specifying the standard deviation of the prior distribution
#'   for the error term (η) in the model. Defaults to `10000`.
#'
#' @return A list formatted as required by Stan Model 3, ready for input into the model.
#' The computations in this model run in natural log space (ln).
#' @export
#'
#' @examples
		#' stan_data_M3 <- prep_stan_M3_2(data = mock_data,
#'                              mock_species_names = "species_names",
#'                              mock_sequencing_columns = c("mock_samp_1", "mock_samp_2", "mock_samp_3"),
#'                              mock_initial_concentration = "initial_concentration",
#'                              species_index = "species_index",
#'                              number_of_PCR = 40,
#'                              tau_mu = 1000,
#'                              tau_sd = 10000)
prep_stan_M3_2 <- function(data,
												 mock_species_names,
												 mock_sequencing_columns,
												 mock_initial_concentration,
												 species_index,
												 number_of_PCR=40,
												 alpha_magnitude=0.001) {
	mock <- data %>%
		select({{ mock_sequencing_columns }})
	ini_mock <- data %>%
		select({{ mock_initial_concentration }}) %>% setNames("i_c")
	species_idx <- data %>%
		select({{ species_index }})
	mock_sp_names <- data %>%
		select({{ mock_species_names }})

	ini_mock$prop <- galr(ini_mock[,1],log="e")

	stan_data <- list(
		############## Integers
		N_sp_i_MC2 = nrow(species_idx),
		N_obs_Y_M5 = ncol(mock),
		############## Data
		alr_M5 = ini_mock$prop,
		Y_M5 = mock,
		NPCR = number_of_PCR,
		############## Parameters
		alpha_magnitude = alpha_magnitude, #increase or decrease this value for easier fit
		############## Other parameters
		Species = mock_sp_names,
		mock_initial_conc = ini_mock,
		species_idx = species_idx
	);str(stan_data)
	return(stan_data)
}

plot_amp_eff <- function(amp_eff_output){
	n_sp <- nrow(amp_eff_output)
	plot1_data <- amp_eff_output %>% select("Pre-PCR","Post-PCR") %>% trans() %>%
		setNames(c("Species","Sample","Reads"))

	plot1_data$Sample <- factor(plot1_data$Sample, levels=c("Pre-PCR", "Post-PCR"))

	p1 <- ggplot(plot1_data,aes(fill=Species,y=Reads,x=Sample))+
		geom_bar(position="stack", stat="identity")+
		scale_fill_manual(values = moma.colors("Lupi", n=n_sp))+
		theme_bw()+
		theme(legend.position = "none")+
		labs(y = "Proportional abundance (%)", x = "")

	p1_leg <-
		suppressWarnings(
			cowplot::get_legend(p1 +
														theme(legend.position = "right",
																	axis.title.x = element_text(size = 10),
																	text = element_text(size = 13),
																	legend.text = element_text(face = "italic")))
		)


	plot2_data <- plot1_data %>% mutate(line_t=if_else(Species==rownames(amp_eff_output)[nrow(amp_eff_output)],"solid","dashed"))

	p2 <- ggplot(data = plot2_data %>% filter(Sample %in% c("Pre-PCR", "Post-PCR")),
							 aes(x = Sample, y = Reads, group = Species)) +
		geom_line(aes(linetype = line_t),
							color = rep(moma.colors("Lupi", n = n_sp), each = 2),
							size = 0.7) +
		geom_point(color = rep(moma.colors("Lupi", n = n_sp), 2), size = 2) +
		scale_x_discrete(expand = c(0, 0.08)) +
		theme_classic() +  # theme_bw()+
		labs(y = "Proportional abundance (%)", x = "") +
		theme(
			legend.position = "none",
			text = element_text(size = 13),
			axis.title.x = element_text(size = 12),
			# axis.text.x = element_text(angle = 90),
			legend.text = element_text(face = "italic"),
			plot.margin = unit(c(0, 0, 0, 0.3), "cm")
		)
	p2_leg <- suppressWarnings(
		cowplot::get_legend(
			ggplot(data.frame(x = c(1, 2), y = c(1, 2))) +
				geom_line(aes(x = x, y = y, color = "Reference species"), size = 1) +
				theme_bw() +
				scale_color_manual(
					name = "",
					breaks = c("Reference species"),
					values = c("black"),
					guide = guide_legend(
						override.aes = list(lty = 2,
																size = 2))) +
				theme(legend.position = "left",
							legend.key.width = unit(1.2, "cm"),
							axis.title.x = element_text(size = 10),
							axis.text.x = element_text(angle = 90),
							text = element_text(size = 13))))

	p_leg_1 <- suppressWarnings(cowplot::plot_grid(p1_leg,p2_leg,ncol = 1,rel_heights = c(1,1),align = "v"))

	plot3_data <-
		amp_eff_output %>% select("amp_eff", "amp_eff_lo", "amp_eff_up") %>% rownames_to_column("Species") %>%
		mutate(co=moma.colors("Lupi",nrow(.))) %>%
		arrange(desc(row_number())) %>%
		mutate(Species=factor(Species, levels=c(Species)))

	p3 <-
		plot3_data %>%
		ggplot()+
		geom_point(aes(x=amp_eff,y=Species),color=plot3_data$co,size=2)+
		geom_errorbar(aes(y=Species,xmin=amp_eff_lo,xmax=amp_eff_up),
									color=plot3_data$co,width=0.1)+
		theme_bw()+
		labs(x = "Amplification efficiency \nrelative to reference species")+
		labs(y = "Species")+
		theme(axis.text.y = element_text(face = "italic"),
					text = element_text(size = 13))+
		geom_vline(xintercept = 1,linetype="dashed",color=plot3_data$co[1])

	pp1 <- cowplot::plot_grid(p1,p2,p3,p_leg_1,nrow = 1,align = "h",rel_widths = c(2,3,6,2))
	return(pp1)
}


plot_mock_variance <- function(mock_variance){
	plot4_data <- mock_variance %>% arrange(desc(row_number())) %>%
		trans() %>% setNames(c("Species","Sample","prop_diff")) %>%
		mutate(co=if_else(prop_diff<0,"tomato2","grey")) %>%
		mutate(co=if_else(prop_diff>0,"deepskyblue2",co)) %>%
		mutate(prop_diff=abs(prop_diff))

	p4 <- ggplot(data=plot4_data)+
		geom_point(aes(x = as.factor(Sample), y = Species, size = prop_diff),color=plot4_data$co)+
		scale_size_continuous(range = c(2, 10))+
		labs(x = "Sample", size = "Difference in %\nbetween modeled\nand observed")+
		labs(y="")+
		theme_classic()+
		theme(axis.text.x = element_text(angle = 90),
					legend.position = "none")+
		labs(title=expression("Error term " * eta * " expressed as difference in %"))

	p4_leg <- suppressWarnings(
		cowplot::get_legend(
			p4+
				theme(legend.position = "right",
							axis.title.x = element_text(size = 10),
							axis.text.x = element_text(angle = 90),
							text = element_text(size = 13)))
	)

	p5_leg <- suppressWarnings(
		cowplot::get_legend(
			ggplot() +
				geom_point(aes(x = NA, y = NA, color = "Underestimation"), size = 1) +
				geom_point(aes(x = NA, y = NA, color = "Overestimation"), size = 1) +
				theme_bw()+
				scale_color_manual(
					name="Model",
					breaks=c("Underestimation","Overestimation"),
					values = c("tomato2","deepskyblue2"),
					guide = guide_legend(
						override.aes = list(lty = c(1,1),
																size = c(4,4)))) +
				theme(legend.position = "right",
							legend.key.width = unit(1.2,"cm"),
							legend.key.height = unit(0.6,"cm"),
							axis.title.x = element_text(size = 10),
							axis.text.x = element_text(angle = 90),
							text = element_text(size = 13),
							legend.justification = c(0.5,3)
				))
	)

	p_leg_2 <- cowplot::plot_grid(p4_leg,p5_leg,ncol = 1,rel_heights = c(4,1))

	pp2 <- cowplot::plot_grid(p4,p_leg_2,nrow = 1,rel_widths = c(7,1))
	return(pp2)
}

run_M3 <- function(stan_object=M3,stan_data=stan_data_M3,plot_fig=T){
	if(!inherits(stan_object, "stanmodel")){
		stop('\nThe provided stan_object is not a stanmodel. \nPlease load the correct model.')
	}
	stanMod_3 <<- sampling(
		object = stan_object,
		chains = 4,
		iter = 5000,
		warmup = 2000,
		data = stan_data
	)

	amp_eff_param <<- amp_eff_param_extract(stanMod = stanMod_3)

	amp_eff_output <<- amp_eff_output_extract(stan_data = stan_data,amp_eff_param = amp_eff_param)

	mock_variance <<- mock_variance_extract(stan_data = stan_data,stanMod = stanMod_3,amp_eff_param = amp_eff_param)

	if (plot_fig==T) {
	return(plot_amp_eff(amp_eff_output))
	}
}

prep_stan_M4 <- function(data,
												 otu_data,
												 otu_species_names,
												 sample_index,
												 number_of_PCR = 40,
												 tau_en_mu=1000,
												 tau_en_sd=10000,
												 ini_prop_mu=0,
												 ini_prop_sd=5) {
	mock <- data$Y_M5
	ini_mock <- data$mock_initial_conc
	species_idx <- data$species_idx
	mock_sp_names <- data$Species
	if(missing(sample_index)) {
		st_idx <- c(1:(ncol(otu_data)-1))
		sample_index <- "missing index"
		} else {
			st_idx <- as.integer(as.factor(sample_index))
			}
	if(missing(sample_index)) {
		sample_index <- c(1:(ncol(otu_data)-1))
	} else {
		sample_index <- as.integer(as.factor(sample_index))
	}
	otu_sp_names <- otu_data %>%
		select({{ otu_species_names }})
	otu <- otu_data %>%
		select(-{{ otu_species_names }})

	l1 <- mock_sp_names$Species %notin% otu_sp_names$Species
	if(sum(l1)==0){
		cat("\n");cat("All species in mock commuinty samples are present in OTU table");cat("\n")}else{
			cat("\n");cat("NOT all species found in mock commuinty samples are present in OTU table");cat("\n")
			cat("Removing species from mock community samples that are NOT present in OTU table...");cat("\n")
			cat("...");cat("\n")
			questions("Removed the following species from mock commuinty samples",col=31);cat("\n")
			writeLines(paste0(c(1:(length(mock_sp_names[l1,]))),".",mock_sp_names[l1,]));cat("\n")
		}

	l2 <- otu_sp_names$Species %notin% mock_sp_names$Species
	if(sum(l2)==0){
		cat("\n");cat("All species in OTU table are present in mock commuinty samples");cat("\n")}else{
			cat("\n");cat("NOT all species found in OTU table are present in mock commuinty samples");cat("\n")
			cat("Removing species from OTU table that are NOT present in mock commuinty samples...");cat("\n")
			cat("...");cat("\n")
			questions("Removed the following species from OTU table",col=31);cat("\n")
			writeLines(paste0(c(1:(length(otu_sp_names[l2,]))),".",otu_sp_names[l2,]));cat("\n")
		}

	mock_sp_names <- mock_sp_names %>% filter(!l1)
	otu_sp_names <- otu_sp_names %>% filter(!l2)

	mock <- mock %>% filter(!l1)
	ini_mock <- ini_mock %>% filter(!l1)
	species_idx <- species_idx %>% filter(!l1)
	otu <- otu %>% filter(!l2)

	ini_mock$prop <- galr(ini_mock$i_c,log="e")

	if(sum(diff(species_idx$idx)>1)>0){
		cat("\n");questions("The indexing is not continuous",31);cat("\n")
		cat("Making the index continuous...");cat("\n")
		species_idx <- c(1:nrow(species_idx)) %>% as.data.frame() %>% setNames("idx")
	}

	label <- cbind(as.data.frame(st_idx) %>% setNames("sample_index"),
		as.data.frame(colnames(otu)) %>% setNames("Sample_name")
	)

	stan_data <- list(
		############## Integers
		N_st_M6 = max(st_idx),
		st_idx_M6 = st_idx,
		N_sp_i_MC2 = nrow(species_idx),
		N_obs_Y_M5 = ncol(mock),
		N_obs_Y_M6 = ncol(otu),
		############## Data
		alr_M5 = ini_mock$prop,
		Y_M5 = mock,
		Y_M6 = otu,
		NPCR = number_of_PCR,
		############## Parameters
		tau_eta_mock_mu = data$tau_eta_mock_mu, #increase or decrease these values for better convergence (relaxes/tighten the error term η)
		tau_eta_mock_sd = data$tau_eta_mock_sd, #increase or decrease these values for better convergence (relaxes/tighten the error term η)
		tau_eta_samp_mu = tau_en_mu, #increase or decrease these values for better convergence (relaxes/tighten the error term η)
		tau_eta_samp_sd = tau_en_sd, #increase or decrease these values for better convergence (relaxes/tighten the error term η)
		ini_prop_mu = ini_prop_mu, #increase or decrease these values for better convergence (relaxes/tighten the error term η)
		ini_prop_sd = ini_prop_sd, #increase or decrease these values for better convergence (relaxes/tighten the error term η)
		############## Other parameters
		Species = mock_sp_names,
		species_idx = species_idx,
		Station_name_indexed = sample_index,
		label_M4 = label
	)
	return(stan_data)
}

plot_est_ini_prop <- function(est_ini_prop,stan_data){
	plot1_data <- est_ini_prop %>% cbind(.,stan_data$Species) %>% rownames_to_column("x") %>% select(-x) %>%
		column_to_rownames("Species") %>% exp() %>% rel.ab() %>% trans() %>% setNames(c("Species","Sample","prop"))

	p1 <- ggplot(plot1_data,aes(fill=Species,y=prop,x=Sample))+
		geom_bar(position="stack", stat="identity")+
		scale_fill_manual(values = moma.colors("Lupi", n=nrow(est_ini_prop)))+
		theme_bw()+
		labs(y = "Proportional abundance (%)", x = "",
				 title = "Estimated initial proportional abundance")+
		theme(legend.position = "right",
					axis.title.x = element_text(size = 10),
					# axis.text.x = element_text(angle = 90),
					text = element_text(size = 13),
					legend.text = element_text(face = "italic"))

	return(p1)
}
plot_obs_prop <- function(stan_data) {
	plot1_data <- stan_data$Y_M6 %>% cbind(.,stan_data$Species) %>%
		column_to_rownames("Species") %>% 
		# setNames(stan_data$Station_name_indexed) %>%
		rel.ab() %>% trans() %>% setNames(c("Species","Sample","prop"))

	p1 <- ggplot(plot1_data,aes(fill=Species,y=prop,x=Sample))+
		geom_bar(position="stack", stat="identity")+
		scale_fill_manual(values = moma.colors("Lupi", n=nrow(est_ini_prop)))+
		theme_bw()+
		labs(y = "Proportional abundance (%)", x = "",
				 title = "Observed proportional abundance")+
		theme(legend.position = "right",
					axis.title.x = element_text(size = 10),
					# axis.text.x = element_text(angle = 90),
					text = element_text(size = 13),
					legend.text = element_text(face = "italic"))
	return(p1)
}
plot_sample_variance <- function(sample_variance){
	plot2_data <- sample_variance %>% arrange(desc(row_number())) %>%
		trans() %>% setNames(c("Species","Sample","prop_diff")) %>%
		mutate(co=if_else(prop_diff<0,"tomato2","grey")) %>%
		mutate(co=if_else(prop_diff>0,"deepskyblue2",co)) %>%
		mutate(prop_diff=abs(prop_diff))

	p2 <- ggplot(data=plot2_data)+
		geom_point(aes(x = as.factor(Sample), y = Species, size = prop_diff),color=plot2_data$co)+
		scale_size_continuous(range = c(2, 10))+
		labs(x = "Sample", size = "Difference in %\nbetween modeled\nand observed")+
		labs(y="")+
		theme_classic()+
		theme(axis.text.x = element_text(angle = 90),
					legend.position = "none")+
		labs(title=expression("Error term " * eta * " expressed as difference in %"))

	p2_leg <- cowplot::get_legend(
		p2+
			theme(legend.position = "right",
						axis.title.x = element_text(size = 10),
						axis.text.x = element_text(angle = 90),
						text = element_text(size = 13)))

	p3_leg <- cowplot::get_legend(
		ggplot() +
			geom_point(aes(x = NA, y = NA, color = "Underestimation"), size = 1) +
			geom_point(aes(x = NA, y = NA, color = "Overestimation"), size = 1) +
			theme_bw()+
			scale_color_manual(
				name="Model",
				breaks=c("Underestimation","Overestimation"),
				values = c("tomato2","deepskyblue2"),
				guide = guide_legend(
					override.aes = list(lty = c(1,1),
															size = c(4,4)))) +
			theme(legend.position = "right",
						legend.key.width = unit(1.2,"cm"),
						legend.key.height = unit(0.6,"cm"),
						axis.title.x = element_text(size = 10),
						axis.text.x = element_text(angle = 90),
						text = element_text(size = 13),
						legend.justification = c(0.5,3)
			))

	p_leg_2 <- cowplot::plot_grid(p2_leg,p3_leg,ncol = 1,rel_heights = c(4,1))

	pp2 <- cowplot::plot_grid(p2,p_leg_2,nrow = 1,rel_widths = c(7,1))

	return(pp2)
}
run_M4 <- function(stan_object=M4,
									 stan_data = stan_data_M4,
									 plot_fig=T){

	stanMod_4 <<- sampling(
		object = stan_object,
		chains = 4,
		iter = 5000,
		warmup = 2000,
		data = stan_data
	)

	amp_eff_param <<- amp_eff_param_extract(stanMod = stanMod_4)

	amp_eff_output <<- amp_eff_output_extract(stan_data = stan_data_M4,amp_eff_param = amp_eff_param)

	est_ini_prop <<- est_ini_prop_extract(stanMod = stanMod_4)

	mock_variance <<- mock_variance_extract(stan_data = stan_data_M4,stanMod = stanMod_4,amp_eff_param = amp_eff_param)

	sample_variance <<- sample_variance_extract(est_ini_prop = est_ini_prop,amp_eff_param = amp_eff_param,stan_data = stan_data_M4,stanMod = stanMod_4)

	if (plot_fig==T) {
		return(plot_est_ini_prop(est_ini_prop = est_ini_prop,stan_data = stan_data_M4))
	}
}

prep_stan_M5_1 <- function(data,ss_conc) {
	data$ss_conc=ss_conc
	str(data)
	return(data)
}
plot_est_ini_conc <- function(est_ini_conc, est_ini_conc_sd, stan_data, k=2){
	if(sum(stan_data$Station_name_indexed=="missing index")==1){
		est_ini_conc <- est_ini_conc %>% setNames(colnames(stan_data$Y_M6))
		est_ini_conc_sd <- est_ini_conc_sd %>% setNames(colnames(stan_data$Y_M6))
	}

	plot1_data_1 <- est_ini_conc %>% cbind(.,stan_data$"Species") %>% column_to_rownames("Species") %>%
		trans() %>% setNames(c("Species","Sample","C"))
	plot1_data_2 <- est_ini_conc_sd %>% cbind(.,stan_data$"Species") %>% column_to_rownames("Species") %>%
		trans() %>% setNames(c("Species","Sample","C_sd"))

	plot1_data <- left_join(plot1_data_1,plot1_data_2,by = c("Species" = "Species","Sample" = "Sample")) %>%
		mutate(c_lo=C-C_sd) %>%
		mutate(c_up=C+C_sd)

	nr <- nrow(est_ini_conc)*k
	nudge_vector <- c(1:nrow(est_ini_conc))*(1/nr)-(max(c(1:nrow(est_ini_conc))*(1/nr))/2)
	p1 <- ggplot(data=plot1_data)+
		geom_point(aes(x=Sample,y=C,color=Species),position = position_nudge(x = nudge_vector))+
		geom_linerange(aes(x=Sample, ymin=c_lo,ymax=c_up,color=Species),position = position_nudge(x = nudge_vector))+
		scale_color_manual(values = moma.colors("Lupi", n=le(plot1_data$Species)))+
		theme_bw()+
		theme(legend.text = element_text(face = "italic"),
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
		labs(y=expression("Log"[e]*"(DNA Concentration)"),x="Sample")

	return(p1)
}
run_M5_1 <- function(stan_data_M5_1,chains,iterations,warmup,plot_fig=T){
	if(missing(plot_fig)) plot_fig <- F
	if(missing(chains)) chains <- 4
	if(missing(iterations)) iterations <- 10000
	if(missing(warmup)) warmup <- 5000

	stanMod_5_1 <<- stan(
		file = "M5_1.stan",
		model_name = "M5.1 model",
		chains = chains,
		iter = iterations,
		warmup = warmup,
		data = stan_data_M5_1
	)

	amp_eff_param <<- amp_eff_param_extract(stanMod = stanMod_5_1)

	amp_eff_output <<- amp_eff_output_extract(stan_data = stan_data_M5_1,amp_eff_param = amp_eff_param)

	est_ini_conc <<- est_ini_conc_extract_5_1(stanMod_5_1,stan_data_M5_1)
	est_ini_conc_sd <<- est_ini_conc_sd_extract_5_1(stanMod_5_1) %>% rbind(.,rep(0,ncol(.)))

	mock_variance <<- mock_variance_extract(stan_data = stan_data_M5_1,stanMod = stanMod_5_1,amp_eff_param = amp_eff_param)

	sample_variance <<- sample_variance_extract(est_ini_prop = est_ini_conc,amp_eff_param = amp_eff_param,stan_data = stan_data_M5_1,stanMod = stanMod_5_1)

	if (plot_fig==T) {
		plot_est_ini_conc(est_ini_conc, stan_data_M5_1, k=5)
		}
}

prep_stan_M5_2 <- function(stan_data_M2,stan_data_M4){
	output <- c(stan_data_M2, stan_data_M4)
	return(output)
}
run_M5_2 <- function(stan_object=M5_2,
										 stan_data=stan_data_M5_2,
										 plot_fig=T){

	stanMod_5_2 <<- sampling(
		object = M5_2,
		chains = 4,
		iter = 5000,
		warmup = 2000,
		data = stan_data
	)

	amp_eff_param <<- amp_eff_param_extract(stanMod = stanMod_5_2)

	amp_eff_output <<- amp_eff_output_extract(stan_data = stan_data_M5_2,amp_eff_param = amp_eff_param)

	est_ini_conc <<- est_ini_conc_extract_5_2(stanMod_5_2,stan_data_M5_2)
	est_ini_conc_sd <<- est_ini_conc_sd_extract_5_2(stanMod_5_2,stan_data_M5_2)

	mock_variance <<- mock_variance_extract(stan_data = stan_data_M5_2,stanMod = stanMod_5_2,amp_eff_param = amp_eff_param)

	sample_variance <<- sample_variance_extract(est_ini_prop = est_ini_conc,amp_eff_param = amp_eff_param,stan_data = stan_data_M5_2,stanMod = stanMod_5_2)

	if (plot_fig==T) {
		plot_est_ini_conc(est_ini_conc, est_ini_conc_sd, stan_data_M5_2, k=3)
	}
}

arrange_by_sp_idx <- function(data, species_idx) {
	data <- data %>%
		left_join(., species_idx, by = "Species") %>%
		arrange(idx) %>%
		select(-idx) %>%
		column_to_rownames("Species")
	return(data)
}

galr <- function(x,log="e"){
	temp <- vector("numeric",length(x))
	for (i in 1:length(x)) {
		if(log==10){
			temp[i] <- log10(x[i]/x[length(x)])
		}
		else if(log=="e"){
			temp[i] <- log(x[i]/x[length(x)])
		}
	}
	return(temp)
}
galr_inv_m <- function(data,log="e"){
	if(log=="e"){
		temp <- data %>% rbind(.,rep(0,ncol(.))) %>% exp()
	} else if(log==10){
		temp <- data %>% rbind(.,rep(0,ncol(.))) %>% mutate_all(~10^.)
	}
	return(temp)
}
galr_inv <- function(data,log="e"){
	if(log=="e"){
		temp <- exp(c(data,0))
	} else if(log==10){
		temp <- 10^(c(data,0))
	}
	return(temp)
}
logsumexp <- function (x) {
	y = max(x)
	y + log(sum(exp(x - y)))
}
softmax <- function (x) {
	exp(x - logsumexp(x))
}
softmax_m <- function (data) {
	temp <- as.data.frame(matrix(NA,ncol = ncol(data),nrow = nrow(data)))
	for (i in 1:ncol(data)) {
		x <- data[,i]
		temp[,i] <- exp(x - logsumexp(x))
	}
	return(temp)
}
galr_inv_m_2 <- function(data,vector){
	temp <- data %>% rbind(.,vector) %>% exp()
	return(temp)
}


make_matrix <- function(x,n){
	return(matrix(x,length(x),n))
}
expand_matrix_by_idx <- function(matrix,idx){
	output <- data.frame(matrix(NA,nrow(matrix),length(idx)))
	for (i in 1:length(idx)) {
		output[,i] <- matrix[,idx[i]]
	}
	colnames(output) <- idx
	return(output)
}
remove_X_from_numeric_column <- function(data){
	colnames(data) <- gsub("X([0-9]+)", "\\1",colnames(data))
	return(data)
}
mean_by_idx_2 <- function(input,idx,lower=0.25,upper=0.75){
	mean_dt <- NA
	sd_dt <- NA
	q_lower <- NA
	q_upper <- NA
	for (i in 1:le(idx)){
		mean_dt[i] <- mean(input[idx==i])
		sd_dt[i] <- sd(input[idx==i])
		q_lower[i] <- quantile(input[idx==i],p=lower)
		q_upper[i] <- quantile(input[idx==i],p=upper)
	}
	return(as.data.frame(cbind(mean_dt,sd_dt,q_lower,q_upper)))
}
mean_by_idx <- function(input,idx){
	mean <- NA
	for (i in 1:le(idx)){
		mean[i] <- mean(input[idx==i])
	}
	return(as.data.frame(mean))
}

post_explore_matrix_M4 <- function(stanmod,param){
	post_explore_data <<- extract_matrix(stanmod,param,'mean') %>% trans() %>% 
		cbind(.,
					extract_matrix(stanmod,param,'2.5%') %>% trans() %>% select(Z) %>% rename(q2="Z"),
					extract_matrix(stanmod,param,'97.5%') %>% trans() %>% select(Z) %>% rename(q98="Z")
		) %>% mutate(X=as.numeric(X)) %>% 
		left_join(.,cbind(stan_data_M4$species_idx,stan_data_M4$Species),
							by=c('X'='species_idx')) %>% 
		mutate(Y=as.numeric(Y))
	
	p <-
		post_explore_data %>% 
		ggplot(aes(x=Y,y=Z,colour = Species))+
		geom_point()+
		geom_errorbar(aes(ymin = q2,ymax = q98))+
		scale_color_manual(values = moma.colors("Lupi", n=le(post_explore_data$Species)))+
		theme_bw()+
		ylab('Parameter distribution')+
		xlab('Sample_idx')
	return(p)
}


# Model's code ------------------------------------------------------------

# M1 stan code ------------------------------------------------------------
# M1_stan_code <- "
# data {
#   int N_st_q; // Total number of observation in qPCR standard samples
#   int N_st_qp; // Total number of observation in qPCR standard samples for only detected samples
#   int Z_qst[N_st_q]; // Presence/Absence response of qPCR standard data
#   real S_q[N_st_q]; // Known concentration (log10) in qPCR data
#   real R_qst[N_st_qp]; // Ct values of qPCR standard data for only detected samples
#   real S_q_p[N_st_qp]; // Known concentration (log10) in qPCR data for only detected samples
# }
# parameters {
#   real alpha_0;
#   real alpha_1;
#   real eta_0;
#   real eta_1;
#   real gamma_0;
#   real<upper=0> gamma_1;
# }
# transformed parameters{
#   vector[N_st_q] theta_st;
#   vector[N_st_qp] mu_st;
#   vector[N_st_qp] sigma_st;
#   for (i in 1:N_st_q){
#     theta_st[i] = alpha_0 + (alpha_1 * S_q[i]);
#   }
#   for (i in 1:N_st_qp){
#     mu_st[i] = eta_0 + (eta_1 * S_q_p[i]);
#     sigma_st[i] = exp(gamma_0+(gamma_1 * S_q_p[i]));
#   }
# }
# model {
#   Z_qst ~ bernoulli(inv_logit(theta_st));
#   R_qst ~ normal(mu_st,sigma_st);
#   alpha_0 ~ normal(0, 2);
#   alpha_1 ~ normal(0, 2);
#   eta_0 ~ normal(0, 3);
#   eta_1 ~ normal(-3, 0.1);
#   gamma_0 ~ normal(0, 0.1);
#   gamma_1 ~ normal(0, 0.1);
# }
# "
M1_stan_code <- "
data {
  int N_st_q; // Total number of observation in qPCR standard samples
  int N_st_qp; // Total number of observation in qPCR standard samples for only detected samples
  int Z_qst[N_st_q]; // Presence/Absence response of qPCR standard data
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
  alpha_0 ~ normal(0, 2);
  alpha_1 ~ normal(0, 2);
  eta_0 ~ normal(0, 3);
  eta_1 ~ normal(-3, 0.1);
  gamma_0 ~ normal(0, 0.1);
  gamma_1 ~ normal(0, 0.1);
}
"


# M2 stan code ------------------------------------------------------------
# M2_stan_code <- "
# data {
#   //Numbers of dimentions
#   int N_st_q; // Total number of observation in qPCR standard samples
#   int N_en_q; // Total number of observation in qPCR environmental samples
#   int N_st_qp; // Total number of observation in qPCR standard samples for only detected samples
#   int N_en_qp; // Total number of observation in qPCR environmental samples for only detected samples
#   int N_j; // Number of samples in environmental data
#   //
#   //Indexes
#   // // // Binomial model
#   int j_qen_idx[N_en_q]; // Species and standard index for qPCR environmental samples
#   // // // Continious model
#   int j_qen_p_idx[N_en_qp]; // Species and standard index for qPCR environmental samples
#   // Data
#   // // // Binomial model
#   int Z_qst[N_st_q]; // Presence/Absence response of qPCR standard data
#   int Z_qen[N_en_q]; // Presence/Absence response of qPCR environmental data
#   real S_q[N_st_q]; // Known concentration (log10) in qPCR data
#   // // // Continious model
#   real R_qst[N_st_qp]; // Ct values of qPCR standard data for only detected samples
#   real R_qen[N_en_qp]; // Ct values of qPCR environmental data for only detected samples
#   real S_q_p[N_st_qp]; // Known concentration (log10) in qPCR data for only detected samples
# }
# parameters {
#   // Parameters
#   // // qPCR
#   // // // Bernoulli model
#   real alpha_0;
#   real alpha_1;
#   // // // Continous model
#   real eta_0;
#   real eta_1;
#   real gamma_0;
#   real<upper=0> gamma_1;
#   vector[N_j] C_q;
# }
# transformed parameters{
#   // Parameters
#   // // qPCR
#   // // // Bernoulli model
#   vector[N_st_q] theta_st;
#   vector[N_en_q] theta_un;
#   // // // Continious model
#   vector[N_st_qp] mu_st;
#   vector[N_en_qp] mu_en;
#   vector[N_st_qp] sigma_st;
#   vector[N_en_qp] sigma_en;
#   //
#   // Model TP
#   // // qPCR model
#   // // // Bernuli module model compartment
#   // // // // // Standard
#   for (i in 1:N_st_q){
#     theta_st[i] = alpha_0 + (alpha_1 * S_q[i]);
#   }
#   // // // // // Unknown
#   for (i in 1:N_en_q){
#     theta_un[i] = alpha_0 + (alpha_1 * C_q[j_qen_idx[i]]);
#   }
#   // // // Continious model compartment
#   // // // // Standard
#   for (i in 1:N_st_qp){
#     mu_st[i] = eta_0 + (eta_1 * S_q_p[i]);
#     // sigma_dd_st[i] = sigma_i[species_st_idx_cl[i]];
#     sigma_st[i] = exp(gamma_0+(gamma_1 * S_q_p[i]));
#   }
#   // // // // Unknown
#   for (i in 1:N_en_qp){
#     mu_en[i] = eta_0 + (eta_1 * C_q[j_qen_p_idx[i]]);
#     // sigma_dd_st[i] = sigma_i[species_st_idx_cl[i]];
#     sigma_en[i] = exp(gamma_0+(gamma_1 * C_q[j_qen_p_idx[i]]));
#   }
# }
# model {
#   // Model
#   // // qPCR
#   // // // Bernoulli model
#   Z_qst ~ bernoulli(inv_logit(theta_st)); //Standards
#   Z_qen ~ bernoulli(inv_logit (theta_un)); //Environmental samples
#   // // // Continuous (Ct) model compartment
#   R_qst ~ normal(mu_st,sigma_st);//Standards
#   R_qen ~ normal(mu_en,sigma_en);//Field samples
#   //
#   // Priors
#   // // qPCR
#   // // // Bernoulli model
#   alpha_0 ~ normal(0, 2);
#   alpha_1 ~ normal(0, 2);
#   // // // Continious model
#   eta_0 ~ normal(0, 3);
#   eta_1 ~ normal(-3, 0.1);
#   gamma_0 ~ normal(0, 0.1);
#   gamma_1 ~ normal(0, 0.1);
#   C_q ~ normal(0,3);
# }
# "
M2_stan_code <- "
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
    // sigma_dd_st[i] = sigma_i[species_st_idx_cl[i]];
    sigma_st[i] = exp(gamma_0+(gamma_1 * S_q_p[i]));
  }
  // // // // Unknown
  for (i in 1:N_en_qp){
    mu_en[i] = eta_0 + (eta_1 * C_q[j_qen_p_idx[i]]);
    // sigma_dd_st[i] = sigma_i[species_st_idx_cl[i]];
    sigma_en[i] = exp(gamma_0+(gamma_1 * C_q[j_qen_p_idx[i]]));
  }
}
model {
  // Model
  // // qPCR
  // // // Bernoulli model
  Z_qst ~ bernoulli(inv_logit(theta_st)); //Standards
  Z_qen ~ bernoulli(inv_logit (theta_un)); //Environmental samples
  // // // Continuous (Ct) model compartment
  R_qst ~ normal(mu_st,sigma_st);//Standards
  R_qen ~ normal(mu_en,sigma_en);//Field samples
  //
  // Priors
  // // qPCR
  // // // Bernoulli model
  alpha_0 ~ normal(0, 2);
  alpha_1 ~ normal(0, 2);
  // // // Continious model
  eta_0 ~ normal(0, 3);
  eta_1 ~ normal(-3, 0.1);
  gamma_0 ~ normal(0, 0.1);
  gamma_1 ~ normal(0, 0.1);
  C_q ~ normal(0,3);
}
"

# M3 stan code ------------------------------------------------------------
M3_stan_code <- "
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
"

# M3_2 stan code ------------------------------------------------------------
M3_2_stan_code <- "
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
  real alpha_magnitude; // dispersion parameter of eta
}
parameters {
  vector[N_sp_i_MC2-1] alpha_raw;
}
transformed parameters{
  matrix[N_sp_i_MC2,N_obs_Y_M5] gamma_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M5] psi_M5;
  vector[N_sp_i_MC2] alpha;
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * alpha_magnitude;
  alpha[N_sp_i_MC2] = 0;
  for (i in 1:N_obs_Y_M5){
    for(j in 1:N_sp_i_MC2){
      gamma_M5[j,i] = alr_M5[j]+(NPCR*(alpha[j]));
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
}
"

# M4 stan code ------------------------------------------------------------
M4_stan_code <- "
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
  int Y_M5[N_sp_i_MC2,N_obs_Y_M5]; // Metabarcoding reads in Model 5
  int Y_M6[N_sp_i_MC2,N_obs_Y_M6]; // Metabarcoding reads in Model 6
  //////////////////////////////////////////// Idx
  int st_idx_M6[N_obs_Y_M6]; // Station index
  ////////////////////////////////////////////  Parameters
  //
  real tau_eta_mock_mu;
  real tau_eta_mock_sd;
  real tau_eta_samp_mu;
  real tau_eta_samp_sd;
  real ini_prop_mu;
  real ini_prop_sd;
  // int k;
}
parameters {
  //////////////////////////////////////////// Model Compartment 2
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M5;
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M6;
  vector[N_sp_i_MC2-1] alpha_raw;
  matrix[N_sp_i_MC2-1,N_obs_Y_M5] eta_raw_M5;
  matrix[N_sp_i_MC2-1,N_obs_Y_M6] eta_raw_M6;
  matrix[N_sp_i_MC2-1,N_st_M6] alr_2_raw;
}
transformed parameters{
  //////////////////////////////////////////// Declaration of transformed parameters
  matrix[N_sp_i_MC2,N_obs_Y_M6] gamma_M6;
  gamma_M6[N_sp_i_MC2,] = to_row_vector(rep_vector(0.0,N_obs_Y_M6)); //This states that the last row of gamma_M6 is 0
  matrix[N_sp_i_MC2,N_obs_Y_M5] gamma_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] psi_M6;
  matrix[N_sp_i_MC2,N_obs_Y_M5] psi_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] eta_M6 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M6);
  matrix[N_sp_i_MC2,N_obs_Y_M5] eta_M5 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M5);
  vector[N_sp_i_MC2] alpha;
  matrix<lower=-30>[N_sp_i_MC2-1,N_st_M6] alr_2;
  //////////////////////////////////////////// Adjusting priors
  // Adjusting Alpha prior
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * 0.01;
  alpha[N_sp_i_MC2] = 0;
  // // Adjusting Eta priors for samples
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M6[i,] = eta_raw_M6[i,] * tau_eta_M6[i];
  }
  // Adjusting Eta priors for mock
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M5[i,] = eta_raw_M5[i,] * tau_eta_M5[i];
  }
  // Adjusting alr_2 priors for env samp
  for (i in 1:(N_sp_i_MC2-1)) {
    alr_2[i,] = ini_prop_mu + alr_2_raw[i,] * ini_prop_sd;
  }
  //////////////////////////////////////////// Model Compartment 2
  // Model 5 (Gamma)
  for (i in 1:N_obs_Y_M5){
    for(j in 1:N_sp_i_MC2){
      gamma_M5[j,i] = alr_M5[j]+(NPCR*(alpha[j]))+eta_M5[j,i];
    }
  }
  // Model 6 (Gamma)
  for (i in 1:N_obs_Y_M6){
    for(j in 1:N_sp_i_MC2-1){
      // k=st_idx_M6[i];
      gamma_M6[j,i] = alr_2[j,st_idx_M6[i]]+(NPCR*(alpha[j]))+eta_M6[j,i];
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
    eta_raw_M5[i,] ~ std_normal();
    eta_raw_M6[i,] ~ std_normal();
  }
  tau_eta_M5 ~ gamma(tau_eta_mock_mu,tau_eta_mock_sd);
  tau_eta_M6 ~ gamma(tau_eta_samp_mu,tau_eta_samp_sd);
}
"

# M5.1 stan code ----------------------------------------------------------
M5_1_stan_code <- "
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
  int Y_M5[N_sp_i_MC2,N_obs_Y_M5]; // Metabarcoding reads in Model 5
  int Y_M6[N_sp_i_MC2,N_obs_Y_M6]; // Metabarcoding reads in Model 6
  real ss_conc[N_obs_Y_M6]; // Metabarcoding reads in Model 6
  //////////////////////////////////////////// Idx
  int st_idx_M6[N_obs_Y_M6]; // Station index
  ////////////////////////////////////////////  Parameters
  //
  real tau_eta_mock_mu;
  real tau_eta_mock_sd;
  real tau_eta_samp_mu;
  real tau_eta_samp_sd;
  real ini_prop_mu;
  real ini_prop_sd;
}
parameters {
  //////////////////////////////////////////// Model Compartment 2
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M5;
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M6;
  vector[N_sp_i_MC2-1] alpha_raw;
  matrix[N_sp_i_MC2-1,N_obs_Y_M5] eta_raw_M5;
  matrix[N_sp_i_MC2-1,N_obs_Y_M6] eta_raw_M6;
  matrix[N_sp_i_MC2-1,N_st_M6] alr_2_raw;
}
transformed parameters{
  //////////////////////////////////////////// Declaration of transformed parameters
  matrix[N_sp_i_MC2,N_obs_Y_M6] gamma_M6;
  gamma_M6[N_sp_i_MC2,] = to_row_vector(rep_vector(0.0,N_obs_Y_M6));; //This states that the last row of gamma_M6 is 0
  matrix[N_sp_i_MC2,N_obs_Y_M5] gamma_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] psi_M6;
  matrix[N_sp_i_MC2,N_obs_Y_M5] psi_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] eta_M6 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M6);
  matrix[N_sp_i_MC2,N_obs_Y_M5] eta_M5 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M5);
  vector[N_sp_i_MC2] alpha;
  matrix<lower=-30>[N_sp_i_MC2-1,N_st_M6] alr_2;
  //////////////////////////////////////////// Adjusting priors
  // Adjusting Alpha prior
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * 0.01;
  alpha[N_sp_i_MC2] = 0;
  // // Adjusting Eta priors for samples
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M6[i,] = eta_raw_M6[i,] * tau_eta_M6[i];
  }
  // Adjusting Eta priors for mock
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M5[i,] = eta_raw_M5[i,] * tau_eta_M5[i];
  }
  // Adjusting alr_2 priors for env samp
  for (i in 1:(N_sp_i_MC2-1)) {
    alr_2[i,] = ini_prop_mu + alr_2_raw[i,] * ini_prop_sd;
  }
  //////////////////////////////////////////// Model Compartment 2
  // Model 5 (Gamma)
  for (i in 1:N_obs_Y_M5){
    for(j in 1:N_sp_i_MC2){
      gamma_M5[j,i] = alr_M5[j]+(NPCR*(alpha[j]))+eta_M5[j,i];
    }
  }
  // Model 6 (Gamma)
  for (i in 1:N_obs_Y_M6){
    for(j in 1:N_sp_i_MC2-1){
      // k=st_idx_M6[i];
      gamma_M6[j,i] = (alr_2[j,st_idx_M6[i]]-ss_conc[i])+(NPCR*(alpha[j]))+eta_M6[j,i];
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
    eta_raw_M5[i,] ~ std_normal();
    eta_raw_M6[i,] ~ std_normal();
  }
  tau_eta_M5 ~ gamma(tau_eta_mock_mu,tau_eta_mock_sd);
  tau_eta_M6 ~ gamma(tau_eta_samp_mu,tau_eta_samp_sd);
}
"


# M5.2 stan code ----------------------------------------------------------
M5_2_stan_code <- "
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
  int Y_M5[N_sp_i_MC2,N_obs_Y_M5]; // Metabarcoding reads in Model 5
  int Y_M6[N_sp_i_MC2,N_obs_Y_M6]; // Metabarcoding reads in Model 6
  //////////////////////////////////////////// Idx
  int st_idx_M6[N_obs_Y_M6]; // Station index
  ////////////////////////////////////////////  Parameters
  //
  real tau_eta_mock_mu;
  real tau_eta_mock_sd;
  real tau_eta_samp_mu;
  real tau_eta_samp_sd;
  real ini_prop_mu;
  real ini_prop_sd;
  //
  //Numbers of dimentions
  int N_st_q; // Total number of observation in qPCR standard samples
  int N_en_q; // Total number of observation in qPCR environmental samples
  int N_st_qp; // Total number of observation in qPCR standard samples for only detected samples
  int N_en_qp; // Total number of observation in qPCR environmental samples for only detected samples
  int N_j; // Number of samples in environmental data >>>>>!!!!!!
  //
  //Indexes
  // // // Binomial model
  int j_qen_idx[N_en_q]; // Species and standard index for qPCR environmental samples
  // // // Continious model
  int j_qen_p_idx[N_en_qp]; // Species and standard index for qPCR environmental samples
  // Data
  // // // Binomial model
  int Z_qst[N_st_q]; // Presence/Absence response of qPCR standard data
  int Z_qen[N_en_q]; // Presence/Absence response of qPCR environmental data
  real S_q[N_st_q]; // Known concentration (log10) in qPCR data
  // // // Continious model
  real R_qst[N_st_qp]; // Ct values of qPCR standard data for only detected samples
  real R_qen[N_en_qp]; // Ct values of qPCR environmental data for only detected samples
  real S_q_p[N_st_qp]; // Known concentration (log10) in qPCR data for only detected samples
  //
}
parameters {
  //////////////////////////////////////////// Model Compartment 2
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M5;
  vector<lower=0>[N_sp_i_MC2-1] tau_eta_M6;
  vector[N_sp_i_MC2-1] alpha_raw;
  matrix[N_sp_i_MC2-1,N_obs_Y_M5] eta_raw_M5;
  matrix[N_sp_i_MC2-1,N_obs_Y_M6] eta_raw_M6;
  matrix[N_sp_i_MC2-1,N_st_M6] alr_2_raw;
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
  vector[N_j] C_q; //>>>>>!!!!!!
}
transformed parameters{
  //////////////////////////////////////////// Declaration of transformed parameters
  matrix[N_sp_i_MC2,N_obs_Y_M6] gamma_M6;
  gamma_M6[N_sp_i_MC2,] = to_row_vector(rep_vector(0.0,N_obs_Y_M6));; //This states that the last row of gamma_M6 is 0
  matrix[N_sp_i_MC2,N_obs_Y_M5] gamma_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] psi_M6;
  matrix[N_sp_i_MC2,N_obs_Y_M5] psi_M5;
  matrix[N_sp_i_MC2,N_obs_Y_M6] eta_M6 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M6);
  matrix[N_sp_i_MC2,N_obs_Y_M5] eta_M5 = rep_matrix(0.0,N_sp_i_MC2,N_obs_Y_M5);
  vector[N_sp_i_MC2] alpha;
  matrix<lower=-30>[N_sp_i_MC2-1,N_st_M6] alr_2;
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
  //////////////////////////////////////////// Adjusting priors
  // Adjusting Alpha prior
  alpha[1:(N_sp_i_MC2-1)] = alpha_raw * 0.01;
  alpha[N_sp_i_MC2] = 0;
  // // Adjusting Eta priors for samples
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M6[i,] = eta_raw_M6[i,] * tau_eta_M6[i];
  }
  // Adjusting Eta priors for mock
  for (i in 1:(N_sp_i_MC2-1)) {
    eta_M5[i,] = eta_raw_M5[i,] * tau_eta_M5[i];
  }
  // Adjusting alr_2 priors for env samp
  for (i in 1:(N_sp_i_MC2-1)) {
    alr_2[i,] = ini_prop_mu + alr_2_raw[i,] * ini_prop_sd;
  }
  //////////////////////////////////////////// Model Compartment 2
  // Model 5 (Gamma)
  for (i in 1:N_obs_Y_M5){
    for(j in 1:N_sp_i_MC2){
      gamma_M5[j,i] = alr_M5[j]+(NPCR*(alpha[j]))+eta_M5[j,i];
    }
  }
  // Model 6 (Gamma)
  for (i in 1:N_obs_Y_M6){
    for(j in 1:N_sp_i_MC2-1){
      gamma_M6[j,i] = (alr_2[j,st_idx_M6[i]]-C_q[i])+(NPCR*(alpha[j]))+eta_M6[j,i];
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
  // Model
  // // qPCR
  // // // Bernoulli model
  Z_qst ~ bernoulli(inv_logit(theta_st)); //Standards
  Z_qen ~ bernoulli(inv_logit (theta_un)); //Environmental samples
  // // // Continuous (Ct) model compartment
  R_qst ~ normal(mu_st,sigma_st);//Standards
  R_qen ~ normal(mu_en,sigma_en);//Field samples
  //
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
    eta_raw_M5[i,] ~ std_normal();
    eta_raw_M6[i,] ~ std_normal();
  }
  tau_eta_M5 ~ gamma(tau_eta_mock_mu,tau_eta_mock_sd);
  tau_eta_M6 ~ gamma(tau_eta_samp_mu,tau_eta_samp_sd);
  // // qPCR
  // // // Bernoulli model
  alpha_0 ~ normal(0, 2);
  alpha_1 ~ normal(0, 2);
  // // // Continious model
  eta_0 ~ normal(0, 3);
  eta_1 ~ normal(-3, 0.1);
  gamma_0 ~ normal(0, 0.1);
  gamma_1 ~ normal(0, 0.1);
  C_q ~ normal(0,3);
}
"
# load_model <- function(model='M1'){
# 	if (model=='M1') {
# 		M1 <- stan_model(model_code = M1_stan_code,model_name = "M1 model")
# 	}
# 	if (model=='M2') {
# 		M2 <- stan_model(model_code = M2_stan_code,model_name = "M2 model")
# 	}
# }
# M3 <- stan_model(model_code = M3_stan_code,model_name = "M3 model")
# M4 <- stan_model(model_code = M4_stan_code,model_name = "M4 model")
# M5_1 <- stan_model(model_code = M5_1_stan_code,model_name = "M5.1 model")
# M5_2 <- stan_model(model_code = M5_2_stan_code,model_name = "M5.2 model")

#' Load and Compile a Stan Model
#'
#' The `load_model` function loads and compiles a specified Stan model from a predefined list of models.
#' The models are written as character strings in the R environment, and this function compiles them into
#' a Stan model object using the `rstan::stan_model` function.
#'
#' @param model A character string specifying which model to load. The available options are
#' `"M1"`, `"M2"`, `"M3"`, `"M4"`, `"M5_1"`, and `"M5_2"`. Default is `"M1"`.
#'
#' @return An object of class `stanmodel`, representing the compiled Stan model. This object can be used
#' directly in `rstan` functions such as `sampling`.
#'
#' @details The function looks up the provided model name in a predefined list that includes the Stan code
#' and model name. If the model name is not found, the function throws an error. If the model name is valid,
#' the corresponding Stan model code is compiled and returned as a `stanmodel` object.
#'
#' @examples
#' \dontrun{
#' # Load and compile the M1 model
#' M1 <- load_model("M1")
#'
#' # Load and compile the M3 model
#' M3 <- load_model("M3")
#'
#' # Load and compile the M5.2 model
#' M5_2 <- load_model("M5_2")
#' }
#'
#' @export
load_model <- function(model = 'M1') {
	model_list <- list(
		M1 = list(code = M1_stan_code, name = "M1 model"),
		M2 = list(code = M2_stan_code, name = "M2 model"),
		M3 = list(code = M3_stan_code, name = "M3 model"),
		M3_2 = list(code = M3_2_stan_code, name = "M3_2 model"),
		M4 = list(code = M4_stan_code, name = "M4 model"),
		M5_1 = list(code = M5_1_stan_code, name = "M5.1 model"),
		M5_2 = list(code = M5_2_stan_code, name = "M5.2 model")
	)

	if (!model %in% names(model_list)) {
		stop("Invalid model name. Choose from 'M1', 'M2', 'M3', 'M4', 'M5_1', 'M5_2'.")
	}

	model_info <- model_list[[model]]
	stan_model(model_code = model_info$code, model_name = model_info$name)
}

multi_grepl <- function (pattern, x) 
{
	grepl(paste(pattern, collapse = "|"), x)
}

#' Custom rbind Function for Dataframes with Different Columns
#'
#' This function combines two dataframes with different columns by adding
#' the missing columns with \code{NA} values and then performing a row bind
#' on the resulting dataframes.
#'
#' @param df1 A dataframe. The first dataframe to be row-bound.
#' @param df2 A dataframe. The second dataframe to be row-bound.
#'
#' @return A dataframe that is the result of row-binding \code{df1} and \code{df2}.
#'         If the dataframes have different columns, missing columns in each dataframe
#'         will be added with \code{NA} values before binding.
#'
#' @examples
#' df1 <- data.frame(A = 1:3, B = letters[1:3])
#' df2 <- data.frame(A = 4:5, C = letters[4:5])
#' 
#' custom_rbind(df1, df2)
#' 
#' @export
custom_rbind <- function(df1, df2) {
	
	# Find columns in df1 that are not in df2
	cols_df1_not_in_df2 <- setdiff(names(df1), names(df2))
	
	# Find columns in df2 that are not in df1
	cols_df2_not_in_df1 <- setdiff(names(df2), names(df1))
	
	# Add missing columns to df2 with NA values
	if (length(cols_df1_not_in_df2) > 0) {
		df2[cols_df1_not_in_df2] <- NA
	}
	
	# Add missing columns to df1 with NA values
	if (length(cols_df2_not_in_df1) > 0) {
		df1[cols_df2_not_in_df1] <- NA
	}
	
	# Bind the rows together
	result <- rbind(df1, df2)
	
	return(result)
}