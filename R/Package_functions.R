# General functions ---------------------------------------------------------------------------
load_QM_packages <- function(){
	library(rstan);options(mc.cores = parallel::detectCores())
	library(dplyr)
	library(ggplot2)
	library(stringr)
	library(tibble)
	library("devtools")
	devtools::install_github("BlakeRMills/MoMAColors");library(MoMAColors)
}

#' Display Example Data Structures for Models M1-M4
#'
#' This function prints example data frame structures showing the required format
#' for input data to be used with models M1 through M4. The examples are for
#' illustrative purposes only and do not contain real data.
#'
#' @param data A character string specifying which model example to display.
#' Valid options: "M1" (default), "M2", "M3", or "M4".
#'
#' @return Invisibly returns NULL. Prints formatted data examples to the console.
#' @export
#'
#' @examples
#' # Show qPCR (standard) samples format for Model M1
#' data_example("M1")
#'
#' # Show qPCR (standard & unknown) samples format for Model M2
#' data_example("M2")
#'
#' # Show metabarcoding (mock community) samples format for Model M3
#' data_example("M3")
#'
#' # Show metabarcoding (mock community & environmental) samples format for Model M4
#' data_example("M4")
#'
#' # Show qPCR (standard & unknown) & metabarcoding (mock community & environmental) samples format for Model M5
#' data_example("M5")
data_example <- function(data="M1") {
	if(data=="M1"){
		cat("    Sample_name Species SampleType           Ct standard_concentation Plate
1         Std-1     Cod   STANDARD  17.17366409          1000000          1
2         Std-1     Cod   STANDARD  17.41136169          1000000          1
3         Std-2     Cod   STANDARD  20.32516289           100000          1
4         Std-2     Cod   STANDARD  20.62251329           100000          1
5         Std-3     Cod   STANDARD  24.26965332            10000          1
6         Std-3     Cod   STANDARD   24.2088623            10000          1
7         Std-4     Cod   STANDARD  26.89413261             1000          1
8         Std-4     Cod   STANDARD  27.22113228             1000          1
9         Std-5     Cod   STANDARD  30.73742294              100          1
10        Std-5     Cod   STANDARD  30.60840034              100          1
11        Std-6     Cod   STANDARD   33.8976326               10          1
12        Std-6     Cod   STANDARD  34.95129395               10          1
13        Std-7     Cod   STANDARD Undetermined                1          1
14        Std-7     Cod   STANDARD  36.95103073                1          1
15        Std-1     Cod   STANDARD  17.17366409          1000000          2
16        Std-1     Cod   STANDARD  17.41136169          1000000          2
17        Std-2     Cod   STANDARD  20.32516289           100000          2
18        Std-2     Cod   STANDARD  20.62251329           100000          2
19        Std-3     Cod   STANDARD  24.26965332            10000          2
20        Std-3     Cod   STANDARD   24.2088623            10000          2
21        Std-4     Cod   STANDARD  26.89413261             1000          2
22        Std-4     Cod   STANDARD  27.22113228             1000          2
23        Std-5     Cod   STANDARD  30.73742294              100          2
24        Std-5     Cod   STANDARD  30.60840034              100          2
25        Std-6     Cod   STANDARD   33.8976326               10          2
26        Std-6     Cod   STANDARD  34.95129395               10          2
27        Std-7     Cod   STANDARD Undetermined                1          2
28        Std-7     Cod   STANDARD  36.95103073                1          2
...")
	}
	if(data=="M2"){
		cat("    Sample_name Species SampleType           Ct standard_concentation Plate
1         Std-1     Cod   STANDARD  17.17366409          1000000          1
2         Std-1     Cod   STANDARD  17.41136169          1000000          1
3         Std-2     Cod   STANDARD  20.32516289           100000          1
4         Std-2     Cod   STANDARD  20.62251329           100000          1
5         Std-3     Cod   STANDARD  24.26965332            10000          1
6         Std-3     Cod   STANDARD   24.2088623            10000          1
7         Std-4     Cod   STANDARD  26.89413261             1000          1
8         Std-4     Cod   STANDARD  27.22113228             1000          1
9         Std-5     Cod   STANDARD  30.73742294              100          1
10        Std-5     Cod   STANDARD  30.60840034              100          1
11        Std-6     Cod   STANDARD   33.8976326               10          1
12        Std-6     Cod   STANDARD  34.95129395               10          1
13        Std-7     Cod   STANDARD Undetermined                1          1
14        Std-7     Cod   STANDARD  36.95103073                1          1
15        Std-1     Cod   STANDARD  17.17366409          1000000          2
16        Std-1     Cod   STANDARD  17.41136169          1000000          2
17        Std-2     Cod   STANDARD  20.32516289           100000          2
18        Std-2     Cod   STANDARD  20.62251329           100000          2
19        Std-3     Cod   STANDARD  24.26965332            10000          2
20        Std-3     Cod   STANDARD   24.2088623            10000          2
21        Std-4     Cod   STANDARD  26.89413261             1000          2
22        Std-4     Cod   STANDARD  27.22113228             1000          2
23        Std-5     Cod   STANDARD  30.73742294              100          2
24        Std-5     Cod   STANDARD  30.60840034              100          2
25        Std-6     Cod   STANDARD   33.8976326               10          2
26        Std-6     Cod   STANDARD  34.95129395               10          2
27        Std-7     Cod   STANDARD Undetermined                1          2
28        Std-7     Cod   STANDARD  36.95103073                1          2
...
40     Sample_1     Cod    UNKNOWN  43.85659409               NA          1
41     Sample_1     Cod    UNKNOWN  46.21499252               NA          1
43     Sample_1     Cod    UNKNOWN Undetermined               NA          2
44     Sample_1     Cod    UNKNOWN Undetermined               NA          2
...
158   Sample_12     Cod    UNKNOWN  30.05127716               NA          1
159   Sample_12     Cod    UNKNOWN  30.39950752               NA          1
160   Sample_12     Cod    UNKNOWN  33.72243118               NA          2
161   Sample_12     Cod    UNKNOWN  33.49518585               NA          2
...")
	}
	if(data=="M3"){
		cat(
			"                        Species ini_conc sp_idx  Mock_r1   Mock_r2   Mock_r3  ...   Mock_r9
1                 Brosme brosme     6022      1    26537     26282     55800  ...     55800
2            Cyclopterus lumpus    12061      2    63611     38494     80634  ...     80634
3  Hippoglossoides platessoides     6812      3   103953     53527     96043  ...     96043
4         Leptoclinus maculatus     3725      4   228228    124488    237519  ...    237519
5             Mallotus villosus     9816      5    72751     35851     94465  ...     94465
6           Maurolicus muelleri     7087      6    99815     29082    130790  ...    130790
7        Myoxocephalus scorpius     8908      7    75120     44139     77521  ...     77521
8              Pholis gunnellus     4477      8    66110     36235     58501  ...     58501
9         Pleuronectes platessa     2637      9    41550     19983     44716  ...     44716
10              Zz_Gadus morhua     5942     10    72763     56460    111345  ...    111345");cat('\n')
		cat('\n');cat('Remember to put \'Zz_\' before the species name of the Reference species and bring it last on the data frame')
	}
	if(data=="M4"){
		cat(
			"                        Species  ini_conc sp_idx  Mock_r1   Mock_r2   Mock_r3  ...   Mock_r9   Sample_1    Sample_2    Sample_3 ... Sample_12
1                 Brosme brosme      6022      1    26537     26282     55800  ...     55800          0           0           0 ...         0
2            Cyclopterus lumpus     12061      2    63611     38494     80634  ...     80634          2           0           0 ...         2
3  Hippoglossoides platessoides      6812      3   103953     53527     96043  ...     96043       8214           0           0 ...      6890
4         Leptoclinus maculatus      3725      4   228228    124488    237519  ...    237519        160           0           0 ...      6800
5             Mallotus villosus      9816      5    72751     35851     94465  ...     94465      70304        3100           6 ...         4
6           Maurolicus muelleri      7087      6    99815     29082    130790  ...    130790          0           0           0 ...         0
7        Myoxocephalus scorpius      8908      7    75120     44139     77521  ...     77521          0           0           0 ...         2
8              Pholis gunnellus      4477      8    66110     36235     58501  ...     58501        100         694           0 ...      2936
9         Pleuronectes platessa      2637      9    41550     19983     44716  ...     44716          0           0           0 ...         0
10                 Gadus morhua      5942     10    72763     56460    111345  ...    111345       8514           8        1752 ...      4462");cat('\n')
		cat('\n');cat('Remember to put \'Zz_\' before the species name of the Reference species and bring it last on the data frame')
	}
	if(data=="M5"){
		cat("> qpcr_data
		Sample_name Species SampleType           Ct standard_concentation Plate
1         Std-1     Cod   STANDARD  17.17366409          1000000          1
2         Std-1     Cod   STANDARD  17.41136169          1000000          1
3         Std-2     Cod   STANDARD  20.32516289           100000          1
4         Std-2     Cod   STANDARD  20.62251329           100000          1
5         Std-3     Cod   STANDARD  24.26965332            10000          1
6         Std-3     Cod   STANDARD   24.2088623            10000          1
7         Std-4     Cod   STANDARD  26.89413261             1000          1
8         Std-4     Cod   STANDARD  27.22113228             1000          1
9         Std-5     Cod   STANDARD  30.73742294              100          1
10        Std-5     Cod   STANDARD  30.60840034              100          1
11        Std-6     Cod   STANDARD   33.8976326               10          1
12        Std-6     Cod   STANDARD  34.95129395               10          1
13        Std-7     Cod   STANDARD Undetermined                1          1
14        Std-7     Cod   STANDARD  36.95103073                1          1
15        Std-1     Cod   STANDARD  17.17366409          1000000          2
16        Std-1     Cod   STANDARD  17.41136169          1000000          2
17        Std-2     Cod   STANDARD  20.32516289           100000          2
18        Std-2     Cod   STANDARD  20.62251329           100000          2
19        Std-3     Cod   STANDARD  24.26965332            10000          2
20        Std-3     Cod   STANDARD   24.2088623            10000          2
21        Std-4     Cod   STANDARD  26.89413261             1000          2
22        Std-4     Cod   STANDARD  27.22113228             1000          2
23        Std-5     Cod   STANDARD  30.73742294              100          2
24        Std-5     Cod   STANDARD  30.60840034              100          2
25        Std-6     Cod   STANDARD   33.8976326               10          2
26        Std-6     Cod   STANDARD  34.95129395               10          2
27        Std-7     Cod   STANDARD Undetermined                1          2
28        Std-7     Cod   STANDARD  36.95103073                1          2
...
40     Sample_1     Cod    UNKNOWN  43.85659409               NA          1
41     Sample_1     Cod    UNKNOWN  46.21499252               NA          1
43     Sample_1     Cod    UNKNOWN Undetermined               NA          2
44     Sample_1     Cod    UNKNOWN Undetermined               NA          2
...
158   Sample_12     Cod    UNKNOWN  30.05127716               NA          1
159   Sample_12     Cod    UNKNOWN  30.39950752               NA          1
160   Sample_12     Cod    UNKNOWN  33.72243118               NA          2
161   Sample_12     Cod    UNKNOWN  33.49518585               NA          2
...");cat('\n');cat('\n')
		cat(
			"> metabarcoding_data
			Species  ini_conc sp_idx  Mock_r1   Mock_r2   Mock_r3  ...   Mock_r9   Sample_1    Sample_2    Sample_3 ... Sample_12
1                 Brosme brosme      6022      1    26537     26282     55800  ...     55800          0           0           0 ...         0
2            Cyclopterus lumpus     12061      2    63611     38494     80634  ...     80634          2           0           0 ...         2
3  Hippoglossoides platessoides      6812      3   103953     53527     96043  ...     96043       8214           0           0 ...      6890
4         Leptoclinus maculatus      3725      4   228228    124488    237519  ...    237519        160           0           0 ...      6800
5             Mallotus villosus      9816      5    72751     35851     94465  ...     94465      70304        3100           6 ...         4
6           Maurolicus muelleri      7087      6    99815     29082    130790  ...    130790          0           0           0 ...         0
7        Myoxocephalus scorpius      8908      7    75120     44139     77521  ...     77521          0           0           0 ...         2
8              Pholis gunnellus      4477      8    66110     36235     58501  ...     58501        100         694           0 ...      2936
9         Pleuronectes platessa      2637      9    41550     19983     44716  ...     44716          0           0           0 ...         0
10                 Gadus morhua      5942     10    72763     56460    111345  ...    111345       8514           8        1752 ...      4462");cat('\n')
		cat('\n');cat('Remember to put \'Zz_\' before the species name of the Reference species and bring it last on the data frame')
	}
}


#' Compute relaltive abundances (normalize to 1) for the selected column
#'
#' This function normalizes a specified column in a data frame by dividing each
#' value by the sum of the column, converting absolute values to relative proportions.
#' Missing values (NA) are excluded from the sum calculation.
#'
#' @param df A data.frame or tibble containing the data to be normalized.
#' @param column_name A character string specifying the name of the column to normalize.
#'
#' @return A data.frame or tibble with the specified column normalized to relative proportions.
#' @export
#'
#' @examples
#' data <- data.frame(count = c(10, 20, 30, NA))
#' rel_col(data, "count") # Returns c(0.1667, 0.3333, 0.5, NA)
rel_col <- function(df, column_name) {
	df %>%
		mutate(!!column_name := .data[[column_name]] / sum(.data[[column_name]], na.rm = TRUE))
}


#' Combine multiple indices into a unique numeric index
#'
#' This function converts multiple numeric index columns (that can be generated through `mutate_indices`) into a combined
#' numeric index by first converting each input index to a letter-based code (e.g., 1 -> "a", 27 -> "aa"),
#' combining these codes, and mapping the unique combinations to a numeric index.
#'
#' @param data A data.frame or tibble containing the indices to combine.
#' @param index_variables A character vector of column names representing the indices to combine.
#' @param combined_index_name A character string specifying the name of the new combined index column.
#'
#' @return A data.frame or tibble with an additional numeric index column representing the unique combinations.
#' @export
#'
#' @note The letter conversion uses modulo 26 arithmetic, cycling through letters "a"-"z".
#' Indices exceeding 26^3 (17,576) will produce duplicate codes. The combined index is ordered by the first unique combination encountered.
#'
#' @examples
#' data <- tibble(plate = c(1, 1, 2, 2), sample = c(1, 2, 1, 2))
#' combine_index(data, c("plate", "sample"), "unique_id") # Returns unique_id c(1, 2, 3, 4)
combine_index <- function(data, index_variables, combined_index_name) {
	data %>%
		mutate(across(all_of(index_variables),
									~ paste0(
										tolower(LETTERS[((. - 1) %/% (26^3)) %% 26 + 1]), # First letter
										tolower(LETTERS[((. - 1) %/% (26^2)) %% 26 + 1]), # Second letter
										tolower(LETTERS[((. - 1) %/% 26) %% 26 + 1]),     # Third letter
										tolower(LETTERS[(. - 1) %% 26 + 1])               # Fourth letter
									),
									.names = "letter_{col}")) %>% # Convert to letters
		rowwise() %>% # Row-wise operation for combining letters
		mutate(combined = paste(across(starts_with("letter_")), collapse = "_")) %>%
		ungroup() %>%
		group_by(combined) %>%
		mutate(!!combined_index_name := cur_group_id()) %>% # Create numeric index
		ungroup() %>%
		select(-starts_with("letter_"), -combined)
}

#' Inverse Logit transformation
#'
#' This function computes the inverse logit transformation for a given numeric input.
#' It is often used to transform linear predictions from a logistic regression
#' model back to probabilities.
#'
#' @param x A numeric vector, matrix, or scalar for which the inverse logit transformation
#'   is to be computed.
#'
#' @return A numeric vector, matrix, or scalar of the same dimensions as `x`, containing the
#'   inverse logit transformation values. The values will range between 0 and 1.
#' @export
#'
#' @examples
#' # Compute inverse logit for a single value
#' result <- inverselogit(0)
#'
#' # Compute inverse logit for a vector of values
#' result <- inverselogit(c(-2, 0, 2))
#'
#' # Apply inverse logit to a matrix
#' mat <- matrix(c(-1, 0, 1), ncol = 3)
#' result <- inverselogit(mat)
inverselogit <- function (x)
{
	return(1 / (1 + exp(-x)))
}


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
	return(fit %>% unlist()%>%as.data.frame%>%round(.,9))
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



#' Scientific notation labels for log-space ggplots
#'
#' This function creates axis labels in scientific notation for ggplot2 plots when
#' working with logarithmic or exponential scales. The labels are formatted as powers of 10
#' (e.g., `1 * 10^1`, `1 * 10^2`, `1 * 10^3`), making them more readable and appropriate for log-scaled axes.
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
	c <- log10(x)
	expo <- floor(c)
	base <- round(x / 10^expo,3)
	formatted <- paste0(base,'%*%',"10^", expo)
	str2expression(formatted)
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
#'   scale_y_continuous(trans = 'log10', labels = scientific)
#'
#' # Example of using scientific_10 for x-axis labels in a ggplot2 plot
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   scale_x_continuous(trans = 'log10', labels = scientific)
scientific <- function(x) {
	c <- scales::scientific_format()(x)
	# t <- gsub("1e", "10^", c)
	t <- gsub("[0-9]e", "10^", c)
	t <- gsub("10\\^\\+00", "10^0", t)
	t2 <- gsub("10\\^\\+", "10\\^", t)
	str2expression(t2)}

#' Round to nearest power of 10
#'
#' This function rounds a numeric value to the nearest power of ten based on its magnitude.
#' For example, 123 rounds to 100, 456 rounds to 500, and 0.123 rounds to 0.1.
#'
#' @param x A numeric vector to be rounded. Must contain positive values only
#' (log10 is undefined for 0/negative values).
#'
#' @return A numeric vector of the same length as x with values rounded to the nearest
#' power of ten. Returns NaN for invalid inputs (zero/negative values).
#' @export
#'
#' @examples
#' round_by10(123) # Returns 100
#' round_by10(456) # Returns 500
#' round_by10(0.123) # Returns 0.1
#' round_by10(c(15, 1500, 0.015)) # Returns c(10, 2000, 0.01)
#'
#' @note This is different from standard rounding functions (e.g., round() which rounds
#' to decimal places). Values exactly halfway between powers of ten (e.g., 5, 50, 500)
#' will round up to the next power (e.g., 5 -> 10, 500 -> 1000). Function will return
#' NaN and warn for zero/negative inputs.
round_by10 <- function(x) {
	c <- log10(x)
	expo <- floor(c)
	base <- round(x / 10^expo)
	return(base*10^expo)
}


#' Group and Add Index to a Data Frame
#'
#' This function groups a data frame by a specified column and adds an index to each group.
#'
#' @param data A data frame to be processed.
#' @param index_column A character string specifying the name of the column to group by.
#'
#' @return A data frame with an added column `index` that assigns a unique ID to each group.
#' @export
#'
#' @examples
#' # Example usage
#' df <- data.frame(group = c("A", "A", "B", "B", "C"), value = 1:5)
#' df %>% mutate_indices('group', 'i_idx')
mutate_indices <- function(data, index_variable, index_name) {
	data %>%
		group_by(across(all_of(index_variable))) %>%
		mutate(!!index_name := cur_group_id()) %>%
		ungroup()
}


#' Compute Additive Log-Ratios
#'
#' This function computes the generalized additive log-ratios for a vector of values.
#' It is used for compositional data transformations.
#'
#' @param x A numeric vector to transform.
#' @param log A character string indicating the logarithm base. Use `"e"` for natural log or `10` for base-10 log.
#'
#' @return A numeric vector of generalized additive log-ratios.
#' @export
#'
#' @examples
#' # Compute generalized additive log-ratios
#' galr(c(1, 2, 3), log = "e")
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


# galr_inv_m <- function(data,log="e"){
# 	if(log=="e"){
# 		temp <- data %>% rbind(.,rep(0,ncol(.))) %>% exp()
# 	} else if(log==10){
# 		temp <- data %>% rbind(.,rep(0,ncol(.))) %>% mutate_all(~10^.)
# 	}
# 	return(temp)
# }
# galr_inv <- function(data,log="e"){
# 	if(log=="e"){
# 		temp <- exp(c(data,0))
# 	} else if(log==10){
# 		temp <- 10^(c(data,0))
# 	}
# 	return(temp)
# }
# galr_inv_m_2 <- function(data,vector){
# 	temp <- data %>% rbind(.,vector) %>% exp()
# 	return(temp)
# }


#' Log-Sum-Exponential (Numerically Stable)
#'
#' Computes the logarithm of the sum of exponentials of input values in a numerically stable way to prevent overflow.
#'
#' @param x A numeric vector. For stability, input values should not be extremely large (see Note).
#'
#' @return A scalar value equal to \eqn{\log(\sum_{i=1}^n e^{x_i})}.
#' @export
#'
#' @examples
#' logsumexp(c(1, 2, 3)) # Returns log(exp(1) + exp(2) + exp(3)) ≈ 3.407606
#'
#' # Avoids overflow compared to naive calculation:
#' x <- c(1000, 1001, 1002)
#' logsumexp(x) # Correctly returns ≈ 1002.408
#' log(sum(exp(x))) # Returns Inf due to overflow
logsumexp <- function (x) {
	y = max(x)
	y + log(sum(exp(x - y)))
}

#' Softmax conversion
#'
#' Converts a vector of real numbers into values that sum to 1 using the softmax function.
#' Probabilities are computed as \eqn{p_i = e^{x_i} / \sum_{j=1}^n e^{x_j}}.
#' Implemented with the numerically stable log-sum-exp trick.
#'
#' @param x A numeric vector. For stability, avoid values where \eqn{e^{x_i}} would overflow.
#'
#' @return A numeric vector of probabilities where all elements are between 0-1 and sum to 1.
#' @export
#'
#' @examples
#' softmax(c(1, 1, 1)) # Returns c(0.333, 0.333, 0.333)
#' softmax(c(1, 2, 3)) # Returns c(0.0900, 0.2447, 0.6652)
softmax <- function (x) {
	exp(x - logsumexp(x))
}

#' Column-wise Softmax for matrices or data Frames
#'
#' Applies the softmax function independently to each column of a matrix or data frame.
#' Converts each column into a probability distribution that sums to 1.
#'
#' @param data A numeric matrix or data frame. Non-numeric columns will cause errors.
#'
#' @return A data frame with the same dimensions as data, where each column sums to 1.
#' @export
#'
#' @examples
#' data <- matrix(c(1,2,3, 4,5,6), ncol=2)
#' softmax_matrix(data)
#' # Returns:
#' # V1 V2
#' # 1 0.0900 0.0900
#' # 2 0.2447 0.2447
#' # 3 0.6652 0.6652
softmax_matrix <- function (data) {
	temp <- as.data.frame(matrix(NA,ncol = ncol(data),nrow = nrow(data)))
	for (i in 1:ncol(data)) {
		x <- data[,i]
		temp[,i] <- exp(x - logsumexp(x))
	}
	return(temp)
}


# make_matrix <- function(x,n){
# 	return(matrix(x,length(x),n))
# }

# expand_matrix_by_idx <- function(matrix,idx){
# 	output <- data.frame(matrix(NA,nrow(matrix),length(idx)))
# 	for (i in 1:length(idx)) {
# 		output[,i] <- matrix[,idx[i]]
# 	}
# 	colnames(output) <- idx
# 	return(output)
# }

# mean_by_idx_2 <- function(input,idx,lower=0.25,upper=0.75){
# 	mean_dt <- NA
# 	sd_dt <- NA
# 	q_lower <- NA
# 	q_upper <- NA
# 	for (i in 1:le(idx)){
# 		mean_dt[i] <- mean(input[idx==i])
# 		sd_dt[i] <- sd(input[idx==i])
# 		q_lower[i] <- quantile(input[idx==i],p=lower)
# 		q_upper[i] <- quantile(input[idx==i],p=upper)
# 	}
# 	return(as.data.frame(cbind(mean_dt,sd_dt,q_lower,q_upper)))
# }
# mean_by_idx <- function(input,idx){
# 	mean <- NA
# 	for (i in 1:le(idx)){
# 		mean[i] <- mean(input[idx==i])
# 	}
# 	return(as.data.frame(mean))
# }

# multi_grepl <- function (pattern, x)
# {
# 	grepl(paste(pattern, collapse = "|"), x)
# }

# rel_ab <- function(df) {
# 	df / colSums(df, na.rm = TRUE)
# }

#' Convert Columns to Relative Abundance
#'
#' This function converts absolute values in each column of a data frame or matrix
#' to relative abundances (proportions) by dividing each element by its column sum.
#' Missing values (NA) are excluded from sum calculations but retained in the output.
#'
#' @param df A numeric data.frame or matrix containing absolute values.
#' Non-numeric columns may produce unexpected results or errors.
#'
#' @return A data.frame (if input is a data.frame) or matrix (if input is a matrix)
#' where each column sums to 1 (ignoring NA values). Columns with all NA values will
#' return NaN for all entries.
#' @export
#'
#' @examples
#' # Basic usage
#' data <- data.frame(A = c(10, 20, 30, NA), B = c(1, 2, 3, 4))
#' rel_ab(data)
#' # Returns:
#' # A B
#' # 1 0.1667 0.1
#' # 2 0.3333 0.2
#' # 3 0.5000 0.3
#' # 4 NA 0.4
#'
#' # Edge case: Column with all NA
#' data_na <- data.frame(C = c(NA, NA, NA), D = c(2, 2, 2))
#' rel_ab(data_na)
#' # Returns:
#' # C D
#' # 1 NaN 0.3333
#' # 2 NaN 0.3333
#' # 3 NaN 0.3333
rel_ab <- function(df) {
	sweep(df, 2, colSums(df, na.rm = TRUE), "/")
}


#' Run Stan Model
#'
#' This function runs Stan Model using the prepared data to estimate the environmental DNA (eDNA) concentrations
#' of unknown environmental samples that have been processed through qPCR alongside standard samples of known concentrations.
#'
#' @param stan_object A `stanmodel` object representing the compiled Stan model (M2).
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan` function.
#'
#' @return An object of class `stanfit` containing the results of the model fitting.
#' The model runs 4 chains, 5000 iterations and burns 2000 for warm-up. For changing these values run the analog version of this function
#' @export
#'
#' @examples
#' stanMod_2 <- run_M2(stan_object = M2, stan_data = stan_data_M2)


Run_Model <- function(stan_object,
											stan_data,
											iterations=5000,
											warmup=2000,
											no_chains=4,
											treedepth=10){
	if(!inherits(stan_object, "stanmodel")){
		stop('\nThe provided stan_object is not a stanmodel. \nPlease load the correct model.')
	}
	stanMod <- sampling(
		object = stan_object,
		chains = no_chains,
		iter = iterations,
		warmup = warmup,
		control = list(max_treedepth = treedepth),
		data = stan_data
	)
	model_output <- list(stan_data,stanMod)
	return(model_output)
}

# Model 1 functions ---------------------------------------------------------------------------

#' Prepare data for Stan Model 1
#'
#' This function prepares the data required to run Stan Model 1 by filtering
#' the input data and selecting the necessary columns.
#'
#' @param qpcr_data A `data.frame` containing the qPCR data. It should include the
#'   Ct values and standard (initial) concentrations.
#' @param Ct A `character` string specifying the name of the column in `qpcr_data`
#'   that contains the Ct values. qPCR non-detects should be defined as 'Undetermined' and not NA or any other value
#' @param standard_concentration A `character` string specifying the name of the
#'   column in `qpcr_data` that contains the standard concentrations.
#' @param plate_index A `character` string specifying the name of the
#'   column in `qpcr_data` that contains the plates each sample was run (if all were run in 1 plate then
#'   create a column that all values are 1.
#'
#' @return A list formatted as required by the Stan model.
#' @export
#'
#' @examples
#' stan_data_M1 <- prep_stan_M1(qpcr_data = qpcr %>% filter(Sample_type=="STANDARD"),
#'                              Ct = "Ct",
#'                              standard_concentration = "st_concentration",
#'                              plate_index = 'Plate')
#'                              prep_stan_M1 <- function(qpcr_data, Ct, standard_concentration,plate_index)
prep_stan_M1 <- function(qpcr_data, Ct, standard_concentration, plate_index) {
	# Check if any required argument is missing
	if (missing(qpcr_data)) stop("The `data` argument is missing. Please provide the qPCR data frame. run data_example('M2')")
	if (missing(Ct)) stop("The `Ct` argument is missing. Please specify the Ct column.")
	if (missing(standard_concentration)) stop("The `standard_concentration` argument is missing. Please specify the standard concentration column.")
	if (missing(plate_index)) stop("The `plate_index` argument is missing. Please specify the plate number index.")

	qpcr_g <- qpcr_data %>%
		rename(Ct = all_of(Ct)) %>%
		rename(st_conc = all_of(standard_concentration)) %>%
		rename(plate_ch = all_of(plate_index))

	if(sum(qpcr_g$Ct=="Undetermined")==0){
		stop(
			'\n\nAre the undetermined CT values called \"Undetermined\"?"
Please rename the Ct values to \"Undetermined\" for non-detects\n
If there are no non-detects STANDARD samples the probability of detection (PoD) cannot be estimated (hence PoD = 100%)!
If you still wish to continue use the model create an artifical non-detect row in the dataset that has concentration below the range of the samples\nBy doing so please apply caution to interpreting the other parameters involved in the model!')}
	if(sum(is.na(qpcr_g$st_conc))>0){
		stop('\nWere other samples than STANDARD samples included in the qpcr_data?"
				 \nPlease select only STANDARDS for this model!\n')
	}
	if(sum(qpcr_g$st_conc==0)){
		stop('\nIs any STANDARD concentration 0"
				 \nThe eDNA concentration of a STANDARD sample cannot be = 0\n')
	}

	qpcr_g <- qpcr_g %>%
		mutate(Ct = replace(Ct, Ct == "Undetermined", NA)) %>%
		mutate(Ct = as.numeric(Ct)) %>%
		mutate(pres = 1, pres = replace(pres, is.na(Ct), 0)) %>%
		mutate(st_conc=as.numeric(st_conc)) %>%
		mutate_indices("plate_ch", "plate")

	st_qpcr <- qpcr_g

	st_qpcr_cm <- st_qpcr %>% filter(pres==1)

	label_qpcr_plate <- st_qpcr_cm %>% distinct(plate_ch,plate) %>% as.data.frame() %>% setNames(c('Plate_name','Plate_index'))

	stan_data <- list(
		N_st_q = nrow(st_qpcr),
		N_st_qp = nrow(st_qpcr_cm),
		N_plate = length(unique(st_qpcr_cm$plate)),
		plate_st_idx = st_qpcr_cm$plate,
		#
		Z_qst = as.integer(st_qpcr$pres),
		S_q = log(st_qpcr$st_conc),
		#
		R_qst = as.numeric(st_qpcr_cm$Ct),
		S_q_p = log(st_qpcr_cm$st_conc),
		label_qpcr_plate = label_qpcr_plate
	)
	cat('Total number of samples included in the model is: ',stan_data$N_st_q);cat('\n')
	cat('Number of samples with positive amplification: ',stan_data$N_st_qp);cat('\n')
	cat('Total number of plates included is: ',stan_data$N_plate);cat('\n')
	if(stan_data$N_plate==max(stan_data$plate_st_idx)){cat('Plate index matches the total number of plates')}
	if(stan_data$N_plate!=max(stan_data$plate_st_idx)){cat('Plate index DOES NOT match the total number of plates')}
	cat('\n');cat('\n')
	cat(str(stan_data))
	return(stan_data)
}


#' Extract Model Parameters from Stan Output (M1 & M2)
#'
#' This function extracts specific model parameters from the Stan model M1 and M2 output.
#'
#' @param stanMod A `stanfit` object containing the results of the model fitting,
#'   typically produced by `run_M1`.
#'
#' @return A `data.frame` containing the extracted parameters: `logit_phi`, `beta_0`, `beta_1`, `gamma_0`, and `gamma_1`.
#' @export
#'
#' @examples
#' # Assuming you have run the model and have a stanfit object called stanMod_1
#' ss_param <- extract_qpcr_param(stanMod = stanMod_1)
extract_qpcr_param <- function(model_output){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]
	l <- c('logit_phi','beta_0','beta_1','gamma_0','gamma_1')
	output <- extract_param(stanMod,l) %>% rownames_to_column('parameter')

	output <- output %>%
		mutate(Plate_index = str_extract(parameter, "(?<=\\[)\\d+(?=\\])")) %>%
		mutate(Plate_index=as.numeric(Plate_index)) %>%
		left_join(.,stan_data$label_qpcr_plate,
							by='Plate_index') %>%
		mutate(parameter = ifelse(grepl("beta_0\\[", parameter),
															str_replace(parameter, "\\[\\d+\\]", paste0("[", Plate_name, "]")),
															parameter)) %>% select(-Plate_index,-Plate_name)
	return(output)
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
#'   the Stan model through `run_M1`. It can also be retrieved by `extract_qpcr_param`.
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
#' plot_qpcr_curves(stan_data = stan_data_M1, ss_param = ss_param)
#'
#' # Plot with a custom DNA concentration range
#' plot_qpcr_curves(stan_data = stan_data_M1, ss_param = ss_param, xmin_log = -2, xmax_log = 5)
plot_qpcr_curves <- function(model_output,xmin_log,xmax_log){
	stan_data <- model_output[[1]]

	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	cowplot::plot_grid(plot_qpcr_prob_det(model_output,xmin_log,xmax_log),
										 plot_qpcr_cont_mod(model_output,xmin_log,xmax_log))
}


#' Plot Probability of Detection
#'
#' This function creates a plot of the probability of detection along the DNA concentration range,
#' based on the Stan model's output.
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M1`.
#' @param ss_param A vector of output (unknown) parameters produced by fitting
#'   the Stan model through `run_M1`. It can also be retrieved by `extract_qpcr_param`.
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
#' plot_qpcr_prob_det(stan_data = stan_data_M1, ss_param = ss_param)
plot_qpcr_prob_det <- function(model_output,xmin_log,xmax_log){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]
	
	ss_param <- extract_qpcr_param(model_output)
	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	
	standard_point <- cbind(stan_data$S_q,stan_data$Z_qst) %>% as.data.frame() %>% setNames(c("x","y"))
	
	if (!is.null(stan_data$Z_qen)){
		samp_point <-
			cbind(stan_data$Z_qen,stan_data$j_qen_idx) %>% as.data.frame() %>% setNames(c('Z','j_idx')) %>% 
			left_join(.,
								extract_param(stanMod,'C_q') %>% select(mean,`2.5%`, `97.5%`) %>% 
									setNames(c('C_est','C_est_2.5%_CI','C_est_97.5%_CI')) %>% 
									rownames_to_column('parameter') %>% 
									mutate(j_idx=str_split_fixed(parameter,'\\[', 3)[,2]) %>%
									mutate(j_idx = str_split_fixed(j_idx,'\\]', 3)[,1]) %>% 
									mutate(j_idx=as.numeric(j_idx)) %>% 
									select(-parameter),
								by='j_idx')}
	
	line_df <- data.frame(x = seq(xmin_log, xmax_log, by = 0.1)) %>%
		mutate(phi= ss_param$mean[1] %>% inverselogit()) %>%
		mutate(phi_lo=ss_param$`2.5%`[1] %>% inverselogit()) %>%
		mutate(phi_up=ss_param$`97.5%`[1] %>% inverselogit()) %>%
		mutate(psi_pred=1-exp(exp(x)*phi*-1)) %>%
		mutate(psi_pred_lo=1-exp(exp(x)*phi_lo*-1)) %>%
		mutate(psi_pred_up=1-exp(exp(x)*phi_up*-1))
	
	pp <-
		line_df %>%
		ggplot()+
		geom_line(aes(x=exp(x),y=psi_pred),lty=2)+
		geom_jitter(data=standard_point,aes(x = exp(x),y=y),width = 0.09, height = 0.03,color="black",alpha=0.6)+
		labs(x='DNA concentration',y='Probability of detection')+
		scale_x_log10(labels=scientific_10,
									breaks=round_by10(c(exp(seq(xmin_log,xmax_log,length.out = 4)))),
									lim=c(exp(xmin_log-0.1),exp(xmax_log+0.1)))+
		theme_bw()+
		theme(axis.title = element_text(size=14),
					axis.text = element_text(size=13))
	
	legend1 <- 
		suppressWarnings(cowplot::get_legend(
			ggplot(data.frame(x=c(1),y=c(1),sample=c("Standard")), aes(x, y, color = sample)) +
				geom_point(size = 2) +
				scale_color_manual(values = c("Standard" = "black"),name = "Sample") +
				theme_bw()+
				theme(legend.text = element_text(size=12),
							legend.title = element_text(size=13))
		))
	p1 <- cowplot::plot_grid(pp,legend1,rel_widths = c(7,1))
	
	if (!is.null(stan_data$Z_qen)){
		legend2 <- 
			suppressWarnings(cowplot::get_legend(
				ggplot(data.frame(x=c(1,1),y=c(1,1),sample=c("Standard", "Environmental")), aes(x, y, color = sample)) +
					geom_point(size = 2) +
					scale_color_manual(values = c("Standard" = "black", "Environmental" = "tomato2"),name = "Sample") +
					theme_bw()+
					theme(legend.text = element_text(size=12),
								legend.title = element_text(size=13))
			))
		
		pp <- pp+
			geom_jitter(data=samp_point,aes(x = exp(C_est),y=Z),width = 0.09, height = 0.03,color="tomato2",alpha=0.6)
		p1 <- cowplot::plot_grid(pp,legend2,rel_widths = c(7,1))
	}
	return(p1)
}


#' Plot Continuous Model (Ct Values)
#'
#' This function creates a plot of the continuous model, showing the predicted cycle threshold (Ct) values
#' along the DNA concentration range, based on the Stan model's output.
#' @param stan_data A `list` of data formatted for input into the Stan model,
#'   produced by `prep_stan_M1`.
#' @param ss_param A vector of output (unknown) parameters produced by fitting
#'   the Stan model through `run_M1`. It can also be retrieved by `extract_qpcr_param`.
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
#' plot_qpcr_cont_mod(stan_data = stan_data_M1, ss_param = ss_param)
plot_qpcr_cont_mod <- function(model_output,xmin_log,xmax_log){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]
	
	ss_param <- extract_qpcr_param(model_output)
	
	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	standard_point <- cbind(stan_data$S_q_p,stan_data$R_qst,stan_data$plate_st_idx) %>%
		as.data.frame() %>%
		setNames(c("k_conc","obs_ct",'plate_idx')) %>%
		left_join(.,
							stan_data$label_qpcr_plate,
							by=c('plate_idx'='Plate_index'))
	
	if (!is.null(stan_data$R_qen)){
		samp_point <-
			cbind(stan_data$R_qen,stan_data$j_qen_p_idx,stan_data$plate_en_idx) %>% as.data.frame() %>% setNames(c('Ct','j_idx','p_idx')) %>% 
			left_join(.,
								extract_param(stanMod,'C_q') %>% select(mean,`2.5%`, `97.5%`) %>% 
									setNames(c('C_est','C_est_2.5%_CI','C_est_97.5%_CI')) %>% 
									rownames_to_column('parameter') %>% 
									mutate(j_idx=str_split_fixed(parameter,'\\[', 3)[,2]) %>%
									mutate(j_idx = str_split_fixed(j_idx,'\\]', 3)[,1]) %>% 
									mutate(j_idx=as.numeric(j_idx)) %>% 
									select(-parameter),
								by='j_idx') %>% 
			left_join(.,stan_data$label_qpcr_plate,
								by=c('p_idx'='Plate_index'))}
	line_df <-
		data.frame(x = rep(seq(xmin_log, xmax_log, by = 0.1),stan_data$N_plate),
							 plate_idx=rep(c(1:stan_data$N_plate),
							 							each=length(seq(xmin_log, xmax_log, by = 0.1)))) %>%
		# left_join(.,stan_data$label_qpcr_plate,by=c('plate_idx'='Plate_index')) %>%
		mutate(gamma_0=ss_param %>% filter(parameter=='gamma_0') %>% pull(mean)) %>%
		mutate(gamma_1=ss_param %>% filter(parameter=='gamma_1') %>% pull(mean)) %>%
		mutate(beta_1=ss_param %>% filter(parameter=='beta_1') %>% pull(mean)) %>%
		left_join(.,
							ss_param %>% filter(grepl('beta_0',parameter)) %>%
								mutate(Plate_name=str_split_fixed(parameter,'\\[', 3)[,2]) %>%
								mutate(Plate_name = str_split_fixed(Plate_name,'\\]', 3)[,1]) %>%
								mutate(Plate_name=as.character(Plate_name)) %>%
								left_join(.,stan_data$label_qpcr_plate %>%
														mutate(Plate_name=as.character(Plate_name)),by='Plate_name') %>%
								# mutate(plate_idx = as.numeric(str_extract(plate_idx, "\\d+"))) %>%
								select(mean,Plate_name,Plate_index) %>% rename(beta_0='mean'),
							by=c('plate_idx'='Plate_index')) %>%
		mutate(pred_mu=beta_0+beta_1*x) %>%
		mutate(pred_sigma=exp(gamma_0+(gamma_1 * x)))
	
	pp <- line_df %>% mutate(Plate_name=as.factor(Plate_name)) %>%
		ggplot()+
		geom_line(aes(x=exp(x),y=pred_mu,colour = Plate_name),alpha=0.4,lty=2)+
		scale_color_manual(values=rep('black',stan_data$N_plate))+
		geom_point(data=standard_point,aes(y=obs_ct,x=exp(k_conc),alpha=0.6),
							 color='black')+
		theme_bw()+
		labs(x='DNA concentration',y='Ct')+
		scale_x_log10(labels=scientific_10,
									breaks=round_by10(c(exp(seq(xmin_log,xmax_log,length.out = 4)))),
									lim=c(exp(xmin_log-0.1),exp(xmax_log+0.1)))+
		theme(legend.position = 'none',
					axis.title = element_text(size=14),
					axis.text = element_text(size=13))
	legend1 <- 
		suppressWarnings(cowplot::get_legend(
			ggplot(data.frame(x=c(1),y=c(1),sample=c("Standard")), aes(x, y, color = sample)) +
				geom_point(size = 2) +
				scale_color_manual(values = c("Standard" = "black"),name = "Sample") +
				theme_bw()+
				theme(legend.text = element_text(size=12),
							legend.title = element_text(size=13))
		))
	p1 <- cowplot::plot_grid(pp,legend1,rel_widths = c(7,1))
	
	if (!is.null(stan_data$R_qen)){
		legend2 <- 
			suppressWarnings(cowplot::get_legend(
				ggplot(data.frame(x=c(1,1),y=c(1,1),sample=c("Standard", "Environmental")), aes(x, y, color = sample)) +
					geom_point(size = 2) +
					scale_color_manual(values = c("Standard" = "black", "Environmental" = "tomato2"),name = "Sample") +
					theme_bw()+
					theme(legend.text = element_text(size=12),
								legend.title = element_text(size=13))
			))
		
		pp <- pp+
			geom_jitter(data=samp_point,aes(x = exp(C_est),y=Ct),width = 0.09, height = 0.03,color="tomato2",alpha=0.6)
		p1 <- cowplot::plot_grid(pp,legend2,rel_widths = c(7,1))
	}
	return(p1)
}


plot_qpcr_cont_mod_plate_specific <- function(model_output,xmin_log,xmax_log){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]
	
	ss_param <- extract_qpcr_param(model_output)
	
	if(missing(xmin_log)) xmin_log <- floor(min(stan_data$S_q))
	if(missing(xmax_log)) xmax_log <- round(max(stan_data$S_q))
	standard_point <- cbind(stan_data$S_q_p,stan_data$R_qst,stan_data$plate_st_idx) %>%
		as.data.frame() %>%
		setNames(c("k_conc","obs_ct",'plate_idx')) %>%
		left_join(.,
							stan_data$label_qpcr_plate,
							by=c('plate_idx'='Plate_index'))
	
	if (!is.null(stan_data$R_qen)){
		samp_point <-
			cbind(stan_data$R_qen,stan_data$j_qen_p_idx,stan_data$plate_en_idx) %>% as.data.frame() %>% setNames(c('Ct','j_idx','p_idx')) %>% 
			left_join(.,
								extract_param(stanMod,'C_q') %>% select(mean,`2.5%`, `97.5%`) %>% 
									setNames(c('C_est','C_est_2.5%_CI','C_est_97.5%_CI')) %>% 
									rownames_to_column('parameter') %>% 
									mutate(j_idx=str_split_fixed(parameter,'\\[', 3)[,2]) %>%
									mutate(j_idx = str_split_fixed(j_idx,'\\]', 3)[,1]) %>% 
									mutate(j_idx=as.numeric(j_idx)) %>% 
									select(-parameter),
								by='j_idx') %>% 
			left_join(.,stan_data$label_qpcr_plate,
								by=c('p_idx'='Plate_index'))}
	line_df <-
		data.frame(x = rep(seq(xmin_log, xmax_log, by = 0.1),stan_data$N_plate),
							 plate_idx=rep(c(1:stan_data$N_plate),
							 							each=length(seq(xmin_log, xmax_log, by = 0.1)))) %>%
		# left_join(.,stan_data$label_qpcr_plate,by=c('plate_idx'='Plate_index')) %>%
		mutate(gamma_0=ss_param %>% filter(parameter=='gamma_0') %>% pull(mean)) %>%
		mutate(gamma_1=ss_param %>% filter(parameter=='gamma_1') %>% pull(mean)) %>%
		mutate(beta_1=ss_param %>% filter(parameter=='beta_1') %>% pull(mean)) %>%
		left_join(.,
							ss_param %>% filter(grepl('beta_0',parameter)) %>%
								mutate(Plate_name=str_split_fixed(parameter,'\\[', 3)[,2]) %>%
								mutate(Plate_name = str_split_fixed(Plate_name,'\\]', 3)[,1]) %>%
								mutate(Plate_name=as.character(Plate_name)) %>%
								left_join(.,stan_data$label_qpcr_plate %>%
														mutate(Plate_name=as.character(Plate_name)),by='Plate_name') %>%
								# mutate(plate_idx = as.numeric(str_extract(plate_idx, "\\d+"))) %>%
								select(mean,Plate_name,Plate_index) %>% rename(beta_0='mean'),
							by=c('plate_idx'='Plate_index')) %>%
		mutate(pred_mu=beta_0+beta_1*x) %>%
		mutate(pred_sigma=exp(gamma_0+(gamma_1 * x)))
	
	pp <- line_df %>% mutate(Plate_name=as.factor(Plate_name)) %>%
		ggplot()+
		geom_line(aes(x=exp(x),y=pred_mu,colour = Plate_name),alpha=0.4,lty=2)+
		scale_color_manual(values=rep('black',stan_data$N_plate))+
		geom_point(data=standard_point,aes(y=obs_ct,x=exp(k_conc),alpha=0.6),
							 color='black')+
		theme_bw()+
		labs(x='DNA concentration',y='Ct')+
		scale_x_log10(labels=scientific_10,
									breaks=round_by10(c(exp(seq(xmin_log,xmax_log,length.out = 4)))),
									lim=c(exp(xmin_log-0.1),exp(xmax_log+0.1)))+
		facet_wrap(~Plate_name)+
		geom_line(aes(x=exp(x),y=pred_mu+(pred_sigma^2),colour = Plate_name),alpha=0.4,lty=2)+
		geom_line(aes(x=exp(x),y=pred_mu-(pred_sigma^2),colour = Plate_name),alpha=0.4,lty=2)+
		theme(legend.position = 'none',
					axis.title = element_text(size=14),
					axis.text = element_text(size=13))
	
	legend1 <- 
		suppressWarnings(cowplot::get_legend(
			ggplot(data.frame(x=c(1),y=c(1),sample=c("Standard")), aes(x, y, color = sample)) +
				geom_point(size = 2) +
				scale_color_manual(values = c("Standard" = "black"),name = "Sample") +
				theme_bw()+
				theme(legend.text = element_text(size=12),
							legend.title = element_text(size=13))
		))
	p1 <- cowplot::plot_grid(pp,legend1,rel_widths = c(7,1))
	
	if (!is.null(stan_data$R_qen)){
		legend2 <- 
			suppressWarnings(cowplot::get_legend(
				ggplot(data.frame(x=c(1,1),y=c(1,1),sample=c("Standard", "Environmental")), aes(x, y, color = sample)) +
					geom_point(size = 2) +
					scale_color_manual(values = c("Standard" = "black", "Environmental" = "tomato2"),name = "Sample") +
					theme_bw()+
					theme(legend.text = element_text(size=12),
								legend.title = element_text(size=13))
			))
		
		pp <- pp+
			geom_jitter(data=samp_point,aes(x = exp(C_est),y=Ct),width = 0.09, height = 0.03,color="tomato2",alpha=0.6)+
			facet_wrap(~Plate_name)+
			geom_line(aes(x=exp(x),y=pred_mu+(pred_sigma^2),colour = Plate_name),alpha=0.4,lty=2)+
			geom_line(aes(x=exp(x),y=pred_mu-(pred_sigma^2),colour = Plate_name),alpha=0.4,lty=2)
		p1 <- cowplot::plot_grid(pp,legend2,rel_widths = c(7,1))
	}
	return(p1)
}

# Model 2 functions ---------------------------------------------------------------------------

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

prep_stan_M2 <- function(qpcr_data, sample_type, Ct, sample_name_column, standard_concentration, plate_index) {
	# Check if any required argument is missing
	if (missing(qpcr_data)) stop("The `qpcr_data` argument is missing. Please provide the qPCR data frame. run data_example('M2')")
	if (missing(sample_type)) stop("The `sample_type` argument is missing. Please specify the sample type (STANDARD | UNKNOWN) column.")
	if (missing(Ct)) stop("The `Ct` argument is missing. Please specify the Ct column.")
	if (missing(sample_name_column)) stop("The `sample_name_column` argument is missing. Please specify the sample name column.")
	if (missing(standard_concentration)) stop("The `standard_concentration` argument is missing. Please specify the standard concentration column.")
	if (missing(plate_index)) stop("The `plate_index` argument is missing. Please specify the plate number index.")

	qpcr_g <- qpcr_data %>%
		rename(sample_type = all_of(sample_type)) %>%
		rename(Ct = all_of(Ct)) %>%
		rename(Sample_name = all_of(sample_name_column)) %>%
		rename(st_conc = all_of(standard_concentration)) %>%
		rename(plate_ch = all_of(plate_index))

	if(sum(qpcr_g$Ct=="Undetermined")==0){
		stop(
			'\n\nAre the undetermined CT values called \"Undetermined\"?"
Please rename the Ct values to \"Undetermined\" for non-detects\n
If there are no non-detects STANDARD samples the probability of detection (PoD) cannot be estimated (hence PoD = 100%)!
If you still wish to continue use the model create an artifical non-detect row in the dataset that has concentration below the range of the samples\nBy doing so please apply caution to interpreting the other parameters involved in the model!')}

	qpcr_g <- qpcr_g %>%
		mutate(Ct = replace(Ct, Ct == "Undetermined", NA)) %>%
		mutate(Ct = as.numeric(Ct)) %>%
		mutate(pres = 1, pres = replace(pres, is.na(Ct), 0)) %>%
		mutate_indices("plate_ch", "plate")

	if (sum(grepl("STANDARD",qpcr_g$sample_type))==0) {
		# Are STANDARD samples included?
		stop('\nAre the standard samples included and called \"STANDARD\"?"
				 \nPlease include and rename the sample_type column to \"STANDARD\" for standard samples\n')
	}

	if (sum(grepl("UNKNOWN",qpcr_g$sample_type))==0) {
		# Are UNKNOWN samples included?
		stop('\nAre the environmental samples included and called \"UNKNOWN\"?"
				 \nPlease include and rename the sample_type column to \"UNKNOWN\" for environmental samples\n')
	}
	if (sum(!(qpcr_g$sample_type=='UNKNOWN'|qpcr_g$sample_type=='STANDARD'))) {
		n <- sum(!(qpcr_g$sample_type=='UNKNOWN'|qpcr_g$sample_type=='STANDARD'))
		# Is sample_type anything else other than STANDARD OR UNKNOWN?
		warning(paste0('\n', n,' sample(s) are being removed from the model becuase they don\'t belong to standard or environmental samples.
If you wish to include those, change the \"sample_type\" asigned value to UNKNOWN (or STANDARDS accordingly).\n'))

		diff_samp_type <- qpcr_g %>%
			filter(!(sample_type=='UNKNOWN'|sample_type=='STANDARD')) %>%
			pull(sample_type) %>% unique()
		warning(paste0('\nSamples with sample_type named ', diff_samp_type, ' are removed'))
	}


	e_qpcr <-  qpcr_g %>% filter(sample_type=="UNKNOWN") %>%
		mutate_indices(index_variable = 'Sample_name',index_name = 'sample_index')
	st_qpcr <- qpcr_g %>% filter(sample_type=="STANDARD") %>%
		mutate(st_conc=as.numeric(st_conc))

	if(sum(is.na(st_qpcr$st_conc))>0){
		# Is there any other values in st_conc other than the expected standard concentrations?
		n <- sum(is.na(st_qpcr$st_conc))
		stop(paste0('\n',n,' STANDARD samples do not have concentration values.\nPlease add their nominal concentration or remove from the dataset.\n'))
	}
	if(sum(st_qpcr$st_conc==0)){
		# Is there any standard concentration 0?
		stop('\nIs any STANDARD concentration 0\nThe eDNA concentration of a STANDARD sample cannot be = 0\n')
	}

	e_qpcr_cm <- e_qpcr %>% filter(pres==1)
	st_qpcr_cm <- st_qpcr %>% filter(pres==1)

	label_qpcr_sample <- e_qpcr %>% distinct(sample_index,Sample_name) %>% arrange(sample_index) %>% as.data.frame()
	label_qpcr_plate <- st_qpcr_cm %>% distinct(plate_ch,plate) %>% as.data.frame() %>% setNames(c('Plate_name','Plate_index'))

	stan_data <- list(
		N_st_q = nrow(st_qpcr),
		N_en_q = nrow(e_qpcr),
		N_st_qp = nrow(st_qpcr_cm),
		N_en_qp = nrow(e_qpcr_cm),
		N_plate = length(unique(st_qpcr_cm$plate)),
		#
		N_j = length(unique(e_qpcr$sample_index)),
		#
		j_qen_idx = e_qpcr$sample_index,
		#
		j_qen_p_idx = e_qpcr_cm$sample_index,
		plate_st_idx = st_qpcr_cm$plate,
		plate_en_idx = e_qpcr_cm$plate,
		#
		Z_qst = as.integer(st_qpcr$pres),
		Z_qen = as.integer(e_qpcr$pres),
		S_q = log(st_qpcr$st_conc),
		#
		R_qst = as.numeric(st_qpcr_cm$Ct),
		R_qen = as.numeric(e_qpcr_cm$Ct),
		S_q_p = log(st_qpcr_cm$st_conc),
		#
		label_qpcr_sample = label_qpcr_sample,
		label_qpcr_plate = label_qpcr_plate
	)
	cat('Total number of STANDARD samples included in the model is: ',stan_data$N_st_q);cat('\n')
	cat('Number of STANDARD samples with positive amplification: ',stan_data$N_st_qp);cat('\n')
	cat('Total number of UNKNOWN samples included in the model is: ',stan_data$N_en_q);cat('\n')
	cat('Number of UNKNOWN samples with positive amplification: ',stan_data$N_en_qp);cat('\n')
	cat('Total number of plates included is: ',stan_data$N_plate);cat('\n')
	if(stan_data$N_plate==max(stan_data$plate_st_idx)){cat('Plate index matches the total number of plates')}
	if(stan_data$N_plate!=max(stan_data$plate_st_idx)){cat('Plate index DOES NOT match the total number of plates')}
	cat('\n');cat('\n')
	cat(str(stan_data))
	return(stan_data)
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
#' est_ss_quant <- extract_est_conc(stanMod = stanMod_2)
extract_est_conc <- function(model_output){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]

	output <- extract_param(stanMod,"C_q") %>%
		tibble::rownames_to_column("sample_index") %>%
		mutate(sample_index = stringr::str_extract(sample_index, "\\d+")) %>%
		mutate(sample_index=as.numeric(sample_index)) %>%
		left_join(.,stan_data$label_qpcr_sample,by="sample_index") %>%
		rename(C_est_log="mean") %>%
		rename(`C_est_log_2.5%CI`='2.5%') %>%
		rename(`C_est_log_97.5%CI`="97.5%") %>%
		select(sample_index,Sample_name,C_est_log,`C_est_log_2.5%CI`,`C_est_log_97.5%CI`)
	return(output)
}


#' Plot estimated eDNA qPCR concentrations from model M2
#'
#' This function creates a plot of estimated DNA concentrations for single-species qPCR,
#' including error bars representing the uncertainty (standard deviation) around each estimate.
#' The y-axis can be displayed on a logarithmic scale if specified.
#'
#' @param est_ss_quant A `data.frame` containing the estimated DNA concentrations and their standard deviations derived from `extract_est_conc` function.
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
#' plot_est_conc(est_ss_quant = est_ss_quant, scale = "log")
#'
#' # Plot the estimated DNA concentrations with a linear y-axis
#' plot_est_conc(est_ss_quant = est_ss_quant)
plot_est_conc <- function(model_output){
	# stan_data <- model_output[[1]]
	# stanMod <- model_output[[2]]

	est_ss_quant <- extract_est_conc(model_output)
	ymin_log <- floor(min(est_ss_quant$`C_est_log_2.5%CI`))
	ymax_log <- round(max(est_ss_quant$`C_est_log_97.5%CI`))
	p1 <- est_ss_quant %>%
		ggplot()+
		geom_point(aes(x=Sample_name,y=exp(C_est_log)))+
		geom_errorbar(aes(x=Sample_name,ymin=exp(`C_est_log_2.5%CI`),ymax=exp(`C_est_log_97.5%CI`)))+
		theme_bw()+
		ylab("DNA concentration")+
		xlab("Sample name")+
		theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
					axis.text.y = element_text(size=12))+
		scale_y_log10(labels=scientific_10,breaks=c(round_by10(exp(seq(ymin_log,ymax_log,by=2)))))
	return(p1)
}




#' Extract Amplification Efficiency Parameters
#'
#' This function extracts amplification efficiency parameters from a fitted Stan model.
#'
#' @param stanMod A `stanfit` object containing the results of the model fitting.
#'
#' @return A data frame containing the summary statistics of the amplification efficiency parameters.
#' @export
#'
#' @examples
#' # Extract amplification efficiency parameters
#' amp_eff_param <- extract_amp_efficiecy(stanMod)


# Model 3 functions ---------------------------------------------------------------------------

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
#'                              alpha_magnitude = 0.01)
prep_stan_M3 <- function(metabarcoding_data,
												 mock_sequencing_columns,
												 mock_initial_concentration,
												 species_index,
												 species_names,
												 number_of_PCR=40,
												 alpha_magnitude=0.01) {

	# Check if any required argument is missing
	if (missing(metabarcoding_data)) stop("The `metabarcoding_data` argument is missing. Please provide the metabarcoding_data data frame. run data_example('M4')")
	if (missing(mock_sequencing_columns)) stop("The `mock_sequencing_columns` argument is missing. Please specify the columns indicating Mock commuinty metabarcoding reads.")
	if (missing(mock_initial_concentration)) stop("The `mock_initial_concentration` argument is missing. Please specify the columns indicating Mocks initial concentration")
	if (missing(species_index)) stop("The `species_index` argument is missing. Please specify the species_index column.")
	if (missing(species_names)) stop("The `species_names` argument is missing. Please specify the species_names column.")
	if (missing(number_of_PCR)) stop("The `number_of_PCR` argument is missing. Please specify the number of PCR cycles that the metabarcoding samples ran through")

	mock <- metabarcoding_data %>%
		select({{ mock_sequencing_columns }})
	ini_mock <- metabarcoding_data %>%
		select({{ mock_initial_concentration }}) %>% setNames("i_c")
	species_idx <- metabarcoding_data %>%
		select({{ species_index }})
	mock_sp_names <- metabarcoding_data %>%
		select({{ species_names }})

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
		alpha_magnitude = alpha_magnitude,
		############## Other parameters
		Species = mock_sp_names,
		mock_initial_conc = ini_mock,
		species_idx = species_idx
	);str(stan_data)
	return(stan_data)
}

extract_amp_efficiecy <- function(model_output){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]

	extract_param(stanMod,'alpha') %>%
		select(mean,`2.5%`,`97.5%`) %>%
		setNames(c('alpha','alpha_2.5%_CI','alpha_97.5%_CI')) %>%
		rownames_to_column('sp_idx') %>%
		mutate(sp_idx = str_extract(sp_idx, "\\d+")) %>%
		mutate(sp_idx = as.numeric(sp_idx)) %>%
		left_join(.,bind_cols(stan_data$Species,stan_data$species_idx),
							by='sp_idx') %>%
		select(Species,sp_idx,colnames(.))
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
amp_eff_output_extract <- function(model_output){
	stan_data <- model_output[[1]]

	amp_eff_param <- extract_amp_efficiecy(model_output)

	output <- bind_cols(
		stan_data$Species %>% as.data.frame() %>% setNames('Species'),
		stan_data$alr_M5 %>% exp() %>% as.data.frame() %>% rel_ab() %>% setNames('Pre-PCR'),
		rowMeans(stan_data$Y_M5) %>% as.data.frame() %>% setNames('Post-PCR') %>% rel_ab(),
		stan_data$alr_M5 %>% as.data.frame() %>% setNames('ALR')) %>%
		# rownames_to_column("x") %>% select(-x) %>% column_to_rownames("Species") %>%
		mutate(`Post-PCR_est` = exp(ALR + (stan_data$NPCR * amp_eff_param$alpha))) %>%
		rel_col('Post-PCR_est') %>%
		mutate(`Post-PCR_est_2.5%_CI` = exp(ALR + (stan_data$NPCR * amp_eff_param$`alpha_2.5%_CI`))) %>%
		rel_col('Post-PCR_est_2.5%_CI') %>%
		mutate(`Post-PCR_est_97.5%_CI` = exp(ALR + (stan_data$NPCR * amp_eff_param$`alpha_97.5%_CI`))) %>%
		rel_col('Post-PCR_est_97.5%_CI')
	return(output)
}



#' Plot Amplification Efficiencies
#'
#' This function generates plots to visualize amplification efficiencies, including pre-PCR and post-PCR
#' relative abundances, estimated amplification efficiencies, and their confidence intervals.
#'
#' @param amp_eff_output A data frame containing amplification efficiency data extracted from the model.
#'
#' @return A `ggplot` object with multiple panels showing amplification efficiency results.
#' @export
#'
#' @examples
#' # Generate amplification efficiency plots
#' plot_amp_eff(amp_eff_output)
plot_amp_eff <- function(model_output){
	# stan_data <- model_output[[1]]
	# stanMod <- model_output[[2]]
	alpha_output <- extract_amp_efficiecy(model_output)
	amp_eff_output <- amp_eff_output_extract(model_output)

	n_sp <- nrow(amp_eff_output)
	color_mapping <- as.vector(moma.colors("Lupi", n=n_sp)) %>% setNames(amp_eff_output$Species)

	plot1_data <- amp_eff_output %>% select(Species,`Pre-PCR`,`Post-PCR`) %>%
		pivot_longer(cols = -Species, names_to = 'Sample', values_to = 'Prop')

	plot1_data$Sample <- factor(plot1_data$Sample, levels=c("Pre-PCR", "Post-PCR"))

	p1 <- ggplot(plot1_data,aes(fill=Species,y=Prop,x=Sample))+
		geom_bar(position="stack", stat="identity")+
		scale_fill_manual(values = moma.colors("Lupi", n=n_sp))+
		theme_bw()+
		labs(y = "Proportional abundance (%)", x = "")+
		theme(legend.position = "none",
					axis.text.y = element_text(size=12),
					axis.text.x = element_text(angle = 45,hjust=1,size=12),
					axis.title.y = element_text(size=12))

	legend <- suppressWarnings(cowplot::get_legend(
		ggplot(amp_eff_output, aes(x = 1)) +
			geom_bar(aes(y = `Pre-PCR`, fill = Species), stat = "identity", position = "dodge") +
			geom_line(aes(y = 1, linetype = "Reference species", group = 1), color = "black") +

			scale_fill_manual(values = color_mapping,name = "Species") +
			scale_linetype_manual(values = c("Reference species" = "dashed"),name = NULL) +

			guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2)) +

			theme_bw()+
			theme(axis.title.x = element_text(size = 10),
						text = element_text(size = 13),
						legend.text = element_text(face = "italic"))
	))


	plot2_data <- plot1_data %>%
		mutate(line_t=if_else(grepl('Zz_',Species),'2','1'))

	p2 <- ggplot(data = plot2_data %>% filter(Sample %in% c("Pre-PCR", "Post-PCR")),
							 aes(x = Sample, y = Prop, group = Species)) +
		geom_line(aes(linetype = line_t),
							color = rep(moma.colors("Lupi", n = n_sp), each = 2),
							linewidth = 0.7) +
		geom_point(color = rep(moma.colors("Lupi", n = n_sp),each=2), size = 2) +
		scale_x_discrete(expand = c(0, 0.08)) +
		theme_classic() +  # theme_bw()+
		labs(y = "Proportional abundance (%)", x = "") +
		theme(
			legend.position = "none",
			axis.title.y = element_text(size=12),
			axis.text.y = element_text(size=12),
			axis.text.x = element_text(angle = 45,hjust=1,size=12),
			legend.text = element_text(face = "italic"),
			plot.margin = unit(c(0, 0, 0, 0.3), "cm")
		)

	plot3_data <- alpha_output %>% select(Species,alpha, `alpha_2.5%_CI`, `alpha_97.5%_CI`) %>% #rownames_to_column("Species") %>%
		mutate(co=moma.colors("Lupi",nrow(.))) %>%
		arrange(desc(row_number())) %>%
		mutate(Species=factor(Species, levels=c(Species)))

	p3 <-
		plot3_data %>%
		ggplot()+
		geom_point(aes(x=alpha,y=Species),color=plot3_data$co,size=2)+
		geom_errorbar(aes(y=Species,xmin=`alpha_2.5%_CI`,xmax=`alpha_97.5%_CI`),
									color=plot3_data$co,width=0.1)+
		theme_bw()+
		labs(x = "Amplification efficiency \nrelative to reference species")+
		labs(y = "Species")+
		theme(axis.text.y = element_text(face = "italic"),
					text = element_text(size = 13))+
		geom_vline(xintercept = 0,linetype="dashed",color=plot3_data$co[1])

	pp1 <- suppressWarnings(cowplot::plot_grid(p1,p2,p3,legend,nrow = 1,align = "h",rel_widths = c(2,3,6,2)))
	return(pp1)
}



# Model 4 functions ---------------------------------------------------------------------------
prep_stan_M4 <- function(metabarcoding_data,
												 mock_sequencing_columns,
												 sample_sequencing_columns,
												 mock_initial_concentration,
												 species_index,
												 species_names,
												 number_of_PCR,
												 alpha_magnitude = 0.1,
												 ini_prop_mu=0,
												 ini_prop_sd=5) {

		# Check if any required argument is missing
		if (missing(metabarcoding_data)) stop("The `metabarcoding_data` argument is missing. Please provide the metabarcoding_data data frame. run data_example('M4')")
		if (missing(mock_sequencing_columns)) stop("The `mock_sequencing_columns` argument is missing. Please specify the columns indicating Mock commuinty metabarcoding reads.")
		if (missing(sample_sequencing_columns)) stop("The `sample_sequencing_columns` argument is missing. Please specify the columns indicating Environmental samples metabarcoding reads.")
		if (missing(mock_initial_concentration)) stop("The `mock_initial_concentration` argument is missing. Please specify the columns indicating Mocks initial concentration")
		if (missing(species_index)) stop("The `species_index` argument is missing. Please specify the species_index column.")
		if (missing(species_names)) stop("The `species_names` argument is missing. Please specify the species_names column.")
		if (missing(number_of_PCR)) stop("The `number_of_PCR` argument is missing. Please specify the number of PCR cycles that the metabarcoding samples ran through")

	mock <- metabarcoding_data %>%
		select({{ mock_sequencing_columns }})
	ini_mock <- metabarcoding_data %>%
		select({{ mock_initial_concentration }}) %>% setNames("i_c")
	species_idx <- metabarcoding_data %>%
		select({{ species_index }})
	mock_sp_names <- metabarcoding_data %>%
		select({{ species_names }})
	mb <- metabarcoding_data %>%
		select(all_of(sample_sequencing_columns))

	ini_mock$prop <- galr(ini_mock$i_c,log="e")

	if(sum(diff(species_idx$sp_idx)>1)>0){
		stop('Species index is not continuous. Please make the index continuous c(1:n)')
	}

	sample_name_mb <- mb %>% colnames()

	index_df <- sample_name_mb %>%
		as.data.frame() %>%
		setNames('Sample_name') %>%
		mutate(Sample_name=as.character(Sample_name)) %>%
		distinct(Sample_name) %>%
		mutate_indices(index_variable = 'Sample_name',index_name = 'sample_index')

	mb <- mb %>% select(index_df %>% arrange(sample_index) %>% pull(Sample_name))

stan_data <- list(
	############## Integers
	N_st_M6 = max(index_df %>% arrange(sample_index) %>% pull(sample_index)),
	st_idx_M6 = index_df %>% arrange(sample_index) %>% pull(sample_index),
	N_sp_i_MC2 = nrow(species_idx),
	N_obs_Y_M5 = ncol(mock),
	N_obs_Y_M6 = ncol(mb),
	############## Data
	alr_M5 = ini_mock$prop,
	Y_M5 = mock,
	Y_M6 = mb,
	NPCR = number_of_PCR,
	############## Parameters
	alpha_magnitude = alpha_magnitude,
	ini_prop_mu = ini_prop_mu,
	ini_prop_sd = ini_prop_sd,
	############## Other parameters
	Species = mock_sp_names,
	species_idx = species_idx,
	# Station_name_indexed = sample_index,
	# label_M4 = label
	label_mb_species = bind_cols(mock_sp_names,species_idx),
	label_mb_sample = index_df %>% arrange(sample_index))
return(stan_data)
}


#' Extract Initial Proportions
#'
#' This function extracts the initial proportional abundances for species in a mock community
#' from a fitted Stan model.
#'
#' @param stanMod A `stanfit` object containing the results of the model fitting.
#'
#' @return A data frame representing the initial proportional abundances of species.
#' @export
#'
#' @examples
#' # Extract initial proportions
#' est_ini_prop <- extract_ini_prop(stanMod)
extract_ini_prop <- function(model_output){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]

	output <- extract_matrix(stanMod,"alr_2") %>% rbind(.,setNames(data.frame(matrix(0,1,ncol(.))),colnames(.)))
	col_names <- stan_data$label_mb_sample$Sample_name
	colnames(output) <- col_names
	output <- output %>%
		exp() %>%
		rel_ab() %>%
		rownames_to_column('sp_idx') %>%
		left_join(.,
							bind_cols(stan_data$Species,stan_data$species_idx) %>%
								mutate(sp_idx=as.character(sp_idx)),
							by='sp_idx') %>%
		select(-sp_idx) %>%
		select(Species,colnames(.))
	return(output)
}

bar_plot_est_ini_prop <- function(model_output){
	# stan_data <- model_output[[1]]
	# stanMod <- model_output[[2]]

	ini_prop <- extract_ini_prop(model_output) %>%
		pivot_longer(cols = -Species, names_to = 'Sample', values_to = 'prop')

	p1 <-
		ggplot(ini_prop,aes(fill=Species,y=prop,x=Sample))+
		geom_bar(position="stack", stat="identity")+
		scale_fill_manual(values = moma.colors("Lupi", n=length(unique(ini_prop$Species))))+
		theme_bw()+
		labs(y = "Proportional abundance (%)", x = "",
				 title = "Estimated initial proportional abundance")+
		theme(legend.position = "right",
					axis.title.x = element_text(size = 10),
					axis.text.x = element_text(angle = 90),
					text = element_text(size = 13),
					legend.text = element_text(face = "italic"))

	return(p1)
}

heatmap_plot_est_ini_prop <- function(model_output){
	ini_prop <- extract_ini_prop(model_output) %>%
		column_to_rownames('Species')

	ini_prop %>%
		as.matrix() %>%
		heatmap(,Rowv = NA,Colv = NA,
						col = hcl.colors(13,palette = "Blues",rev=T),
						scale="none",
						margins=c(10,13),cexRow=1.2,cexCol=1.1,
						labRow=as.expression(lapply(rownames(.), function(a) bquote(italic(.(a))))))
	legend(x="right",title="Relative abundance", legend=round(seq(0,1,length.out = 13),2),
				 fill=hcl.colors(13,palette = "Blues",rev=T))
}

# Model 5 functions ---------------------------------------------------------------------------
prep_stan_M5 <- function(qpcr_data, sample_type, Ct, sample_name_column, standard_concentration, plate_index,
												 metabarcoding_data,
												 mock_sequencing_columns,
												 sample_sequencing_columns,
												 mock_initial_concentration,
												 species_index,
												 species_names,
												 number_of_PCR,
												 alpha_magnitude = 0.1,
												 ini_prop_mu=0,
												 ini_prop_sd=5) {
	# Check if any required argument is missing
	if (missing(qpcr_data)) stop("The `qpcr_data` argument is missing. Please provide the qPCR data frame. run data_example('M2')")
	if (missing(sample_type)) stop("The `sample_type` argument is missing. Please specify the sample type (STANDARD | UNKNOWN) column.")
	if (missing(Ct)) stop("The `Ct` argument is missing. Please specify the Ct column.")
	if (missing(sample_name_column)) stop("The `sample_name_column` argument is missing. Please specify the sample name column.")
	if (missing(standard_concentration)) stop("The `standard_concentration` argument is missing. Please specify the standard concentration column.")
	if (missing(plate_index)) stop("The `plate_index` argument is missing. Please specify the plate number index.")

	# Check if any required argument is missing
	if (missing(metabarcoding_data)) stop("The `metabarcoding_data` argument is missing. Please provide the metabarcoding_data data frame. run data_example('M4')")
	if (missing(mock_sequencing_columns)) stop("The `mock_sequencing_columns` argument is missing. Please specify the columns indicating Mock commuinty metabarcoding reads.")
	if (missing(sample_sequencing_columns)) stop("The `sample_sequencing_columns` argument is missing. Please specify the columns indicating Environmental samples metabarcoding reads.")
	if (missing(mock_initial_concentration)) stop("The `mock_initial_concentration` argument is missing. Please specify the columns indicating Mocks initial concentration")
	if (missing(species_index)) stop("The `species_index` argument is missing. Please specify the species_index column.")
	if (missing(species_names)) stop("The `species_names` argument is missing. Please specify the species_names column.")
	if (missing(number_of_PCR)) stop("The `number_of_PCR` argument is missing. Please specify the number of PCR cycles that the metabarcoding samples ran through")

	qpcr_g <- qpcr_data %>%
		rename(sample_type = all_of(sample_type)) %>%
		rename(Ct = all_of(Ct)) %>%
		rename(Sample_name = all_of(sample_name_column)) %>%
		rename(st_conc = all_of(standard_concentration)) %>%
		rename(plate_ch = all_of(plate_index))


	if(sum(qpcr_g$Ct=="Undetermined")==0){
		stop(
			'\n\nAre the undetermined CT values called \"Undetermined\"?"
Please rename the Ct values to \"Undetermined\" for non-detects\n
If there are no non-detects STANDARD samples the probability of detection (PoD) cannot be estimated (hence PoD = 100%)!
If you still wish to continue use the model create an artifical non-detect row in the dataset that has concentration below the range of the samples\nBy doing so please apply caution to interpreting the other parameters involved in the model!')}

	qpcr_g <- qpcr_g %>%
		mutate(Ct = replace(Ct, Ct == "Undetermined", NA)) %>%
		mutate(Ct = as.numeric(Ct)) %>%
		mutate(pres = 1, pres = replace(pres, is.na(Ct), 0)) %>%
		mutate_indices("plate_ch", "plate")

	if (sum(grepl("STANDARD",qpcr_g$sample_type))==0) {
		# Are STANDARD samples included?
		stop('\nAre the standard samples included and called \"STANDARD\"?"
				 \nPlease include and rename the sample_type column to \"STANDARD\" for standard samples\n')
	}

	if (sum(grepl("UNKNOWN",qpcr_g$sample_type))==0) {
		# Are UNKNOWN samples included?
		stop('\nAre the environmental samples included and called \"UNKNOWN\"?"
				 \nPlease include and rename the sample_type column to \"UNKNOWN\" for environmental samples\n')
	}
	if (sum(!(qpcr_g$sample_type=='UNKNOWN'|qpcr_g$sample_type=='STANDARD'))) {
		n <- sum(!(qpcr_g$sample_type=='UNKNOWN'|qpcr_g$sample_type=='STANDARD'))
		# Is sample_type anything else other than STANDARD OR UNKNOWN?
		warning(paste0('\n', n,' sample(s) are being removed from the model becuase they don\'t belong to standard or environmental samples.
If you wish to include those, change the \"sample_type\" asigned value to UNKNOWN (or STANDARDS accordingly).\n'))

		diff_samp_type <- qpcr_g %>%
			filter(!(sample_type=='UNKNOWN'|sample_type=='STANDARD')) %>%
			pull(sample_type) %>% unique()
		warning(paste0('\nSamples with sample_type named ', diff_samp_type, ' are removed'))
	}


	mock <- metabarcoding_data %>%
		select({{ mock_sequencing_columns }})
	ini_mock <- metabarcoding_data %>%
		select({{ mock_initial_concentration }}) %>% setNames("i_c")
	species_idx <- metabarcoding_data %>%
		select({{ species_index }})
	mock_sp_names <- metabarcoding_data %>%
		select({{ species_names }})
	mb <- metabarcoding_data %>%
		select(all_of(sample_sequencing_columns))

	ini_mock$prop <- galr(ini_mock$i_c,log="e")


	sample_name_qpcr <- qpcr_g %>% filter(sample_type=="UNKNOWN") %>% pull(Sample_name)
	sample_name_mb <- mb %>% colnames()

	temp_qpcr_logic_df <- sample_name_qpcr %>%
		as.data.frame() %>%
		setNames('qPCR_samp_name') %>%
		mutate(qPCR_samp_name=as.character(qPCR_samp_name)) %>%
		distinct(qPCR_samp_name) %>% mutate(pres_qpcr=1) %>%
		full_join(.,sample_name_mb %>%
								as.data.frame() %>%
								setNames('mb_samp_name') %>%
								mutate(mb_samp_name=as.character(mb_samp_name)) %>%
								distinct(mb_samp_name) %>%
								mutate(pres_mb=1),
							by=c('qPCR_samp_name'='mb_samp_name')) %>%
		rename(Sample_name='qPCR_samp_name') %>%
		mutate(across(everything(), ~replace(., is.na(.), 0))) %>%
		mutate(tot=pres_qpcr+pres_mb)

	if(!sum(temp_qpcr_logic_df$tot>1)){stop('\nNo qPCR samples are in Metabarcoding data and vice-versa. Please include samples that are analysed jointly')}
	if(sum(temp_qpcr_logic_df$pres_mb==0)>0){
		v <- temp_qpcr_logic_df %>% filter(pres_mb==0) %>% pull(Sample_name)
		warning('\n The following samples were excluded from qPCR dataframe due to the samples missing in metabarcoding data:',paste0('\n',v))}
	if (sum(temp_qpcr_logic_df$pres_qpcr==0)>0){
		v <- temp_qpcr_logic_df %>% filter(pres_qpcr==0) %>% pull(Sample_name)
		warning('\n The following samples were excluded from Metabarcoding dataframe due to the samples missing in qPCR data:',paste0('\n',v))}

	index_df <- temp_qpcr_logic_df %>% filter(tot==2) %>% select(Sample_name) %>%
		mutate_indices(index_variable = 'Sample_name',index_name = 'sample_index')

	e_qpcr <-  qpcr_g %>% filter(sample_type=="UNKNOWN") %>%
		left_join(.,index_df,by='Sample_name')
	# mutate_indices(index_variable = 'Sample_name',index_name = 'sample_index')
	st_qpcr <- qpcr_g %>% filter(sample_type=="STANDARD") %>%
		mutate(st_conc=as.numeric(st_conc))

	mb <- mb %>% select(index_df %>% arrange(sample_index) %>% pull(Sample_name))

	if(sum(is.na(st_qpcr$st_conc))>0){
		# Is there any other values in st_conc other than the expected standard concentrations?
		n <- sum(is.na(st_qpcr$st_conc))
		stop(paste0('\n',n,' STANDARD samples do not have concentration values.\nPlease add their nominal concentration or remove from the dataset.\n'))
	}
	if(sum(st_qpcr$st_conc==0)){
		# Is there any standard concentration 0?
		stop('\nIs any STANDARD concentration 0\nThe eDNA concentration of a STANDARD sample cannot be = 0\n')
	}

	e_qpcr_cm <- e_qpcr %>% filter(pres==1)
	st_qpcr_cm <- st_qpcr %>% filter(pres==1)

	if(sum(diff(species_idx$sp_idx)>1)>0){
		stop('Species index is not continuous. Please make the index continuous c(1:n)')
	}

	label_qpcr_sample <- e_qpcr %>% distinct(sample_index,Sample_name) %>% arrange(sample_index) %>% as.data.frame()
	label_qpcr_plate <- st_qpcr_cm %>% distinct(plate_ch,plate) %>% as.data.frame() %>% setNames(c('Plate_name','Plate_index'))
	stan_data <- list(
		N_st_q = nrow(st_qpcr),
		N_en_q = nrow(e_qpcr),
		N_st_qp = nrow(st_qpcr_cm),
		N_en_qp = nrow(e_qpcr_cm),
		N_plate = length(unique(st_qpcr_cm$plate)),
		#
		N_j = length(unique(e_qpcr$sample_index)),
		#
		j_qen_idx = e_qpcr$sample_index,
		#
		j_qen_p_idx = e_qpcr_cm$sample_index,
		plate_st_idx = st_qpcr_cm$plate,
		plate_en_idx = e_qpcr_cm$plate,
		#
		N_st_M6 = max(index_df %>% arrange(sample_index) %>% pull(sample_index)),
		st_idx_M6 = index_df %>% arrange(sample_index) %>% pull(sample_index),
		N_sp_i_MC2 = nrow(species_idx),
		N_obs_Y_M5 = ncol(mock),
		N_obs_Y_M6 = ncol(mb),
		############## Data
		alr_M5 = ini_mock$prop,
		Y_M5 = mock,
		Y_M6 = mb,
		NPCR = number_of_PCR,
		#
		Z_qst = as.integer(st_qpcr$pres),
		Z_qen = as.integer(e_qpcr$pres),
		S_q = log(st_qpcr$st_conc),
		#
		R_qst = as.numeric(st_qpcr_cm$Ct),
		R_qen = as.numeric(e_qpcr_cm$Ct),
		S_q_p = log(st_qpcr_cm$st_conc),
		############## Parameters
		alpha_magnitude = alpha_magnitude,
		ini_prop_mu = ini_prop_mu,
		ini_prop_sd = ini_prop_sd,
		############## Other parameters
		Species = mock_sp_names,
		species_idx = species_idx,
		#
		# Station_name_indexed = sample_index,
		# label_M4 = label
		label_mb_species = bind_cols(mock_sp_names,species_idx),
		label_mb_sample = index_df %>% arrange(sample_index),
		label_qpcr_sample = label_qpcr_sample,
		label_qpcr_plate = label_qpcr_plate
	)
	return(stan_data)
}


extract_ini_conc <- function(model_output,value='mean'){
	stanMod <- model_output[[2]]

	mat <- extract_matrix(stanMod,"alr_2",vector=value)
	last_r <- extract_param(stanMod,"C_q") %>% pull({{ value }})
		# pull(mean)
	return(rbind(mat,last_r))
}

plot_est_ini_conc <- function(model_output, k=2){
	stan_data <- model_output[[1]]

	est_ini_conc <- extract_ini_conc(model_output)
	est_ini_conc_lo <- extract_ini_conc(model_output,value = '2.5%')
	est_ini_conc_up <- extract_ini_conc(model_output,value = '97.5%')

	est_ini_conc_long <- est_ini_conc %>% setNames(stan_data$label_mb_sample$Sample_name) %>%
		rownames_to_column('sp_idx') %>% mutate(sp_idx=as.numeric(sp_idx)) %>%
		left_join(.,bind_cols(stan_data$Species,stan_data$species_idx),
							by='sp_idx') %>% select(Species,colnames(.),-sp_idx) %>%
		pivot_longer(cols = -Species,
								 names_to = 'Sample',
								 values_to = 'Conc') %>%
		bind_cols(.,
							est_ini_conc_lo %>% pivot_longer(cols = everything(),values_to = 'Conc_2.5%') %>% select(-name),
							est_ini_conc_up %>% pivot_longer(cols = everything(),values_to = 'Conc_97.5%')%>% select(-name))

	nr <- nrow(est_ini_conc)*k
	nudge_vector <- rep(c(1:nrow(est_ini_conc))*(1/nr)-(max(c(1:nrow(est_ini_conc))*(1/nr))/2),each=ncol(est_ini_conc))

	p1 <-
		est_ini_conc_long %>% bind_cols(.,nudge_vector %>% as.data.frame() %>% setNames('nudge_vector')) %>%
		ggplot()+
		geom_point(aes(x=Sample,y=exp(Conc),color=Species),position = position_nudge(x = nudge_vector))+
		geom_linerange(aes(x=Sample, ymin=exp(`Conc_2.5%`),ymax=exp(`Conc_97.5%`),color=Species),position = position_nudge(x = nudge_vector))+
		scale_color_manual(values = moma.colors("Lupi", n=nrow(est_ini_conc)))+
		scale_y_log10(labels=scientific_10)+
		theme_bw()+
		theme(legend.text = element_text(face = "italic",size=12),
					# legend.key.size = unit(1, "cm"),
					legend.key.size = unit(20, "pt"),
					axis.title.y = element_text(size=13),
					axis.title.x = element_text(size=13),
					axis.text.y = element_text(size=12),
					axis.text.x = element_text(angle = 90, size=12,vjust = 0.5, hjust=1))+
		labs(y="DNA Concentration",x="Sample")

	return(p1)
}


arrange_by_sp_idx <- function(data, species_idx) {
	data <- data %>%
		left_join(., species_idx, by = "Species") %>%
		arrange(idx) %>%
		select(-idx) %>%
		column_to_rownames("Species")
	return(data)
}


# Load model ----------------------------------------------------------------------------------


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
	if(model=='M1'){
		url <- "https://raw.githubusercontent.com/gledguri/QM/main/Stan/M1.stan"
		mod <- list(code = readLines(url), name = "M1 model")
	}
	if(model=='M2'){
		url <- "https://raw.githubusercontent.com/gledguri/QM/main/Stan/M2.stan"
		mod <- list(code = readLines(url), name = "M2 model")
	}
	if(model=='M3'){
		url <- "https://raw.githubusercontent.com/gledguri/QM/main/Stan/M3.stan"
		mod <- list(code = readLines(url), name = "M3 model")
	}
	if(model=='M4'){
		url <- "https://raw.githubusercontent.com/gledguri/QM/main/Stan/M4.stan"
		mod <- list(code = readLines(url), name = "M4 model")
	}
	if(model=='M5'){
		url <- "https://raw.githubusercontent.com/gledguri/QM/main/Stan/M5.stan"
		mod <- list(code = readLines(url), name = "M5 model")
	}
		stan_model(model_code = mod$code, model_name = mod$name)
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




# Diagnose model ------------------------------------------------------------------------------

diagnose_model <- function(model_output){
	stan_data <- model_output[[1]]
	stanMod <- model_output[[2]]
	stan_post_df <- extract_param(stanMod)

	# Determine the range of ESS
	ess_range <- stan_post_df$n_eff %>%
		range(na.rm = TRUE) %>% log() %>%
		floor() %>% as.data.frame() %>%
		setNames('ESS_range') %>%
		mutate(ESS_range = if_else(row_number(.) == 2, ESS_range + 1, ESS_range)) %>%
		mutate(ESS_range=exp(ESS_range)) %>%
		pull(ESS_range)


	ess_breaks <-
		seq(from = sqrt(ess_range[1]), to = sqrt(ess_range[2]), length.out = 4) %>%
		as.data.frame() %>%
		setNames('ESS_breaks') %>%
		mutate(ESS_breaks=ESS_breaks^2) %>%
		mutate(ESS_breaks=round_by10(ESS_breaks)) %>%
		pull(ESS_breaks)

	# Diagnostics & Goodness-of-Fit
	p1 <-
		stan_post_df %>%
		ggplot()+
		geom_histogram(aes(x=n_eff),binwidth = 2,fill = "skyblue", color = "black")+
		scale_x_sqrt(labels=scientific_10,limits=ess_range,breaks=ess_breaks) +
		theme_bw()+
		scale_y_sqrt()+
		xlab('ESS (Effective Sampling Size)')+
		ylab('Number of\nparameters')+
		theme(
			axis.title.x = element_text(size=13),
			axis.text.x = element_text(size=13),
			axis.text.y = element_text(size=13),
			axis.title.y = element_text(size=14)
		)


	# Determine the range of Rhat
	rhat_range <-
		stan_post_df$Rhat %>%
		as.data.frame() %>%
		setNames('Rhat') %>%
		mutate(nparam = nrow(.),
					 sd=sd(Rhat, na.rm = TRUE),
					 max = max(Rhat, na.rm = TRUE),
					 min = min(Rhat, na.rm = TRUE)) %>%
		summarise(nparam=first(nparam),
							sd = first(sd),
							max = first(max),
							min = first(min)) %>%
		mutate(binwidth=(sd*((nparam+100)^(1/3)))/100) %>%
		mutate(min=if_else(min==1,min-((max-min)),min)) %>%
		mutate(min=if_else(max==1,max+((max-min)),min)) %>%
		mutate(min=if_else(sd==0,min-0.02,min)) %>%
		mutate(max=if_else(sd==0,max+0.02,max)) %>%
		mutate(binwidth=if_else(sd==0,0.3/nparam,binwidth))


	rhat_breaks <-
		seq(from = rhat_range$min, to = rhat_range$max, length.out = 4) %>%
		tibble(values = .) %>%
		mutate(values = ifelse(abs(values - 1) == min(abs(values - 1)), 1, values)) %>%
		mutate(values = round(values,round(log10(rhat_range$binwidth)*-1))) %>%
		pull(values)

	p2 <-
		stan_post_df %>%
		ggplot()+
		geom_histogram(aes(x=Rhat),binwidth = rhat_range$binwidth,fill = "skyblue", color = "black")+
		theme_bw()+
		scale_x_continuous(limits=c(rhat_range$min,rhat_range$max),labels = rhat_breaks,breaks = rhat_breaks)+
		scale_y_sqrt()+
		xlab('Rhat')+
		theme(
			axis.title.x = element_text(size=13),
			axis.text.x = element_text(size=13),
			axis.text.y = element_text(size=13),
			axis.title.y = element_blank()
		)

	pp1 <- suppressWarnings(cowplot::plot_grid(p1,p2,nrow = 1))

	sampler_params <- get_sampler_params(stanMod, inc_warmup = T)

	warmup_it <- stanMod@stan_args[[1]]$warmup
	samp_it <- stanMod@stan_args[[1]]$iter

	# Extract tree depth for each chain
	tre <- lapply(sampler_params, function(x) x[, "treedepth__"])
	acc <- lapply(sampler_params, function(x) x[, "accept_stat__"])
	sts <- lapply(sampler_params, function(x) x[, "stepsize__"])
	div <- lapply(sampler_params, function(x) x[, "divergent__"])
	log_lik <- get_logposterior(stanMod, inc_warmup = TRUE)

	tree_depths_df <-
		tibble(Iteration = rep(1:length(tre[[1]]),length(tre)),
					 Chain = rep(1:length(tre),each = length(tre[[1]])),
					 Tree_Depth = unlist(tre))
	acc_df <-
		tibble(Iteration = rep(1:length(acc[[1]]),length(acc)),
					 Chain = rep(1:length(acc),each = length(acc[[1]])),
					 Tree_Depth = unlist(acc))
	sts_df <-
		tibble(Iteration = rep(1:length(sts[[1]]),length(sts)),
					 Chain = rep(1:length(sts),each = length(sts[[1]])),
					 Tree_Depth = unlist(sts))
	div_df <-
		tibble(Iteration = rep(1:length(div[[1]]),length(div)),
					 Chain = rep(1:length(div),each = length(div[[1]])),
					 Tree_Depth = unlist(div))

	lik_df <-
		tibble(Iteration = rep(1:length(log_lik[[1]]),length(log_lik)),
					 Chain = rep(1:length(log_lik),each = length(log_lik[[1]])),
					 Tree_Depth = unlist(log_lik))

	chain_col_map <- setNames(
		c('#F0944C',
			'#A3A55F',
			'#b4e5dd',
			'#C47688',
			'#B2B9BC',
			'#2E92A2'),c(1:6))


	p1 <-
		ggplot() +
		geom_line(data=tree_depths_df %>% filter(Iteration>=warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5) +
		geom_line(data=tree_depths_df %>% filter(Iteration<warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5,alpha=0.6) +
		labs(x = "Iteration",y = "Tree depth",color = "Chain") +
		theme_bw()+
		scale_color_manual(values = chain_col_map)+
		geom_rect(aes(xmin = 1, xmax = warmup_it, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2)+
		theme(
			axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.text.y = element_text(size=13),
			axis.title.y = element_text(size=14),
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14),
			legend.key.size = unit(1.7, "lines"),
			legend.position = 'none')

	p2 <-
		ggplot() +
		geom_point(data=div_df %>% filter(Iteration>=warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain))) +
		geom_point(data=div_df %>% filter(Iteration<warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),alpha=0.6) +
		labs(x = "Iteration",y = "Divergence\ntransition",color = "Chain") +
		scale_y_continuous(limits = c(-0.25,1.25), breaks=c(0,1),labels = c('No','Yes'))+
		theme_bw()+
		scale_color_manual(values = chain_col_map)+
		geom_rect(aes(xmin = 1, xmax = warmup_it, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2)+
		theme(
			axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.text.y = element_text(size=13),
			axis.title.y = element_text(size=14),
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14),
			legend.key.size = unit(1.7, "lines"),
			legend.position = 'none')


	p3 <-
		ggplot() +
		geom_line(data=sts_df %>% filter(Iteration>=warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5) +
		geom_line(data=sts_df %>% filter(Iteration<warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5,alpha=0.6) +
		labs(x = "Iteration",y = "Step size",color = "Chain") +
		theme_bw()+
		ylim(quantile(sts_df$Tree_Depth,0.0005),quantile(sts_df$Tree_Depth,0.9995))+
		scale_color_manual(values = chain_col_map)+
		geom_rect(aes(xmin = 1, xmax = warmup_it, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2)+
		theme(
			axis.title.x = element_text(size=14),
			axis.text.x = element_text(size=13),
			axis.text.y = element_text(size=13),
			axis.title.y = element_text(size=14),
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14),
			legend.key.size = unit(1.7, "lines"),
			legend.position = 'none')

	p4 <-
		ggplot() +
		geom_line(data=acc_df %>% filter(Iteration>=warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5) +
		geom_line(data=acc_df %>% filter(Iteration<warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5,alpha=0.6) +
		labs(x = "Iteration",y = "Accepctance rate",color = "Chain") +
		theme_bw()+
		scale_color_manual(values = chain_col_map)+
		geom_rect(aes(xmin = 1, xmax = warmup_it, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2)+
		theme(
			axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.text.y = element_text(size=13),
			axis.title.y = element_text(size=14),
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14),
			legend.key.size = unit(1.7, "lines"),
			legend.position = 'none')


	lik_range <-
		lik_df %>% filter(Iteration>=warmup_it) %>%
		mutate(lik_mean=mean(Tree_Depth),
					 lik_min=min(Tree_Depth),
					 lik_max=max(Tree_Depth)) %>%
		summarise(lik_mean=first(lik_mean),
							lik_min=first(lik_min),
							lik_max=first(lik_max)) %>%
		mutate(diff=lik_max-lik_min)

	precision <- (log10(abs(lik_range$lik_mean)) - log10(lik_range$diff)) %>% round()
	if(precision<0){precision = 0}

	scientific_10 <- function(x) {
		c <- log10(abs(x))
		expo <- floor(c)
		base <- round(x / 10^expo,precision)
		formatted <- paste0(base,'%*%',"10^", expo)
		str2expression(formatted)
	}

	p5 <-
		ggplot() +
		geom_line(data=lik_df %>% filter(Iteration>=warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5) +
		geom_line(data=lik_df %>% filter(Iteration<warmup_it),aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 0.5,alpha=0.6) +
		labs(x = "Iteration",y = "Posterior likelihood",color = "Chain") +
		theme_bw()+
		scale_y_continuous(labels=scientific_10,limits=c(lik_range$lik_min,lik_range$lik_max))+
		scale_color_manual(values = chain_col_map)+
		geom_rect(aes(xmin = 1, xmax = warmup_it, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2)+
		theme(
			axis.title.x = element_blank(),
			axis.text.x = element_blank(),
			axis.text.y = element_text(size=11),
			axis.title.y = element_text(size=14),
			legend.title = element_text(size = 16),
			legend.position = 'none')

	legend <-
		ggplot() +
		geom_line(data=lik_df,aes(x = Iteration, y = Tree_Depth, color = as.factor(Chain)),linewidth = 1) +
		theme_bw()+
		labs(x = "Iteration",y = "Accepctance rate",color = "Chain") +
		scale_color_manual(values = chain_col_map)+
		geom_rect(aes(xmin = 1, xmax = 200, ymin = -Inf, ymax = Inf, fill = "Warmup"),color = "black", alpha = 0.3) +
		geom_rect(aes(xmin = 200, xmax = 1000, ymin = -Inf, ymax = Inf, fill = "Sampling"), color = "black",alpha = 0.3) +
		scale_fill_manual(name = "Phase", values = c("Warmup" = "grey70", "Sampling" = "white")) +
		theme(
			legend.title = element_text(size = 16),
			legend.text = element_text(size = 14),
			legend.key.size = unit(1.7, "lines"))

	leg <- suppressWarnings(cowplot::get_legend(legend))

	pp2 <- suppressWarnings(cowplot::plot_grid(p5,p4,p1,p2,p3,ncol = 1,align = 'v',rel_heights = c(5,5,5,2.2,6)))

	pp3 <- suppressWarnings(cowplot::plot_grid(pp1,pp2,ncol = 1,rel_heights = c(1,3),labels = c('A','B'),label_x = 1))
	df1 <- suppressWarnings(cowplot::plot_grid(pp3,leg,ncol = 2,rel_widths = c(7,1.5)))

	return(df1)
}
