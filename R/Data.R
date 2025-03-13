#' Atlantic cod qPCR Dataset Description
#'
#' This dataset contains qPCR results, from Guri et al. 2024 (Quantifying the Detection Sensitivity and Precision of qPCR and ddPCR Mechanisms for eDNA Samples).
#'
#' @format A tibble (data frame):
#' \describe{
#'   \item{Well}{A character vector indicating the well identifier (e.g., "A1", "B2").}
#'   \item{Sample_name}{A character vector indicating the name of the sample. Samples with the same names are considered technical replicates.}
#'   \item{Species}{A character vector indicating the targeted species.}
#'   \item{Sample_type}{A character vector indicating the task associated with each well (e.g., "STANDARD", "UNKNOWN").}
#'   \item{Ct}{A character vector with the quantification cycle (Cq) value for each well. "Undetermined" indicates no amplification.}
#'   \item{Plate}{A character vector indicating the plate in which the samples are run.}
#'   \item{Std_concentration}{A numeric vector with the DNA concentration of standards. NA for Sample_type == UNKNOWN}
#' }
#' @source qPCR data from Guri et al. 2024 (Quantifying the Detection Sensitivity and Precision of qPCR and ddPCR Mechanisms for eDNA Samples).
#'
#' @examples
#' data(cod_qpcr)
#' head(cod_qpcr)
#' summary(cod_qpcr)
"cod_qpcr"

#' Atlantic herring qPCR Dataset Description
#'
#' This dataset contains qPCR results, from Guri et al. 2024 (Quantifying the Detection Sensitivity and Precision of qPCR and ddPCR Mechanisms for eDNA Samples).
#'
#' @format A tibble (data frame):
#' \describe{
#'   \item{Well}{A character vector indicating the well identifier (e.g., "A1", "B2").}
#'   \item{Sample_name}{A character vector indicating the name of the sample. Samples with the same names are considered technical replicates.}
#'   \item{Species}{A character vector indicating the targeted species.}
#'   \item{Sample_type}{A character vector indicating the task associated with each well (e.g., "STANDARD", "UNKNOWN").}
#'   \item{Ct}{A character vector with the quantification cycle (Cq) value for each well. "Undetermined" indicates no amplification.}
#'   \item{Plate}{A character vector indicating the plate in which the samples are run.}
#'   \item{Std_concentration}{A numeric vector with the DNA concentration of standards. NA for Sample_type == UNKNOWN}
#' }
#' @source qPCR data from Guri et al. 2024 (Quantifying the Detection Sensitivity and Precision of qPCR and ddPCR Mechanisms for eDNA Samples).
#'
#' @examples
#' data(herring_qpcr)
#' head(herring_qpcr)
#' summary(herring_qpcr)
"herring_qpcr"

#' metabarcoding Dataset Description
#'
#' This dataset contains metabarcoding data from Guri et al. 2024 (Predicting trawl catches using environmental DNA)
#'
#' @format A tibble (data frame):
#' \describe{
#'   \item{Species}{A character vector indicating the targeted species.}
#'   \item{sp_idx}{An integer vector indicating species index. The index should be incremental and the last species should be the reference species (also the species analysed using qPCR)}
#'   \item{ini_conc}{An integer vector holding known concentration of each species input of the mock community samples before the PCR run}
#'   \item{Mock_1:Mock_6}{Integer vectors holding the metabarcoding sequencing reads of each species in the mock community after the PCR run.}
#'   \item{2019629_11:2019629_8}{Integer vectors holding the metabarcoding sequencing reads of each species in the environmental samples after the PCR run.}
#' }
#' @source metabarcoding data from Guri et al. 2024 (Predicting trawl catches using environmental DNA)
#'
#' @examples
#' data(metabarcoding)
#' head(metabarcoding)
#' summary(metabarcoding)
"metabarcoding"
