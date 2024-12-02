#' qPCR Dataset Description
#'
#' This dataset contains qPCR experiment results, including information about wells, tasks, Cq values, quantities, and sample identifiers.
#'
#' @format A tibble (data frame):
#' \describe{
#'   \item{Well}{A character vector indicating the well identifier (e.g., "A1", "B2").}
#'   \item{Task}{A character vector indicating the task associated with each well (e.g., "STANDARD", "UNKNOWN").}
#'   \item{Cq}{A character vector with the quantification cycle (Cq) value for each well. "Undetermined" indicates no amplification.}
#'   \item{Quantity}{A numeric vector with the quantity values for standards. NA for unknowns.}
#'   \item{Sample}{An integer vector with the sample identifiers for unknowns. NA for standards.}
#' }
#' @source Experimental qPCR results generated for analysis.
#'
#' @examples
#' data(qpcr)
#' head(qpcr)
#' summary(qpcr)
"qpcr"
