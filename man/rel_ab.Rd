% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Package_functions.R
\name{rel_ab}
\alias{rel_ab}
\title{Convert Columns to Relative Abundance}
\usage{
rel_ab(df)
}
\arguments{
\item{df}{A numeric data.frame or matrix containing absolute values.
Non-numeric columns may produce unexpected results or errors.}
}
\value{
A data.frame (if input is a data.frame) or matrix (if input is a matrix)
where each column sums to 1 (ignoring NA values). Columns with all NA values will
return NaN for all entries.
}
\description{
This function converts absolute values in each column of a data frame or matrix
to relative abundances (proportions) by dividing each element by its column sum.
Missing values (NA) are excluded from sum calculations but retained in the output.
}
\examples{
# Basic usage
data <- data.frame(A = c(10, 20, 30, NA), B = c(1, 2, 3, 4))
rel_ab(data)
# Returns:
# A B
# 1 0.1667 0.1
# 2 0.3333 0.2
# 3 0.5000 0.3
# 4 NA 0.4

# Edge case: Column with all NA
data_na <- data.frame(C = c(NA, NA, NA), D = c(2, 2, 2))
rel_ab(data_na)
# Returns:
# C D
# 1 NaN 0.3333
# 2 NaN 0.3333
# 3 NaN 0.3333
}
