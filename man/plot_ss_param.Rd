% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Package_functions.R
\name{plot_ss_param}
\alias{plot_ss_param}
\title{Plot Model Parameters}
\usage{
plot_ss_param(stan_data, ss_param, xmin_log, xmax_log)
}
\arguments{
\item{stan_data}{A `list` of data formatted for input into the Stan model,
produced by `prep_stan_M1`.}

\item{ss_param}{A vector of output (unknown) parameters produced by fitting
the Stan model through `run_M1`. It can also be retrieved by `ss_param_extract`.}

\item{xmin_log}{A `numeric` value specifying the lower bound of the DNA concentration
range to be plotted on a logarithmic scale. If not provided, it defaults to
the floor of the minimum DNA concentration in `stan_data`.}

\item{xmax_log}{A `numeric` value specifying the upper bound of the DNA concentration
range to be plotted on a logarithmic scale. If not provided, it defaults to
the rounded maximum DNA concentration in `stan_data`.}
}
\value{
A `ggplot` object showing the probability of detection (left) and
  the continuous model (right) along the DNA concentration (x-axis).
}
\description{
This function creates a plot of the model parameters based on the Stan output,
including both the probability of detection and the continuous model along
the DNA concentration range.
}
\examples{
# Plot with automatically determined DNA concentration range
plot_ss_param(stan_data = stan_data_M1, ss_param = ss_param)

# Plot with a custom DNA concentration range
plot_ss_param(stan_data = stan_data_M1, ss_param = ss_param, xmin_log = -2, xmax_log = 5)
}
