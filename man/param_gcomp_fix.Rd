% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcomp.R
\name{param_gcomp_fix}
\alias{param_gcomp_fix}
\title{Implementation of the parametric g-formula for a single time point.}
\usage{
param_gcomp_fix(
  dat,
  idxs,
  baseline,
  exposures,
  outcome,
  form,
  shift_function,
  survey_weights,
  SL_library,
  cross_val
)
}
\arguments{
\item{dat}{Dataset with confounders, exposures, and outcome. A dataframe.}

\item{idxs}{Indexes for bootstrapping. A vector of integers.}

\item{baseline}{Baseline confounders. A vector of strings.}

\item{exposures}{Exposure variables. A vector of strings.}

\item{outcome}{Outcome variable. A string.}

\item{form}{Model formula. A strings.}

\item{shift_function}{Function to shift the exposures. A function.}

\item{survey_weights}{Optional vector of survey/balancing weights. A vector.}

\item{SL_library}{Vector of learners or function. A vector of strings or function.}

\item{cross_val}{TBD.}
}
\value{
The counterfactual means of the outcome under no intervention
(natural course) and the intervention of interest as
specified by the shift function.
}
\description{
Implementation of the parametric g-formula for a single time point.
}
