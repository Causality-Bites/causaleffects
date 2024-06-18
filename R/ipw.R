#' Implementation of an IPW estimator.
#'
#' @param dat Dataset with confounders, exposures, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param time_varying Time-varying confounders. A vector of vectors of strings.
#' @param exposures Exposure variables. A vector of vectors of strings.
#' @param outcome Outcome variable. A string.
#' @param censoring Censoring variable. A vector of strings.
#' @param forms Models formulas. A vector of strings.
#' @param shift_functions Functions to shift the exposures. A vector of functions.
#' @param survey_weights Optional vector of survey/balancing weights. A vector.
#' @param SL_library Vector of learners or function. A vector of strings or function.
#' @param cross_val TBD.
#' @param method Which method to use for estimating the weights. A string.
#'
#' @return The counterfactual mean of the outcome.
#' @export
ipw <- function(dat,
                idxs,
                baseline,
                time_varying,
                exposures,
                outcome,
                censoring,
                forms,
                shift_functions,
                survey_weights,
                SL_library,
                cross_val,
                method) {
  # Dispatch function
  if (is.null(time_varying)) {
    # Single time point
    res <- ipw_fix()

  } else {
    # Time-varying exposures and confounders
  }
} # End function ipw
################################################################################

#' Implementation of a IPW estimator (categorical exposures)
#' for a single time point.
#'
#' @param dat Dataset with confounders, exposures, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param exposures Exposure variables. A vector of strings.
#' @param outcome Outcome variable. A string.
#' @param forms Model formula. A strings.
#' @param shift_function Function to shift the exposures. A function.
#' @param survey_weights Optional vector of survey/balancing weights. A vector.
#' @param SL_library Vector of learners or function. A vector of strings or function.
#' @param cross_val TBD.
#'
#' @return The counterfactual means of the outcome under no intervention
#'        (natural course) and the intervention of interest as
#'        specified by the shift function.
#' @export
ipw_cat_fix <- function(dat,
                        idxs,
                        baseline,
                        exposures,
                        outcome,
                        forms,
                        shift_function,
                        survey_weights,
                        SL_library,
                        cross_val) {
  # Sample for bootstrapping
  dat <- dat[idxs, ]

  # Dummy variables of the exposures
  levels_exposures <- lapply(
    dat[[exposures]], function(x) {
      length(unique(dat[[x]]))
    }
  )
  names(levels_exposures) <- exposures
  old_colnames <- colnames(dat)
  dat <- fastDummies::dummy_cols(
    .data = dat,
    select_columns = exposures,
    remove_first_dummy = FALSE,
    remove_most_frequent_dummy = FALSE,
    remove_selected_columns = FALSE
  )
  dat <- dat |>
    dplyr::mutate(
      dplyr::across(setdiff(colnames(dat), old_colnames), as.factor)
    )

  # Implementation #
  if (is.null(cross_val)) {
    # No cross-fitting

    if (SL_library == "polr") {
      # Ordinal logistic model
      mod <- MASS::polr(
        formula =
      )

    } else {
      # SL

    } # End if for type model

  } else {
    # Sample splitting

  } # End if for CV choice

  return(
    c()
  )
} # End function ipw_fix
################################################################################
