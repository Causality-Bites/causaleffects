#' Implementation of the parametric g-formula.
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
#'
#' @return The counterfactual mean of the outcome.
#' @export
parametric_g_computation <- function(dat,
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
                                     cross_val) {
  # Dispatch function
  if (is.null(time_varying)) {
    # Single time point
    res <- param_gcomp_fix()

  } else {
    # Time-varying exposures and confounders
  }

} # End function parametric_g_computation
################################################################################

#' Implementation of the parametric g-formula for a single time point.
#'
#' @param dat Dataset with confounders, exposures, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param exposures Exposure variables. A vector of strings.
#' @param outcome Outcome variable. A string.
#' @param form Model formula. A strings.
#' @param shift_function Function to shift the exposures. A function.
#' @param survey_weights Optional vector of survey/balancing weights. A vector.
#' @param SL_library Vector of learners or function. A vector of strings or function.
#' @param cross_val TBD.
#'
#' @return The counterfactual means of the outcome under no intervention
#'        (natural course) and the intervention of interest as
#'        specified by the shift function.
#' @export
param_gcomp_fix <- function(dat,
                            idxs,
                            baseline,
                            exposures,
                            outcome,
                            form,
                            shift_function,
                            survey_weights,
                            SL_library,
                            cross_val) {
  # Sample for bootstrapping
  dat <- dat[idxs, ]

  # Implementation #
  if (is.null(cross_val)) {
    # No cross-fitting

    if (SL_library == "glm") {
      # "Simple" linear model
      fam <- ifelse(
        is.factor(dat[[outcome]]) == TRUE,
        stats::binomial(link = "logit"),
        stats::gaussian(link = "identity")
      )
      mod <- stats::glm(
        formula = stats::as.formula(form),
        family = fam[[1]],
        data = dat,
        weights = survey_weights,
        na.action = "na.omit"
      )

      # Create counterfactual data and predict outcome
      dat_shifted <- shift_function(dat, exposures)
      dat[[outcome]] <- stats::predict(mod, newdata = dat)
      dat_shifted[[outcome]] <- stats::predict(mod, newdata = dat_shifted)

    } else {
      # SL
    } # End if for type model

  } else {
    # Sample splitting

  } # End if for CV choice

  return(
    c(mean(dat[[outcome]]), mean(dat_shifted[[outcome]]))
  )
} # End function param_gcomp_fix
################################################################################
