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
} # End function ipw
################################################################################

#' Implementation of a specific IPW estimator (categorical exposures)
#' for a single time point.
#'
#' @param dat Dataset with confounders, exposures, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param exposure Exposure variable. A string.
#' @param outcome Outcome variable. A string.
#' @param form Model formula. A string.
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
                        exposure,
                        outcome,
                        form,
                        survey_weights,
                        SL_library,
                        cross_val) {
  # Sample for bootstrapping
  dat <- dat[idxs, ]
  old_colnames <- colnames(dat)
  levels_exposure <- length(unique(dat[[exposure]]))

  # Implementation #
  if (is.null(cross_val)) {
    # No cross-fitting

    if (SL_library == "polr") {
      # Ordinal logistic model
      mod <- MASS::polr(
        formula = as.formula(form),
        data = dat,
        weights = survey_weights,
        na.action = "na.omit",
        method = "logit"
      )

    } else if (SL_library == "glm") {
      # Series of logistic regression models
      dat <- fastDummies::dummy_cols(
        .data = dat,
        select_columns = exposure,
        remove_first_dummy = FALSE,
        remove_most_frequent_dummy = FALSE,
        remove_selected_columns = FALSE
      )
      dat <- dat |>
        dplyr::mutate(
          dplyr::across(setdiff(colnames(dat), old_colnames), as.factor)
        )

      mods <- lapply(1:(levels_exposure - 1), function(lev) {
        dat_fit <- ifelse(
          lev == 1,
          dat,
          dat[dat[[exposure]] != min(dat[[exposure]], na.rm = TRUE), ]
        )
        glm(
          formula = as.formula(form),
          data = dat_fit,
          family = binomial(link = "logit"),
          weights = survey_weights,
          na.action = "na.omit"
        )
      }) # End fitting models

      # Estimate probabilities and weights
      for (lev in 1:levels_exposure) {
        prob_label <- paste0("p", lev)
        if (lev > 1 & lev < levels_exposure) {
          dat[[prob_label]] <- stats::predict(
            mods[[lev]], newdata = dat, type = "response"
          ) * (1 - predict(mods[[1]], type = "response"))
        } else if (lev == 1) {
          dat[[prob_label]] <- predict(mods[[lev]], type = "response")
        } else {
          # Maximum value of the exposure
          sum_probs <- lapply(2:(idx - 1), function(i) {
            predict(mods[[i]], newdata = dat, type = "response")
          }) |>
            dplyr::bind_cols()
          sum_probs <- apply(sum_probs, MARGIN = 1, FUN = sum)
          dat[[prob_label]] <- (1 - sum_probs) *
            (1 - predict(mods[[1]], type = "response"))
        }
      } # End estimation probabilities

      dat[["weights"]] <- NA
      dat[dat[[exposure]] == levels_exposure, "weights"] <- 0
      dat[dat[[exposure]] == 1, "weights"] <- 1 +
        (dat[dat[[exposure]] == 1, "p2"] / dat[dat[[exposure]] == 1, "p1"])
      for (lev in 2:(levels_exposure - 1)) {
        num <- dat[dat[[exposure]] == idx, paste0("p", idx + 1)]
        den <- dat[dat[[exposure]] == idx, paste0("p", idx)]
        dat[dat[[exposure]] == lev, "weights"] <- num / den
      } # End estimation weights

    } else {
      # SL

    } # End if for type model

  } else {
    # Sample splitting

  } # End if for CV choice

  return(
    c(mean(dat[[outcome]]), mean(dat[["weights"]] * dat[[outcome]]))
  )
} # End function ipw_cat_fix
################################################################################
