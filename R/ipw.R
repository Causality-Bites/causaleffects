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
  unique_exposures <- sort(unique(as.numeric(as.character(dat[[exposure]]))))
  levels_exposure <- length(unique_exposures)
  min_exposure <- min(as.numeric(as.character(dat[[exposure]])), na.rm = TRUE)
  max_exposure <- max(as.numeric(as.character(dat[[exposure]])), na.rm = TRUE)

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

      mods <- lapply(unique_exposures[1:(levels_exposure-1)], function(lev) {
        if (lev == min_exposure) {
          dat_fit <- dat
        } else {
          dat_fit <- dat[dat[[exposure]] != min_exposure, ]
        }
        glm(
          formula = as.formula(paste0(
            paste0(exposure, "_", lev),
            " ~ ",
            form
          )),
          data = dat_fit,
          family = binomial(link = "logit"),
          weights = survey_weights,
          na.action = "na.omit"
        )
      }) # End fitting models
      names(mods) <- unique_exposures[1:(levels_exposure-1)]

      # Estimate probabilities and weights
      for (lev in unique_exposures) {
        prob_label <- paste0("p", lev)
        if (lev > min_exposure & lev < max_exposure) {
          dat[[prob_label]] <- stats::predict(
            mods[[as.character(lev)]], newdata = dat, type = "response"
          ) * (1 - predict(mods[[1]], type = "response"))
        } else if (lev == min_exposure) {
          dat[[prob_label]] <- predict(mods[[1]], type = "response")
        } else {
          # Maximum value of the exposure
          sum_probs <- lapply(unique_exposures[2:(levels_exposure-1)], function(i) {
            predict(mods[[as.character(i)]], newdata = dat, type = "response")
          }) |>
            dplyr::bind_cols()
          sum_probs <- apply(sum_probs, MARGIN = 1, FUN = sum)
          dat[[prob_label]] <- (1 - sum_probs) *
            (1 - predict(mods[[1]], type = "response"))
        }
      } # End estimation probabilities

      dat[["weights"]] <- NA
      dat[dat[[exposure]] == max_exposure, "weights"] <- 0
      for (lev in unique_exposures[2:(levels_exposure-1)]) {
        pos <- which(unique_exposures == lev)
        num <- dat[dat[[exposure]] == lev, paste0("p", unique_exposures[pos+1])]
        den <- dat[dat[[exposure]] == lev, paste0("p", lev)]
        dat[dat[[exposure]] == lev, "weights"] <- num / den
      }
      dat[dat[[exposure]] == min_exposure, "weights"] <- 1 +
        (dat[dat[[exposure]] == min_exposure, paste0("p", unique_exposures[2])] /
           dat[dat[[exposure]] == min_exposure, paste0("p", unique_exposures[1])])
      # End estimation weights

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
