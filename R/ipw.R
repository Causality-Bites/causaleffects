#' Implementations of IPW estimators.
#'
#' @param dat Dataset with confounders, exposures, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param time_varying Time-varying confounders. A vector of vectors of strings.
#' @param exposures Exposure variables. A vector of vectors of strings.
#' @param outcome Outcome variable. A string.
#' @param censoring Censoring variable. A vector of strings.
#' @param forms Models formulas. A vector of strings.
#' @param form_cens TBD
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
                form_cens,
                shift_functions,
                survey_weights,
                SL_library,
                cross_val,
                method) {
  # Dispatch function
} # End function ipw
################################################################################

#' Implementation of a specific IPW estimator
#' for a single binary exposure and time point.
#'
#' @param dat Dataset with confounders, exposure, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param exposure Exposure variable. A string.
#' @param outcome Outcome variable. A string.
#' @param form Model formula. A string.
#' @param form_cens TBD
#' @param survey_weights Optional vector of survey/balancing weights. A vector.
#' @param SL_library Vector of learners or function. A vector of strings or function.
#' @param cross_val TBD.
#'
#' @return The counterfactual means of the outcome under no intervention
#'        (natural course) and the intervention of interest as
#'        specified by the shift function.
#' @export
ipw_bin_fix <- function(dat,
                        idxs,
                        baseline,
                        exposure,
                        outcome,
                        form,
                        form_cens,
                        survey_weights,
                        SL_library,
                        cross_val) {
  # Sample for bootstrapping
  dat <- dat[idxs, ]

  # Implementation #
  if (is.null(cross_val)) {
    # No cross-fitting

    if (SL_library == "glm") {
      # Logistic regression model
      mod <- glm(
        formula = as.formula(paste0(exposure, " ~ ", form)),
        data = dat,
        family = binomial(link = "logit"),
        weights = survey_weights,
        na.action = "na.omit"
      )

      # Estimate probabilities and weights
      p1 <- predict(mod, newdata = dat, type = "response")
      dat <- dat |>
        dplyr::mutate(
          weights = dplyr::case_when(
            .data[[exposure]] == 1 ~ 0,
            .data[[exposure]] == 0 ~ (1 + (p1 / (1 - p1))),
          )
        )

      # Eventually estimate censoring weights P[C=0|A,W]
      if (sum(is.na(dat[[outcome]])) > 0) {
        dat[["observed"]] <- as.factor(!is.na(dat[[outcome]]))
        cens_mod <- glm(
          formula = as.formula(paste0("observed ~ ", form_cens)),
          data = dat,
          family = binomial(link = "logit"),
          weights = survey_weights,
          na.action = "na.omit"
        )
        dat <- dat |>
          dplyr::mutate(
            p_cens = predict(cens_mod, newdata = dat, type = "response"),
            w_cens = 1 / p_cens
          )
        dat[dat$observed == FALSE, "outcome"] <- 0
      } else {
        dat <- dat |>
          dplyr::mutate(w_cens = 1)
      } # End censoring weights

    } else if (SL_library == "SL") {
      # SL

    } # End if for type model

  } else {
    # Sample splitting

  } # End if for CV choice

  return(
    c(
      mean(dat[["w_cens"]] * dat[[outcome]]),
      mean(dat[["weights"]] * dat[["w_cens"]] * dat[[outcome]])
    )
  )
} # End ipw_bin_fix
################################################################################

#' Implementation of a specific IPW estimator
#' for a single categorical exposure and time point.
#'
#' @param dat Dataset with confounders, exposure, and outcome. A dataframe.
#' @param idxs Indexes for bootstrapping. A vector of integers.
#' @param baseline Baseline confounders. A vector of strings.
#' @param exposure Exposure variable. A string.
#' @param outcome Outcome variable. A string.
#' @param form Model formula. A string.
#' @param form_cens TBD
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
                        form_cens,
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
      p0 <- predict(mods[[1]], newdata = dat, type = "response")
      for (lev in unique_exposures) {
        prob_label <- paste0("p", lev)
        if (lev > min_exposure & lev < max_exposure) {
          dat[[prob_label]] <- stats::predict(
            mods[[as.character(lev)]], newdata = dat, type = "response"
          ) * (1 - p0)
        } else if (lev == min_exposure) {
          dat[[prob_label]] <- p0
        } else {
          # Maximum value of the exposure
          sum_probs <- lapply(unique_exposures[2:(levels_exposure-1)], function(i) {
            predict(mods[[as.character(i)]], newdata = dat, type = "response")
          }) |>
            dplyr::bind_cols()
          sum_probs <- apply(sum_probs, MARGIN = 1, FUN = sum)
          dat[[prob_label]] <- 1 - p0 - (1 - p0) * sum_probs
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

      # Eventually estimate censoring weights P[C=0|A,W]
      if (sum(is.na(dat[[outcome]])) > 0) {
        dat[["observed"]] <- as.factor(!is.na(dat[[outcome]]))
        cens_mod <- glm(
          formula = as.formula(paste0("observed ~ ", form_cens)),
          data = dat,
          family = binomial(link = "logit"),
          weights = survey_weights,
          na.action = "na.omit"
        )
        dat <- dat |>
          dplyr::mutate(
            p_cens = predict(cens_mod, newdata = dat, type = "response"),
            w_cens = 1 / p_cens
          )
        dat[dat$observed == FALSE, "outcome"] <- 0
      } else {
        dat <- dat |>
          dplyr::mutate(w_cens = 1)
      } # End censoring weights

    } else if (SL_library == "SL") {
      # SL

    } # End if for type model

  } else {
    # Sample splitting

  } # End if for CV choice

  return(
    c(
      mean(dat[["w_cens"]] * dat[[outcome]]),
      mean(dat[["weights"]] * dat[["w_cens"]] * dat[[outcome]])
    )
  )
} # End function ipw_cat_fix
################################################################################
