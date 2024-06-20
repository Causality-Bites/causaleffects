rm(list=ls())
source("R/ipw.R")

turn_off <- function(dat, exposure) {
  as.factor(rep(0, length(dat[[exposure]])))
}
shift_lmtp <- function(dat, exposure) {
  dat[[exposure]] <- as.numeric(dat[[exposure]])
  tmp <- (dat[[exposure]] == 1) * (dat[[exposure]] - 1) +
    (dat[[exposure]] == 0) * dat[[exposure]]
  as.factor(tmp)
}

dat <- readr::read_csv("dat.csv")
dat <- dat |>
  dplyr::mutate(
    exposure = dplyr::case_when(
      exposure %in% c(1, 2) ~ 0,
      exposure %in% c(3, 4) ~ 1,
    ),
    dplyr::across(
      c(sex, eth, edu, sep, exposure, C),
      as.factor
    )
  )

exposure = "exposure"
outcome = "outcome"
form = "sex*log(age) + eth + edu + sep + log(weight)"
form_cens = "exposure"
survey_weights = NULL
SL_library = "glm"
cross_val = NULL

boot::boot(
  data = dat,
  statistic = ipw_bin_fix,
  ########################
  baseline = c("sex", "eth", "age", "edu", "sep", "weight"),
  exposure = "exposure",
  outcome = "outcome",
  form = "sex*log(age) + eth + edu + sep + log(weight)",
  form_cens = "exposure",
  survey_weights = NULL,
  SL_library = "glm",
  cross_val = NULL,
  ########################
  R = 499,
  sim = "ordinary",
  parallel = "multicore",
  ncpus = parallel::detectCores() - 2
)

lmtp_ipw_res_null <- lmtp::lmtp_ipw(
  data = dat,
  trt = "exposure",
  outcome = "outcome",
  baseline = c("sex", "eth", "age", "edu", "sep", "weight"),
  cens = "C",
  shift = NULL,
  mtp = TRUE,
  outcome_type = "continuous",
  folds = 1
)
lmtp_ipw_res_shift <- lmtp::lmtp_ipw(
  data = dat,
  trt = "exposure",
  outcome = "outcome",
  baseline = c("sex", "eth", "age", "edu", "sep", "weight"),
  cens = "C",
  shift = shift_lmtp,
  mtp = TRUE,
  outcome_type = "continuous",
  folds = 1
)
lmtp_ipw_res_shift$theta - lmtp_ipw_res_null$theta
