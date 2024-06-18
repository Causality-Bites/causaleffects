#' Compute confidence intervals from results of bootstrapping.
#'
#' @param res_boot Result from a call to `boot::boot`. A named list.
#'
#' @return Tibble with point estimate and confidence interval.
#' @export
inference_boot <- function(res_boot) {
  se <- sd(res_boot$t[, 1] - res_boot$t[, 2])
  meant0 <- res_boot$t0[1] - res_boot$t0[2]
  lower <- meant0 - qnorm(0.975) * se
  upper <- meant0 + qnorm(0.975) * se

  tibble::tribble(
    ~contrast, ~estimate, ~std.error, ~conf.low, ~conf.high,
    "Natural course - Shifted", meant0, se, lower, upper
  )
}
################################################################################
