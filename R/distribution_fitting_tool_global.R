#' A function to run the distribution fitting tool application
#'
#' @return Opens the distribution fitting tool application
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_distribution_fitting_tool()
#' }
run_shiny_distribution_fitting_tool <- function(){
  shinyApp(ui = distribution_fitting_tool_UI, server = distribution_fitting_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the distribution fitting tool.

# Maximum likelihood Gamma fit used by the severity analysis. This is the same
# optimisation MASS::fitdistr(x, "gamma", method = "L-BFGS-B", lower = c(0, 0),
# start = list(scale = 1, shape = 1)) runs, written out so that MASS is not a
# dependency; it returns the estimates in the same order (scale, shape).
fit_gamma_mle <- function(x) {
  negative_loglik <- function(p) -sum(dgamma(x, shape = p[2], scale = p[1], log = TRUE))
  result <- optim(par = c(scale = 1, shape = 1), fn = negative_loglik, method = "L-BFGS-B", lower = c(0, 0))
  if (result$convergence > 0L) stop("optimization failed")
  list(estimate = result$par)
}

# Empirical cdf of claims evaluated at points: share of claims <= each point.
empirical_cdf_at <- function(points, claims) {
  findInterval(points, sort(claims)) / length(claims)
}

# Empirical mean excess function of claims evaluated at points:
# mean(claims[claims > point]) - point, NaN where no claim exceeds the point.
# Uses cumulative sums of the sorted claims instead of one subset per point.
mean_excess_at <- function(points, claims) {
  sorted <- sort(claims)
  # number of claims <= each point
  below <- findInterval(points, sorted)
  sum_below <- c(0, cumsum(sorted))[below + 1]
  count_above <- length(sorted) - below
  (sum(sorted) - sum_below) / count_above - points
}

# Piecewise Pareto helpers. They cover the case the tool uses (no truncation,
# no reporting thresholds, no censoring, unit weights) with the same arithmetic
# as Pareto::PiecewisePareto_ML_Estimator_Alpha() and Pareto::pPiecewisePareto(),
# so the Pareto package is not needed.

# Maximum likelihood estimate of the Pareto alphas of a piecewise Pareto
# distribution with strictly increasing thresholds t, fitted to losses.
piecewise_pareto_alpha <- function(losses, t) {
  k <- length(t)
  if (!is.numeric(t) || k < 1 || anyNA(t) || any(t <= 0) || any(is.infinite(t))) {
    warning("t must be positive.")
    return(NaN)
  }
  if (!is.numeric(losses) || length(losses) < 1 || anyNA(losses) || any(losses < 0) || any(is.infinite(losses))) {
    warning("losses must be non-negative.")
    return(rep(NaN, k))
  }
  if (k > 1 && min(diff(t)) <= 0) {
    warning("t must be strictly ascending.")
    return(rep(NaN, k))
  }
  if (max(losses) <= max(t)) {
    warning("Number of losses > max(t) must be positive.")
    return(rep(NaN, k))
  }
  losses <- losses[losses > t[1]]
  if (length(losses) == 0) {
    warning("No losses larger than t[1].")
    return(rep(NaN, k))
  }
  upper <- c(t[-1], Inf)
  alpha <- numeric(k)
  for (i in seq_len(k)) {
    in_layer <- losses[losses >= t[i]]
    alpha[i] <- (length(in_layer) - sum(losses >= upper[i])) /
      sum(log(pmin(in_layer, upper[i]) / t[i]))
  }
  alpha
}

# Cumulative distribution function of a piecewise Pareto distribution with
# thresholds t and alphas alpha, evaluated at the vector x.
piecewise_pareto_cdf <- function(x, t, alpha) {
  if (is.null(x) || length(x) == 0) return(numeric())
  k <- length(t)
  valid <- is.numeric(t) && is.numeric(alpha) && k >= 1 && length(alpha) == k &&
    !anyNA(t) && !anyNA(alpha) && all(t > 0) && all(is.finite(t)) &&
    all(alpha >= 0) && all(is.finite(alpha)) && alpha[k] > 0 &&
    (k == 1 || min(diff(t)) > 0)
  if (!valid) {
    warning("t must be positive and increasing, alpha non-negative with a positive last value.")
    return(rep(NaN, length(x)))
  }
  # survival probability at each threshold: S(t[1]) = 1, S(t[j+1]) = S(t[j]) * (t[j] / t[j+1])^alpha[j]
  survival <- c(1, cumprod((t[-k] / t[-1])^alpha[-k]))
  j <- findInterval(x, t, left.open = TRUE)
  p <- numeric(length(x))
  above <- which(j > 0)
  p[above] <- 1 - survival[j[above]] * (t[j[above]] / x[above])^alpha[j[above]]
  p[is.na(x)] <- NaN
  p
}
