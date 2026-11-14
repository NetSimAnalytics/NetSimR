#' Run the distribution fitting application
#'
#' @description Opens NetDisFit, a shiny application that fits frequency and
#'   severity distributions to an uploaded claims file, compares them with the
#'   data, and splices Pareto tails onto a LogNormal body or fits a piecewise
#'   Pareto at chosen slicing points, ready for the NetSimR simulator.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{run_shiny_simulator}} and
#'   \code{\link{run_shiny_glm_fitting_tool}}, two of the package's other applications;
#'   \code{\link{SlicedLNormParetoMean}}, \code{\link{SlicedLNormParetoCappedMean}},
#'   \code{\link{SlicedGammaParetoMean}} and \code{\link{SlicedGammaParetoCappedMean}}
#'   for the moments of the sliced distributions the tool fits.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_distribution_fitting_tool()
#' }
run_shiny_distribution_fitting_tool <- function(){
  shinyApp(ui = distribution_fitting_tool_UI, server = distribution_fitting_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the distribution fitting tool. The charts, tables and data reading
# it shares with the other tools are in R/tool_charts.R, R/tool_tables.R and R/tool_data.R.

# The example data sets of the Data tab: motor policies with claim counts and exposure
# (for the Frequency tab) and liability claims at their policy limits (for the Severity tab).
dft_examples <- c("Motor policies: claim counts and exposure" = "motor",
                  "Liability claims: amounts and policy limits" = "liability")

# The settings file of the tool: the inputs it keeps, each with its kind (how it is restored).
# A column is chosen once data with that column is read; the slicing points and the piecewise
# thresholds are kept too, and set when their analysis runs.
dft_settings_tool <- "Distribution fitting tool"
dft_settings_version <- 1L
dft_settings_inputs <- c(
  data_source = "radio", example = "select", data_includes_header = "switch", sep = "radio", dec = "radio",
  quote = "radio",
  counts_var = "column", counts_weighted_var = "switch", counts_weights_var = "column", count_hist_bins = "slider",
  FreqDistri = "radio",
  severity_var = "column", sev_threshold = "numeric", sev_limit_source = "radio", sev_limit = "numeric",
  sev_limit_col = "column", sev_fit_log_scale = "switch", severity_hist_bins = "slider",
  sliced_sev_var = "column", sev_cens_fit_log_scale = "switch",
  piecewise_pareto_var = "column", num_pareto_slices = "slider", piecewise_pareto_fit_log_scale = "switch",
  export_frequency = "select", export_severity = "select"
)

# Rounds a slider bound to three significant digits, down or up, so that the
# bound still includes the data.
dft_nice_bound <- function(x, up = FALSE) {
  if (!is.finite(x) || x == 0) return(x)
  step <- 10^(floor(log10(abs(x))) - 2)
  if (up) ceiling(x / step) * step else floor(x / step) * step
}

# The top of a slider that places a slicing point or threshold among the
# claims: the largest claim once the top half a percent are left out, rounded
# up, rather than the largest claim itself. Claims are heavy tailed, and with
# the largest claim as the top the body of the distribution took less than a
# pixel of the bar (3,000 claims around 1,000 and one of 5,000,000: the first
# user click snapped to 380,000 or to the bottom). Leaving out whole claims
# rather than taking the 99.5th percentile keeps the largest claim as the top
# of fewer than two hundred claims, where half a percent is not a claim at
# all; a point above the top can still be typed.
dft_slider_top <- function(x) {
  sorted <- sort(x)
  if (!length(sorted)) return(NA_real_)
  dft_nice_bound(sorted[length(sorted) - floor(length(sorted) * 0.005)], up = TRUE)
}

# Parameter cell: a muted parameter name followed by its value.
dft_param_cell <- function(name, value, digits = 4) {
  if (is.null(name) || identical(name, "")) return(tags$span(class = "dft-na", netsimr_dash))
  tagList(tags$span(class = "dft-param-name", name), netsimr_fmt(value, digits))
}

# Maximum likelihood Gamma fit used by the severity analysis, with the
# estimates in the order MASS::fitdistr(x, "gamma") gives them (scale, shape).
# It solves the likelihood equations (fit_gamma_profile()) rather than running
# an optimiser from scale = 1, shape = 1 as fitdistr() does: that optimiser
# stops short of the maximum, and for claims of 1e9 or more it reports success
# far from it (a shape of 141 instead of 2 at 1e12).
fit_gamma_mle <- function(x) {
  fit_gamma_profile(x)
}

# Maximum likelihood Gamma fit from the profile likelihood: the shape k solves
# log(k) - digamma(k) = log(mean(x)) - mean(log(x)), whose left side falls from
# +Inf to 0, and the scale is mean(x) / k. The right side is computed on
# x / mean(x), so that it does not depend on the scale of the claims.
fit_gamma_profile <- function(x) {
  target <- -mean(log(x / mean(x)))
  if (!is.finite(target) || target <= 0) stop("the claims must be positive and not all equal")
  shape <- stats::uniroot(function(k) log(k) - digamma(k) - target,
                          lower = 1e-8, upper = max(10, 10 / target), tol = 1e-12)$root
  list(estimate = c(scale = mean(x) / shape, shape = shape))
}

# Empirical cdf of claims evaluated at points: share of claims <= each point.
# With weights, each claim counts `weight` times.
empirical_cdf_at <- function(points, claims, weights = NULL) {
  if (is.null(weights)) return(findInterval(points, sort(claims)) / length(claims))
  o <- order(claims)
  cumulative <- c(0, cumsum(weights[o]))
  cumulative[findInterval(points, claims[o]) + 1] / sum(weights)
}

# Kolmogorov-Smirnov distance between the empirical cdf of claims and the
# cdf function cdf_fun: the largest gap on either side of each step of the
# empirical cdf, as in stats::ks.test(). sorted = TRUE says the claims are
# already sorted, when several cdfs are compared with the same claims.
ks_distance <- function(claims, cdf_fun, sorted = FALSE) {
  x <- if (sorted) claims else sort(claims)
  n <- length(x)
  if (n == 0) return(NA_real_)
  fitted <- cdf_fun(x)
  if (anyNA(fitted)) return(NA_real_)
  max(seq_len(n) / n - fitted, fitted - (seq_len(n) - 1) / n)
}

# Grid of claim sizes for drawing fitted cdfs: evenly spaced from zero, or on
# a log scale from the smallest claim, up to the largest claim.
dft_severity_grid <- function(claims, log_scale = FALSE, n = 500) {
  top <- max(claims)
  if (log_scale) {
    bottom <- min(claims[claims > 0])
    if (bottom >= top) return(top)
    exp(seq(log(bottom), log(top), length.out = n))
  } else {
    seq(0, top, length.out = n)
  }
}

# Points at which to draw the empirical cdf as a step line: every distinct
# claim, or evenly spaced quantiles when there are more than max_points.
dft_ecdf_points <- function(claims, max_points = 2000) {
  points <- sort(unique(claims))
  if (length(points) > max_points) {
    points <- unique(stats::quantile(claims, seq(0, 1, length.out = max_points), type = 1, names = FALSE))
  }
  points
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
# so the Pareto package is not needed, except that a loss equal to the first
# threshold counts in the first layer.

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
  # the first layer includes a claim at t[1], so that with t[1] the smallest
  # claim it is the Severity tab's Pareto (the Pareto package leaves it out)
  losses <- losses[losses >= t[1]]
  if (length(losses) == 0) {
    warning("No losses at or above t[1].")
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

# Maximum likelihood fits of the Frequency tab, with the fields of
# fitdistrplus::fitdist() that the tool uses: estimate, sd, cor, loglik, aic,
# bic and n (the number of rows, as fitdist() counts it for the BIC). Weights
# are case weights, as in fitdist(): a row of weight 3 counts as three
# observations, so the log-likelihood is the weighted sum of the log densities
# and the standard errors come from the information of the total weight.

# Poisson: the MLE of lambda is the (weighted) mean, and its standard error
# sqrt(lambda / total weight). fitdist() finds it with an optimiser, which
# stops within about 1e-8 of it.
fit_poisson_mle <- function(x, weights = NULL) {
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  total <- sum(w)
  lambda <- sum(w * x) / total
  loglik <- sum(w * stats::dpois(x, lambda, log = TRUE))
  list(estimate = c(lambda = lambda), sd = c(lambda = sqrt(lambda / total)), cor = NULL,
       loglik = loglik, aic = -2 * loglik + 2, bic = -2 * loglik + log(length(x)),
       n = length(x), distname = "pois", weighted = !is.null(weights), capped = FALSE)
}

# The Negative Binomial size at which the fit stops: when the variance of the
# counts is not above their mean, the likelihood rises without end as the size
# grows, towards the Poisson, and has no maximum. At this size the Negative
# Binomial is the Poisson for every practical purpose.
dft_nbinom_max_size <- 1e8

# t - log(1 + t) for t >= 0, without the cancellation of the direct formula
# for small t (its series t^2/2 - t^3/3 + ... converges fast below 0.1).
dft_log1p_excess <- function(t) {
  if (t >= 0.1) return(t - log1p(t))
  m <- 2:30
  sum((-1)^m * t^m / m)
}

# Sums over the rows of w * sum(f(j) for j = 0, ..., x - 1), for the integer
# counts x, as sums over j of f(j) times the weight of the counts above j.
# Counts above `limit` use the closed forms of the two sums the fit needs,
# from j = limit up: sum(j / (size + j)) = (x - limit) - size * (digamma(size
# + x) - digamma(size + limit)) and sum(1 / (size + j)^2) = trigamma(size +
# limit) - trigamma(size + x).
dft_nbinom_count_sums <- function(x, w, limit = 1e6) {
  top <- min(max(x), limit)
  # weight of the counts above j, for j = 0, ..., top - 1
  by_value <- numeric(top + 1)
  summed <- rowsum(w, pmin(x, top))
  by_value[as.numeric(rownames(summed)) + 1] <- summed
  above <- rev(cumsum(rev(by_value)))[-1]
  j <- seq_len(top) - 1
  big <- x > limit
  list(
    # sum over the rows of w * sum(j / (size + j), j < x)
    ratio = function(size) {
      s <- sum(above * j / (size + j))
      if (any(big)) s <- s + sum(w[big] * ((x[big] - limit) - size * (digamma(size + x[big]) - digamma(size + limit))))
      s
    },
    # sum over the rows of w * sum(1 / (size + j)^2, j < x)
    square = function(size) {
      s <- sum(above / (size + j)^2)
      if (any(big)) s <- s + sum(w[big] * (trigamma(size + limit) - trigamma(size + x[big])))
      s
    }
  )
}

# Negative Binomial in fitdist()'s parameters, size and mu. The MLE of mu is
# the (weighted) mean whatever the size, so the size solves the likelihood
# equation for it with mu at the mean. That equation has a root, and only one,
# exactly when the variance of the counts (divided by the total weight) is
# above the mean (Levin and Reeds 1977); otherwise the likelihood rises
# towards the Poisson as the size grows. The standard errors come from the
# observed information, whose off-diagonal term is zero at the maximum.
# fitdist() optimises both parameters with Nelder-Mead, which stops about 1e-4
# (relative) from the maximum.
fit_nbinom_mle <- function(x, weights = NULL) {
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  total <- sum(w)
  mu <- sum(w * x) / total
  if (!(mu > 0)) stop("the claim counts are all zero")
  spread <- sum(w * (x - mu)^2) / total
  sums <- dft_nbinom_count_sums(x, w)
  # the derivative of the log-likelihood in the size, times the size, with mu at the mean:
  # positive below the MLE and negative above it. As digamma(x + size) - digamma(size)
  # - log(1 + mu / size) it is a difference of nearly equal numbers when the size is
  # large (its sign at 1e8 was rounding error); since sum(w * x) = total * mu it is
  # also the difference of these two positive sums, which keep their precision
  score <- function(log_size) {
    size <- exp(log_size)
    total * size * dft_log1p_excess(mu / size) - sums$ratio(size)
  }
  capped <- !(spread > mu)
  if (!capped) {
    # a bracket around the root, from the moment estimate mu^2 / (variance - mu): the
    # score is positive as the size tends to 0 and negative for large sizes
    start <- min(2 * log(mu) - log(spread - mu), 600)
    lower <- start
    while (isTRUE(score(lower) <= 0) && lower > start - 60) lower <- lower - 1
    upper <- lower + 1
    while (isTRUE(score(upper) >= 0) && upper < start + 60) upper <- upper + 1
    # the variance above the mean only by rounding: no sign change within e^60 of the start
    capped <- !isTRUE(score(lower) > 0 && score(upper) < 0)
  }
  size <- if (capped) {
    dft_nbinom_max_size
  } else {
    exp(stats::uniroot(score, c(lower, upper), tol = 1e-12)$root)
  }
  loglik <- sum(w * stats::dnbinom(x, size = size, mu = mu, log = TRUE))
  # observed information, minus the second derivatives of the log-likelihood; in the
  # size, with mu at the mean, it is sum(w * sum(1 / (size + j)^2, j < x)) - total * mu /
  # (size * (size + mu)) (trigamma(x + size) - trigamma(size) = -sum(1 / (size + j)^2, j < x))
  info <- matrix(c(
    sums$square(size) - total * mu / (size * (size + mu)),
    -sum(w * (x - mu) / (size + mu)^2),
    -sum(w * (x - mu) / (size + mu)^2),
    sum(w * (x / mu^2 - (x + size) / (size + mu)^2))
  ), 2, 2, dimnames = list(c("size", "mu"), c("size", "mu")))
  if (capped) {
    # the likelihood is flat in the size: it has no standard error
    sd <- c(size = NA_real_, mu = 1 / sqrt(info[2, 2]))
    correlation <- NULL
  } else {
    vcov <- tryCatch(solve(info), error = function(e) matrix(NA_real_, 2, 2, dimnames = dimnames(info)))
    variances <- diag(vcov)
    sd <- ifelse(is.finite(variances) & variances > 0, sqrt(variances), NA_real_)
    names(sd) <- c("size", "mu")
    correlation <- if (all(is.finite(sd))) stats::cov2cor(vcov)
  }
  list(estimate = c(size = size, mu = mu), sd = sd, cor = correlation,
       loglik = loglik, aic = -2 * loglik + 4, bic = -2 * loglik + 2 * log(length(x)),
       n = length(x), distname = "nbinom", weighted = !is.null(weights), capped = capped)
}

# Zero-inflated counts: zero with probability `zero`, and otherwise a count of
# the base distribution (which gives zeros of its own). The fits return the
# fields of the two above, with `inflated`: FALSE when the data has no more zeros
# than the base distribution fitted to it gives, so that the best zero
# probability is 0 and the fit is the base distribution's.
#
# The likelihood splits in two: whether a row has claims, with probability
# (1 - zero) * (1 - p0), p0 the base distribution's probability of no claim; and
# the number of claims of a row with claims, which follows the base distribution
# truncated at zero whatever `zero` is. So the base parameters are fitted to the
# rows with claims alone, and `zero` then sets the probability of a row with
# claims to their share of the rows: zero = 1 - share / (1 - p0). When that is
# not above 0 the maximum is on the boundary zero = 0. The standard errors come
# from the observed information of the whole likelihood, by differences.

# Density and distribution function of a zero-inflated count; `base_d` and
# `base_p` are the base distribution's.
dft_zero_inflated_d <- function(x, zero, base_d) (x == 0) * zero + (1 - zero) * base_d(x)
dft_zero_inflated_p <- function(q, zero, base_p) ifelse(q < 0, 0, zero + (1 - zero) * base_p(q))

# The fields the count fits share, for a zero-inflated fit with `estimate` (the
# base parameters, then zero) and the base density as a function of the
# estimates.
dft_zero_inflated_result <- function(x, w, estimate, base_d, distname, weighted, capped, inflated) {
  k <- length(estimate)
  negloglik <- function(p) {
    d <- suppressWarnings(dft_zero_inflated_d(x, p[k], function(q) base_d(q, p)))
    value <- -sum(w * log(d))
    if (is.finite(value)) value else 1e300
  }
  loglik <- -negloglik(estimate)
  sd <- stats::setNames(rep(NA_real_, k), names(estimate))
  correlation <- NULL
  # (on a boundary, or with the size at its cap, the likelihood has no curvature to measure)
  if (inflated && !capped) {
    vcov <- tryCatch(solve(stats::optimHess(estimate, negloglik)), error = function(e) NULL)
    if (!is.null(vcov) && all(is.finite(vcov)) && all(diag(vcov) > 0)) {
      dimnames(vcov) <- list(names(estimate), names(estimate))
      sd[] <- sqrt(diag(vcov))
      correlation <- stats::cov2cor(vcov)
    }
  }
  list(estimate = estimate, sd = sd, cor = correlation, loglik = loglik, aic = -2 * loglik + 2 * k,
       bic = -2 * loglik + k * log(length(x)), n = length(x), distname = distname, weighted = weighted,
       capped = capped, inflated = inflated)
}

# The lambda of the Poisson truncated at zero with a given mean (above 1): the
# solution of lambda / (1 - exp(-lambda)) = mean.
dft_truncated_poisson_lambda <- function(mean_positive) {
  stats::uniroot(function(l) l / -expm1(-l) - mean_positive, c(1e-12, mean_positive), tol = 1e-13)$root
}

# Zero-inflated Poisson: lambda solves the mean of the Poisson truncated at
# zero, lambda / (1 - exp(-lambda)), equal to the mean of the rows with claims.
fit_zip_mle <- function(x, weights = NULL) {
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  positive <- x > 0
  if (!any(positive)) stop("the claim counts are all zero")
  share <- sum(w[positive]) / sum(w)
  mean_positive <- sum(w[positive] * x[positive]) / sum(w[positive])
  lambda <- zero <- NA_real_
  # (rows with claims that all have one claim: the truncated Poisson tends to lambda = 0)
  if (mean_positive > 1) {
    lambda <- dft_truncated_poisson_lambda(mean_positive)
    zero <- 1 - share / -expm1(-lambda)
  }
  inflated <- isTRUE(zero > 1e-9)
  if (!inflated) {
    lambda <- sum(w * x) / sum(w)
    zero <- 0
  }
  dft_zero_inflated_result(x, w, c(lambda = lambda, zero = zero), function(q, p) stats::dpois(q, p[1]),
                           "zipois", !is.null(weights), capped = FALSE, inflated = inflated)
}

# Zero-inflated Negative Binomial, in size and mu as the Negative Binomial fit.
# The Negative Binomial truncated at zero is fitted to the rows with claims by
# a bounded search from the Negative Binomial fit of all the rows. Where that
# does no better than the Poisson truncated at zero, the rows with claims are
# no more spread than a Poisson's: the fit tends to the zero-inflated Poisson,
# and takes its lambda with the size at the cap.
fit_zinb_mle <- function(x, weights = NULL) {
  w <- if (is.null(weights)) rep(1, length(x)) else weights
  positive <- x > 0
  if (!any(positive)) stop("the claim counts are all zero")
  base <- fit_nbinom_mle(x, weights)
  share <- sum(w[positive]) / sum(w)
  xp <- x[positive]
  wp <- w[positive]
  truncated <- function(p) {
    size <- exp(p[1])
    mu <- exp(p[2])
    value <- -sum(wp * (stats::dnbinom(xp, size = size, mu = mu, log = TRUE) -
                          log(-expm1(stats::dnbinom(0, size = size, mu = mu, log = TRUE)))))
    if (is.finite(value)) value else 1e300
  }
  lower <- c(log(1e-6), log(1e-8))
  upper <- c(log(dft_nbinom_max_size), log(max(x)) + 5)
  start <- pmin(pmax(log(c(base$estimate[["size"]], base$estimate[["mu"]])), lower), upper)
  size <- mu <- zero <- NA_real_
  capped <- FALSE
  if (any(xp > 1)) {
    best <- NULL
    # (from the fit of all the rows, and from a size of 1, in case the first stops early)
    for (from in list(start, c(0, start[2]))) {
      found <- tryCatch(stats::nlminb(from, truncated, lower = lower, upper = upper,
                                      control = list(iter.max = 500, eval.max = 1000, rel.tol = 1e-12)),
                        error = function(e) NULL)
      if (!is.null(found) && (is.null(best) || found$objective < best$objective)) best <- found
    }
    if (!is.null(best)) {
      size <- exp(best$par[1])
      mu <- exp(best$par[2])
      lambda <- dft_truncated_poisson_lambda(sum(wp * xp) / sum(wp))
      poisson <- -sum(wp * (stats::dpois(xp, lambda, log = TRUE) - log(-expm1(-lambda))))
      capped <- size >= dft_nbinom_max_size * 0.999 || best$objective > poisson - 1e-7 * max(1, abs(poisson))
      if (capped) {
        size <- dft_nbinom_max_size
        mu <- lambda
      }
      zero <- 1 - share / -expm1(stats::dnbinom(0, size = size, mu = mu, log = TRUE))
    }
  }
  inflated <- isTRUE(zero > 1e-9)
  if (!inflated) {
    size <- base$estimate[["size"]]
    mu <- base$estimate[["mu"]]
    zero <- 0
    capped <- isTRUE(base$capped)
  }
  dft_zero_inflated_result(x, w, c(size = size, mu = mu, zero = zero),
                           function(q, p) stats::dnbinom(q, size = p[1], mu = p[2]),
                           "zinbinom", !is.null(weights), capped = capped, inflated = inflated)
}

# The fitted cdf of a count fit, as a function of the counts.
dft_count_fit_cdf <- function(fit) {
  e <- fit$estimate
  switch(
    fit$distname,
    pois = function(q) stats::ppois(q, e[["lambda"]]),
    nbinom = function(q) stats::pnbinom(q, size = e[["size"]], mu = e[["mu"]]),
    zipois = function(q) dft_zero_inflated_p(q, e[["zero"]], function(v) stats::ppois(v, e[["lambda"]])),
    zinbinom = function(q) dft_zero_inflated_p(q, e[["zero"]], function(v) stats::pnbinom(v, size = e[["size"]], mu = e[["mu"]]))
  )
}

# The names of the count models, by the `distname` of their fits.
dft_count_model_names <- c(pois = "Poisson", nbinom = "Negative Binomial",
                           zipois = "Zero-inflated Poisson", zinbinom = "Zero-inflated Negative Binomial")

# Whether a count fit is a model of its own: a Negative Binomial with its size at
# the cap is a Poisson in all but name, and a zero-inflated fit without extra
# zeros is its base distribution.
dft_count_fit_distinct <- function(fit) {
  !is.null(fit) && !isTRUE(fit$capped) && !identical(fit$inflated, FALSE)
}

# Prints a count fit as summary() of a fitdistrplus::fitdist() fit did.
dft_print_count_fit <- function(fit) {
  name <- dft_count_model_names[[fit$distname]]
  cat(name, "distribution fitted by maximum likelihood\n")
  cat("Parameters:\n")
  print(data.frame(estimate = fit$estimate, `Std. Error` = fit$sd, check.names = FALSE))
  cat("Loglikelihood: ", fit$loglik, "  AIC: ", fit$aic, "  BIC: ", fit$bic, "\n")
  if (!is.null(fit$cor)) {
    cat("Correlation matrix:\n")
    print(fit$cor)
  }
  if (identical(fit$inflated, FALSE)) {
    cat("\nThe data has no more zeros than the ", sub("Zero-inflated ", "", name), " fitted to it gives, so the\n",
        "probability of an extra zero is 0 and the fit is that distribution's.\n", sep = "")
  } else if (isTRUE(fit$capped) && identical(fit$distname, "zinbinom")) {
    cat("\nThe rows with claims are no more spread than a Poisson's, so the likelihood keeps rising as the\n",
        "size grows and the fit tends to the zero-inflated Poisson. The size is set to ",
        netsimr_fmt_count(dft_nbinom_max_size), ".\n", sep = "")
  }
  if (isTRUE(fit$capped) && !identical(fit$distname, "zinbinom")) {
    cat("\nThe variance of the counts is not above their mean, so the likelihood keeps rising as the\n",
        "size grows and the Negative Binomial tends to the Poisson. The size is set to ",
        netsimr_fmt_count(dft_nbinom_max_size), ".\n", sep = "")
  }
  if (isTRUE(fit$weighted)) cat("\nEach row counts as many observations as its weight.\n")
  invisible(fit)
}

# Data of the charts. Each function returns what its chart draws, so that it
# can be checked without drawing; `cdfs` is a named list of the fitted cdfs.

# Frequency histogram: the share of the observations (of the weight, with
# weights) in about `bins` bins of consecutive counts, and the probability of
# each bin under each fitted model. The last bin ends at the largest count.
# When the smallest count is above zero, a first bin holds the counts below
# it: the data has none there, but a fitted model may put some of its
# probability there (zero-truncated counts), which is the lack of fit the
# chart is there to show.
dft_count_hist_data <- function(counts, weights = NULL, bins = 20, cdfs = list()) {
  w <- if (is.null(weights)) rep(1, length(counts)) else weights
  lowest <- min(counts)
  width <- max(1, ceiling((max(counts) - lowest + 1) / max(1, bins)))
  starts <- seq(lowest, max(counts), by = width)
  ends <- starts + width - 1
  ends[length(ends)] <- max(counts)
  bin <- factor((counts - lowest) %/% width + 1, levels = seq_along(starts))
  observed <- as.numeric(tapply(w, bin, sum))
  observed[is.na(observed)] <- 0
  if (lowest > 0) {
    starts <- c(0, starts)
    ends <- c(lowest - 1, ends)
    observed <- c(0, observed)
  }
  # (format = "d" gives "NA" from 2^31 up)
  label <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  labels <- ifelse(starts == ends, label(starts), paste0(label(starts), "-", label(ends)))
  list(labels = labels, observed = observed / sum(w),
       fitted = lapply(cdfs, function(cdf) cdf(ends) - cdf(starts - 1)))
}

# Frequency cdf chart: the (weighted) empirical cdf and the fitted cdfs at
# every count from 0 to the largest, or at 5,000 of them.
dft_count_cdf_data <- function(counts, weights = NULL, cdfs = list()) {
  top <- max(counts)
  points <- if (top <= 5000) seq(0, top) else unique(round(seq(0, top, length.out = 5000)))
  list(x = points, empirical = empirical_cdf_at(points, counts, weights),
       fitted = lapply(cdfs, function(cdf) cdf(points)))
}

# Claim size cdf chart: the empirical cdf at its steps and the fitted cdfs on a
# grid (evenly or log spaced) that includes the extra points, the slicing
# points or thresholds where spliced cdfs change shape.
dft_severity_cdf_data <- function(claims, cdfs = list(), log_scale = FALSE, extra_points = NULL) {
  points <- dft_ecdf_points(claims)
  grid <- sort(unique(c(dft_severity_grid(claims, log_scale), extra_points)))
  list(x = points, empirical = empirical_cdf_at(points, claims), grid = grid,
       fitted = lapply(cdfs, function(cdf) cdf(grid)))
}

# Claim size histogram: about `bins` bins with round breaks, as hist() makes them.
dft_severity_hist_data <- function(claims, bins = 20) {
  h <- graphics::hist(claims, breaks = max(1, bins), plot = FALSE)
  list(breaks = h$breaks, counts = h$counts)
}

# Mean excess chart: the empirical mean excess at the steps of the empirical
# cdf, leaving out the largest claim, which no claim exceeds.
dft_mean_excess_data <- function(claims) {
  points <- dft_ecdf_points(claims)
  mean_excess <- mean_excess_at(points, claims)
  defined <- is.finite(mean_excess)
  list(x = points[defined], y = mean_excess[defined])
}

# Series of a cdf chart for netsimr_line_chart(): the empirical cdf as a step
# line, then each fitted cdf in its colour; step draws the fitted cdfs as
# steps too (counts).
dft_cdf_series <- function(data, colours = netsimr_palette[seq_along(data$fitted)], step = FALSE) {
  x <- if (is.null(data$grid)) data$x else data$grid
  fitted <- Map(function(name, y, colour) list(name = name, x = x, y = y, colour = colour, step = step),
                names(data$fitted), data$fitted, colours)
  c(list(list(name = "Empirical", x = data$x, y = data$empirical, colour = "empirical", step = TRUE, lwd = 2.5)),
    unname(fitted))
}

# ---------------------------------------------------------------- To the simulator

# The models the tool can send to the simulator, as the "To the simulator" tab
# offers them: the label shown, and the tab whose fit each one uses.
dft_export_frequency_models <- c("Poisson" = "frequency", "Negative Binomial" = "frequency",
                                 "Zero-inflated Poisson" = "frequency",
                                 "Zero-inflated Negative Binomial" = "frequency")
dft_export_severity_models <- c(
  "Normal" = "severity", "LogNormal" = "severity", "Exponential" = "severity",
  "Gamma" = "severity", "Pareto" = "severity", "Weibull" = "severity", "Burr" = "severity",
  "Generalised Pareto" = "severity",
  "LogNormal - Pareto" = "sliced", "LogNormal - Pareto - Pareto" = "sliced",
  "Piecewise Pareto" = "piecewise"
)

# The settings that put a fitted model into the claims simulator, as a settings
# file of the simulator holds them (sim_settings_input_ids()), so that the file
# loads with its "Load settings" button.
#
# `frequency` is NULL (the simulator's frequency is left as it is) or a list with
# `model` ("Poisson" or "Negative Binomial", or either after "Zero-inflated ")
# and its fitted parameters: `lambda`, or `size` and `mu` as fitdistrplus and
# the Frequency tab give them; the simulator's Negative Binomial is a
# Poisson-Gamma mixture with shape r and scale beta, so r = size and beta = mu /
# size. A zero-inflated model also has `zero`, its probability of an extra zero.
#
# `severity` is NULL or a list with `model` ("Normal", "LogNormal",
# "Exponential", "Gamma", "Pareto", "Weibull", "Burr" or "Generalised Pareto"),
# its parameters (`mean` and `sd`; `mu` and `sigma`; `mean`; `shape` and `scale`;
# `xm` and `alpha`; `shape` and `scale`; `alpha`, `gamma` and `scale`;
# `location`, `scale` and `shape`) and optionally
# `slices`, a data frame of increasing thresholds `x_m` and their `alpha`: above
# each threshold the claims follow a Pareto with that alpha, which is the
# simulator's splice (pareto_splice()) and the tool's sliced and piecewise
# models alike. A severity without slices sets the number of slices to 0.
#
# `frequency_share` is the share of all claims that the counts count: for a
# severity fitted to claims reported above a threshold, the counts are of those
# claims, a share S(d) of all claims, while the simulator draws all claims from
# its severity. Each claim being reported with probability p, a Poisson(lambda)
# count of all claims gives a Poisson(p lambda) count of reported ones, and a
# Negative Binomial(r, beta) one a Negative Binomial(r, p beta); so lambda and
# beta are divided by the share. A period of extra zeros has no claims of either
# kind, so the probability of an extra zero stays as it is.
#
# Every parameter must be a positive, finite number (a Normal mean, a LogNormal
# mu and a Generalised Pareto shape any finite number, and a Generalised Pareto
# location zero or more); otherwise it stops with a message naming the parameter.
dft_simulator_settings <- function(frequency = NULL, severity = NULL, frequency_share = 1) {
  if (!(is.numeric(frequency_share) && length(frequency_share) == 1 && is.finite(frequency_share) &&
        frequency_share > 0 && frequency_share <= 1)) {
    stop("The share of claims that the counts count must be above 0 and at most 1.", call. = FALSE)
  }
  values <- list()
  check <- function(x, what, positive = TRUE) {
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || (positive && x <= 0)) {
      stop(what, " is ", if (is.numeric(x) && length(x) == 1 && is.finite(x)) netsimr_fmt(x) else "not a finite number",
           ", which the simulator does not accept", if (positive) "; it needs a positive number" else "", ".", call. = FALSE)
    }
    unname(x)
  }
  check_zero <- function(x) {
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0 || x >= 1) {
      stop("The probability of an extra zero must be at least 0 and below 1.", call. = FALSE)
    }
    unname(x)
  }
  if (!is.null(frequency)) {
    values <- switch(
      frequency$model,
      "Poisson" = list(freqDistr = "Poisson", lamda = check(frequency$lambda, "The Poisson lambda") / frequency_share),
      "Negative Binomial" = {
        size <- check(frequency$size, "The Negative Binomial size")
        list(freqDistr = "Negative_Binomial", r = size,
             beta = check(frequency$mu, "The Negative Binomial mean") / size / frequency_share)
      },
      "Zero-inflated Poisson" = list(
        freqDistr = "Zero_Inflated_Poisson",
        zip_lambda = check(frequency$lambda, "The zero-inflated Poisson lambda") / frequency_share,
        zip_zero = check_zero(frequency$zero)
      ),
      "Zero-inflated Negative Binomial" = {
        size <- check(frequency$size, "The zero-inflated Negative Binomial size")
        list(freqDistr = "Zero_Inflated_Negative_Binomial", zinb_r = size,
             zinb_beta = check(frequency$mu, "The zero-inflated Negative Binomial mean") / size / frequency_share,
             zinb_zero = check_zero(frequency$zero))
      },
      stop("The simulator has no ", frequency$model, " frequency.", call. = FALSE)
    )
  }
  if (!is.null(severity)) {
    sev <- switch(
      severity$model,
      "Normal" = list(sevDistr = "Normal", normal_mean = check(severity$mean, "The Normal mean", positive = FALSE),
                      normal_sd = check(severity$sd, "The Normal standard deviation")),
      "LogNormal" = list(sevDistr = "LogNormal", mu = check(severity$mu, "The LogNormal mu", positive = FALSE),
                         sigma = check(severity$sigma, "The LogNormal sigma")),
      "Exponential" = list(sevDistr = "Exponential", rate = 1 / check(severity$mean, "The Exponential mean")),
      "Gamma" = list(sevDistr = "Gamma", shape = check(severity$shape, "The Gamma shape"),
                     scale = check(severity$scale, "The Gamma scale")),
      #(x_m before alpha, as the simulator shows them)
      "Pareto" = list(sevDistr = "Pareto", x_m = check(severity$xm, "The Pareto x_m"),
                      alpha = check(severity$alpha, "The Pareto alpha")),
      "Weibull" = list(sevDistr = "Weibull", weibull_shape = check(severity$shape, "The Weibull shape"),
                       weibull_scale = check(severity$scale, "The Weibull scale")),
      "Burr" = list(sevDistr = "Burr", burr_alpha = check(severity$alpha, "The Burr alpha"),
                    burr_gamma = check(severity$gamma, "The Burr gamma"),
                    burr_scale = check(severity$scale, "The Burr scale")),
      "Generalised Pareto" = {
        location <- check(severity$location, "The Generalised Pareto location", positive = FALSE)
        if (location < 0) stop("The Generalised Pareto location is negative, which the simulator does not accept.", call. = FALSE)
        list(sevDistr = "Generalised_Pareto", gpd_location = location,
             gpd_scale = check(severity$scale, "The Generalised Pareto scale"),
             gpd_shape = check(severity$shape, "The Generalised Pareto shape", positive = FALSE))
      },
      stop("The simulator has no ", severity$model, " severity.", call. = FALSE)
    )
    slices <- severity$slices
    k <- if (is.null(slices)) 0L else nrow(slices)
    if (k > max_number_of_pareto_slices) {
      stop("The simulator takes at most ", max_number_of_pareto_slices, " Pareto slices; this model has ", k, ".", call. = FALSE)
    }
    if (k > 1 && any(diff(slices$x_m) <= 0)) stop("The slice thresholds must increase.", call. = FALSE)
    sev$pareto_slice_times <- k
    #the simulator's slice fields: alpha in the odd ones, the threshold in the even ones (the
    #threshold written first, as the simulator shows it)
    for (j in seq_len(k)) {
      sev[[paste0("slice_pareto_param_", 2 * j)]] <- check(slices$x_m[j], paste("Slice", j, "threshold"))
      sev[[paste0("slice_pareto_param_", 2 * j - 1)]] <- check(slices$alpha[j], paste("Slice", j, "alpha"))
    }
    values <- c(values, sev)
  }
  values
}

# The piecewise Pareto of the Piecewise Pareto tab, from thresholds `t` (the
# first the smallest claim) and their alphas, as the simulator's severity: a
# Pareto from the first threshold, with a slice at each later one.
dft_piecewise_severity <- function(t, alpha) {
  k <- length(t)
  list(model = "Pareto", xm = t[1], alpha = alpha[1],
       slices = if (k > 1) data.frame(x_m = t[-1], alpha = alpha[-1]))
}

# A summary of exported settings for the "To the simulator" tab: one row per
# setting, its label in the simulator and its value.
dft_simulator_settings_rows <- function(values) {
  labels <- c(
    freqDistr = "Frequency distribution", lamda = "lambda (mean claims)", r = "r (shape)", beta = "beta (scale)",
    zip_lambda = "lambda (mean of the Poisson)", zip_zero = "Probability of an extra zero",
    zinb_r = "r (shape)", zinb_beta = "beta (scale)", zinb_zero = "Probability of an extra zero",
    sevDistr = "Severity distribution", normal_mean = "Mean", normal_sd = "Standard deviation",
    mu = "mu", sigma = "sigma", rate = "rate", shape = "shape", scale = "scale",
    x_m = "x_m (minimum)", alpha = "alpha (shape)", pareto_slice_times = "Number of Pareto slices",
    weibull_shape = "Shape", weibull_scale = "Scale",
    burr_alpha = "alpha (first shape)", burr_gamma = "gamma (second shape)", burr_scale = "Scale",
    gpd_location = "Location (threshold)", gpd_scale = "Scale", gpd_shape = "Shape (xi)"
  )
  shown <- c(Negative_Binomial = "Negative Binomial", Zero_Inflated_Poisson = "Zero-inflated Poisson",
             Zero_Inflated_Negative_Binomial = "Zero-inflated Negative Binomial",
             Generalised_Pareto = "Generalised Pareto")
  rows <- list()
  for (id in names(values)) {
    value <- values[[id]]
    slice <- regmatches(id, regexec("^slice_pareto_param_([0-9]+)$", id))[[1]]
    label <- if (length(slice) == 2) {
      i <- as.integer(slice[2])
      paste("Slice", (i + 1) %/% 2, if (i %% 2 == 1) "alpha" else "threshold (x_m)")
    } else labels[[id]]
    text <- if (is.character(value)) (if (value %in% names(shown)) shown[[value]] else value) else netsimr_fmt(value, 6)
    rows[[length(rows) + 1]] <- list(label, text)
  }
  rows
}

# ------------------------------------------------- Claims reported above a threshold

# Maximum likelihood fits of the Severity tab's distributions to claims `x`
# that are known only above a reporting threshold `d` > 0 (left truncated
# data): each claim's density is divided by the fitted probability of
# exceeding d, f(x) / S(d), so the fit is of the claims from zero (ground up)
# without the claims below d that the data cannot hold. Claims capped at their
# policy limit are `censored` (right censored): the loss was at least the claim,
# so each adds the probability of exceeding it, S(x) / S(d), instead of its
# density. With censoring the threshold can be 0, for no threshold.
#
# Returns the models in the form of the Severity tab: `name`, `params` (of the
# ground-up distribution, named as the untruncated fits name them), `mean` (the
# fitted mean of a claim above d, which the empirical mean estimates), `cdf`
# (the fitted cdf of a claim above d, for the charts and the K-S distance),
# `loglik` (of the truncated likelihood), `k` and `survival_at_threshold`
# (S(d), the share of all claims that exceed d). A fit that fails has NA
# parameters and no cdf, and its message is in the `failed` attribute. The last
# two models are the Burr and the generalised Pareto (dft_heavy_tail_models()).
dft_truncated_severity_models <- function(x, d, censored = NULL) {
  n <- length(x)
  if (is.null(censored)) censored <- rep(FALSE, n)
  if (length(censored) != n || anyNA(censored)) stop("censored must say for each claim whether it is censored")
  censoring <- any(censored)
  if (!(is.numeric(d) && length(d) == 1 && is.finite(d) && (d > 0 || (censoring && d == 0)))) {
    stop("the threshold must be a positive number")
  }
  truncated <- d > 0
  if (n < 2 || any(x <= d)) stop("the claims must all be above the threshold")
  xu <- x[!censored]
  xc <- x[censored]
  nu <- length(xu)
  if (nu < 2 || length(unique(xu)) < 2) stop("at least two different claims must be below their policy limits")
  # what the fits are of: claims above the threshold, censored at their limits, or both
  what <- if (truncated) "fit above the threshold" else "fit with the policy limits"
  failed <- character()
  # the fit from minus the log-likelihood over unconstrained parameters
  optimise <- function(nll, start, parscale) dft_minimise(nll, start, parscale)$par
  # the log-likelihood of a distribution from its log density and log survival function: the
  # density of each claim below its limit, the survival at its limit of each claim at it, both
  # over the survival at the threshold
  loglik_of <- function(log_density, log_survival) {
    density_sum <- if (is.function(log_density)) sum(log_density(xu)) else log_density
    density_sum + (if (length(xc)) sum(log_survival(xc)) else 0) - (if (truncated) n * log_survival(d) else 0)
  }
  # the optimiser evaluates the likelihood thousands of times, so the sum of the log densities
  # of the claims below their limits is taken from their sufficient statistics (their number,
  # mean, sum of squares about it, and the same of their logs) and not from every claim again
  lxu <- log(xu)
  mean_u <- mean(xu)
  ss_u <- sum((xu - mean_u)^2)
  sum_u <- sum(xu)
  mean_lu <- mean(lxu)
  ss_lu <- sum((lxu - mean_lu)^2)
  sum_lu <- sum(lxu)
  half_log_2pi <- log(2 * pi) / 2
  normal_sum <- function(mu, sigma) -nu * (half_log_2pi + log(sigma)) - (ss_u + nu * (mean_u - mu)^2) / (2 * sigma^2)
  lnorm_sum <- function(mu, sigma) -sum_lu - nu * (half_log_2pi + log(sigma)) - (ss_lu + nu * (mean_lu - mu)^2) / (2 * sigma^2)
  gamma_sum <- function(shape, scale) (shape - 1) * sum_lu - sum_u / scale - nu * (lgamma(shape) + shape * log(scale))
  weibull_sum <- function(shape, scale) {
    nu * (log(shape) - shape * log(scale)) + (shape - 1) * sum_lu - sum(exp(shape * (lxu - log(scale))))
  }
  # a model from its log survival function, density and ground-up parameters. (Each fit is
  # made in local(): tryCatch() evaluates in this function's frame, where the closures of one
  # fit would see the parameters of the next.) A fit that leaves almost no claims above the
  # threshold has run to the edge of its parameters: the claims above the threshold have a
  # heavier tail than any of its distributions, and the claims below it are not fitted at all
  model <- function(name, params, log_survival, log_density, mean_above, k = 2, optimised = TRUE) {
    log_s_d <- if (truncated) log_survival(d) else 0
    if (truncated && optimised && (!is.finite(log_s_d) || log_s_d < log(1e-9))) {
      stop("the claims above the threshold have a heavier tail than any ", name, ", and the fit runs to the edge of ",
           "its parameters, leaving almost no claims above the threshold", call. = FALSE)
    }
    list(name = name, params = params, mean = mean_above, k = k,
         # (claim sizes are positive: below zero the cdf of every distribution but the Normal is 0)
         cdf = if (truncated) function(q) ifelse(q <= d, 0, -expm1(log_survival(pmax(q, d)) - log_s_d)) else
           function(q) -expm1(log_survival(if (name == "Normal") q else pmax(q, 0))),
         loglik = loglik_of(log_density, log_survival), survival_at_threshold = exp(log_s_d))
  }
  failed_model <- function(name, params, e) {
    failed <<- c(failed, paste0(name, " ", what, " failed: ", conditionMessage(e)))
    list(name = name, params = params, mean = NA_real_, k = 2, cdf = NULL, loglik = NA_real_,
         survival_at_threshold = NA_real_)
  }

  normal <- tryCatch(local({
    s0 <- stats::sd(x)
    p <- optimise(function(p) -loglik_of(normal_sum(p[1], exp(p[2])),
                                         function(q) stats::pnorm(q, p[1], exp(p[2]), lower.tail = FALSE, log.p = TRUE)),
                  c(mean(x), log(s0)), c(s0, 1))
    mu <- p[1]
    sigma <- exp(p[2])
    a <- (d - mu) / sigma
    # E[X | X > d] = mu + sigma phi(a) / (1 - Phi(a)), its ratio from the logs for a far tail
    model("Normal", c(Mean = mu, `Std. dev.` = sigma),
          function(q) stats::pnorm(q, mu, sigma, lower.tail = FALSE, log.p = TRUE),
          function(q) stats::dnorm(q, mu, sigma, log = TRUE),
          if (truncated) mu + sigma * exp(stats::dnorm(a, log = TRUE) - stats::pnorm(a, lower.tail = FALSE, log.p = TRUE)) else mu)
  }), error = function(e) failed_model("Normal", c(Mean = NA_real_, `Std. dev.` = NA_real_), e))

  lnorm <- tryCatch(local({
    lx <- log(x)
    p <- optimise(function(p) -loglik_of(lnorm_sum(p[1], exp(p[2])),
                                         function(q) stats::plnorm(q, p[1], exp(p[2]), lower.tail = FALSE, log.p = TRUE)),
                  c(mean(lx), log(stats::sd(lx))), c(1, 1))
    mu <- p[1]
    sigma <- exp(p[2])
    model("LogNormal", c(mu = mu, sigma = sigma),
          function(q) stats::plnorm(q, mu, sigma, lower.tail = FALSE, log.p = TRUE),
          function(q) stats::dlnorm(q, mu, sigma, log = TRUE),
          exp(mu + sigma^2 / 2 + stats::pnorm((log(d) - mu - sigma^2) / sigma, lower.tail = FALSE, log.p = TRUE) -
                stats::pnorm((log(d) - mu) / sigma, lower.tail = FALSE, log.p = TRUE)))
  }), error = function(e) failed_model("LogNormal", c(mu = NA_real_, sigma = NA_real_), e))

  # (memoryless: the excess over the threshold is Exponential with the same rate; each claim
  # at its limit adds its excess over the threshold to the exposure, but no event)
  rate <- nu / sum(x - d)
  exponential <- model("Exponential", c(Mean = 1 / rate),
                       function(q) stats::pexp(q, rate, lower.tail = FALSE, log.p = TRUE),
                       function(q) stats::dexp(q, rate, log = TRUE), d + 1 / rate, k = 1, optimised = FALSE)

  gamma <- tryCatch(local({
    start <- fit_gamma_profile(x)$estimate
    p <- optimise(function(p) -loglik_of(gamma_sum(exp(p[1]), exp(p[2])),
                                         function(q) stats::pgamma(q, shape = exp(p[1]), scale = exp(p[2]), lower.tail = FALSE,
                                                                   log.p = TRUE)),
                  log(c(start[["shape"]], start[["scale"]])), c(1, 1))
    shape <- exp(p[1])
    scale <- exp(p[2])
    model("Gamma", c(Scale = scale, Shape = shape),
          function(q) stats::pgamma(q, shape = shape, scale = scale, lower.tail = FALSE, log.p = TRUE),
          function(q) stats::dgamma(q, shape = shape, scale = scale, log = TRUE),
          # E[X | X > d] = shape scale S_{shape + 1}(d) / S_shape(d)
          shape * scale * exp(stats::pgamma(d, shape = shape + 1, scale = scale, lower.tail = FALSE, log.p = TRUE) -
                                stats::pgamma(d, shape = shape, scale = scale, lower.tail = FALSE, log.p = TRUE)))
  }), error = function(e) failed_model("Gamma", c(Scale = NA_real_, Shape = NA_real_), e))

  # a Pareto from the threshold (or from the smallest claim without one), whose claims all
  # exceed it; each claim at its limit adds to the sum of logs, but no event
  xm <- if (truncated) d else min(x)
  alpha <- nu / sum(log(x / xm))
  pareto <- model("Pareto", c(xm = xm, alpha = alpha),
                  function(q) ifelse(q <= xm, 0, alpha * (log(xm) - log(q))),
                  function(q) log(alpha) + alpha * log(xm) - (alpha + 1) * log(q),
                  if (alpha > 1) alpha * xm / (alpha - 1) else Inf, optimised = FALSE)

  weibull <- tryCatch(local({
    start <- dft_fit_weibull(x)$estimate
    # log S(q) = -(q / scale)^shape
    p <- optimise(function(p) -loglik_of(weibull_sum(exp(p[1]), exp(p[2])),
                                         function(q) -(q / exp(p[2]))^exp(p[1])),
                  log(c(start[["shape"]], start[["scale"]])), c(1, 1))
    shape <- exp(p[1])
    scale <- exp(p[2])
    z <- (d / scale)^shape
    model("Weibull", c(Scale = scale, Shape = shape),
          function(q) -(q / scale)^shape,
          function(q) stats::dweibull(q, shape = shape, scale = scale, log = TRUE),
          # E[X | X > d] = scale Gamma(1 + 1/shape, z) e^z, with the upper incomplete gamma
          scale * exp(z + lgamma(1 + 1 / shape) + stats::pgamma(z, 1 + 1 / shape, lower.tail = FALSE, log.p = TRUE)))
  }), error = function(e) failed_model("Weibull", c(Scale = NA_real_, Shape = NA_real_), e))

  # the Burr, and the generalised Pareto from the threshold
  heavy <- dft_heavy_tail_models(x, d, censored)
  structure(c(list(normal, lnorm, exponential, gamma, pareto, weibull), heavy), failed = c(failed, attr(heavy, "failed")))
}

# The Kaplan-Meier (product-limit) estimate of the cdf of claims, some censored at their
# policy limits (a censored claim is at least as large as its value) and all above a
# threshold: at each claim size that is not censored, the share of the claims still open
# that close there. Returns the claim sizes and the estimated cdf just after each; without
# censoring it is the empirical cdf.
dft_kaplan_meier <- function(x, censored) {
  events <- sort(unique(x[!censored]))
  closed <- as.numeric(table(factor(x[!censored], levels = events)))
  # the claims still open at each size: those at least as large, censored or not
  open <- length(x) - findInterval(events, sort(x), left.open = TRUE)
  survival <- cumprod(1 - closed / open)
  list(x = events, cdf = 1 - survival)
}

# The Kolmogorov-Smirnov distance between a fitted cdf and the Kaplan-Meier estimate, at the
# claim sizes below the limits (each step, from both sides), where the estimate is known.
dft_km_distance <- function(km, cdf_fun) {
  fitted <- cdf_fun(km$x)
  before <- c(0, km$cdf[-length(km$cdf)])
  max(abs(km$cdf - fitted), abs(before - fitted))
}

# The quantiles of the Kaplan-Meier estimate at the probabilities p it reaches: the smallest
# claim size whose estimated cdf is at least p, NA beyond its largest value.
dft_km_quantiles <- function(km, p) {
  at <- findInterval(p, km$cdf, left.open = TRUE) + 1
  ifelse(at <= length(km$x), km$x[pmin(at, length(km$x))], NA_real_)
}
# Maximum likelihood fit of a Weibull distribution to positive claims, as
# fitdistrplus::fitdist(x, "weibull") gives it: for a shape k the likelihood is
# highest at the scale mean(x^k)^(1/k), so only the shape is searched, on the log
# scale. The claims are divided by their median first, so that x^k stays finite.
dft_fit_weibull <- function(x) {
  if (length(x) < 2 || any(!is.finite(x)) || any(x <= 0)) stop("a Weibull fit needs at least two positive claims")
  if (length(unique(x)) < 2) stop("a Weibull fit needs claims of more than one size")
  unit <- stats::median(x)
  z <- x / unit
  scale_for <- function(k) mean(z^k)^(1 / k)
  nll <- function(log_k) {
    k <- exp(log_k)
    v <- -sum(stats::dweibull(z, shape = k, scale = scale_for(k), log = TRUE))
    if (is.finite(v)) v else 1e300
  }
  log_k <- stats::optimize(nll, c(log(0.02), log(100)), tol = 1e-10)$minimum
  shape <- exp(log_k)
  list(estimate = c(shape = shape, scale = scale_for(shape) * unit))
}

# The Anderson-Darling statistic of claims against a fitted cdf: like the
# Kolmogorov-Smirnov distance it measures how far the empirical cdf is from the
# fitted one, but it weighs the differences in the tails, where an excess of loss
# layer lies, far more. The lower, the closer. sorted = TRUE says the claims are
# already sorted.
ad_statistic <- function(claims, cdf_fun, sorted = FALSE) {
  x <- if (sorted) claims else sort(claims)
  n <- length(x)
  if (n == 0) return(NA_real_)
  fitted <- cdf_fun(x)
  if (anyNA(fitted)) return(NA_real_)
  # a claim where the fitted cdf is 0 or 1 is impossible under the fit: the statistic is
  # then very large rather than infinite
  fitted <- pmin(pmax(fitted, 1e-300), 1 - 1e-16)
  i <- seq_len(n)
  -n - sum((2 * i - 1) * (log(fitted) + log1p(-rev(fitted)))) / n
}

# The quantiles of a fitted cdf at the probabilities p, found by bisection on all
# of them at once (the models have a cdf but not a quantile function). The search
# starts from the range of the claims and widens until it holds every quantile.
dft_model_quantiles <- function(cdf, p, claims) {
  span <- diff(range(claims))
  if (!is.finite(span) || span <= 0) span <- max(abs(claims), 1)
  lo <- min(claims) - span
  hi <- max(claims)
  for (i in 1:200) if (cdf(lo) <= min(p)) break else lo <- lo - span * 2^i
  for (i in 1:200) if (cdf(hi) >= max(p)) break else hi <- hi + span * 2^i
  lower <- rep(lo, length(p))
  upper <- rep(hi, length(p))
  for (i in 1:100) {
    middle <- (lower + upper) / 2
    below <- cdf(middle) < p
    lower[below] <- middle[below]
    upper[!below] <- middle[!below]
  }
  (lower + upper) / 2
}

# The points of a Q-Q plot: the empirical quantiles of the claims at `points`
# probabilities between 0.5% and 99.5% and further into the tail (up to the
# (n - 0.5)/n quantile, beside the largest claim), with each model's quantiles
# at the same probabilities. A point on the diagonal is a quantile the model
# gets right.
dft_qq_data <- function(claims, cdfs, points = 200) {
  n <- length(claims)
  p <- sort(unique(c(seq(0.005, 0.995, length.out = points),
                     if (n >= 200) c(0.999, 1 - 0.5 / n))))
  p <- p[p > 0.5 / n & p < 1 - 0.5 / n | p == 1 - 0.5 / n]
  empirical <- stats::quantile(claims, p, names = FALSE, type = 7)
  list(p = p, empirical = empirical,
       fitted = lapply(cdfs, function(cdf) dft_model_quantiles(cdf, p, claims)))
}

# The points of a P-P plot: at each claim size, the empirical probability of a
# claim up to it against each model's cdf there. A point on the diagonal is a
# probability the model gets right. The empirical probability of the i-th of n
# sorted claims is (i - 0.5) / n, the middle of its step; with policy limits it
# is the middle of each step of the Kaplan-Meier estimate `km`
# (dft_kaplan_meier()), at the claims below their limits. At most `points`
# claims are kept, evenly spread over the steps.
dft_pp_data <- function(claims, cdfs, km = NULL, points = 500) {
  if (is.null(km)) {
    x <- sort(claims)
    empirical <- (seq_along(x) - 0.5) / length(x)
  } else {
    x <- km$x
    empirical <- (c(0, km$cdf[-length(km$cdf)]) + km$cdf) / 2
  }
  if (length(x) > points) {
    keep <- unique(round(seq(1, length(x), length.out = points)))
    x <- x[keep]
    empirical <- empirical[keep]
  }
  list(x = x, empirical = empirical, fitted = lapply(cdfs, function(cdf) cdf(x)))
}

# The levels of the conditional tail expectations in the Severity tab.
dft_cte_levels <- c(0.9, 0.95, 0.99, 0.995)

# The empirical conditional tail expectation (CTE, also TVaR or expected
# shortfall) of claims at each level p: the mean of the largest n (1 - p)
# claims, the last of them counted in part when n (1 - p) is not whole (the
# estimator of Acerbi and Tasche, 2002). Below one claim it is the largest.
dft_empirical_cte <- function(claims, p) {
  x <- sort(claims, decreasing = TRUE)
  n <- length(x)
  vapply(p, function(level) {
    # (rounded: 200 * (1 - 0.995) is a hair below 1 in floating point)
    k <- round(n * (1 - level), 9)
    whole <- floor(k)
    if (whole >= n) return(mean(x))
    if (k <= 0) return(x[1])
    (sum(x[seq_len(whole)]) + (k - whole) * x[whole + 1]) / k
  }, numeric(1))
}

# The mean claim above v, E[X | X > v], of a fitted distribution of the Severity
# tab (by its name and ground-up parameters, named as dft_truncated_severity_models()
# names them), in closed form from the log survival functions, so that it stays
# accurate far into the tail. Returns a function of v, or NULL for a fit without
# parameters. Above a reporting threshold d < v it is also the mean of a claim
# above v among those above d.
dft_tail_mean <- function(name, params) {
  p <- unname(params)
  if (length(p) == 0 || anyNA(p)) return(NULL)
  upper_log <- function(q, ...) stats::pgamma(q, ..., lower.tail = FALSE, log.p = TRUE)
  switch(
    name,
    # mu + sigma phi(a) / (1 - Phi(a)), with a = (v - mu) / sigma
    Normal = function(v) {
      a <- (v - p[1]) / p[2]
      p[1] + p[2] * exp(stats::dnorm(a, log = TRUE) - stats::pnorm(a, lower.tail = FALSE, log.p = TRUE))
    },
    LogNormal = function(v) {
      exp(p[1] + p[2]^2 / 2 + stats::pnorm((log(v) - p[1] - p[2]^2) / p[2], lower.tail = FALSE, log.p = TRUE) -
            stats::pnorm((log(v) - p[1]) / p[2], lower.tail = FALSE, log.p = TRUE))
    },
    # memoryless: v plus the mean
    Exponential = function(v) pmax(v, 0) + p[1],
    # (Scale, Shape) = (p[1], p[2]): shape scale S_{shape + 1}(v) / S_shape(v)
    Gamma = function(v) {
      p[1] * p[2] * exp(upper_log(v, shape = p[2] + 1, scale = p[1]) - upper_log(v, shape = p[2], scale = p[1]))
    },
    # (xm, alpha): alpha v / (alpha - 1) above xm, infinite for alpha <= 1
    Pareto = function(v) if (p[2] > 1) p[2] * pmax(v, p[1]) / (p[2] - 1) else rep(Inf, length(v)),
    # (Scale, Shape): scale Gamma(1 + 1/shape, z) e^z, with z = (v / scale)^shape
    Weibull = function(v) {
      z <- (pmax(v, 0) / p[1])^p[2]
      p[1] * exp(z + lgamma(1 + 1 / p[2]) + upper_log(z, shape = 1 + 1 / p[2]))
    },
    # (Scale, alpha, gamma)
    Burr = function(v) burr_tail_mean(v, p[2], p[3], p[1]),
    # (Location, Scale, Shape)
    `Generalised Pareto` = function(v) gpd_tail_mean(v, p[1], p[2], p[3]),
    NULL
  )
}

# The conditional tail expectations of a fitted model of the Severity tab at the
# levels p: the mean claim above its quantile at each level. The quantiles are of
# the model's cdf, which above a reporting threshold is of the claims above it.
# NA for a fit that failed.
dft_model_cte <- function(model, p, claims) {
  tail_mean <- dft_tail_mean(model$name, model$params)
  if (is.null(model$cdf) || is.null(tail_mean)) return(rep(NA_real_, length(p)))
  tail_mean(dft_model_quantiles(model$cdf, p, claims))
}


# The note under the Severity tab's table on the fits above a threshold that
# failed (the `failed` messages of dft_truncated_severity_models()): those that
# run to the edge of their parameters together, any other failure as it is.
dft_failed_fits_note <- function(failed) {
  if (length(failed) == 0) return("")
  names <- sub(" fit( above the threshold| with the policy limits)? failed: .*$", "", failed)
  edge <- grepl("heavier tail than any", failed, fixed = TRUE)
  either <- function(x) if (length(x) == 1) x else paste(paste(x[-length(x)], collapse = ", "), "or", x[length(x)])
  paste0(
    if (any(edge)) paste0(" No ", either(names[edge]), " fits: the claims above the threshold have a heavier tail than any ",
                          either(names[edge]), ", whose fit runs to the edge of its parameters."),
    if (any(!edge)) paste0(" ", paste0(failed[!edge], ".", collapse = " "))
  )
}
