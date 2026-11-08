# More reserving methods: the actual against the expected development of the latest
# calendar period, Clark's growth curve methods, and the chain ladder of a triangle
# restated for claims inflation.

#' Actual against expected development
#'
#' @description Checks the chain ladder against the claims of the latest calendar
#'   period: the chain ladder of the triangle without its latest diagonal predicts how
#'   much each origin develops in that period, which is compared with what was
#'   reported.
#'
#' @details The triangle without its latest diagonal (each origin's latest value
#'   left out) gives development factors \eqn{f_k} and Mack's \eqn{\sigma_k} and
#'   factor standard errors. An origin with \eqn{C} claims at its previous age
#'   \eqn{k} was expected to develop by \eqn{C (f_k - 1)}, with a standard error
#'   \eqn{\sqrt{C^{2 - \alpha} \sigma_k^2 + C^2 se(f_k)^2}} (process and parameter
#'   error, \eqn{\alpha} as in \code{\link{chain_ladder}}). The standardised
#'   difference (actual less expected, over its standard error) shows which origins
#'   moved more than the method allows for: beyond about 2 in either direction is
#'   worth a look. The total's standard error adds that the origins at the same age
#'   share a factor. An origin that has a single value, or whose previous age has no
#'   development factor in the earlier triangle (the oldest origin, at the end of the
#'   triangle), has no expected development and is left out of the total.
#' @param triangle A triangle of cumulative claims, as for \code{\link{chain_ladder}}.
#' @param ... Arguments passed to \code{chain_ladder()} for the earlier triangle,
#'   such as \code{average} or \code{periods}.
#' @return An object of class \code{netsimr_reserve}: a list with \code{by_origin}, a
#'   data frame with each origin's claims at its previous age, the expected and the
#'   actual development, their difference, the ratio of actual to expected, the
#'   standard error of the expected development and the standardised difference;
#'   \code{total}, the same for all origins; and \code{chain_ladder}, the chain ladder
#'   of the earlier triangle.
#' @seealso \code{\link{chain_ladder}}; \code{\link{merz_wuthrich}} for the
#'   uncertainty of the next period's development.
#' @export
#' @examples
#' ave <- actual_vs_expected(raa_triangle)
#' ave
#' ave$by_origin[, c("origin", "expected", "actual", "z")]
actual_vs_expected <- function(triangle, ...) {
  checked <- reserving_check_triangle(triangle)
  C <- checked$triangle
  latest <- checked$latest
  m <- nrow(C)
  previous <- C
  previous[cbind(seq_len(m), latest)] <- NA
  has_previous <- latest >= 2
  if (sum(has_previous) < 2) stop("The triangle needs at least two origins with two or more values.", call. = FALSE)
  earlier <- reserving_trim_triangle(previous[has_previous, , drop = FALSE])
  cl <- chain_ladder(earlier, ...)
  age <- latest - 1
  value_at <- function(i) if (has_previous[i]) C[i, age[i]] else NA_real_
  from <- vapply(seq_len(m), value_at, numeric(1))
  k <- ifelse(has_previous & age <= length(cl$factors), age, NA_integer_)
  factor <- unname(cl$factors[k])
  sigma <- unname(cl$sigma[k])
  factor_se <- unname(cl$factor_se[k])
  expected <- from * (factor - 1)
  actual <- ifelse(has_previous, C[cbind(seq_len(m), latest)] - from, NA_real_)
  process <- abs(from)^(2 - cl$alpha) * sigma^2
  parameter <- from^2 * factor_se^2
  se <- sqrt(process + parameter)
  difference <- actual - expected
  by_origin <- data.frame(
    origin = rownames(C), previous = from, expected = expected, actual = actual, difference = difference,
    actual_to_expected = ifelse(is.finite(expected) & expected != 0, actual / expected, NA_real_),
    se = se, z = ifelse(is.finite(se) & se > 0, difference / se, NA_real_),
    stringsAsFactors = FALSE, row.names = NULL
  )
  counted <- is.finite(expected)
  # the origins at the same age share the factor, so its parameter error adds up across them
  total_parameter <- sum(vapply(split(which(counted), k[counted]), function(rows) {
    sum(from[rows])^2 * factor_se[rows[1]]^2
  }, numeric(1)))
  total_se <- sqrt(sum(process[counted]) + total_parameter)
  total_expected <- sum(expected[counted])
  total_actual <- sum(actual[counted])
  total <- c(previous = sum(from[counted]), expected = total_expected, actual = total_actual,
             difference = total_actual - total_expected,
             actual_to_expected = if (total_expected != 0) total_actual / total_expected else NA_real_,
             se = total_se, z = if (total_se > 0) (total_actual - total_expected) / total_se else NA_real_)
  structure(list(method = "Actual vs expected development of the latest calendar period", by_origin = by_origin,
                 total = total, chain_ladder = cl), class = "netsimr_reserve")
}

#' Clark's growth curve reserves
#'
#' @description Reserves from a growth curve fitted to the incremental claims by
#'   maximum likelihood, after Clark (2003): the LDF method, with an ultimate for
#'   each origin, or the Cape Cod method, with one expected loss ratio on the
#'   premiums. Its standard errors have a process part and a parameter part.
#'
#' @details The share of an origin's ultimate developed by age \eqn{x} is a growth
#'   curve: the log-logistic \eqn{G(x) = x^\omega / (x^\omega + \theta^\omega)} or
#'   the Weibull \eqn{G(x) = 1 - e^{-(x / \theta)^\omega}}. The age is measured from
#'   the average date of the claims in an origin period, \code{offset} of a period
#'   after its start (a half for claims spread evenly over it), so the increment of
#'   development period \eqn{t} is expected to be
#'   \eqn{\mu_{it} = U_i (G(t - o) - G(t - 1 - o))}, with \eqn{G = 0} at ages up to 0.
#'   \eqn{U_i} is the ultimate of each origin (LDF method) or its premium times one
#'   expected loss ratio (Cape Cod method). The increments are taken to be
#'   over-dispersed Poisson: the parameters maximise
#'   \eqn{\sum c_{it} \log \mu_{it} - \mu_{it}}, so that each \eqn{U_i} (or the loss
#'   ratio) has a closed form given \eqn{\omega} and \eqn{\theta}, which are found
#'   numerically. The scale \eqn{\sigma^2} is the sum of the squared Pearson
#'   residuals over the degrees of freedom (the cells less the parameters).
#'
#'   The reserve of an origin is \eqn{U_i (G(T - o) - G(a_i - o))}, from its latest
#'   age \eqn{a_i} to the truncation age \eqn{T} (infinite by default; the growth
#'   curves can have long tails, so a finite truncation is often used with the LDF
#'   method). Its process variance is \eqn{\sigma^2} times the reserve; its parameter
#'   variance comes from the covariance of the parameters, \eqn{\sigma^2} times the
#'   inverse of minus the second derivatives of the log-likelihood, by the delta
#'   method. The results agree with the ChainLadder package's \code{ClarkLDF()} and
#'   \code{ClarkCapeCod()}.
#' @param triangle A triangle of cumulative claims, as for \code{\link{chain_ladder}}.
#' @param curve \code{"loglogistic"} (the default) or \code{"weibull"}.
#' @param premium The premium of each origin: needed by the Cape Cod method, and
#'   used for the loss ratios when given to the LDF method.
#' @param method \code{"ldf"} (the default) or \code{"cape cod"}.
#' @param truncation The age, in development periods, at which the curve is cut
#'   off and the claims are taken to be fully developed: \code{Inf} (the default)
#'   for none.
#' @param offset The average date of the claims in an origin period, as a share of
#'   a period from its start: 0.5 (the default) for claims spread evenly over it,
#'   0 to measure ages from the start.
#' @return An object of class \code{netsimr_reserve}: a list with \code{by_origin},
#'   a data frame with each origin's latest claims, share developed, ultimate, IBNR,
#'   standard error, coefficient of variation and its process and parameter parts
#'   (and premium and ultimate loss ratio when there are premiums); \code{total};
#'   \code{parameters}, the curve's \eqn{\omega} and \eqn{\theta} (and the loss
#'   ratio of the Cape Cod method), \eqn{\sigma^2} and the degrees of freedom;
#'   \code{growth}, the fitted curve as a function of the age; \code{fitted} and
#'   \code{residuals}, the fitted increments and the Pearson residuals
#'   \eqn{(c - \mu) / \sqrt{\sigma^2 \mu}}, as triangles; and \code{covariance}.
#' @references Clark, D. R. (2003). LDF curve-fitting and stochastic reserving: a
#'   maximum likelihood approach. Casualty Actuarial Society Forum, Fall 2003,
#'   41-92.
#' @seealso \code{\link{chain_ladder}} and \code{\link{cape_cod}}.
#' @export
#' @examples
#' clark <- clark_growth(raa_triangle)
#' clark
#' clark$parameters
#'
#' # the Cape Cod method with a Weibull curve
#' premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
#' clark_growth(raa_triangle, curve = "weibull", premium = premium, method = "cape cod")$total
clark_growth <- function(triangle, curve = c("loglogistic", "weibull"), premium = NULL, method = c("ldf", "cape cod"),
                         truncation = Inf, offset = 0.5) {
  curve <- match.arg(curve)
  method <- match.arg(method)
  checked <- reserving_check_triangle(triangle)
  C <- checked$triangle
  latest <- checked$latest
  m <- nrow(C)
  n <- ncol(C)
  if (!is.null(premium)) premium <- reserving_per_origin(premium, m, "premium", single = FALSE)
  if (method == "cape cod" && is.null(premium)) stop("The Cape Cod method needs the premium of each origin.", call. = FALSE)
  if (!is.numeric(truncation) || length(truncation) != 1 || is.na(truncation) || truncation <= n - 1) {
    stop("truncation must be an age beyond the triangle's last development period (", n - 1, "), or Inf.", call. = FALSE)
  }
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset) || offset < 0 || offset >= 1) {
    stop("offset must be a share of a period, from 0 up to 1.", call. = FALSE)
  }
  increments <- cbind(C[, 1], C[, -1, drop = FALSE] - C[, -n, drop = FALSE])
  dimnames(increments) <- dimnames(C)
  cells <- which(!is.na(increments), arr.ind = TRUE)
  origin_of <- cells[, 1]
  period_of <- cells[, 2]
  value <- increments[cells]
  growth <- function(age, omega, theta) {
    age <- pmax(age, 0)
    switch(curve,
      loglogistic = ifelse(age > 0, 1 / (1 + (theta / age)^omega), 0),
      weibull = -expm1(-(age / theta)^omega))
  }
  end_age <- function(period) period - offset
  latest_age <- end_age(latest)
  growth_to <- function(omega, theta) {
    if (is.finite(truncation)) growth(end_age(truncation), omega, theta) else 1
  }
  # the scale of the origins (their ultimates, or the premiums times the loss ratio) given the curve
  paid <- C[cbind(seq_len(m), latest)]
  scale_given <- function(omega, theta) {
    developed <- growth(latest_age, omega, theta)
    if (method == "ldf") {
      list(scale = paid / developed, elr = NA_real_)
    } else {
      elr <- sum(paid) / sum(premium * developed)
      list(scale = premium * elr, elr = elr)
    }
  }
  expected <- function(omega, theta, scale) {
    scale[origin_of] * (growth(end_age(period_of), omega, theta) - growth(end_age(period_of - 1), omega, theta))
  }
  negative_loglik <- function(p) {
    omega <- exp(p[1])
    theta <- exp(p[2])
    s <- scale_given(omega, theta)
    mu <- expected(omega, theta, s$scale)
    if (any(!is.finite(mu)) || any(mu <= 0)) return(1e300)
    -sum(value * log(mu) - mu)
  }
  # start from the age at which the chain ladder's development reaches a half
  start_theta <- max(1, n / 3)
  best <- NULL
  for (start in list(c(log(1.5), log(start_theta)), c(log(1), log(n / 2)), c(log(3), log(n / 4)))) {
    fit <- tryCatch(stats::optim(start, negative_loglik, method = "Nelder-Mead",
                                 control = list(maxit = 5000, reltol = 1e-14)), error = function(e) NULL)
    if (!is.null(fit)) {
      polished <- tryCatch(stats::optim(fit$par, negative_loglik, method = "BFGS", control = list(reltol = 1e-14)),
                           error = function(e) fit)
      if (polished$value <= fit$value) fit <- polished
      if (is.null(best) || fit$value < best$value) best <- fit
    }
  }
  if (is.null(best) || best$value >= 1e300) stop("The growth curve could not be fitted to the triangle.", call. = FALSE)
  omega <- exp(best$par[1])
  theta <- exp(best$par[2])
  s <- scale_given(omega, theta)
  mu <- expected(omega, theta, s$scale)
  parameters_count <- if (method == "ldf") m + 2 else 3
  df <- length(value) - parameters_count
  if (df <= 0) stop("The triangle has too few cells for the ", parameters_count, " parameters of the method.", call. = FALSE)
  sigma2 <- sum((value - mu)^2 / mu) / df

  # the parameters, the reserve and its gradient: the scales (the ultimates, or the loss
  # ratio), then omega and theta
  full_parameters <- c(if (method == "ldf") s$scale else s$elr, omega, theta)
  unpack <- function(b) {
    k <- length(b)
    list(scale = if (method == "ldf") b[seq_len(m)] else premium * b[1], omega = b[k - 1], theta = b[k])
  }
  reserve_of <- function(b) {
    u <- unpack(b)
    u$scale * (growth_to(u$omega, u$theta) - growth(latest_age, u$omega, u$theta))
  }
  mean_of <- function(b) {
    u <- unpack(b)
    expected(u$omega, u$theta, u$scale)
  }
  jacobian <- function(f, b) {
    base <- f(b)
    vapply(seq_along(b), function(j) {
      h <- 1e-6 * max(abs(b[j]), 1e-8)
      up <- b
      down <- b
      up[j] <- b[j] + h
      down[j] <- b[j] - h
      (f(up) - f(down)) / (2 * h)
    }, numeric(length(base)))
  }
  # the derivatives of the curve with respect to omega and theta, a column each
  growth_derivatives <- function(age, omega, theta) {
    age <- pmax(age, 0)
    out <- matrix(0, length(age), 2)
    pos <- age > 0
    if (curve == "loglogistic") {
      r <- (theta / age[pos])^omega
      g <- r / (1 + r)^2
      out[pos, 1] <- -g * log(theta / age[pos])
      out[pos, 2] <- -g * omega / theta
    } else {
      z <- (age[pos] / theta)^omega
      g <- exp(-z) * z
      out[pos, 1] <- g * log(age[pos] / theta)
      out[pos, 2] <- -g * omega / theta
    }
    out
  }
  # the derivatives of each cell's mean with respect to the parameters
  mean_derivatives <- function(b) {
    u <- unpack(b)
    share <- growth(end_age(period_of), u$omega, u$theta) - growth(end_age(period_of - 1), u$omega, u$theta)
    d_share <- growth_derivatives(end_age(period_of), u$omega, u$theta) -
      growth_derivatives(end_age(period_of - 1), u$omega, u$theta)
    by_scale <- if (method == "ldf") {
      outer(origin_of, seq_len(m), "==") * share
    } else {
      cbind(premium[origin_of] * share)
    }
    cbind(by_scale, u$scale[origin_of] * d_share)
  }
  # the covariance of the parameters: sigma^2 times the inverse of minus the second
  # derivatives of the log-likelihood (Clark's observed information), from its gradient
  loglik_gradient <- function(b) colSums((value / mean_of(b) - 1) * mean_derivatives(b))
  hessian <- jacobian(loglik_gradient, full_parameters)
  hessian <- (hessian + t(hessian)) / 2
  covariance <- tryCatch(sigma2 * solve(-hessian), error = function(e) {
    matrix(NA_real_, length(full_parameters), length(full_parameters))
  })
  reserve <- reserve_of(full_parameters)
  d_reserve <- jacobian(reserve_of, full_parameters)
  process <- sigma2 * pmax(reserve, 0)
  parameter <- rowSums((d_reserve %*% covariance) * d_reserve)
  total_parameter <- sum(colSums(d_reserve) %*% covariance * colSums(d_reserve))
  ultimate <- paid + reserve
  se <- sqrt(process + parameter)
  developed <- growth(latest_age, omega, theta) / growth_to(omega, theta)
  by_origin <- data.frame(
    origin = rownames(C), latest = paid, developed = developed, ultimate = ultimate, ibnr = reserve,
    se = se, cv = ifelse(reserve != 0, se / reserve, NA_real_), process_se = sqrt(process), parameter_se = sqrt(parameter),
    stringsAsFactors = FALSE, row.names = NULL
  )
  total_se <- sqrt(sum(process) + total_parameter)
  total <- c(latest = sum(paid), developed = sum(paid) / sum(ultimate), ultimate = sum(ultimate), ibnr = sum(reserve),
             se = total_se, cv = if (sum(reserve) != 0) total_se / sum(reserve) else NA_real_,
             process_se = sqrt(sum(process)), parameter_se = sqrt(total_parameter))
  if (!is.null(premium)) {
    by_origin$premium <- premium
    by_origin$ultimate_loss_ratio <- ultimate / premium
    total[["premium"]] <- sum(premium)
    total[["ultimate_loss_ratio"]] <- sum(ultimate) / sum(premium)
  }
  fitted <- residuals <- increments
  fitted[] <- NA_real_
  residuals[] <- NA_real_
  fitted[cells] <- mu
  residuals[cells] <- (value - mu) / sqrt(sigma2 * mu)
  names(full_parameters) <- c(if (method == "ldf") paste0("ultimate_", rownames(C)) else "loss_ratio", "omega", "theta")
  dimnames(covariance) <- list(names(full_parameters), names(full_parameters))
  structure(list(
    method = paste0("Clark ", if (method == "ldf") "LDF" else "Cape Cod", " method, ",
                    if (curve == "loglogistic") "log-logistic" else "Weibull", " curve"),
    by_origin = by_origin, total = total,
    parameters = c(omega = omega, theta = theta, if (method == "cape cod") c(loss_ratio = s$elr), sigma2 = sigma2, df = df),
    growth = function(age) growth(age - offset, omega, theta) / growth_to(omega, theta),
    fitted = fitted, residuals = residuals, covariance = covariance, triangle = C,
    curve = curve, truncation = truncation, offset = offset
  ), class = "netsimr_reserve")
}

#' Chain ladder for claims inflation
#'
#' @description The chain ladder of a triangle restated in today's money: past
#'   payments are brought to the level of the latest calendar period by past claims
#'   inflation, developed, and the future payments are then inflated by the expected
#'   future inflation, so that the reserve reflects inflation that differs from the
#'   past's.
#'
#' @details The chain ladder of nominal claims projects the average inflation of the
#'   past into the future. Here each incremental payment of calendar period \eqn{c}
#'   is multiplied by the inflation from \eqn{c} to the latest calendar period, the
#'   chain ladder of the restated cumulative triangle projects the future increments
#'   in today's money, and each is multiplied by the future inflation from the latest
#'   calendar period to its own. With no inflation the result is the chain ladder's.
#'   A tail is paid in the calendar period after the triangle's last development
#'   period.
#' @param triangle A triangle of cumulative claims, as for \code{\link{chain_ladder}};
#'   paid claims, as incurred claims include case reserves that are not inflated by
#'   period.
#' @param past The past claims inflation: one rate per calendar period (0.05 for 5\%
#'   a period), or a rate for each step between consecutive calendar periods, oldest
#'   first (one fewer than the number of calendar periods).
#' @param future The future claims inflation per calendar period: one rate, or a rate
#'   for each future calendar period, nearest first. By default the same single rate as
#'   \code{past} when that is one rate, otherwise 0.
#' @param ... Arguments passed to \code{chain_ladder()} for the restated triangle,
#'   such as \code{average} or \code{tail}.
#' @return An object of class \code{netsimr_reserve}: a list with \code{by_origin}, a
#'   data frame with each origin's latest claims, ultimate and IBNR, the IBNR in
#'   today's money (before future inflation) and the cost of future inflation;
#'   \code{total}; \code{chain_ladder}, the chain ladder of the restated triangle; and
#'   \code{restated}, the triangle in today's money.
#' @seealso \code{\link{chain_ladder}}.
#' @export
#' @examples
#' # 5% a year in the past, 8% expected in the future
#' inflation_adjusted_chain_ladder(raa_triangle, past = 0.05, future = 0.08)
#'
#' # the same inflation for both is close to the plain chain ladder
#' inflation_adjusted_chain_ladder(raa_triangle, past = 0.05)$total[["ibnr"]]
#' chain_ladder(raa_triangle)$total[["ibnr"]]
inflation_adjusted_chain_ladder <- function(triangle, past = 0, future = if (length(past) == 1) past else 0, ...) {
  checked <- reserving_check_triangle(triangle)
  C <- checked$triangle
  latest <- checked$latest
  m <- nrow(C)
  n <- ncol(C)
  calendar <- row(C) + col(C) - 1
  newest <- max(seq_len(m) + latest - 1)
  oldest <- 1
  steps <- newest - oldest
  if (!is.numeric(past) || any(!is.finite(past)) || any(past <= -1) || !(length(past) %in% c(1, steps))) {
    stop("past must be one inflation rate, or one for each of the ", steps, " steps between the calendar periods.", call. = FALSE)
  }
  rates <- rep_len(past, steps)
  # the inflation from each calendar period to the latest
  to_latest <- rev(cumprod(rev(c(1 + rates, 1))))
  increments <- cbind(C[, 1], C[, -1, drop = FALSE] - C[, -n, drop = FALSE])
  restated_increments <- increments * to_latest[pmin(calendar, newest)]
  restated <- t(apply(restated_increments, 1, cumsum))
  restated[is.na(C)] <- NA
  dimnames(restated) <- dimnames(C)
  cl <- chain_ladder(restated, ...)
  full <- cl$full_triangle[, seq_len(n), drop = FALSE]
  tail_amount <- cl$full_triangle[, "Ultimate"] - full[, n]
  future_increments <- cbind(full[, 1], full[, -1, drop = FALSE] - full[, -n, drop = FALSE])
  future_increments[!is.na(C)] <- 0
  # the tail is paid in the period after the last development period
  future_calendar <- cbind(calendar, seq_len(m) + n)
  future_increments <- cbind(future_increments, tail_amount)
  horizon <- max(future_calendar) - newest
  if (!is.numeric(future) || any(!is.finite(future)) || any(future <= -1) || length(future) < 1) {
    stop("future must be one inflation rate, or one for each future calendar period.", call. = FALSE)
  }
  future_rates <- if (length(future) == 1) rep(future, horizon) else c(future, rep(future[length(future)], max(0, horizon - length(future))))
  from_latest <- c(1, cumprod(1 + future_rates))
  ahead <- pmax(future_calendar - newest, 0)
  inflated <- future_increments * from_latest[ahead + 1]
  ibnr_today <- rowSums(future_increments, na.rm = TRUE)
  ibnr <- rowSums(inflated, na.rm = TRUE)
  paid <- C[cbind(seq_len(m), latest)]
  by_origin <- data.frame(
    origin = rownames(C), latest = paid, ultimate = paid + ibnr, ibnr = ibnr, ibnr_today = ibnr_today,
    future_inflation = ibnr - ibnr_today, stringsAsFactors = FALSE, row.names = NULL
  )
  total <- c(latest = sum(paid), ultimate = sum(paid) + sum(ibnr), ibnr = sum(ibnr), ibnr_today = sum(ibnr_today),
             future_inflation = sum(ibnr) - sum(ibnr_today))
  structure(list(method = "Chain ladder for claims inflation", by_origin = by_origin, total = total,
                 chain_ladder = cl, restated = restated, past = rates, future = future_rates),
            class = "netsimr_reserve")
}
