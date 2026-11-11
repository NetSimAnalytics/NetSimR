# The Burr and the generalised Pareto distributions of claim sizes, which the
# simulator draws from and the distribution fitting tool fits, and the Hill plot
# of the tail index. Base R has neither distribution; the functions here work on
# the log scale, so that they stay accurate far into the tail.

# log(1 + e^z), without overflow for a large z.
log1pexp <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))

# The mean and the standard deviation of a Weibull distribution:
# scale Gamma(1 + 1/shape), and scale sqrt(Gamma(1 + 2/shape) - Gamma(1 + 1/shape)^2);
# infinite where the Gamma function overflows (a shape near zero).
weibull_moments <- function(shape, scale) {
  if (shape <= 0 || scale <= 0) return(c(NA_real_, NA_real_))
  mean_value <- scale * exp(lgamma(1 + 1 / shape))
  second <- scale^2 * exp(lgamma(1 + 2 / shape))
  c(mean_value, if (is.finite(second) && is.finite(mean_value)) sqrt(max(second - mean_value^2, 0)) else Inf)
}

# ------------------------------------------------- Burr

# The Burr (type XII) distribution has the survival function
# S(x) = (1 + (x / scale)^gamma)^(-alpha) for x > 0: a Pareto tail of index
# alpha gamma, over a body whose shape gamma sets. alpha = 1 is the log-logistic,
# gamma = 1 the Lomax (a Pareto from zero).

# log S(x)
burr_log_survival <- function(x, alpha, gamma, scale) {
  -alpha * log1pexp(gamma * (log(pmax(x, 0)) - log(scale)))
}

# log f(x), for x > 0
burr_log_density <- function(x, alpha, gamma, scale) {
  z <- gamma * (log(x) - log(scale))
  log(alpha) + log(gamma) - log(x) + z - (alpha + 1) * log1pexp(z)
}

# The x that is exceeded with probability s: (s^(-1/alpha) - 1)^(1/gamma) scale.
burr_tail_quantile <- function(s, alpha, gamma, scale) {
  scale * expm1(-log(s) / alpha)^(1 / gamma)
}

# The mean and the standard deviation: E[X^k] = scale^k Gamma(alpha - k/gamma)
# Gamma(1 + k/gamma) / Gamma(alpha), which exists for alpha gamma > k.
burr_moments <- function(alpha, gamma, scale) {
  if (alpha <= 0 || gamma <= 0 || scale <= 0) return(c(NA_real_, NA_real_))
  moment <- function(k) {
    if (alpha * gamma <= k) return(Inf)
    scale^k * exp(lgamma(alpha - k / gamma) + lgamma(1 + k / gamma) - lgamma(alpha))
  }
  mean_value <- moment(1)
  second <- moment(2)
  c(mean_value, if (is.finite(second)) sqrt(max(second - mean_value^2, 0)) else Inf)
}

# The mean claim above v, E[X | X > v]: with u = (v/scale)^gamma / (1 + (v/scale)^gamma),
# scale alpha B(1 + 1/gamma, alpha - 1/gamma) times the upper tail at u of that Beta
# distribution, over S(v). Infinite for alpha gamma <= 1.
burr_tail_mean <- function(v, alpha, gamma, scale) {
  if (alpha * gamma <= 1) return(rep(Inf, length(v)))
  z <- gamma * (log(pmax(v, 0)) - log(scale))
  scale * alpha * exp(lbeta(1 + 1 / gamma, alpha - 1 / gamma) +
                        stats::pbeta(stats::plogis(z), 1 + 1 / gamma, alpha - 1 / gamma, lower.tail = FALSE, log.p = TRUE) +
                        alpha * log1pexp(z))
}

# ------------------------------------------------- Generalised Pareto

# The generalised Pareto distribution of the claims above a location (a
# threshold, or zero): S(x) = (1 + shape (x - location) / scale)^(-1 / shape),
# the Exponential for shape = 0. The limit distribution of the excesses over a
# high threshold: shape > 0 is a Pareto tail of index 1 / shape, and shape < 0 a
# tail that ends at location - scale / shape.

# log S(x)
gpd_log_survival <- function(x, location, scale, shape) {
  z <- pmax(x - location, 0) / scale
  if (abs(shape) < 1e-9) -z else -log1p(pmax(shape * z, -1)) / shape
}

# log f(x), for x above the location (and below the end of the tail for a negative shape)
gpd_log_density <- function(x, location, scale, shape) {
  z <- (x - location) / scale
  if (abs(shape) < 1e-9) return(-log(scale) - z)
  inside <- shape * z > -1
  out <- rep(-Inf, length(z))
  out[inside] <- -log(scale) - (1 / shape + 1) * log1p(shape * z[inside])
  out
}

# The x that is exceeded with probability s.
gpd_tail_quantile <- function(s, location, scale, shape) {
  if (abs(shape) < 1e-9) location - scale * log(s) else location + scale * expm1(-shape * log(s)) / shape
}

# The mean, location + scale / (1 - shape) for a shape below 1, and the standard
# deviation, scale / ((1 - shape) sqrt(1 - 2 shape)) for a shape below 1/2.
gpd_moments <- function(location, scale, shape) {
  if (scale <= 0) return(c(NA_real_, NA_real_))
  c(if (shape < 1) location + scale / (1 - shape) else Inf,
    if (shape < 0.5) scale / ((1 - shape) * sqrt(1 - 2 * shape)) else Inf)
}

# The mean claim above v: the excesses over v are generalised Pareto again, with
# the scale scale + shape (v - location).
gpd_tail_mean <- function(v, location, scale, shape) {
  if (shape >= 1) return(rep(Inf, length(v)))
  v <- pmax(v, location)
  v + pmax(scale + shape * (v - location), 0) / (1 - shape)
}

# ------------------------------------------------- Fits

# The parameters that minimise minus a log-likelihood `nll` over unconstrained
# parameters: Nelder-Mead from the starting point, then BFGS from where it stops.
dft_minimise <- function(nll, start, parscale = rep(1, length(start))) {
  guarded <- function(p) {
    v <- nll(p)
    if (is.finite(v)) v else 1e300
  }
  fit <- stats::optim(start, guarded, control = list(maxit = 5000, reltol = 1e-14, parscale = parscale))
  polished <- tryCatch(stats::optim(fit$par, guarded, method = "BFGS",
                                    control = list(maxit = 1000, reltol = 1e-14, parscale = parscale)),
                       error = function(e) fit)
  if (polished$value <= fit$value) fit <- polished
  if (fit$convergence != 0 || fit$value >= 1e300) stop("the optimisation did not converge")
  fit
}

# The claims of a fit above a threshold with policy limits: checked, and split
# into those below their limits and those at them (censored).
dft_fit_claims <- function(x, d = 0, censored = NULL) {
  n <- length(x)
  if (is.null(censored)) censored <- rep(FALSE, n)
  if (length(censored) != n || anyNA(censored)) stop("censored must say for each claim whether it is censored")
  if (!(is.numeric(d) && length(d) == 1 && is.finite(d) && d >= 0)) stop("the threshold must be zero or a positive number")
  if (n < 2 || any(!is.finite(x)) || any(x <= d)) stop("the claims must all be above the threshold")
  xu <- x[!censored]
  if (length(xu) < 2 || length(unique(xu)) < 2) stop("at least two different claims must be below their policy limits")
  list(n = n, xu = xu, xc = x[censored])
}

# Burr shapes beyond these are a fit at the edge of the Burr family: alpha
# growing without bound is a Weibull, gamma growing without bound a Pareto.
dft_burr_max_shape <- 1000

# Maximum likelihood fit of a Burr distribution to claims `x`, known only above
# a reporting threshold `d` (0 for none) and some `censored` at their policy
# limits: the likelihood of dft_truncated_severity_models(). The search is over
# the logs of the three parameters, from the log-logistic (alpha = 1) with the
# median and the spread of the logs of the claims.
# Returns the `estimate` (alpha, gamma, scale) and the `loglik`. Stops when the
# fit runs to the edge of the family, where a Weibull or a Pareto fits as well
# with a parameter less.
dft_fit_burr <- function(x, d = 0, censored = NULL) {
  claims <- dft_fit_claims(x, d, censored)
  lxu <- log(claims$xu)
  sum_lxu <- sum(lxu)
  lxc <- log(claims$xc)
  nu <- length(lxu)
  log_d <- if (d > 0) log(d) else -Inf
  nll <- function(p) {
    alpha <- exp(p[1])
    gamma <- exp(p[2])
    zu <- gamma * (lxu - p[3])
    density <- nu * (p[1] + p[2]) - sum_lxu + sum(zu) - (alpha + 1) * sum(log1pexp(zu))
    limits <- if (length(lxc)) -alpha * sum(log1pexp(gamma * (lxc - p[3]))) else 0
    threshold <- if (d > 0) claims$n * alpha * log1pexp(gamma * (log_d - p[3])) else 0
    -(density + limits + threshold)
  }
  lx <- log(x)
  gamma0 <- pi / (sqrt(3) * max(stats::sd(lx), 1e-6))
  inside <- function(fit) !is.null(fit) && all(is.finite(fit$par)) && all(fit$par[1:2] <= log(dft_burr_max_shape))
  fit <- tryCatch(dft_minimise(nll, c(0, log(gamma0), stats::median(lx))), error = function(e) NULL)
  if (!inside(fit)) {
    # (a search that stops at the edge, or not at all, is made again from a lighter tail)
    again <- tryCatch(dft_minimise(nll, c(log(4), log(gamma0), stats::median(lx) + log(4) / gamma0)), error = function(e) NULL)
    if (is.null(fit) || (!is.null(again) && again$value < fit$value)) fit <- again
  }
  if (is.null(fit)) stop("the optimisation did not converge")
  estimate <- c(alpha = exp(fit$par[1]), gamma = exp(fit$par[2]), scale = exp(fit$par[3]))
  if (estimate[["alpha"]] > dft_burr_max_shape) {
    stop("the claims have a lighter tail than any Burr, whose fit runs to a Weibull", call. = FALSE)
  }
  if (estimate[["gamma"]] > dft_burr_max_shape) {
    stop("the fit runs to a Pareto from the smallest claim, the edge of the Burr family", call. = FALSE)
  }
  if (any(!is.finite(estimate))) stop("the optimisation did not converge")
  list(estimate = estimate, loglik = -fit$value)
}

# Maximum likelihood fit of a generalised Pareto distribution to claims `x`
# above its `location` (the reporting threshold, or zero), some `censored` at
# their policy limits. The search is over the log of the scale and the shape,
# which is kept above -1/2 (below it the likelihood has no regular maximum), from
# their method of moments estimates. Returns the `estimate` (scale, shape) and
# the `loglik`.
dft_fit_gpd <- function(x, location = 0, censored = NULL) {
  claims <- dft_fit_claims(x, location, censored)
  zu <- claims$xu - location
  zc <- claims$xc - location
  nu <- length(zu)
  largest <- max(zu, zc)
  nll <- function(p) {
    scale <- exp(p[1])
    shape <- p[2]
    if (shape < -0.5 || shape * largest / scale <= -1) return(Inf)
    minus_log_s <- function(z) if (abs(shape) < 1e-9) z / scale else log1p(shape * z / scale) / shape
    nu * p[1] + sum(minus_log_s(zu)) + sum(log1p(shape * zu / scale)) + (if (length(zc)) sum(minus_log_s(zc)) else 0)
  }
  z <- x - location
  m <- mean(z)
  v <- stats::var(z)
  shape0 <- if (is.finite(v) && v > 0) min(max(0.5 * (1 - m^2 / v), -0.2), 0.9) else 0.1
  fit <- dft_minimise(nll, c(log(m * (1 - shape0)), shape0))
  list(estimate = c(scale = exp(fit$par[1]), shape = fit$par[2]), loglik = -fit$value)
}

# The Burr and generalised Pareto models of the Severity tab, in the form of its
# other models (dft_truncated_severity_models()): fitted to claims `x` above a
# reporting threshold `d` (0 for none), some `censored` at their policy limits.
# The Burr is of the claims from zero, as the other distributions are; the
# generalised Pareto starts at the threshold (at zero without one), as the
# Pareto does, so all its claims exceed the threshold. A fit that fails has NA
# parameters and no cdf, and its message is in the `failed` attribute.
dft_heavy_tail_models <- function(x, d = 0, censored = NULL) {
  truncated <- d > 0
  censoring <- !is.null(censored) && any(censored)
  what <- if (truncated) "fit above the threshold" else if (censoring) "fit with the policy limits" else "fit"
  failed <- character()
  failed_model <- function(name, params, k, e) {
    failed <<- c(failed, paste0(name, " ", what, " failed: ", conditionMessage(e)))
    list(name = name, params = params, mean = NA_real_, k = k, cdf = NULL, loglik = NA_real_, survival_at_threshold = NA_real_)
  }
  # (each model in its own environment: the cdfs keep their own parameters)
  burr <- tryCatch(local({
    fit <- dft_fit_burr(x, d, censored)
    alpha <- fit$estimate[["alpha"]]
    gamma <- fit$estimate[["gamma"]]
    scale <- fit$estimate[["scale"]]
    log_s_d <- if (truncated) burr_log_survival(d, alpha, gamma, scale) else 0
    if (truncated && (!is.finite(log_s_d) || log_s_d < log(1e-9))) {
      stop("the claims above the threshold have a heavier tail than any Burr, and the fit runs to the edge of ",
           "its parameters, leaving almost no claims above the threshold", call. = FALSE)
    }
    list(name = "Burr", params = c(Scale = scale, alpha = alpha, gamma = gamma),
         mean = burr_tail_mean(d, alpha, gamma, scale), k = 3,
         cdf = function(q) ifelse(q <= d, 0, -expm1(burr_log_survival(pmax(q, d), alpha, gamma, scale) - log_s_d)),
         loglik = fit$loglik, survival_at_threshold = exp(log_s_d))
  }), error = function(e) failed_model("Burr", c(Scale = NA_real_, alpha = NA_real_, gamma = NA_real_), 3, e))
  gpd <- tryCatch(local({
    fit <- dft_fit_gpd(x, d, censored)
    scale <- fit$estimate[["scale"]]
    shape <- fit$estimate[["shape"]]
    list(name = "Generalised Pareto", params = c(Location = d, Scale = scale, Shape = shape),
         mean = gpd_moments(d, scale, shape)[1], k = 2,
         cdf = function(q) -expm1(gpd_log_survival(q, d, scale, shape)),
         loglik = fit$loglik, survival_at_threshold = 1)
  }), error = function(e) failed_model("Generalised Pareto", c(Location = NA_real_, Scale = NA_real_, Shape = NA_real_), 2, e))
  structure(list(burr, gpd), failed = failed)
}

# ------------------------------------------------- Hill plot

# The points of a Hill plot: for each number k of the largest claims, the Hill
# estimate of the tail index alpha of the claims above the (k + 1)-th largest,
# 1 / (the mean of the logs of the k largest - the log of the (k + 1)-th), with
# its 95% band alpha (1 +- 1.96 / sqrt(k)) and that claim, the threshold of the
# estimate. Where the estimate is level over a range of k, the claims above the
# threshold there follow a Pareto with that alpha. The estimates are from the
# `from` largest claims to the larger half of the claims (below it the body of
# the distribution, not its tail, is measured), at most `points` of them,
# evenly spread.
dft_hill_data <- function(x, from = 5, points = 400) {
  x <- sort(x[is.finite(x) & x > 0], decreasing = TRUE)
  n <- length(x)
  if (n < from + 2) stop("a Hill plot needs at least ", from + 2, " claims")
  lx <- log(x)
  k <- from:max(from + 1, min(n - 1, ceiling(n / 2)))
  alpha <- 1 / (cumsum(lx)[k] / k - lx[k + 1])
  # the k largest claims all the size of the next have no excess and no estimate: found from
  # the claims, as the running sum can leave a rounding error in place of zero (macOS)
  alpha[x[1] == x[k + 1]] <- Inf
  if (length(k) > points) {
    keep <- unique(round(seq(1, length(k), length.out = points)))
    k <- k[keep]
    alpha <- alpha[keep]
  }
  known <- is.finite(alpha)
  if (sum(known) < 2) stop("the largest claims are all the same size")
  k <- k[known]
  alpha <- alpha[known]
  half <- 1.96 * alpha / sqrt(k)
  list(k = k, alpha = alpha, lower = pmax(alpha - half, 0), upper = alpha + half, threshold = x[k + 1])
}
