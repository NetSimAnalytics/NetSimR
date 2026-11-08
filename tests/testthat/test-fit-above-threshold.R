#Severity fits to claims reported only above a threshold (left truncated data)

#the model of a name among the fits
fit_named <- function(models, name) models[[match(name, vapply(models, `[[`, character(1), "name"))]]

test_that("fits above a threshold recover the distribution of the claims from zero", {
  set.seed(42)
  d <- 5000
  draw_above <- function(draw) {
    x <- draw(1e6)
    x[x > d][1:20000]
  }
  check <- function(name, x, expected, tolerance) {
    models <- dft_truncated_severity_models(x, d)
    #(the other distributions may fail on these claims)
    expect_false(any(startsWith(attr(models, "failed"), name)), label = paste(name, "fit failed"))
    expect_equal(unname(fit_named(models, name)$params), expected, tolerance = tolerance, label = name)
  }
  check("LogNormal", draw_above(function(n) stats::rlnorm(n, 8, 1.2)), c(8, 1.2), 0.02)
  check("Gamma", draw_above(function(n) stats::rgamma(n, shape = 1.5, scale = 4000)), c(4000, 1.5), 0.06)
  check("Normal", draw_above(function(n) stats::rnorm(n, 6000, 3000)), c(6000, 3000), 0.03)
  check("Exponential", draw_above(function(n) stats::rexp(n, 1 / 3000)), 3000, 0.03)
  check("Pareto", draw_above(function(n) 1000 * stats::runif(n)^(-1 / 1.7)), c(5000, 1.7), 0.03)
  #fitted to the claims above the threshold as if they were all the claims, the LogNormal is far off
  x <- draw_above(function(n) stats::rlnorm(n, 8, 1.2))
  naive <- c(mean(log(x)), sqrt(mean((log(x) - mean(log(x)))^2)))
  expect_gt(abs(naive[1] - 8), 0.5)
})

test_that("a fit above a threshold has the maximum of the truncated likelihood, and a cdf of the claims above it", {
  set.seed(7)
  d <- 2000
  x <- stats::rlnorm(5000, 7.5, 1.1)
  x <- x[x > d]
  models <- dft_truncated_severity_models(x, d)
  lnorm <- fit_named(models, "LogNormal")
  p <- unname(lnorm$params)
  loglik <- function(mu, sigma) {
    sum(stats::dlnorm(x, mu, sigma, log = TRUE)) - length(x) * stats::plnorm(d, mu, sigma, lower.tail = FALSE, log.p = TRUE)
  }
  expect_equal(lnorm$loglik, loglik(p[1], p[2]), tolerance = 1e-10)
  #no nearby parameters do better
  for (step in list(c(0.01, 0), c(-0.01, 0), c(0, 0.01), c(0, -0.01))) {
    expect_lt(loglik(p[1] + step[1], p[2] + step[2]), lnorm$loglik)
  }
  #its cdf is the conditional cdf above the threshold, and its survival the share above it
  q <- c(1000, 2000, 3000, 10000, 1e5)
  s_d <- stats::plnorm(d, p[1], p[2], lower.tail = FALSE)
  expect_equal(lnorm$cdf(q), pmax(0, (stats::plnorm(q, p[1], p[2]) - (1 - s_d)) / s_d), tolerance = 1e-10)
  expect_equal(lnorm$survival_at_threshold, s_d, tolerance = 1e-12)
  #each mean is the fitted mean of a claim above the threshold
  #(the Pareto's tail decays too slowly for integrate(): its mean is checked on its own)
  pareto <- fit_named(models, "Pareto")
  expect_equal(pareto$mean, pareto$params[["alpha"]] * d / (pareto$params[["alpha"]] - 1))
  for (m in models) {
    if (!is.finite(m$mean) || m$name == "Pareto") next
    s <- function(q) 1 - m$cdf(q)
    #(in two parts: over an infinite range alone, integrate() can miss where the mass is)
    middle <- d + 50 * (mean(x) - d)
    integral <- stats::integrate(s, d, middle, rel.tol = 1e-9)$value +
      stats::integrate(s, middle, Inf, rel.tol = 1e-9, stop.on.error = FALSE)$value
    expect_equal(m$mean, d + integral, tolerance = 1e-6, label = m$name)
  }
  #the Exponential's is the empirical mean (its excess over the threshold is Exponential)
  expect_equal(fit_named(models, "Exponential")$mean, mean(x))
})

test_that("each fit above a threshold has its own cdf", {
  #(the fits once shared their parameters' names in one frame, and the Normal's cdf was drawn
  #with the LogNormal's mu and sigma)
  set.seed(5)
  d <- 4000
  x <- stats::rnorm(1e5, 6000, 3000)
  x <- x[x > d][1:5000]
  models <- dft_truncated_severity_models(x, d)
  for (m in models) {
    p <- unname(m$params)
    ground <- switch(m$name,
      Normal = function(q) stats::pnorm(q, p[1], p[2]), LogNormal = function(q) stats::plnorm(q, p[1], p[2]),
      Exponential = function(q) stats::pexp(q, 1 / p[1]), Gamma = function(q) stats::pgamma(q, shape = p[2], scale = p[1]),
      Pareto = function(q) ifelse(q < p[1], 0, 1 - (p[1] / q)^p[2]),
      Weibull = function(q) stats::pweibull(q, shape = p[2], scale = p[1]))
    q <- c(4500, 6000, 9000)
    expect_equal(m$cdf(q), (ground(q) - ground(d)) / (1 - ground(d)), tolerance = 1e-9, label = m$name)
  }
})

test_that("a fit that runs to the edge of its parameters is reported as failed", {
  #LogNormal claims above a threshold: their tail is heavier than any Normal's or Gamma's, and
  #those fits run off to a Normal mean of minus billions and a Gamma shape of almost 0
  set.seed(7)
  d <- 2000
  x <- stats::rlnorm(5000, 7.5, 1.1)
  x <- x[x > d]
  models <- dft_truncated_severity_models(x, d)
  failed <- attr(models, "failed")
  expect_length(failed, 2)
  expect_match(failed[1], "^Normal fit above the threshold failed: the claims above the threshold have a heavier tail")
  expect_match(failed[2], "^Gamma fit")
  expect_null(fit_named(models, "Normal")$cdf)
  expect_true(all(is.na(fit_named(models, "Gamma")$params)))
  expect_true(is.function(fit_named(models, "LogNormal")$cdf))
})

test_that("a fit above a threshold refuses claims at or below it, and reports a failed fit", {
  expect_error(dft_truncated_severity_models(c(10, 20, 30), 15), "above the threshold")
  expect_error(dft_truncated_severity_models(c(10, 20, 30), 0), "positive number")
  #two claims: the two-parameter fits may fail, but the result keeps its form
  models <- dft_truncated_severity_models(c(10.5, 11), 10)
  expect_length(models, 6)
  for (m in models) expect_true(is.null(m$cdf) || is.function(m$cdf))
})

test_that("a fit above a threshold is sent to the simulator from zero, with the frequency of all claims", {
  #the counts of claims above the threshold, a share p of all claims: all claims are as many
  #as the reported ones divided by p (a Poisson's or a Negative Binomial's thinning)
  values <- dft_simulator_settings(list(model = "Poisson", lambda = 2), list(model = "Gamma", shape = 2, scale = 100),
                                   frequency_share = 0.4)
  expect_equal(values$lamda, 5)
  values <- dft_simulator_settings(list(model = "Negative Binomial", size = 1.5, mu = 2), frequency_share = 0.4)
  expect_equal(c(values$r, values$beta), c(1.5, 2 / 1.5 / 0.4))
  #thinning the simulator's Negative Binomial by p gives the reported counts back
  set.seed(3)
  all_claims <- stats::rpois(2e5, stats::rgamma(2e5, shape = values$r, scale = values$beta))
  reported <- stats::rbinom(length(all_claims), all_claims, 0.4)
  expect_equal(mean(reported), 2, tolerance = 0.02)
  expect_equal(stats::var(reported), 2 + 2^2 / 1.5, tolerance = 0.03)
  expect_error(dft_simulator_settings(list(model = "Poisson", lambda = 2), frequency_share = 0), "share")
})

test_that("the note on failed fits above a threshold names them together", {
  edge <- function(name) paste0(name, " fit above the threshold failed: the claims above the threshold have a heavier tail than any ",
                                name, ", and the fit runs to the edge of its parameters, leaving almost no claims above the threshold")
  expect_identical(dft_failed_fits_note(character()), "")
  expect_identical(dft_failed_fits_note(c(edge("Normal"), edge("Gamma"))),
                   paste(" No Normal or Gamma fits: the claims above the threshold have a heavier tail than any Normal or Gamma,",
                         "whose fit runs to the edge of its parameters."))
  expect_identical(dft_failed_fits_note("LogNormal fit above the threshold failed: the optimisation did not converge"),
                   " LogNormal fit above the threshold failed: the optimisation did not converge.")
})

test_that("the Weibull fit is the maximum likelihood fit, from zero and above a threshold", {
  set.seed(12)
  x <- stats::rweibull(4000, shape = 0.8, scale = 5000)
  fit <- dft_fit_weibull(x)$estimate
  #the likelihood is at its highest: no small step in either parameter raises it
  loglik <- function(shape, scale) sum(stats::dweibull(x, shape = shape, scale = scale, log = TRUE))
  best <- loglik(fit[["shape"]], fit[["scale"]])
  for (step in c(0.999, 1.001)) {
    expect_lt(loglik(fit[["shape"]] * step, fit[["scale"]]), best)
    expect_lt(loglik(fit[["shape"]], fit[["scale"]] * step), best)
  }
  expect_equal(fit[["shape"]], 0.8, tolerance = 0.05)
  expect_error(dft_fit_weibull(c(1, -1)), "positive")
  #above a threshold: the ground-up parameters, and the mean of the claims above it
  d <- 3000
  above <- x[x > d]
  models <- dft_truncated_severity_models(above, d)
  weibull <- fit_named(models, "Weibull")
  expect_equal(weibull$params[["Shape"]], 0.8, tolerance = 0.1)
  s <- function(q) 1 - weibull$cdf(q)
  integral <- stats::integrate(s, d, d + 50 * (mean(above) - d), rel.tol = 1e-9)$value +
    stats::integrate(s, d + 50 * (mean(above) - d), Inf, rel.tol = 1e-9, stop.on.error = FALSE)$value
  expect_equal(weibull$mean, d + integral, tolerance = 1e-6)
})

test_that("the Anderson-Darling statistic and the Q-Q points measure a fit", {
  #a hand computation: three claims against a uniform cdf
  u <- c(0.2, 0.5, 0.9)
  by_hand <- -3 - ((1 * (log(0.2) + log(1 - 0.9))) + (3 * (log(0.5) + log(1 - 0.5))) + (5 * (log(0.9) + log(1 - 0.2)))) / 3
  expect_equal(ad_statistic(u, stats::punif), by_hand)
  expect_true(is.na(ad_statistic(numeric(0), stats::punif)))
  #the true distribution is closer than a wrong one, more clearly in the tail
  set.seed(3)
  x <- stats::rlnorm(3000, 8, 1.2)
  right <- function(q) stats::plnorm(q, 8, 1.2)
  wrong <- function(q) stats::pgamma(q, shape = 1, scale = mean(x))
  expect_lt(ad_statistic(x, right), ad_statistic(x, wrong))
  expect_lt(ad_statistic(x, right), 3)
  #a cdf that gives a claim no chance gives a large, finite statistic
  expect_true(is.finite(ad_statistic(c(1, 2, 3), function(q) ifelse(q < 2, 0, 1))))
  #the Q-Q points: the model's quantiles are those of its cdf
  q <- dft_qq_data(x, list(LogNormal = right))
  expect_equal(q$fitted$LogNormal, stats::qlnorm(q$p, 8, 1.2), tolerance = 1e-9)
  expect_equal(q$empirical, stats::quantile(x, q$p, names = FALSE))
  expect_true(max(q$p) > 0.999)
  #a Normal's quantiles below zero are found too
  normal <- dft_model_quantiles(function(v) stats::pnorm(v, 0, 1), c(0.01, 0.5, 0.99), c(0.5, 1, 2))
  expect_equal(normal, stats::qnorm(c(0.01, 0.5, 0.99)), tolerance = 1e-9)
})
