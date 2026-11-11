#The Burr and generalised Pareto distributions (and the Weibull) of the simulator and the
#distribution fitting tool, and the Hill plot

rburr <- function(n, alpha, gamma, scale) burr_tail_quantile(stats::runif(n), alpha, gamma, scale)
rgpd <- function(n, location, scale, shape) gpd_tail_quantile(stats::runif(n), location, scale, shape)

test_that("the Burr functions are those of the distribution", {
  alpha <- 3
  gamma <- 2
  scale <- 1000
  survival <- function(q) exp(burr_log_survival(q, alpha, gamma, scale))
  expect_equal(survival(c(0, 500, 1500)), (1 + (c(0, 500, 1500) / scale)^gamma)^(-alpha))
  expect_equal(stats::integrate(function(q) exp(burr_log_density(q, alpha, gamma, scale)), 0, Inf)$value, 1, tolerance = 1e-7)
  #the density is minus the slope of the survival function
  h <- 1e-3
  expect_equal(exp(burr_log_density(700, alpha, gamma, scale)), (survival(700 - h) - survival(700 + h)) / (2 * h), tolerance = 1e-6)
  #the tail quantile is its inverse, far into the tail too
  s <- c(0.9, 0.5, 0.01, 1e-12)
  expect_equal(survival(burr_tail_quantile(s, alpha, gamma, scale)), s, tolerance = 1e-10)
  #the mean and the standard deviation
  moment <- function(k) stats::integrate(function(q) q^k * exp(burr_log_density(q, alpha, gamma, scale)), 0, Inf, rel.tol = 1e-10)$value
  expect_equal(burr_moments(alpha, gamma, scale), c(moment(1), sqrt(moment(2) - moment(1)^2)), tolerance = 1e-6)
  #a tail index alpha gamma of at most 2 has no standard deviation, and of at most 1 no mean
  expect_equal(burr_moments(1, 1.5, scale)[2], Inf)
  expect_true(is.finite(burr_moments(1, 1.5, scale)[1]))
  expect_equal(burr_moments(0.5, 2, scale), c(Inf, Inf))
  expect_equal(burr_moments(-1, 2, scale), c(NA_real_, NA_real_))
  #the mean claim above v: v plus the integral of the survival function above it, over the survival at v
  for (v in c(0, 800, 20000)) {
    width <- max(v, scale)
    numeric <- v + width * stats::integrate(function(u) survival(v + width * u) / survival(v), 0, Inf, rel.tol = 1e-9)$value
    expect_equal(burr_tail_mean(v, alpha, gamma, scale), numeric, tolerance = 1e-6, label = paste("Burr tail mean at", v))
  }
  expect_equal(burr_tail_mean(0, alpha, gamma, scale), burr_moments(alpha, gamma, scale)[1])
  expect_equal(burr_tail_mean(c(10, 100), 0.5, 2, scale), c(Inf, Inf))
  #alpha = 1 is the log-logistic, and gamma = 1 the Lomax
  expect_equal(exp(burr_log_survival(300, 1, 2.5, 400)), 1 / (1 + (300 / 400)^2.5))
  expect_equal(exp(burr_log_survival(300, 2.5, 1, 400)), (400 / (400 + 300))^2.5)
})

test_that("the generalised Pareto functions are those of the distribution", {
  check <- function(location, scale, shape, upper = Inf) {
    survival <- function(q) exp(gpd_log_survival(q, location, scale, shape))
    q <- location + scale * c(0, 0.5, 2)
    expect_equal(survival(q), if (shape == 0) exp(-(q - location) / scale) else (1 + shape * (q - location) / scale)^(-1 / shape))
    expect_equal(survival(location - 10), 1)
    expect_equal(stats::integrate(function(q) exp(gpd_log_density(q, location, scale, shape)), location, upper)$value, 1,
                 tolerance = 1e-6)
    s <- c(0.9, 0.5, 0.01, 1e-10)
    expect_equal(survival(gpd_tail_quantile(s, location, scale, shape)), s, tolerance = 1e-9)
    moment <- function(k) {
      stats::integrate(function(q) q^k * exp(gpd_log_density(q, location, scale, shape)), location, upper, rel.tol = 1e-10)$value
    }
    expect_equal(gpd_moments(location, scale, shape), c(moment(1), sqrt(moment(2) - moment(1)^2)), tolerance = 1e-5)
    v <- location + 3 * scale
    if (v < upper) {
      numeric <- stats::integrate(function(q) q * exp(gpd_log_density(q, location, scale, shape)), v, upper, rel.tol = 1e-10)$value /
        survival(v)
      expect_equal(gpd_tail_mean(v, location, scale, shape), numeric, tolerance = 1e-5)
    }
    #below the location, the mean claim is the mean
    expect_equal(gpd_tail_mean(0, location, scale, shape), gpd_moments(location, scale, shape)[1])
  }
  check(500, 1000, 0.2)
  check(0, 250, 0)
  #a negative shape: the tail ends at location - scale / shape
  check(100, 1000, -0.25, upper = 100 + 1000 / 0.25)
  expect_equal(exp(gpd_log_survival(5000, 100, 1000, -0.25)), 0)
  expect_equal(gpd_log_density(5000, 100, 1000, -0.25), -Inf)
  #a shape of 1/2 or more has no standard deviation, and of 1 or more no mean
  expect_equal(gpd_moments(0, 100, 0.5)[2], Inf)
  expect_equal(gpd_moments(0, 100, 1), c(Inf, Inf))
  expect_equal(gpd_tail_mean(500, 0, 100, 1.2), Inf)
  #a Pareto from its minimum is a generalised Pareto: location x_m, scale x_m / alpha, shape 1 / alpha
  expect_equal(exp(gpd_log_survival(3000, 1000, 1000 / 1.5, 1 / 1.5)), (1000 / 3000)^1.5)
})

test_that("the Burr fit is the maximum likelihood fit, with a threshold and policy limits too", {
  set.seed(1)
  x <- rburr(4000, 2, 1.5, 10000)
  fit <- dft_fit_burr(x)
  nll <- function(p) -sum(burr_log_density(x, exp(p[1]), exp(p[2]), exp(p[3])))
  direct <- stats::optim(c(0, 0, log(5000)), nll, control = list(reltol = 1e-13, maxit = 5000))
  expect_equal(unname(fit$estimate), exp(direct$par), tolerance = 1e-3)
  expect_equal(fit$loglik, -direct$value, tolerance = 1e-8)
  expect_named(fit$estimate, c("alpha", "gamma", "scale"))
  #no small change of the parameters has a higher likelihood
  for (i in 1:3) for (step in c(-0.01, 0.01)) {
    p <- log(fit$estimate)
    p[i] <- p[i] + step
    expect_lt(-nll(p), fit$loglik)
  }
  #above a threshold, each claim's density over the fitted probability of exceeding it
  d <- 5000
  above <- x[x > d]
  truncated <- dft_fit_burr(above, d)
  expect_equal(truncated$loglik,
               sum(burr_log_density(above, truncated$estimate[1], truncated$estimate[2], truncated$estimate[3])) -
                 length(above) * burr_log_survival(d, truncated$estimate[1], truncated$estimate[2], truncated$estimate[3]),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(unname(truncated$estimate), c(2, 1.5, 10000), tolerance = 0.35)
  #at a policy limit, the probability of exceeding it
  limit <- 40000
  capped <- dft_fit_burr(pmin(x, limit), 0, x >= limit)
  expect_equal(unname(capped$estimate), unname(fit$estimate), tolerance = 0.1)
  e <- capped$estimate
  expect_equal(capped$loglik, sum(burr_log_density(x[x < limit], e[1], e[2], e[3])) +
                 sum(x >= limit) * burr_log_survival(limit, e[1], e[2], e[3]), tolerance = 1e-10, ignore_attr = TRUE)
  #ignoring the limits gives a lighter tail
  ignored <- dft_fit_burr(pmin(x, limit))$estimate
  expect_gt(ignored[["alpha"]] * ignored[["gamma"]], e[["alpha"]] * e[["gamma"]])
})

test_that("a Burr fit at the edge of the family is refused", {
  set.seed(2)
  #half-Normal claims: the likelihood rises all the way to the Weibull, alpha without bound
  expect_error(dft_fit_burr(abs(stats::rnorm(3000, 0, 1000))), "lighter tail than any Burr")
  #Pareto claims: gamma without bound
  expect_error(dft_fit_burr(1000 / stats::runif(3000)^(1 / 1.5)), "runs to a Pareto")
  expect_error(dft_fit_burr(c(10, 20, 30), 15), "above the threshold")
  expect_error(dft_fit_burr(c(10, 10, 10)), "two different claims")
  expect_error(dft_fit_burr(c(10, 20, 30), 0, c(TRUE, TRUE, FALSE)), "two different claims")
})

test_that("the generalised Pareto fit is the maximum likelihood fit, from a threshold and with policy limits", {
  set.seed(3)
  x <- rgpd(4000, 0, 1000, 0.4)
  fit <- dft_fit_gpd(x)
  nll <- function(p) -sum(gpd_log_density(x, 0, exp(p[1]), p[2]))
  direct <- stats::optim(c(log(500), 0.1), nll, control = list(reltol = 1e-13, maxit = 5000))
  expect_equal(unname(fit$estimate), c(exp(direct$par[1]), direct$par[2]), tolerance = 1e-4)
  expect_equal(fit$loglik, -direct$value, tolerance = 1e-9)
  expect_equal(unname(fit$estimate), c(1000, 0.4), tolerance = 0.12)
  #the excesses over a threshold: the same fit as of the excesses from zero
  shifted <- dft_fit_gpd(x + 2500, 2500)
  expect_equal(shifted$estimate, fit$estimate, tolerance = 1e-5)
  expect_equal(shifted$loglik, fit$loglik, tolerance = 1e-9)
  #Pareto claims above their minimum: shape 1 / alpha and scale x_m / alpha
  pareto <- dft_fit_gpd(1000 / stats::runif(4000)^(1 / 1.5), 1000)
  expect_equal(unname(pareto$estimate), c(1000 / 1.5, 1 / 1.5), tolerance = 0.1)
  #claims at their policy limits
  limit <- 8000
  capped <- dft_fit_gpd(pmin(x, limit), 0, x >= limit)
  expect_equal(unname(capped$estimate), unname(fit$estimate), tolerance = 0.1)
  expect_lt(dft_fit_gpd(pmin(x, limit))$estimate[["shape"]], capped$estimate[["shape"]])
  #a tail that ends: a negative shape, kept at -1/2 or above
  expect_equal(dft_fit_gpd(rgpd(4000, 0, 1000, -0.25))$estimate[["shape"]], -0.25, tolerance = 0.15)
  expect_equal(dft_fit_gpd(stats::runif(2000, 0, 1000))$estimate[["shape"]], -0.5, tolerance = 1e-6)
  expect_error(dft_fit_gpd(c(10, 20, 30), 15), "above the threshold")
})

test_that("the Severity tab's Burr and generalised Pareto have the form of its other models", {
  set.seed(4)
  d <- 5000
  x <- rburr(6000, 2, 1.5, 10000)
  above <- x[x > d]
  models <- dft_heavy_tail_models(above, d)
  expect_identical(vapply(models, `[[`, character(1), "name"), c("Burr", "Generalised Pareto"))
  expect_length(attr(models, "failed"), 0)
  burr <- models[[1]]
  gpd <- models[[2]]
  expect_named(burr$params, c("Scale", "alpha", "gamma"))
  expect_named(gpd$params, c("Location", "Scale", "Shape"))
  expect_identical(c(burr$k, gpd$k), c(3, 2))
  #the Burr is of the claims from zero, and the generalised Pareto of those above the threshold
  #(the share of the claims above the threshold is known from their shape alone, so loosely)
  expect_equal(burr$survival_at_threshold, mean(x > d), tolerance = 0.3)
  expect_identical(gpd$survival_at_threshold, 1)
  expect_identical(gpd$params[["Location"]], d)
  #the cdfs are of a claim above the threshold, and the means those of the claims
  for (m in models) {
    expect_equal(m$cdf(c(d - 1, d)), c(0, 0), label = m$name)
    expect_equal(m$cdf(1e12), 1, tolerance = 1e-6, label = m$name)
    expect_equal(m$cdf(stats::quantile(above, c(0.25, 0.5, 0.9), names = FALSE)), c(0.25, 0.5, 0.9), tolerance = 0.03, label = m$name)
    expect_equal(m$mean, mean(above), tolerance = 0.03, label = m$name)
  }
  p <- burr$params
  ground <- function(q) 1 - (1 + (q / p[["Scale"]])^p[["gamma"]])^(-p[["alpha"]])
  expect_equal(burr$cdf(c(6000, 30000)), (ground(c(6000, 30000)) - ground(d)) / (1 - ground(d)), tolerance = 1e-9)
  #they are the last two models of the fits above a threshold, and their tail means those of the distributions
  all_models <- dft_truncated_severity_models(above, d)
  expect_equal(all_models[[7]]$params, burr$params)
  expect_equal(all_models[[8]]$loglik, gpd$loglik)
  expect_equal(dft_tail_mean("Burr", burr$params)(d), burr$mean)
  expect_equal(dft_tail_mean("Generalised Pareto", gpd$params)(20000), gpd_tail_mean(20000, d, gpd$params[["Scale"]], gpd$params[["Shape"]]))

  #without a threshold the generalised Pareto starts at zero; a Burr at the edge of its family is a failed fit
  half_normal <- abs(stats::rnorm(3000, 0, 1000))
  light <- dft_heavy_tail_models(half_normal)
  expect_identical(attr(light, "failed"), "Burr fit failed: the claims have a lighter tail than any Burr, whose fit runs to a Weibull")
  expect_true(all(is.na(light[[1]]$params)))
  expect_null(light[[1]]$cdf)
  expect_named(light[[1]]$params, c("Scale", "alpha", "gamma"))
  expect_identical(light[[2]]$params[["Location"]], 0)
  expect_equal(light[[2]]$mean, 1000 * sqrt(2 / pi), tolerance = 0.06)
  expect_lt(light[[2]]$params[["Shape"]], 0)
  #the note under the table repeats the message
  expect_identical(dft_failed_fits_note(attr(light, "failed")),
                   " Burr fit failed: the claims have a lighter tail than any Burr, whose fit runs to a Weibull.")
  #with policy limits, the message says so
  capped <- dft_heavy_tail_models(pmin(half_normal, 1500), 0, half_normal >= 1500)
  expect_match(attr(capped, "failed"), "^Burr fit with the policy limits failed: ")
})

test_that("the Hill plot's estimates are the tail index of the largest claims", {
  set.seed(5)
  x <- 1000 / stats::runif(5000)^(1 / 1.5)
  h <- dft_hill_data(x)
  expect_named(h, c("k", "alpha", "lower", "upper", "threshold"))
  expect_lte(length(h$k), 400)
  expect_identical(range(h$k), c(5L, 2500L))
  #Pareto claims: the index at every k, within its band
  expect_equal(h$alpha[h$k > 200], rep(1.5, sum(h$k > 200)), tolerance = 0.15)
  expect_true(mean(h$lower <= 1.5 & h$upper >= 1.5) > 0.9)
  #each estimate is the Pareto alpha of the k largest claims over the next one
  sorted <- sort(x, decreasing = TRUE)
  for (i in c(1, 50, length(h$k))) {
    k <- h$k[i]
    expect_equal(h$alpha[i], k / sum(log(sorted[1:k] / sorted[k + 1])))
    expect_equal(h$threshold[i], sorted[k + 1])
    expect_equal(c(h$lower[i], h$upper[i]), h$alpha[i] * (1 + c(-1.96, 1.96) / sqrt(k)))
  }
  #LogNormal claims have no tail index: the estimate keeps rising with the number of claims used
  set.seed(6)
  lognormal <- dft_hill_data(stats::rlnorm(5000, 8, 1.2))
  expect_gt(lognormal$alpha[which.min(abs(lognormal$k - 50))], lognormal$alpha[lognormal$k == 2500] * 1.5)
  #few claims: every k up to all but one of them
  expect_identical(dft_hill_data(c(1, 2, 4, 8, 16, 32, 64, 128))$k, 5:6)
  expect_error(dft_hill_data(1:6), "at least 7 claims")
  expect_error(dft_hill_data(rep(5, 20)), "same size")
})

test_that("the simulator draws Weibull, Burr and generalised Pareto claims", {
  expect_true(all(c("Weibull", "Burr", "Generalised_Pareto") %in% names(sev_dist_options)))
  #every parameter id of the severity distributions is its own
  ids <- unlist(lapply(sev_dist_options, function(o) o@paramIDs))
  expect_identical(anyDuplicated(ids), 0L)
  expect_identical(nrow(sev_dist_parameter_placeholders), 3L)

  cases <- list(
    list(id = "Weibull", p = c(0.8, 1000), survival = function(q) stats::pweibull(q, 0.8, 1000, lower.tail = FALSE)),
    list(id = "Burr", p = c(3, 2, 1000), survival = function(q) (1 + (q / 1000)^2)^(-3)),
    list(id = "Generalised_Pareto", p = c(500, 1000, 0.2), survival = function(q) ifelse(q < 500, 1, (1 + 0.2 * (q - 500) / 1000)^(-5)))
  )
  set.seed(7)
  for (case in cases) {
    object <- sev_dist_options[[case$id]]
    expect_length(distribution_param_problems(object, case$p), 0)
    q <- c(200, 800, 3000)
    expect_equal(object@survival_func(q, case$p), case$survival(q), label = case$id)
    expect_equal(object@survival_func(object@tail_quantile_func(c(0.7, 0.01), case$p), case$p), c(0.7, 0.01), label = case$id)
    draws <- object@simulate_func(2e5, case$p)
    moments <- distribution_moments(sev_dist_options, case$id, case$p)
    expect_equal(mean(draws), moments[[1]], tolerance = 0.01, label = case$id)
    expect_equal(stats::sd(draws), moments[[2]], tolerance = 0.03, label = case$id)
    expect_equal(mean(draws > 800), case$survival(800), tolerance = 0.02, label = case$id)
  }
  #the Weibull's moments, and where the Gamma function overflows
  expect_equal(weibull_moments(1, 500), c(500, 500))
  expect_equal(weibull_moments(2, 500), 500 * c(gamma(1.5), sqrt(1 - gamma(1.5)^2)))
  expect_equal(weibull_moments(0.001, 500), c(Inf, Inf))
  #a Burr without a mean, as a Pareto with alpha at most 1
  expect_equal(unname(distribution_moments(sev_dist_options, "Burr", c(0.5, 1.5, 1000))), c(Inf, Inf))
  #parameters out of range
  expect_match(distribution_param_problems(sev_dist_options$Burr, c(0, 2, 1000)), "alpha")
  expect_match(distribution_param_problems(sev_dist_options$Generalised_Pareto, c(-1, 1000, 0.2)), "Location")
  expect_match(distribution_param_problems(sev_dist_options$Generalised_Pareto, c(0, 0, 0.2)), "Scale")
  expect_length(distribution_param_problems(sev_dist_options$Generalised_Pareto, c(0, 1000, -0.3)), 0)
})

test_that("simulate_claims() takes the new severities by name, with layers and slices", {
  set.seed(8)
  burr <- simulate_claims(4e4, "Poisson", 5, "burr", c(alpha = 3, gamma = 2, scale = 1000))
  expect_equal(mean(burr$total_claims), 5 * burr_moments(3, 2, 1000)[1], tolerance = 0.02)
  #named parameters in another order
  expect_error(simulate_claims(100, "Poisson", 5, "Burr", c(3, 2)), "Burr")
  #a layer of 5,000 excess of 2,000 of generalised Pareto claims: the integral of the survival function over it
  layered <- simulate_claims(4e4, "Poisson", 5, "Generalised Pareto", c(location = 500, scale = 1000, shape = 0.2),
                             eel_layer = "limited", eel_deductible = 2000, eel_limit = 5000)
  expected <- 5 * stats::integrate(function(q) exp(gpd_log_survival(q, 500, 1000, 0.2)), 2000, 7000)$value
  expect_equal(mean(layered$total_claims), expected, tolerance = 0.03)
  #a Weibull body with a Pareto slice above 5,000
  sliced <- simulate_claims(4e4, "Poisson", 5, "Weibull", c(0.8, 1000), pareto_thresholds = 5000, pareto_alphas = 2.5,
                            severity_cap = 50000)
  body <- function(q) stats::pweibull(q, 0.8, 1000, lower.tail = FALSE)
  mean_claim <- stats::integrate(body, 0, 5000)$value +
    body(5000) * stats::integrate(function(q) (5000 / q)^2.5, 5000, 50000)$value
  expect_equal(mean(sliced$total_claims), 5 * mean_claim, tolerance = 0.03)
})

test_that("the fitted Weibull, Burr and generalised Pareto are sent as the simulator's same distributions", {
  x <- c(10, 100, 1000, 5000, 20000)
  check <- function(severity, cdf) {
    values <- dft_simulator_settings(severity = severity)
    object <- sev_dist_options[[values$sevDistr]]
    expect_true(all(object@paramIDs %in% names(values)))
    model <- severity_model(object, unlist(values[object@paramIDs]))
    expect_equal(model$survival(x), 1 - cdf(x), tolerance = 1e-12, label = severity$model)
    expect_identical(values$pareto_slice_times, 0L)
  }
  check(list(model = "Weibull", shape = 0.7, scale = 2000), function(q) stats::pweibull(q, 0.7, 2000))
  check(list(model = "Burr", alpha = 2, gamma = 1.5, scale = 4000), function(q) 1 - (1 + (q / 4000)^1.5)^(-2))
  check(list(model = "Generalised Pareto", location = 50, scale = 800, shape = 0.3),
        function(q) ifelse(q < 50, 0, 1 - (1 + 0.3 * (q - 50) / 800)^(-1 / 0.3)))
  #a negative shape and a location of zero are sent; a negative location is not
  expect_identical(dft_simulator_settings(severity = list(model = "Generalised Pareto", location = 0, scale = 800,
                                                           shape = -0.2))$gpd_shape, -0.2)
  expect_error(dft_simulator_settings(severity = list(model = "Generalised Pareto", location = -5, scale = 800, shape = 0.1)),
               "location is negative")
  expect_error(dft_simulator_settings(severity = list(model = "Burr", alpha = NA_real_, gamma = 1.5, scale = 4000)), "Burr alpha")
  #the summary names the settings as the simulator does
  rows <- dft_simulator_settings_rows(dft_simulator_settings(severity = list(model = "Generalised Pareto", location = 50,
                                                                             scale = 800, shape = 0.3)))
  expect_identical(vapply(rows, `[[`, character(1), 1),
                   c("Severity distribution", "Location (threshold)", "Scale", "Shape (xi)", "Number of Pareto slices"))
  expect_identical(rows[[1]][[2]], "Generalised Pareto")
  #the simulator loads the file
  plan <- sim_settings_plan(dft_simulator_settings(severity = list(model = "Burr", alpha = 2, gamma = 1.5, scale = 4000)))
  expect_true(all(c("burr_alpha", "burr_gamma", "burr_scale") %in% vapply(plan, `[[`, character(1), "id")))
})
