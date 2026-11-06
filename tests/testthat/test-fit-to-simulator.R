#Fitted models sent from the distribution fitting tool to the claims simulator

#the simulator's severity for exported settings, with its slices, as a run builds it
exported_severity <- function(values) {
  object <- sev_dist_options[[values$sevDistr]]
  model <- severity_model(object, unlist(values[object@paramIDs]))
  k <- values$pareto_slice_times
  if (k == 0) return(model)
  pareto_splice(model,
                thresholds = unlist(values[paste0("slice_pareto_param_", 2 * seq_len(k))]),
                alphas = unlist(values[paste0("slice_pareto_param_", 2 * seq_len(k) - 1)]))
}

test_that("a fitted frequency is sent with the simulator's parameters and the same moments", {
  expect_identical(dft_simulator_settings(list(model = "Poisson", lambda = 2.5)),
                   list(freqDistr = "Poisson", lamda = 2.5))
  #fitdistrplus's size and mu; the simulator's Poisson-Gamma with shape r and scale beta
  values <- dft_simulator_settings(list(model = "Negative Binomial", size = 1.6, mu = 4))
  expect_identical(values$freqDistr, "Negative_Binomial")
  expect_equal(c(values$r, values$beta), c(1.6, 2.5))
  moments <- freq_dist_options$Negative_Binomial@moments_func(c(values$r, values$beta))
  expect_equal(moments, c(4, sqrt(4 + 4^2 / 1.6)))
  #the same distribution, not just the same moments
  expect_equal(stats::dnbinom(0:30, size = values$r, prob = 1 / (1 + values$beta)),
               stats::dnbinom(0:30, size = 1.6, mu = 4))
})

test_that("each fitted severity is sent as the simulator's same distribution", {
  x <- c(10, 100, 1000, 5000, 20000)
  check <- function(severity, cdf) {
    values <- dft_simulator_settings(severity = severity)
    expect_identical(values$pareto_slice_times, 0L)
    expect_equal(exported_severity(values)$survival(x), 1 - cdf(x), tolerance = 1e-12, label = severity$model)
  }
  check(list(model = "Normal", mean = 3000, sd = 2000), function(q) stats::pnorm(q, 3000, 2000))
  check(list(model = "LogNormal", mu = 7.5, sigma = 1.4), function(q) stats::plnorm(q, 7.5, 1.4))
  check(list(model = "Exponential", mean = 2500), function(q) stats::pexp(q, 1 / 2500))
  check(list(model = "Gamma", shape = 0.8, scale = 4000), function(q) stats::pgamma(q, shape = 0.8, scale = 4000))
  check(list(model = "Pareto", xm = 10, alpha = 1.3), function(q) ifelse(q < 10, 0, 1 - (10 / q)^1.3))
})

test_that("a sliced LogNormal - Pareto - Pareto is sent as the simulator's slices, the same distribution", {
  #the tool's cdf of the model (as the Sliced severity tab draws it)
  body <- c(7, 1.2)
  x1 <- 3000
  x2 <- 20000
  a_mid <- 1.1
  a_top <- 1.8
  p1 <- stats::plnorm(x1, body[1], body[2])
  p2 <- (1 - (x1 / x2)^a_mid) * (1 - p1) + p1
  cdf <- function(q) {
    ifelse(q <= x2, ifelse(q <= x1, stats::plnorm(q, body[1], body[2]), (1 - (x1 / q)^a_mid) * (1 - p1) + p1),
           (1 - (x2 / q)^a_top) * (1 - p2) + p2)
  }
  values <- dft_simulator_settings(severity = list(model = "LogNormal", mu = body[1], sigma = body[2],
                                                    slices = data.frame(x_m = c(x1, x2), alpha = c(a_mid, a_top))))
  expect_identical(values$pareto_slice_times, 2L)
  expect_equal(unlist(values[paste0("slice_pareto_param_", 1:4)], use.names = FALSE), c(a_mid, x1, a_top, x2))
  q <- c(100, 1000, 2999, 3000, 3001, 10000, 19999, 20000, 1e5, 1e7)
  expect_equal(exported_severity(values)$survival(q), 1 - cdf(q), tolerance = 1e-12)
})

test_that("a piecewise Pareto is sent as a Pareto with a slice at each later threshold, the same distribution", {
  t <- c(500, 2000, 8000, 50000)
  alpha <- c(0.9, 1.4, 1.1, 2.2)
  values <- dft_simulator_settings(severity = dft_piecewise_severity(t, alpha))
  expect_identical(values$sevDistr, "Pareto")
  expect_equal(c(values$x_m, values$alpha), c(500, 0.9))
  expect_identical(values$pareto_slice_times, 3L)
  q <- c(500, 1000, 2000, 2001, 7999, 8000, 30000, 50000, 1e6)
  expect_equal(exported_severity(values)$survival(q), 1 - piecewise_pareto_cdf(q, t, alpha), tolerance = 1e-12)
  #one threshold, the smallest claim: a plain Pareto
  expect_identical(dft_simulator_settings(severity = dft_piecewise_severity(500, 1.2))$pareto_slice_times, 0L)
})

test_that("exported settings make a settings file that the simulator loads", {
  values <- dft_simulator_settings(list(model = "Negative Binomial", size = 2, mu = 3),
                                   dft_piecewise_severity(c(100, 1000, 10000), c(1, 1.5, 2)))
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  write_settings_file(values, file, tool = sim_settings_tool, version = sim_settings_version)
  read <- sim_settings_read(file)
  expect_type(read, "list")
  expect_true(sim_settings_validate(read$inputs, read$version))
  expect_equal(read$inputs[names(values)], values)
  #the simulator sets each of them, and clears the slice fields beyond the two slices
  plan <- sim_settings_plan(read$inputs)
  ids <- vapply(plan, `[[`, character(1), "id")
  expect_true(all(names(values) %in% ids))
  later <- plan[ids %in% paste0("slice_pareto_param_", 5:12)]
  expect_length(later, 8)
  expect_true(all(vapply(later, function(e) is.na(e$value), logical(1))))
  #what the file leaves out is not touched
  expect_false(any(c("numberOfSimulations", "reinsuranceStructureEEL", "seedValue") %in% ids))
  #a frequency alone leaves the simulator's severity and slices as they are
  only <- dft_simulator_settings(list(model = "Poisson", lambda = 3))
  expect_false(any(c("sevDistr", "pareto_slice_times") %in% names(only)))
})

test_that("a model the simulator cannot take is refused with a message", {
  expect_error(dft_simulator_settings(list(model = "Poisson", lambda = 0)), "The Poisson lambda is 0")
  expect_error(dft_simulator_settings(list(model = "Negative Binomial", size = NaN, mu = 2)), "size is not a finite number")
  expect_error(dft_simulator_settings(severity = list(model = "Gamma", shape = NA_real_, scale = 2)), "Gamma shape")
  #a layer of the piecewise Pareto with alpha 0 (the simulator's slice alphas must be positive)
  expect_error(dft_simulator_settings(severity = dft_piecewise_severity(c(1, 10, 100), c(1, 0, 2))), "Slice 1 alpha is 0")
  expect_error(dft_simulator_settings(severity = dft_piecewise_severity(1:8, rep(1, 8))), "at most 6 Pareto slices")
  expect_error(dft_simulator_settings(severity = list(model = "Pareto", xm = 1, alpha = 1,
                                                      slices = data.frame(x_m = c(5, 3), alpha = c(1, 1)))), "must increase")
  #the Normal mean may be negative
  expect_identical(dft_simulator_settings(severity = list(model = "Normal", mean = -5, sd = 1))$normal_mean, -5)
})

test_that("the summary of exported settings names them as the simulator does", {
  values <- dft_simulator_settings(list(model = "Negative Binomial", size = 2, mu = 3),
                                   list(model = "LogNormal", mu = 7, sigma = 1.2, slices = data.frame(x_m = 5000, alpha = 1.5)))
  rows <- dft_simulator_settings_rows(values)
  labels <- vapply(rows, `[[`, character(1), 1)
  expect_identical(labels, c("Frequency distribution", "r (shape)", "beta (scale)", "Severity distribution", "mu", "sigma",
                             "Number of Pareto slices", "Slice 1 threshold (x_m)", "Slice 1 alpha"))
  expect_identical(rows[[1]][[2]], "Negative Binomial")
  expect_identical(rows[[8]][[2]], "5,000")
})

test_that("a settings file with a frequency only leaves the simulator's severity truncation as it is", {
  ids <- function(inputs) vapply(sim_settings_plan(inputs), `[[`, character(1), "id")
  expect_false("sevTruncateAtZero" %in% ids(dft_simulator_settings(list(model = "Poisson", lambda = 3))))
  #a file with a severity still switches it off when it lacks it
  plan <- sim_settings_plan(dft_simulator_settings(severity = list(model = "Normal", mean = 5, sd = 1)))
  truncation <- plan[[which(vapply(plan, `[[`, character(1), "id") == "sevTruncateAtZero")]]
  expect_false(truncation$value)
})
