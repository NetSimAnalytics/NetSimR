#fitted models sent from the distribution fitting tool to the reinsurance pricing tool's Programmes
#tab: the simulator settings file NetDisFit's Export tab saves, read as the gross portfolio

test_that("compare_reinsurance() takes Pareto slices of the claim size", {
  # above the threshold the claims are a Pareto: a layer from there cedes their mean excess
  threshold <- 1e5
  alpha <- 4
  layer <- list(XL = list(xl = list(deductible = threshold, limit = 1e12)))
  cmp <- compare_reinsurance("Poisson", 50, "LogNormal", c(10, 1), layer, premium = 5e6, n = 20000, seed = 3,
                             pareto_thresholds = threshold, pareto_alphas = alpha)
  above <- stats::plnorm(threshold, 10, 1, lower.tail = FALSE)
  expect_equal(cmp$summary$expected_ceded[2], 50 * above * threshold / (alpha - 1), tolerance = 0.03)
  # the claims below the threshold are the LogNormal's: the same years without the tail's excess
  plain <- compare_reinsurance("Poisson", 50, "LogNormal", c(10, 1), layer, premium = 5e6, n = 20000, seed = 3)
  retained <- function(x) mean(x$gross) - x$summary$expected_ceded[2]
  expect_equal(retained(cmp), retained(plain))
  # two slices: a heavier tail above the second threshold cedes more to a layer there
  top <- list(XL = list(xl = list(deductible = 1e6, limit = 1e12)))
  two <- function(alphas) {
    compare_reinsurance("Poisson", 50, "LogNormal", c(10, 1), top, premium = 5e6, n = 20000, seed = 3,
                        pareto_thresholds = c(1e5, 1e6), pareto_alphas = alphas)$summary$expected_ceded[2]
  }
  expect_gt(two(c(2.5, 1.5)), 1.5 * two(c(2.5, 3)))
  # the same seed gives the same years
  expect_identical(two(c(2.5, 1.5)), two(c(2.5, 1.5)))

  bad <- function(thresholds, alphas, message) {
    expect_error(compare_reinsurance("Poisson", 10, "Gamma", c(2, 1000), list(A = list(quota_share = 0.2)), premium = 1e5,
                                     n = 500, pareto_thresholds = thresholds, pareto_alphas = alphas), message)
  }
  bad(c(1e4, 1e5), 2, "one alpha per threshold")
  bad(c(1e5, 1e4), c(2, 2), "increasing claim sizes above 0")
  bad(0, 2, "increasing claim sizes above 0")
  bad(1e4, -1, "pareto_alphas must be above 0")
  bad(1:7 * 1e4, rep(2, 7), "At most 6 Pareto slices")
})

test_that("every model the distribution fitting tool exports is read as the gross portfolio", {
  frequencies <- list(
    list(model = "Poisson", lambda = 2.5),
    list(model = "Negative Binomial", size = 1.6, mu = 4),
    list(model = "Zero-inflated Poisson", lambda = 3, zero = 0.2),
    list(model = "Zero-inflated Negative Binomial", size = 2, mu = 5, zero = 0.1)
  )
  severities <- list(
    list(model = "Normal", mean = 5000, sd = 800),
    list(model = "LogNormal", mu = 8, sigma = 1.2),
    list(model = "Exponential", mean = 4000),
    list(model = "Gamma", shape = 0.8, scale = 5000),
    list(model = "Pareto", xm = 1000, alpha = 1.8),
    list(model = "Weibull", shape = 0.7, scale = 4000),
    list(model = "Burr", alpha = 2, gamma = 1.3, scale = 6000),
    list(model = "Generalised Pareto", location = 0, scale = 3000, shape = 0.3),
    # a sliced model, and a piecewise Pareto
    list(model = "LogNormal", mu = 8, sigma = 1.2, slices = data.frame(x_m = c(2e4, 1e5), alpha = c(1.5, 2.2))),
    dft_piecewise_severity(c(1000, 5000, 20000), c(1.2, 1.6, 2.1))
  )
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path))
  through_file <- function(frequency, severity) {
    write_settings_file(dft_simulator_settings(frequency, severity), path, sim_settings_tool, sim_settings_version)
    xlp_fit_from_settings(read_settings_file(path, sim_settings_tool)$values)
  }
  programmes <- list(QS = list(quota_share = 0.5))
  for (f in frequencies) {
    fit <- through_file(f, NULL)
    expect_null(fit$severity)
    expect_identical(fit$notes, character(0))
    expect_true(fit$frequency$name %in% names(xlp_prog_frequencies))
    expect_length(fit$frequency$params, length(xlp_prog_frequencies[[fit$frequency$name]]$params))
    # the parameters are those of simulate_claims(), in its order
    expect_s3_class(compare_reinsurance(fit$frequency$name, fit$frequency$params, "Exponential", 1e-3, programmes,
                                        premium = 1e4, n = 200, seed = 1), "netsimr_programmes")
  }
  for (s in severities) {
    fit <- through_file(NULL, s)
    expect_null(fit$frequency)
    expect_identical(fit$notes, character(0))
    expect_true(fit$severity$name %in% names(xlp_prog_severities))
    expect_length(fit$severity$params, length(xlp_prog_severities[[fit$severity$name]]$params))
    expect_identical(is.null(fit$slices), is.null(s$slices))
    if (!is.null(s$slices)) expect_equal(fit$slices, data.frame(threshold = s$slices$x_m, alpha = s$slices$alpha))
    cmp <- compare_reinsurance("Poisson", 3, fit$severity$name, fit$severity$params, programmes, premium = 1e5, n = 200,
                               seed = 1, pareto_thresholds = fit$slices$threshold, pareto_alphas = fit$slices$alpha)
    expect_true(all(is.finite(cmp$gross)))
  }
  # the parameters keep their meaning: the Negative Binomial's mean, the Exponential's rate, the Pareto's order
  expect_equal(prod(through_file(frequencies[[2]], NULL)$frequency$params), 4)
  expect_equal(through_file(NULL, severities[[3]])$severity$params, 1 / 4000)
  expect_equal(through_file(NULL, severities[[5]])$severity$params, c(1.8, 1000))
  expect_equal(through_file(NULL, severities[[8]])$severity$params, c(0, 3000, 0.3))

  # a simulator's file with what the tab does not take: left out, with the reason
  fit <- xlp_fit_from_settings(list(freqDistr = "Binomial", n = 10, p = 0.2, sevDistr = "Fixed_Severity", Fixed_sev_amount = 100))
  expect_null(fit$frequency)
  expect_null(fit$severity)
  expect_match(fit$notes, "no Binomial claim count|no Fixed Severity claim size")
  fit <- xlp_fit_from_settings(list(sevDistr = "LogNormal", mu = 8, sigma = 1, sevCapBinary = TRUE, sev_cap_amount = 1e6,
                                    pareto_slice_times = 2, slice_pareto_param_1 = 2, slice_pareto_param_2 = 1e5,
                                    slice_pareto_param_3 = 2, slice_pareto_param_4 = 5e4))
  expect_identical(fit$severity$name, "LogNormal")
  expect_null(fit$slices)
  expect_match(fit$notes, "slices of the file are not usable|severity cap of the file is not applied")
  expect_match(xlp_fit_from_settings(list(sevDistr = "LogNormal", mu = 8))$notes, "every parameter of the LogNormal claim size")
})

test_that("the reinsurance pricing tool loads a fitted model and compares its programmes on it", {
  skip_server_on_cran()
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path))
  values <- dft_simulator_settings(
    list(model = "Zero-inflated Poisson", lambda = 150, zero = 0.1),
    list(model = "LogNormal", mu = 11, sigma = 1.4, slices = data.frame(x_m = c(5e5, 2e6), alpha = c(1.6, 2.4)))
  )
  write_settings_file(values, path, sim_settings_tool, sim_settings_version)
  testServer(reinsurance_pricing_tool_Server, {
    session$setInputs(deductible = 1e6, limit = 4e6, reinstatements = 1, reinstatement_rate = 100, aggregate_deductible = 0,
                      rating_year = 2025, rating_premium = 6e7, loading = 25,
                      prog_frequency = "Poisson", prog_severity = "Gamma", prog_f1 = 200, prog_s1 = 0.5, prog_s2 = 4e5,
                      prog_n = 2000, prog_level = "0.995", prog_loading = 25, prog_coc = 10, prog_seed = 1)
    session$setInputs(prog_fit = list(datapath = path, name = "fitted.txt"))
    expect_equal(prog_slices(), data.frame(threshold = c(5e5, 2e6), alpha = c(1.6, 2.4)))
    expect_match(as.character(output$prog_tail$html), "alpha 1.6 above 500,000; alpha 2.4 above 2,000,000", fixed = TRUE)
    # (the page sets the inputs the server updates)
    expect_identical(own_updates$waiting("prog_frequency"), "Zero-inflated Poisson")
    expect_identical(own_updates$waiting("prog_severity"), "LogNormal")
    session$setInputs(prog_frequency = "Zero-inflated Poisson", prog_f1 = 150, prog_f2 = 0.1, prog_severity = "LogNormal",
                      prog_s1 = 11, prog_s2 = 1.4, run_programmes = 1)
    expect_length(c(own_updates$waiting("prog_frequency"), own_updates$waiting("prog_severity")), 0)
    cmp <- current_programmes()
    direct <- compare_reinsurance("Zero_Inflated_Poisson", c(150, 0.1), "LogNormal", c(11, 1.4), programme_list(),
                                  premium = 6e7, n = 2000, seed = 1, pareto_thresholds = c(5e5, 2e6), pareto_alphas = c(1.6, 2.4))
    expect_equal(cmp$summary, direct$summary)
    expect_identical(cmp$portfolio$frequency, "Zero-inflated Poisson (150, 0.1)")
    # the report names the portfolio and its tail
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    expect_match(html, "Zero-inflated Poisson (150, 0.1)", fixed = TRUE)
    expect_match(html, "alpha 1.6 above 500,000; alpha 2.4 above 2,000,000", fixed = TRUE)

    # the tool's own settings file keeps the portfolio with its tail
    saved <- read_settings_file(output$save_settings, xlp_settings_tool)$values
    expect_identical(saved$prog_frequency, "Zero-inflated Poisson")
    expect_equal(saved$prog_tail_thresholds, c(5e5, 2e6))
    expect_equal(saved$prog_tail_alphas, c(1.6, 2.4))
    # the tail is removed, and comes back with the tool's settings
    session$setInputs(prog_tail_remove = 1)
    expect_null(prog_slices())
    own <- tempfile(fileext = ".txt")
    write_settings_file(saved, own, xlp_settings_tool, xlp_settings_version)
    session$setInputs(load_settings = list(datapath = own, name = "own.txt"))
    expect_equal(prog_slices(), data.frame(threshold = c(5e5, 2e6), alpha = c(1.6, 2.4)))
    # a settings file without a tail has none
    write_settings_file(saved[setdiff(names(saved), c("prog_tail_thresholds", "prog_tail_alphas"))], own, xlp_settings_tool,
                        xlp_settings_version)
    session$setInputs(load_settings = list(datapath = own, name = "own.txt"))
    expect_null(prog_slices())
    unlink(own)

    # an extra zero probability of 1 or more is refused
    session$setInputs(prog_f2 = 1, run_programmes = 2)
    expect_error(current_programmes(), "extra zero must be at least 0 and below 1")
    # another tool's file is refused, and the portfolio stays
    write_settings_file(list(deductible = 1), path, xlp_settings_tool, xlp_settings_version)
    session$setInputs(prog_fit = list(datapath = path, name = "other.txt"))
    expect_identical(input$prog_frequency, "Zero-inflated Poisson")
  })
})
