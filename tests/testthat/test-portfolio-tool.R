#the portfolio capital tool

test_that("the tool runs the portfolio of its lines, with one correlation or a matrix", {
  skip_server_on_cran()
  testServer(portfolio_tool_Server, {
    session$setInputs(line_source = "model", n_lines = 3, copula = "gaussian", corr_mode = "all", corr_all = 0.3,
                      n_sims = 5000, level = "0.995", seed = 1)
    expect_identical(names(model_lines()), c("Motor", "Property", "Liability"))
    expect_match(output$lines_table$html, "Negative Binomial, mean 8")
    session$setInputs(run = 1)
    p <- current()$portfolio
    #the same as simulate_portfolio() with the same lines
    direct <- simulate_portfolio(lapply(model_lines(), function(l) l[c("frequency", "frequency_params", "severity",
                                                                       "severity_params")]),
                                 correlation = 0.3, n = 5000, seed = 1)
    expect_identical(p$losses, direct$losses)
    expect_match(output$capital_stats$html, netsimr_amount(p$summary$var[4]), fixed = TRUE)
    expect_match(output$capital_table$html, "Liability")
    expect_false(stale())
    #a matrix, and the run is then out of date until run again
    session$setInputs(corr_mode = "matrix", corr_1_2 = 0.5, corr_1_3 = 0.2, corr_2_3 = 0.1)
    expect_equal(unname(correlation()), matrix(c(1, 0.5, 0.2, 0.5, 1, 0.1, 0.2, 0.1, 1), 3))
    expect_true(stale())
    expect_match(output$capital_stats$html, "have changed")
    #an impossible matrix is reported before a run
    session$setInputs(corr_1_2 = 0.9, corr_1_3 = 0.9, corr_2_3 = -0.9)
    expect_match(output$dependence_stats$html, "Not valid")
    session$setInputs(corr_2_3 = 0.8, copula = "t", copula_df = 3, run = 2)
    expect_identical(current()$portfolio$copula, "t")
    #the downloads
    summary <- utils::read.csv(output$download_summary)
    expect_identical(summary$line, c("Motor", "Property", "Liability", "total"))
    losses <- utils::read.csv(output$download_losses, check.names = FALSE)
    expect_equal(nrow(losses), 5000)
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    expect_match(html, "Portfolio Capital Report", fixed = TRUE)
    expect_match(html, "t, 3 degrees of freedom", fixed = TRUE)
  })
})

test_that("the lines can be uploaded simulated losses, and the settings file restores the lines", {
  skip_server_on_cran()
  set.seed(5)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(simulation = 1:2000, motor = stats::rgamma(2000, 4, 1e-4), fire = stats::rlnorm(2000, 11, 1)),
                   path, row.names = FALSE)
  testServer(portfolio_tool_Server, {
    session$setInputs(line_source = "upload", upload_file = list(datapath = path, name = "sims.csv"), upload_header = TRUE,
                      upload_sep = ",", upload_dec = ".", corr_mode = "all", corr_all = 0.2, copula = "gaussian",
                      level = "0.99", seed = 2)
    session$setInputs(upload_columns = c("motor", "fire"))
    expect_identical(names(upload_lines()), c("motor", "fire"))
    session$setInputs(run = 1)
    p <- current()$portfolio
    expect_equal(p$n, 2000)
    expect_equal(sort(p$losses$fire), sort(utils::read.csv(path)$fire))
    expect_equal(p$level, 0.99)
  })
  testServer(portfolio_tool_Server, {
    session$setInputs(line_source = "model", n_lines = 2, line_name_1 = "Home", line_frequency_1 = "Poisson", line_f1_1 = 12,
                      line_severity_1 = "Gamma", line_s1_1 = 2, line_s2_1 = 3000, corr_mode = "all", corr_all = 0.4)
    saved <- output$save_settings
    settings <- read_settings_file(saved, pfo_settings_tool)$values
    expect_identical(settings$line_name, c("Home", "Property"))
    expect_equal(settings$line_s2, c(3000, 20000))
    session$setInputs(load_settings = list(datapath = saved, name = "portfolio_settings.txt"))
    expect_identical(loaded_lines()$severity, c("Gamma", "Pareto"))
    expect_equal(loaded_lines()$s1, c(2, 1.8))
  })
})

test_that("the correlations above the diagonal make a symmetric matrix, by row", {
  R <- pfo_correlation_matrix(4, c(0.12, 0.13, 0.14, 0.23, 0.24, 0.34))
  expect_equal(R[1, 4], 0.14)
  expect_equal(R[2, 3], 0.23)
  expect_equal(R[4, 3], 0.34)
  expect_equal(R, t(R))
  expect_equal(diag(R), rep(1, 4))
  expect_equal(pfo_correlation_matrix(3, 0.3)[3, 1], 0.3)
  expect_error(pfo_upload_lines(data.frame(a = 1:200, b = c(NA, 2:200)), c("a", "b")), "'b' has")
  expect_error(pfo_upload_lines(data.frame(a = 1:200), "a"), "at least two columns")
})
