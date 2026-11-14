#the tools' examples, Excel files, reports, IFRS 17 and standard formula tabs, and programmes

test_that("the example data sets have the columns the tools use", {
  expect_identical(names(motor_policies), c("policy_id", "age", "region", "vehicle", "vehicle_value", "exposure",
                                            "claim_count", "claim_cost"))
  expect_identical(nrow(motor_policies), 10000L)
  expect_true(all(motor_policies$claim_cost[motor_policies$claim_count == 0] == 0))
  expect_true(all(liability_claims$amount <= liability_claims$policy_limit))
  expect_gt(sum(liability_claims$amount == liability_claims$policy_limit), 100)
})

test_that("Excel workbooks are read like text files, and written one sheet per table", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(first = data.frame(a = 1:3, `b c` = c("x", "y", "z"), check.names = FALSE),
                           second = data.frame(z = 1)), path)
  # Shiny keeps the extension, but the file is recognised by its first bytes
  plain <- tempfile()
  file.copy(path, plain)
  d <- netsimr_read_data(plain)
  expect_identical(names(d), c("a", "b c"))
  expect_equal(d$a, 1:3)
  expect_identical(d$`b c`, c("x", "y", "z"))
  out <- tempfile(fileext = ".xlsx")
  netsimr_write_excel(list(`One/two: three` = data.frame(x = factor("a")), empty = data.frame()), out)
  expect_identical(readxl::excel_sheets(out), "One two  three")
  expect_identical(readxl::read_excel(out)$x, "a")
})

test_that("the GLM tool's example fits at one click, and its report and rate workbook download", {
  skip_server_on_cran()
  testServer(GLMFittingToolServer, {
    session$setInputs(try_example = 1)
    expect_identical(imported()$source, "Example: motor policies")
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log", offset = "exposure",
                      offset_log = TRUE, weights = "None", formula = "ns(age, df = 4) + region + vehicle + log(vehicle_value)",
                      holdout_share = "0.2", holdout_seed = 1, fit_model = 1)
    expect_null(fit_result()$error)
    html <- paste(readLines(output$download_glm_report, warn = FALSE), collapse = "\n")
    for (section in c("GLM Report", "Coefficients", "Rate table", "Lift chart", "Lorenz curve", "Example: motor policies")) {
      expect_match(html, section, fixed = TRUE)
    }
    skip_if_not_installed("writexl")
    skip_if_not_installed("readxl")
    expect_identical(readxl::excel_sheets(output$download_rates_excel),
                     c("Rate table", "age", "region", "vehicle", "vehicle_value", "Coefficients"))
  })
})

test_that("the distribution tool's example fits the liability claims with their limits", {
  skip_server_on_cran()
  testServer(distribution_fitting_tool_Server, {
    session$setInputs(try_example = 1, data_source = "example", example = "liability", sev_limit_source = "column")
    expect_identical(column_choices()$selected$severity_var, "amount")
    session$setInputs(severity_var = "amount", sev_limit_col = "policy_limit", execute_sev_analysis = 1)
    expect_equal(sum(sev_censored()), sum(liability_claims$amount >= liability_claims$policy_limit))
    expect_match(output$data_overview$html, "Liability claims")
    session$setInputs(example = "motor", counts_var = "claim_count", counts_weighted_var = FALSE, execute_freq_analysis = 1)
    expect_equal(sum(freq_input()$counts), sum(motor_policies$claim_count))
  })
})

test_that("the distribution tool saves and loads its settings, and writes a report of the analyses run", {
  skip_server_on_cran()
  testServer(distribution_fitting_tool_Server, {
    session$setInputs(data_source = "example", example = "liability", data_includes_header = TRUE, sep = "\t", dec = ".",
                      quote = "", severity_var = "amount", sliced_sev_var = "policy_limit", sev_limit_source = "column",
                      sev_limit_col = "policy_limit", sev_threshold = NA, num_pareto_slices = 2, export_severity = "Burr")
    v <- read_settings_file(output$save_settings, dft_settings_tool)$values
    expect_identical(v[c("data_source", "example", "sep", "quote", "sliced_sev_var", "export_severity")],
                     list(data_source = "example", example = "liability", sep = "\t", quote = "", sliced_sev_var = "policy_limit",
                          export_severity = "Burr"))
    expect_true(is.na(v$sev_threshold))
    # (no analysis has run: no slicing points or thresholds)
    expect_null(v$slicing_points)

    # a file's columns beat the guesses once the data has them; its slicing points are set at the run
    path <- tempfile(fileext = ".txt")
    write_settings_file(list(data_source = "example", example = "liability", sliced_sev_var = "policy_limit",
                             slicing_points = c(1e5, 5e5)), path, dft_settings_tool, dft_settings_version)
    session$setInputs(sliced_sev_var = "amount", example = "motor")
    session$setInputs(load_settings = list(datapath = path, name = "mine.txt"))
    expect_identical(pending$columns$sliced_sev_var, "policy_limit")
    expect_equal(pending$slicing, c(1e5, 5e5))
    session$setInputs(example = "liability")
    expect_identical(column_choices()$selected$sliced_sev_var, "policy_limit")
    expect_null(pending$columns$sliced_sev_var)
    session$setInputs(sliced_sev_var = "amount", execute_sliced_sev_analysis = 1)
    expect_true(slicing_range()[1] <= 1e5 && slicing_range()[2] >= 5e5)
    expect_null(pending$slicing)
    # another tool's file is refused
    write_settings_file(list(deductible = 1), path, xlp_settings_tool, xlp_settings_version)
    session$setInputs(load_settings = list(datapath = path, name = "other.txt"))
    expect_null(pending$columns$severity_var)
    unlink(path)

    # the report: each analysis run, with its table and charts
    session$setInputs(slicing_point_left = 1e5, slicing_point_right = 5e5, execute_sev_analysis = 1)
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    for (text in c("Distribution Fitting Report", "Liability claims", "Closest severity", "Conditional tail expectations",
                   "Q-Q plot", "Sliced severity", "Mean excess function", "LogNormal - Pareto - Pareto")) {
      expect_match(html, text, fixed = TRUE)
    }
    expect_no_match(html, "Piecewise Pareto</h2>", fixed = TRUE)
    expect_gt(lengths(regmatches(html, gregexpr("data:image/png", html, fixed = TRUE))), 10)
  })
})

test_that("the reserving tool discounts its reserves, adds a risk adjustment and the reserve risk, and writes a workbook", {
  skip_server_on_cran()
  testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "motor", yield_curve = "3, 3.5", period_length = "1",
                      payment_timing = "0.5", ra_method = "cost of capital", coc_rate = 6, ra_capital = "sii", sii_segment = "1")
    d <- discounted()
    expect_equal(d$total[["reserve"]], cl()$total[["ibnr"]])
    expect_equal(d$yield_curve, c(0.03, 0.035))
    expect_equal(sii_reserve()$capital, 3 * 0.09 * d$total[["discounted"]])
    expect_equal(ra()$capital, sii_reserve()$capital)
    expect_match(output$ifrs_stats$html, "Liability for incurred claims")
    session$setInputs(ra_method = "percentile", ra_level = 75)
    expect_match(output$ifrs_stats$html, "See the Risk adjustment tab")
    session$setInputs(boot_n = 500, boot_seed = 1, boot_tail = TRUE, boot_process = "gamma", run_bootstrap = 1)
    expect_equal(ra()$confidence_level, 0.75)
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    expect_match(html, "IFRS 17 and Solvency II", fixed = TRUE)
    session$setInputs(yield_curve = "three")
    expect_error(discounted(), "spot rates")
    skip_if_not_installed("writexl")
    skip_if_not_installed("readxl")
    sheets <- readxl::excel_sheets(output$download_excel)
    expect_true(all(c("Ultimates", "Mack standard errors", "Bootstrap") %in% sheets))
  })
})

test_that("the portfolio tool's standard formula tab joins the segments", {
  skip_server_on_cran()
  testServer(portfolio_tool_Server, {
    s <- sf_result()
    expect_equal(s$by_segment$segment, c(1, 2, 4, 5))
    direct <- sii_premium_reserve_risk(data.frame(segment = c(1, 2, 4, 5), premium = c(40e6, 25e6, 30e6, 10e6),
                                                  reserve = c(60e6, 10e6, 20e6, 15e6), diversification = 1))
    expect_equal(s$total, direct$total)
    session$setInputs(sf_np_4 = TRUE, sf_premium_3 = 5e6)
    expect_equal(sf_result()$by_segment$segment, c(1, 2, 3, 4, 5))
    expect_equal(sf_result()$by_segment$premium_sd[4], 0.8 * 0.08)
    expect_match(output$sf_stats$html, "Run the Capital tab to compare")
    settings <- read_settings_file(output$save_settings, pfo_settings_tool)$values
    expect_equal(settings$sf_premium[3], 5e6)
    expect_identical(settings$sf_np[4], TRUE)
  })
})

test_that("the reinsurance pricing tool compares its programmes", {
  skip_server_on_cran()
  testServer(reinsurance_pricing_tool_Server, {
    session$setInputs(deductible = 1e6, limit = 4e6, reinstatements = 1, reinstatement_rate = 100, aggregate_deductible = 0,
                      rating_year = 2025, rating_premium = 6e7, loading = 25,
                      prog_frequency = "Poisson", prog_severity = "LogNormal", prog_f1 = 200, prog_s1 = 11, prog_s2 = 1.5,
                      prog_n = 2000, prog_level = "0.995", prog_loading = 25, prog_coc = 10, prog_seed = 1)
    expect_identical(names(programme_list()), c("Quota share 30%", "The layer", "Stop loss", "Quota share 20% and the layer",
                                                "Surplus 4 lines"))
    session$setInputs(prog_name_3 = "", run_programmes = 1)
    s <- current_programmes()$summary
    expect_identical(s$programme, c("No reinsurance", "Quota share 30%", "The layer", "Quota share 20% and the layer",
                                    "Surplus 4 lines"))
    expect_match(output$programme_stats$html, "Best value")
    csv <- utils::read.csv(output$download_programmes)
    expect_equal(csv$capital, s$capital)
    # the surplus treaty's risks are the Exposure tab's profile, each band with its share of the premium
    surplus <- programme_list()[["Surplus 4 lines"]]$surplus
    profile <- NetSimR::xl_property_profile
    expect_equal(surplus[c("retention", "lines", "commission")], list(retention = 1e6, lines = 4, commission = 0.3))
    expect_equal(surplus$sum_insured, profile$sum_insured)
    expect_equal(surplus$weights, profile$premium)
    share <- pmin(pmax(profile$sum_insured - 1e6, 0), 4e6) / profile$sum_insured
    expect_equal(s$reinsurance_premium[5], 0.7 * 6e7 * sum(profile$premium * share) / sum(profile$premium))
    # a casualty profile's policy limits stand for the sums insured
    session$setInputs(profile_example = "casualty")
    expect_equal(programme_list()[["Surplus 4 lines"]]$surplus$sum_insured, NetSimR::xl_casualty_profile$policy_limit)
    session$setInputs(prog_surplus_lines_5 = NA)
    expect_error(programme_list(), "lines of the surplus treaty of 'Surplus 4 lines'")
    session$setInputs(prog_surplus_lines_5 = 2, prog_surplus_retention_1 = 5e5, prog_surplus_lines_1 = 9)

    # the settings file keeps the grid, a vector for each column
    v <- read_settings_file(output$save_settings, xlp_settings_tool)$values
    expect_identical(v$prog_names, c("Quota share 30%", "The layer", "", "Quota share 20% and the layer", "Surplus 4 lines"))
    expect_equal(v$prog_surplus_retentions, c(5e5, NA, NA, NA, 1e6))
    expect_equal(v$prog_surplus_liness, c(9, NA, NA, NA, 2))
    # a file saved with four programmes and no surplus treaty empties the fifth and the surplus columns
    old <- tempfile(fileext = ".txt")
    write_settings_file(list(prog_names = c("A", "B", "", ""), prog_qss = c(40, 10, NA, NA),
                             prog_xl_limits = c(NA, 2e6, NA, NA)), old, xlp_settings_tool, xlp_settings_version)
    session$setInputs(load_settings = list(datapath = old, name = "old.txt"))
    grid <- loaded_programmes()
    expect_identical(grid$name, c("A", "B", "", "", ""))
    expect_equal(grid$qs, c(40, 10, NA, NA, NA))
    expect_true(all(is.na(grid$surplus_retention)))
    expect_true(all(lengths(grid) == 5))
    unlink(old)

    # (the first programme is now the file's)
    session$setInputs(prog_name_2 = "A")
    expect_error(programme_list(), "own name")
  })
})
