#the distribution fitting and GLM fitting tools, driven through shiny::testServer()

write_claims_csv <- function() {
  set.seed(10)
  d <- data.frame(counts = rpois(300, 3), w = sample(1:4, 300, TRUE), sev = round(exp(rnorm(300, 7, 1.2))), txt = "x")
  d$sev[1:3] <- c(-1, 0, NA)
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  list(path = path, data = d)
}

test_that("distribution fitting tool fits frequency and severity distributions", {
  csv <- write_claims_csv()
  shiny::testServer(distribution_fitting_tool_Server, {
    session$setInputs(file1 = list(datapath = csv$path, name = "claims.csv"), data_includes_header = TRUE, sep = ",", quote = intToUtf8(34))
    expect_equal(nrow(data()), 300)
    #frequency: the Poisson MLE is the mean, the summary outputs render
    session$setInputs(counts_var = "counts", counts_weighted_var = FALSE, counts_weights_var = "w", FreqDistri = "Poisson", count_hist_bins = 20, execute_freq_analysis = 1)
    expect_equal(counts_data(), csv$data$counts)
    expect_equal(unname(freq_po_fit()$estimate), mean(csv$data$counts))
    expect_match(output$selected_freq_params, "Mean")
    expect_match(output$distribution_proposal, "Poisson|Negative Binomial")
    session$setInputs(FreqDistri = "NegativeBinomial")
    expect_match(output$selected_freq_params, "Beta")
    expect_true(is.character(output$freq_fit_plot) || is.list(output$freq_fit_plot))
    #severity: negative, zero and missing values are dropped and the fits use the cleaned data
    session$setInputs(severity_var = "sev", severity_hist_bins = 20, sev_fit_log_scale = FALSE, execute_sev_analysis = 1)
    cleaned <- csv$data$sev[!is.na(csv$data$sev) & csv$data$sev > 0]
    expect_equal(severity_data(), cleaned)
    expect_equal(sev_gamma_fit()$estimate, fit_gamma_mle(cleaned)$estimate)
    expect_equal(sev_pareto_alpha(), length(cleaned) / sum(log(cleaned / min(cleaned))))
    expect_match(output$sev_param_summary, "LogNormal")
    expect_match(output$sev_param_summary, "Gamma")
    expect_true(is.character(output$sev_fit_plot) || is.list(output$sev_fit_plot))
  })
})

test_that("distribution fitting tool sliced and piecewise Pareto analyses run", {
  csv <- write_claims_csv()
  shiny::testServer(distribution_fitting_tool_Server, {
    session$setInputs(file1 = list(datapath = csv$path, name = "claims.csv"), data_includes_header = TRUE, sep = ",", quote = intToUtf8(34))
    session$setInputs(sliced_sev_var = "sev", sev_cens_fit_log_scale = FALSE, execute_sliced_sev_analysis = 1)
    session$setInputs(slicing_point_left = 2000, slicing_point_right = 8000)
    cleaned <- sliced_sev_data()
    #the Pareto alphas above each slicing point are the usual MLEs
    above <- cleaned[cleaned > 2000]
    expect_equal(paretoX1Alpha(), length(above) / sum(log(above / 2000)))
    expect_true(is.numeric(paretoX2AlphaMod()) && is.finite(paretoX2AlphaMod()))
    expect_true(is.character(output$mean_excess_func_plot) || is.list(output$mean_excess_func_plot))
    expect_true(is.character(output$sliced_sev_cdf_plot) || is.list(output$sliced_sev_cdf_plot))
    expect_match(output$slc_sev_fitted_param_summary, "Pareto x2 upper")
    #piecewise Pareto with three sliders
    session$setInputs(piecewise_pareto_var = "sev", num_pareto_slices = 3, piecewise_pareto_fit_log_scale = FALSE, execute_piecewise_sev_analysis = 1)
    expect_match(as.character(dynamic_sliders()), "slider_3")
    session$setInputs(slider_1 = 1500, slider_2 = 4000, slider_3 = 12000)
    #the piecewise fit works on the sorted claims
    expect_equal(piecewise_sev_data(), sort(cleaned))
    expect_equal(piecwise_pareto_mu(), c(min(cleaned), 1500, 4000, 12000))
    expect_equal(piecwise_pareto_alpha(), piecewise_pareto_alpha(sort(cleaned), c(min(cleaned), 1500, 4000, 12000)))
    expect_equal(predicted_piecwise_cdf(), piecewise_pareto_cdf(sort(cleaned), piecwise_pareto_mu(), piecwise_pareto_alpha()))
    expect_match(output$piecewise_pareto_ks_test, "k-s test")
    #sliders that are not increasing give NaN alphas (with a warning) but no error
    suppressWarnings({
      session$setInputs(slider_1 = 1500, slider_2 = 1500, slider_3 = 12000)
      alphas <- piecwise_pareto_alpha()
    })
    expect_true(all(is.nan(alphas)))
    session$setInputs(num_pareto_slices = 1, slider_1 = 3000)
    expect_equal(piecwise_pareto_mu(), c(min(cleaned), 3000))
    expect_length(piecwise_pareto_alpha(), 2)
  })
})

write_glm_csv <- function() {
  set.seed(2)
  d <- data.frame(y = rpois(200, 3), x1 = rnorm(200), x2 = runif(200), w = sample(1:5, 200, TRUE), e = runif(200, 0.5, 2))
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  list(path = path, data = d)
}

test_that("GLM fitting tool fits the same model as glm()", {
  csv <- write_glm_csv()
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = csv$path, name = "glm.csv"), submit = 1)
    expect_equal(nrow(selected_data()), 200)
    session$setInputs(response_variable = "y", glm_distribution = "poisson", link_function = "log", offset = "e", weights = "w", formula = "x1 + x2", fit_model = 1)
    reference <- glm(y ~ offset(e) + x1 + x2, data = csv$data, weights = csv$data$w, family = poisson(link = "log"))
    expect_equal(coef(fitted_model()), coef(reference))
    expect_equal(AIC(fitted_model()), AIC(reference))
    expect_match(output$model_summary, "poisson")
    session$setInputs(save_formula_1 = 1)
    expect_match(output$aic_output_1, "AIC formula 1")
    session$setInputs(visualize_variable = "x1", number_of_bands_input = 10, execute_visualization = 1)
    expect_true(is.character(output$fitness_plot) || is.list(output$fitness_plot))
    #another family and link, without offset and weights (e is positive, as the Gamma family requires)
    session$setInputs(response_variable = "e", glm_distribution = "Gamma", link_function = "inverse", offset = "None", weights = "None", formula = "x1", fit_model = 2)
    expect_equal(coef(fitted_model()), coef(glm(e ~ x1, data = csv$data, family = Gamma(link = "inverse"))))
    #a formula that does not fit gives no model and a message in the summary
    session$setInputs(formula = "not_a_column", fit_model = 3)
    expect_null(fitted_model())
    expect_match(output$model_summary, "failed")
  })
})

test_that("GLM fitting tool database import checks for the driver package and reports errors", {
  shiny::testServer(GLMFittingToolServer, {
    expect_false(glm_tool_db_package_available("Not a database"))
    #a connection that cannot be opened gives no data instead of an error
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = file.path(tempdir(), "does_not_exist", "x.sqlite"), sql_query = "SELECT 1", submit = 1)
    expect_null(selected_data())
  })
})

test_that("GLM fitting tool imports a table from SQLite through the DBI helper", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  DBI::dbWriteTable(con, "claims", data.frame(id = 1:5, amount = c(10, 20, 30, 40, 50)))
  DBI::dbDisconnect(con)
  expect_equal(glm_tool_query_dbi(RSQLite::SQLite(), "SELECT * FROM claims", dbname = db), data.frame(id = 1:5, amount = c(10, 20, 30, 40, 50)))
  shiny::testServer(GLMFittingToolServer, {
    expect_true(glm_tool_db_package_available("SQLite"))
    session$setInputs(data_source = "Database", db_type = "SQLite", db_name = db, sql_query = "SELECT id, amount FROM claims WHERE amount > 20", submit = 1)
    expect_equal(selected_data(), data.frame(id = 3:5, amount = c(30, 40, 50)))
  })
})
