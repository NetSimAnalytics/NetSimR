#the GLM fitting tool's validation and variable selection: rows held out for testing, the
#deviance per row, the Gini coefficient, lift and double lift tables, and stepwise selection

#claims with two rating factors that matter (age, region, vehicle) and two that do not
glm_selection_data <- function(n = 4000) {
  set.seed(5)
  d <- data.frame(policy_id = sprintf("P%05d", seq_len(n)), age = sample(18:80, n, TRUE),
                  region = sample(c("North", "South", "East", "West"), n, TRUE),
                  vehicle = sample(c("car", "van", "bike"), n, TRUE), noise = round(rnorm(n), 3),
                  colour = sample(c("red", "blue", "green", "black", "white"), n, TRUE),
                  exposure = round(runif(n, 0.2, 1), 3), stringsAsFactors = FALSE)
  d$claim_count <- rpois(n, d$exposure * exp(-2 + 0.015 * d$age + 0.4 * (d$region == "North") + 0.6 * (d$vehicle == "bike")))
  d
}

#fits as the tool does: the call refers to model_data and family_object
glm_tool_style_fit <- function(formula, data, family = stats::poisson()) {
  env <- new.env(parent = globalenv())
  env$model_data <- data
  env$family_object <- family
  f <- stats::as.formula(formula, env = globalenv())
  model <- eval(bquote(stats::glm(.(f), family = family_object, data = model_data, na.action = stats::na.exclude)), env)
  model$call$family <- call(family$family, link = family$link)
  model
}

test_that("the rows held out are the same for a seed, and leave the caller's random numbers alone", {
  set.seed(99)
  before <- .Random.seed
  test <- glm_holdout_rows(1000, "0.2", 7)
  expect_identical(.Random.seed, before)
  expect_identical(sum(test), 200L)
  expect_identical(test, glm_holdout_rows(1000, 0.2, 7))
  expect_false(identical(test, glm_holdout_rows(1000, 0.2, 8)))
  #none, nonsense or too few rows hold nothing out; a share is at most a half, and one row stays
  expect_false(any(glm_holdout_rows(1000, "0", 1)))
  expect_false(any(glm_holdout_rows(1000, "lots", 1)))
  expect_false(any(glm_holdout_rows(1, 0.3, 1)))
  expect_identical(sum(glm_holdout_rows(10, 0.9, 1)), 5L)
  expect_identical(sum(glm_holdout_rows(2, 0.3, 1)), 1L)
  expect_identical(glm_holdout_seed("12"), 12L)
  expect_identical(glm_holdout_seed(NA), 1L)
  expect_identical(glm_holdout_seed(1e12), 1L)
})

test_that("rows the model was not fitted to are predicted, and new factor values are NA", {
  d <- glm_selection_data(600)
  model <- glm_tool_style_fit("claim_count ~ offset(log(exposure)) + age + factor(region)", d[1:500, ])
  newdata <- d[501:600, ]
  newdata$region[1:3] <- c("Mars", NA, "North")
  predicted <- glm_predict_all(model, newdata)
  expect_identical(predicted$unseen, 1L)
  expect_true(is.na(predicted$predicted[1]) && is.na(predicted$predicted[2]))
  expect_equal(predicted$predicted[-(1:2)], unname(predict(model, newdata[-(1:2), ], type = "response")))
  #a text response is coded as glm() codes it
  expect_identical(glm_response_coded(factor(c("no", "yes", NA, "yes"))), c(0, 1, NA, 1))
  expect_identical(glm_response_coded(c(TRUE, FALSE)), c(1, 0))
})

test_that("the deviance per row and the Gini coefficient measure the fit and the ranking", {
  d <- glm_selection_data(2000)
  model <- glm_tool_style_fit("claim_count ~ offset(log(exposure)) + age + region + vehicle", d)
  #on the rows a model was fitted to, the deviance per row is its deviance over its rows
  expect_equal(glm_unit_deviance(model$family, d$claim_count, fitted(model)), deviance(model) / nrow(d))
  expect_equal(glm_unit_deviance(stats::gaussian(), c(1, 3), c(2, 2), c(1, 3)), 1)
  expect_true(is.na(glm_unit_deviance(stats::poisson(), NA, 1)))
  #a perfect ranking has a higher Gini than the model's, and the model's is above chance
  model_gini <- glm_gini_curve(d$claim_count, fitted(model), d$exposure)
  expect_gt(model_gini$gini, 0.05)
  expect_gt(glm_gini_curve(d$claim_count, d$claim_count + 1e-9 * d$exposure, d$exposure)$gini, model_gini$gini)
  #a flat prediction is one step: no ranking at all
  expect_equal(glm_gini_curve(d$claim_count, rep(1, nrow(d)) * d$exposure, d$exposure)$gini, 0)
  expect_identical(c(head(model_gini$x, 1), tail(model_gini$x, 1), tail(model_gini$y, 1)), c(0, 1, 1))
  expect_null(glm_gini_curve(c(0, 0), c(1, 2)))
  thin <- glm_thin_curve(model_gini, 50)
  expect_lte(length(thin$x), 50)
  expect_identical(c(thin$x[1], tail(thin$x, 1)), c(0, 1))
})

test_that("a lift table cuts the sorted rows into bands of the same exposure", {
  actual <- c(0, 0, 1, 0, 1, 1, 2, 1, 3, 2)
  predicted <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  table <- glm_lift_table(actual, predicted, sort_by = predicted, bands = 5)
  expect_identical(table$band, 1:5)
  expect_identical(table$exposure, rep(2, 5))
  expect_equal(table$actual, c(0, 0.5, 1, 1.5, 2.5))
  expect_equal(table$predicted, c(0.15, 0.35, 0.55, 0.75, 0.95))
  #with weights the exposure of a row counts that many times; a second model is a column
  double <- glm_lift_table(actual, predicted, sort_by = rev(predicted), weight = 2, bands = 2, others = list(other = rev(predicted)))
  expect_identical(double$exposure, c(10, 10))
  expect_equal(double$other, c(0.3, 0.8))
  expect_null(glm_lift_table(NA, 1, 1))
})

test_that("stepwise selection adds the factors that matter and leaves out the noise", {
  d <- glm_selection_data()
  test <- glm_holdout_rows(nrow(d), 0.25, 1)
  start <- glm_tool_style_fit("claim_count ~ offset(log(exposure)) + age", d[!test, ])
  candidates <- glm_candidate_columns(d, c("claim_count", "exposure"))
  #the ID is not a candidate
  expect_setequal(candidates, c("age", "region", "vehicle", "noise", "colour"))
  for (criterion in c("AIC", "BIC")) {
    s <- glm_stepwise(start, d[!test, ], candidates, criterion, test = d[test, ])
    expect_setequal(strsplit(s$terms, " + ", fixed = TRUE)[[1]], c("age", "region", "vehicle"))
    expect_identical(s$path$change[1], "Start")
    expect_true(all(diff(s$path$criterion) < 0))
    #the test deviance falls as the factors that matter come in
    expect_true(all(is.finite(s$path$test_deviance)))
    expect_lt(tail(s$path$test_deviance, 1), s$path$test_deviance[1])
    expect_identical(s$criterion, criterion)
  }
  #the offset stays, and a term can be dropped
  start <- glm_tool_style_fit("claim_count ~ offset(log(exposure)) + age + noise + colour", d)
  s <- glm_stepwise(start, d, character(0), "BIC")
  expect_identical(s$terms, "age")
  expect_true(all(grepl("^- ", s$path$change[-1])))
  expect_true(all(is.na(s$path$test_deviance)))
  #rows with a candidate missing are left out of every step
  d$noise[1:10] <- NA
  start <- glm_tool_style_fit("claim_count ~ offset(log(exposure)) + age", d)
  s <- glm_stepwise(start, d, c("noise", "region"), "AIC")
  expect_identical(s$left_out, 10L)
  expect_identical(s$rows, nrow(d) - 10L)
})

test_that("the tool fits to the training rows, predicts the test rows and validates on them", {
  d <- glm_selection_data()
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "claims.csv"), submit = 1)
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "exposure", offset_log = TRUE, weights = "None", formula = "age",
                      holdout_share = "0.2", holdout_seed = 3, fit_model = 1)
    result <- fit_result()
    expect_identical(sum(result$test), 800L)
    expect_identical(stats::nobs(result$model), 3200L)
    expect_identical(result$test, glm_holdout_rows(4000, 0.2, 3))
    expect_match(output$model_stats$html, "800 rows held out for testing", fixed = TRUE)
    #every row has a prediction, the test rows too
    predicted <- shown_predictions()
    expect_length(predicted, 4000)
    expect_true(all(is.finite(predicted)))
    file <- output$download_data_with_predictions
    downloaded <- utils::read.csv(file)
    expect_identical(as.vector(table(downloaded$sample)[c("test", "training")]), c(800L, 3200L))
    #the validation tab's figures, for the training and the test rows
    stats_html <- output$validation_stats$html
    expect_match(stats_html, "3,200 / 800", fixed = TRUE)
    expect_match(stats_html, "Gini, training / test", fixed = TRUE)
    v <- validation_rows()
    expect_equal(validation_measures(v, !v$test)$deviance, deviance(result$model) / 3200)
    expect_identical(validation_set()$used, "test")
    expect_identical(sum(lift_table()$exposure), sum(d$exposure[result$test]))
    for (id in c("lift_plot", "lorenz_plot")) expect_match(output[[id]]$src, "^data:image/png;base64,")
    session$setInputs(validation_rows = "training", validation_bands = "5")
    expect_identical(nrow(lift_table()), 5L)
    expect_identical(sum(lift_table()$exposure), sum(d$exposure[!result$test]))
    #actual vs predicted on the test rows alone
    session$setInputs(visualize_variable = "region", avp_rows = "test", execute_visualization = 1)
    expect_equal(sum(fitness_data()$exposure), sum(d$exposure[result$test]))

    #the selection, from the fitted model, measured on the rows held out
    session$setInputs(select_criterion = "BIC", select_candidates = c("region", "vehicle", "noise", "colour"), run_selection = 1)
    s <- selection()
    expect_null(s$error)
    expect_setequal(strsplit(s$terms, " + ", fixed = TRUE)[[1]], c("age", "region", "vehicle"))
    expect_match(output$selection_result$html, "Test deviance per row", fixed = TRUE)
    session$setInputs(use_selection = 1)
    expect_identical(spec_value("formula"), s$terms)

    #a double lift of the selected model against the stored starting model, on the same rows
    session$setInputs(save_formula_1 = 1)
    session$setInputs(formula = s$terms, fit_model = 2)
    session$setInputs(validation_rows = "test", validation_compare = "1")
    table <- double_lift()$table
    expect_true(all(c("actual", "predicted", "other") %in% names(table)))
    expect_match(output$double_lift_plot$src, "^data:image/png;base64,")
    expect_gt(validation_measures(validation_rows(), validation_rows()$test)$gini,
              glm_gini_curve(d$claim_count[result$test], predicted[result$test], d$exposure[result$test])$gini)
    #the stored model holds the split, and a model with other rows held out is not comparable
    expect_match(output$aic_output_1$html, "20% held out (seed 3)", fixed = TRUE)
    session$setInputs(save_formula_2 = 1)
    session$setInputs(holdout_seed = 4, fit_model = 3, save_formula_2 = 2)
    expect_match(output$aic_output_2$html, "rows held out", fixed = TRUE)
    expect_error(double_lift(), "other rows held out")
  })
})

test_that("without rows held out the validation is of the training rows, and the selection needs a model", {
  d <- glm_selection_data(1500)
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "claims.csv"), submit = 1)
    session$setInputs(run_selection = 1)
    expect_match(selection()$error, "Fit a model first")
    session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                      offset = "exposure", offset_log = TRUE, weights = "None", formula = "age + region", fit_model = 1)
    expect_false(any(fit_result()$test))
    expect_match(output$validation_stats$html, "No rows were held out", fixed = TRUE)
    expect_identical(validation_set()$used, "training")
    expect_match(output$lift_note$html, "No rows were held out", fixed = TRUE)
    #the predictions are those of glm(), as before
    expect_equal(shown_predictions(), unname(fitted(fit_result()$model)))
    expect_false("sample" %in% names(utils::read.csv(output$download_data_with_predictions)))
    expect_error(double_lift(), "Choose a stored model")
    session$setInputs(validation_compare = "2")
    expect_error(double_lift(), "Model 2 is empty")
  })
})

test_that("the settings file keeps the rows held out, the seed and the criterion", {
  values <- glm_settings_values(list(holdout_share = "0.3", holdout_seed = 12, select_criterion = "BIC"))
  expect_identical(values, list(holdout_share = "0.3", holdout_seed = 12L, select_criterion = "BIC"))
  expect_null(glm_settings_values(list(holdout_share = "0.9", holdout_seed = "x", select_criterion = "Cp")))
  file <- tempfile(fileext = ".txt")
  write_settings_file(list(holdout_share = "0.2", holdout_seed = 5L, select_criterion = "BIC"), file,
                      glm_settings_tool, glm_settings_version)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(load_config = list(datapath = file, name = "settings.txt"))
    expect_identical(spec_value("holdout_share"), "0.2")
    expect_identical(spec_value("holdout_seed"), 5L)
    expect_identical(current_spec()$holdout, "0.2")
    expect_identical(current_spec()$seed, 5L)
    session$setInputs(holdout_share = "0.2", holdout_seed = 5, select_criterion = "BIC")
    saved <- read_settings_file(output$DownloadDataHandlerConf, glm_settings_tool)$values
    expect_equal(saved[c("holdout_share", "holdout_seed", "select_criterion")],
                 list(holdout_share = "0.2", holdout_seed = 5, select_criterion = "BIC"))
  })
})
