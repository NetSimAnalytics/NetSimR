#the Tweedie family: its deviance, its fit, and its use in the GLM fitting tool

tweedie_data <- function(n = 3000) {
  set.seed(8)
  d <- data.frame(age = sample(18:80, n, TRUE), region = sample(c("North", "South", "East"), n, TRUE),
                  exposure = round(runif(n, 0.2, 1), 3), stringsAsFactors = FALSE)
  claims <- rpois(n, d$exposure * exp(-1.5 + 0.015 * d$age + 0.3 * (d$region == "North")))
  d$loss <- vapply(claims, function(k) sum(rgamma(k, shape = 2, scale = 800)), numeric(1))
  d
}

test_that("the Tweedie family has the Tweedie variance and unit deviance", {
  family <- tweedie_family(1.6)
  expect_identical(family$family, "Tweedie")
  expect_identical(family$variance_power, 1.6)
  expect_identical(family$link, "log")
  expect_equal(family$variance(c(1, 2, 4)), c(1, 2, 4)^1.6)
  y <- c(0, 0.5, 3, 10)
  mu <- c(0.4, 1, 2, 8)
  p <- 1.6
  by_formula <- 2 * (ifelse(y > 0, y^(2 - p), 0) / ((1 - p) * (2 - p)) - y * mu^(1 - p) / (1 - p) + mu^(2 - p) / (2 - p))
  expect_equal(family$dev.resids(y, mu, 1), by_formula)
  #a unit deviance is zero where the prediction is the response, and positive elsewhere
  expect_equal(family$dev.resids(c(2, 5), c(2, 5), 1), c(0, 0))
  expect_true(all(family$dev.resids(c(2, 5), c(3, 1), 1) > 0))
  expect_true(is.na(family$aic(y, 1, mu, 1, 1)))
  expect_error(tweedie_family(1), "between 1 and 2")
  expect_error(tweedie_family(2.5), "between 1 and 2")
  expect_error(tweedie_family(1.5, "logit"), "link must be")
})

test_that("a Tweedie fit solves the Tweedie score equations and recovers the relativities", {
  d <- tweedie_data()
  fit <- glm(loss ~ offset(log(exposure)) + age + region, family = tweedie_family(1.5), data = d)
  #with the log link, the score of each coefficient is sum x (y - mu) mu^(1 - p)
  x <- model.matrix(fit)
  mu <- fitted(fit)
  score <- colSums(x * (d$loss - mu) * mu^(1 - 1.5))
  expect_lt(max(abs(score) / colSums(abs(x) * mu^(2 - 1.5))), 1e-6)
  expect_equal(unname(coef(fit)["age"]), 0.015, tolerance = 0.3)
  expect_true(is.na(AIC(fit)))
  expect_gt(summary(fit)$dispersion, 1)
  expect_error(glm(c(-1, 2, 3) ~ 1, family = tweedie_family()), "zero or positive")
})

test_that("the GLM tool fits a Tweedie model, without AIC, and refuses a stepwise selection", {
  d <- tweedie_data()
  path <- tempfile(fileext = ".csv")
  write.csv(d, path, row.names = FALSE)
  shiny::testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "loss.csv"), submit = 1)
    session$setInputs(response_variable = "loss", glm_distribution = "tweedie", link_function = "log", tweedie_power = 1.6,
                      offset = "exposure", offset_log = TRUE, weights = "None", formula = "age + region",
                      holdout_share = "0.2", holdout_seed = 1, fit_model = 1)
    result <- fit_result()
    expect_null(result$error)
    model <- result$model
    expect_identical(model$family$family, "Tweedie")
    expect_identical(model$family$variance_power, 1.6)
    expect_match(paste(deparse(model$call), collapse = ""), "NetSimR::tweedie_family(power = 1.6, link = \"log\")", fixed = TRUE)
    expect_match(output$model_stats$html, "None for a Tweedie model", fixed = TRUE)
    #the validation works on the deviance and the Gini coefficient
    expect_match(output$validation_stats$html, "Gini, training / test", fixed = TRUE)
    session$setInputs(run_selection = 1)
    expect_match(selection()$error, "Tweedie model does not have")
    #the model downloads, and refits in R from its call
    saved <- readRDS(output$download_model)
    model_data <- d[!result$test, ]
    expect_equal(coef(update(saved, . ~ . - region)),
                 coef(glm(loss ~ offset(log(exposure)) + age, family = tweedie_family(1.6), data = d[!result$test, ])))
    #stored models describe the power, and settings keep it
    session$setInputs(save_formula_1 = 1)
    expect_match(output$aic_output_1$html, "Tweedie, power 1.6 (log link)", fixed = TRUE)
    saved_settings <- read_settings_file(output$DownloadDataHandlerConf, glm_settings_tool)$values
    expect_identical(saved_settings$glm_distribution, "tweedie")
    expect_equal(saved_settings$tweedie_power, 1.6)
  })
  expect_identical(glm_settings_values(list(tweedie_power = 2.4)), NULL)
  expect_identical(glm_settings_values(list(glm_distribution = "tweedie", tweedie_power = 1.3)),
                   list(glm_distribution = "tweedie", tweedie_power = 1.3))
  d$loss[1] <- -5
  expect_error(stats::glm(loss ~ age, family = tweedie_family(), data = d), "zero or positive")
})
