#the GLM fitting tool's rate table, its scoring of new data, and splines and bands in its formulas

rating_data <- function(n = 3000) {
  set.seed(2)
  d <- data.frame(age = sample(18:80, n, TRUE), region = sample(c("N", "S", "E"), n, TRUE),
                  exposure = round(stats::runif(n, 0.2, 1), 3), van = sample(c(TRUE, FALSE), n, TRUE),
                  stringsAsFactors = FALSE)
  d$claims <- stats::rpois(n, d$exposure * exp(-2 + 0.01 * (d$age - 45) + 0.3 * (d$region == "N")))
  d
}

test_that("the base rate times the relativities reproduces every prediction, splines, bands and interactions included", {
  d <- rating_data()
  f <- stats::as.formula("claims ~ offset(log(exposure)) + ns(age, 3) + region * van + cut(age, c(17, 25, 81))",
                         env = glm_formula_env())
  m <- stats::glm(f, stats::poisson(), d)
  rates <- glm_rate_table(m, d)
  expect_identical(names(rates$factors), c("age", "region", "van", "region × van"))
  expect_true(rates$exact)
  expect_identical(attr(rates$factors$age, "terms"), c("ns(age, 3)", "cut(age, c(17, 25, 81))"))
  #each factor's base value has relativity 1: the most common level, the value nearest the median
  for (f in rates$factors) expect_equal(f$relativity[f$base], 1)
  expect_lte(abs(rates$base$age - stats::median(d$age)), 0.5)
  expect_identical(rates$base$region, names(which.max(table(d$region))))
  #the rows of each level add up to the data's
  expect_equal(sum(rates$factors$region$rows), nrow(d))
  product <- rates$base_rate * d$exposure
  for (f in rates$factors) {
    vars <- setdiff(names(f), c("effect", "relativity", "base", "rows"))
    key <- do.call(paste, c(lapply(vars, function(v) as.character(f[[v]])), sep = "|"))
    row_key <- do.call(paste, c(lapply(vars, function(v) as.character(d[[v]])), sep = "|"))
    product <- product * f$relativity[match(row_key, key)]
  }
  expect_equal(unname(product), unname(stats::predict(m, d, type = "response")), tolerance = 1e-10)
  #the CSV: the base rate first, then a row per value of each factor
  long <- glm_rate_table_long(rates)
  expect_identical(long$rating_factor[1], "Base rate")
  expect_equal(long$relativity[1], rates$base_rate)
  expect_equal(nrow(long), 1 + sum(vapply(rates$factors, nrow, integer(1))))
  expect_true(all(grepl("^region = ", long$level[long$rating_factor == "region × van"])))
})

test_that("numbers with many values are shown at quantiles, and other links have no relativity", {
  d <- rating_data()
  d$value <- stats::runif(nrow(d), 1000, 50000)
  m <- stats::glm(claims ~ offset(log(exposure)) + value, stats::poisson(), d)
  rates <- glm_rate_table(m, d, max_values = 20)
  expect_false(rates$exact)
  expect_lte(nrow(rates$factors$value), 20)
  expect_true(all(is.na(rates$factors$value$rows)))
  gaussian <- stats::glm(claims ~ region, stats::gaussian(), d)
  g <- glm_rate_table(gaussian, d)
  expect_true(is.na(g$relativity))
  expect_true(all(is.na(g$factors$region$relativity)))
  expect_identical(names(glm_rate_table_long(g))[4], "relativity")
  logit <- stats::glm(I(claims > 0) ~ region, stats::binomial(), d)
  expect_identical(glm_rate_table(logit, d)$relativity, "odds ratio")
  expect_identical(names(glm_rate_table_long(glm_rate_table(logit, d)))[4], "odds_ratio")
  #an intercept-only model is its base rate
  only <- glm_rate_table(stats::glm(claims ~ offset(log(exposure)), stats::poisson(), d), d)
  expect_length(only$factors, 0)
  expect_equal(only$base_rate, sum(d$claims) / sum(d$exposure))
})

test_that("a new file is scored, with NA for unseen levels and missing values", {
  d <- rating_data()
  m <- stats::glm(claims ~ offset(log(exposure)) + age + region, stats::poisson(), d)
  new <- d[1:6, c("age", "region", "exposure")]
  new$region[2] <- "W"
  new$age[3] <- NA
  new$prediction <- 1
  s <- glm_score(m, new, response = "claims")
  expect_identical(s$column, "model_prediction")
  expect_equal(s$predicted, 4)
  expect_equal(s$unseen, 1)
  expect_equal(s$incomplete, 1)
  expect_true(all(is.na(s$data$model_prediction[2:3])))
  expect_equal(s$data$model_prediction[1], unname(stats::predict(m, d[1, ], type = "response")))
  expect_error(glm_score(m, new[c("age", "region")], response = "claims"), "no column 'exposure'")
})

test_that("the tool fits splines and bands, shows the rate table and scores a file", {
  skip_server_on_cran()
  d <- rating_data(1500)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(d, path, row.names = FALSE)
  score_path <- tempfile(fileext = ".csv")
  utils::write.csv(d[1:20, c("age", "region", "exposure", "van")], score_path, row.names = FALSE)
  testServer(GLMFittingToolServer, {
    session$setInputs(data_source = "CSV File", csv_file = list(datapath = path, name = "policies.csv"), submit = 1)
    session$setInputs(response_variable = "claims", glm_distribution = "poisson", link_function = "log", offset = "exposure",
                      offset_log = TRUE, weights = "None", formula = "ns(age, df = 3) + cut(age, c(17, 30, 50, 81)) + region",
                      holdout_share = "0", fit_model = 1)
    expect_null(fit_result()$error)
    #the model keeps the spline basis, so it predicts in a session without the splines package attached
    model <- fitted_model()
    expect_true(exists("ns", envir = environment(stats::formula(model)), inherits = FALSE))
    rates <- rate_table()
    expect_identical(names(rates$factors), c("age", "region"))
    expect_match(output$rate_stats$html, "Base rate")
    session$setInputs(rate_factor = "region")
    expect_match(output$rate_table_ui$html, "Relativity")
    expect_match(output$rate_table_ui$html, "Base")
    csv <- utils::read.csv(output$download_rate_table)
    expect_identical(csv$rating_factor[1], "Base rate")
    session$setInputs(score_file = list(datapath = score_path, name = "new.csv"))
    expect_equal(scored()$predicted, 20)
    scored_csv <- utils::read.csv(output$download_scored)
    expect_equal(scored_csv$prediction, unname(stats::predict(model, d[1:20, ], type = "response")))
    #a file without the model's columns is refused
    bad <- tempfile(fileext = ".csv")
    utils::write.csv(d[1:5, c("age", "exposure")], bad, row.names = FALSE)
    session$setInputs(score_file = list(datapath = bad, name = "bad.csv"))
    expect_null(scored())
  })
})
