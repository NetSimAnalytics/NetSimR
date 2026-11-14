#The GLM tool's fit, called as a function: what the tool's inputs describe, checked against the
#data, without a running tool

glm_spec <- function(...) {
  utils::modifyList(list(response = "claim_count", family = "poisson", link = "log", power = 1.5,
                         formula = "region + vehicle", offset = "exposure", offset_log = TRUE, weights = "None",
                         holdout = "0", seed = 1L), list(...))
}

test_that("the tool's fit is the glm() of its spec, on the rows not held out", {
  fit <- glm_fit_model(glm_spec(), motor_policies)
  direct <- stats::glm(claim_count ~ offset(log(exposure)) + region + vehicle, family = stats::poisson(),
                       data = motor_policies)
  expect_equal(unname(stats::coef(fit$model)), unname(stats::coef(direct)))
  expect_identical(fit$spec$link, "log")
  expect_false(any(fit$test))
  held <- glm_fit_model(glm_spec(holdout = "0.2"), motor_policies)
  expect_equal(sum(held$test), round(0.2 * nrow(motor_policies)))
  expect_equal(stats::nobs(held$model), nrow(motor_policies) - sum(held$test))
})

test_that("the tool's fit stops with a message the tool can show", {
  expect_error(glm_fit_model(glm_spec(), NULL), "import data")
  expect_error(glm_fit_model(glm_spec(response = "nothing"), motor_policies), "choose the response column")
  expect_error(glm_fit_model(glm_spec(formula = "claim_count ~ region"), motor_policies), "only the terms after ~")
  expect_error(glm_fit_model(glm_spec(formula = "system('x')"), motor_policies), "may only use")
  expect_error(glm_fit_model(glm_spec(offset = "region"), motor_policies), "offset column must be numeric")
})

test_that("a column name that is not syntactic is quoted in a formula", {
  expect_identical(glm_term("age"), "age")
  expect_identical(glm_term("vehicle value"), "`vehicle value`")
  expect_identical(glm_offset_term("exposure", TRUE), "offset(log(exposure))")
  expect_null(glm_offset_term("None", TRUE))
})
