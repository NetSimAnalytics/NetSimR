#comparing reinsurance programmes

test_that("every programme sees the same claims, and each structure cedes what it should", {
  programmes <- list(
    QS = list(quota_share = 0.3, commission = 0.25),
    XL = list(xl = list(deductible = 1e6, limit = 2e6, reinstatements = 1, premium = 1e6)),
    SL = list(stop_loss = list(attachment = 40e6, limit = 10e6, premium = 5e5))
  )
  set.seed(3)
  before <- .Random.seed
  cmp <- compare_reinsurance("Poisson", 200, "LogNormal", c(11, 1.5), programmes, premium = 60e6, n = 4000, seed = 1)
  expect_identical(.Random.seed, before)
  expect_s3_class(cmp, "netsimr_programmes")
  s <- cmp$summary
  expect_identical(s$programme, c("No reinsurance", "QS", "XL", "SL"))
  r <- cmp$results
  gross_result <- r[, "No reinsurance"]
  expect_equal(gross_result, 60e6 - cmp$gross)
  # a quota share scales the result, plus its commission
  expect_equal(r[, "QS"], 0.7 * gross_result + 0.25 * 0.3 * 60e6)
  # a stop loss caps the retained claims at its attachment, up to its limit
  sl_claims <- 60e6 - 5e5 - r[, "SL"]
  expect_equal(sl_claims, cmp$gross - pmin(pmax(cmp$gross - 40e6, 0), 10e6))
  # the layer's annual recoveries are at most two limits, and its reinstatement is paid pro rata
  ceded_xl <- s$expected_ceded[s$programme == "XL"]
  expect_lte(ceded_xl, 4e6)
  expect_equal(s$capital[1] - s$capital, s$capital_relief)
  expect_equal(s$value, 0.1 * s$capital_relief - s$margin)
  # the same seed, the same comparison
  expect_identical(compare_reinsurance("Poisson", 200, "LogNormal", c(11, 1.5), programmes, premium = 60e6, n = 4000,
                                       seed = 1)$results, r)
  expect_output(print(cmp), "Reinsurance programmes")
})

test_that("unpriced layers are priced at the loading, and the inputs are checked", {
  cmp <- compare_reinsurance("Negative Binomial", c(4, 25), "Pareto", c(1.6, 20000),
                             list(XL = list(xl = list(deductible = 2e5, limit = 1e6))), premium = 5e6, n = 3000,
                             loading = 0.2, seed = 2)
  s <- cmp$summary[2, ]
  expect_equal(s$reinsurance_premium, s$expected_ceded / 0.8)
  expect_error(compare_reinsurance("Poisson", 10, "Gamma", c(2, 1000), list(list(quota_share = 0.2)), premium = 1e5, n = 500),
               "named list")
  expect_error(compare_reinsurance("Poisson", 10, "Gamma", c(2, 1000), list(A = list(quota_share = 1.5)), premium = 1e5, n = 500),
               "at most 1")
  expect_error(compare_reinsurance("Poisson", 10, "Gamma", c(2, 1000), list(A = list(xl = list(deductible = 0, limit = 0))),
                                   premium = 1e5, n = 500), "above 0")
  expect_error(compare_reinsurance("Poisson", 10, "Gamma", c(2, 1000), list(`No reinsurance` = list()), premium = 1e5, n = 500),
               "own name")
})
