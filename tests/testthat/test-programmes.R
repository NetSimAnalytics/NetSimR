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

test_that("a surplus treaty cedes of each claim the share of its risk above the retention", {
  si <- c(5e5, 1e6, 2e6, 5e6, 1e7)
  w <- c(20, 15, 12, 8, 5)
  # nothing within the retention, the part above it of a risk within the lines, the lines of a larger one
  share <- c(0, 0, 0.5, 0.8, 0.4)
  surplus <- list(retention = 1e6, lines = 4, sum_insured = si, weights = w, commission = 0.3)
  programmes <- list(QS = list(quota_share = 0.3, commission = 0.25), Surplus = list(surplus = surplus),
                     Both = list(surplus = surplus, quota_share = 0.2, commission = 0.25),
                     Given = list(surplus = c(surplus[1:4], list(premium = 1e7))),
                     Equal = list(surplus = surplus[1:3]),
                     All = list(surplus = list(retention = 1, lines = Inf, sum_insured = 1e6)))
  cmp <- compare_reinsurance("Poisson", 200, "LogNormal", c(11, 1.5), programmes, premium = 60e6, n = 4000, seed = 1)
  s <- cmp$summary
  r <- cmp$results
  # the claims of the seed are those of a comparison without a surplus treaty
  plain <- compare_reinsurance("Poisson", 200, "LogNormal", c(11, 1.5), programmes["QS"], premium = 60e6, n = 4000, seed = 1)
  expect_identical(cmp$gross, plain$gross)
  expect_identical(r[, "QS"], plain$results[, "QS"])

  # the premium ceded is the subject premium times the weighted ceded share, less the commission
  ceded_premium <- 60e6 * sum(w * share) / sum(w)
  at <- function(name) s[s$programme == name, ]
  expect_equal(at("Surplus")$reinsurance_premium, 0.7 * ceded_premium)
  # the claims fall on the risks by their weights, whatever their size: the expected ceded share
  expect_equal(at("Surplus")$expected_ceded / mean(cmp$gross), sum(w * share) / sum(w), tolerance = 0.03)
  net <- function(name, paid) 60e6 - paid - r[, name]
  left <- net("Surplus", 0.7 * ceded_premium)
  # (of a year's claims, at most the largest share of a risk)
  expect_true(all(left <= cmp$gross + 1e-6 & left >= 0.2 * cmp$gross - 1e-6))
  # a quota share takes its share of what the surplus leaves, of the claims and of the premium
  both_paid <- 0.7 * ceded_premium + 0.2 * (60e6 - ceded_premium) * 0.75
  expect_equal(at("Both")$reinsurance_premium, both_paid)
  expect_equal(net("Both", both_paid), 0.8 * left)
  # a given premium, without a commission; the same risks as the other programmes
  expect_equal(at("Given")$reinsurance_premium, 1e7)
  expect_equal(net("Given", 1e7), left)
  # equal weights without any
  expect_equal(at("Equal")$reinsurance_premium, 60e6 * mean(share))
  # a tiny retention with unlimited lines cedes nearly everything
  expect_equal(at("All")$expected_ceded, mean(cmp$gross) * (1 - 1e-6))
  expect_equal(at("All")$capital, s$capital[1] * 1e-6, tolerance = 1e-6)

  bad <- function(surplus, message) {
    expect_error(compare_reinsurance("Poisson", 10, "Gamma", c(2, 1000), list(A = list(surplus = surplus)), premium = 1e5, n = 500),
                 message)
  }
  bad(list(retention = 0, lines = 2, sum_insured = si), "surplus retention of 'A' must be above 0")
  bad(list(retention = 1e6, sum_insured = si), "surplus lines of 'A' must be above 0")
  bad(list(retention = 1e6, lines = 2), "needs sum_insured")
  bad(list(retention = 1e6, lines = 2, sum_insured = c(1e6, 0)), "needs sum_insured")
  bad(list(retention = 1e6, lines = 2, sum_insured = si, weights = 1:2), "surplus weights of 'A'")
  bad(list(retention = 1e6, lines = 2, sum_insured = si, weights = rep(0, 5)), "surplus weights of 'A'")
  bad(list(retention = 1e6, lines = 2, sum_insured = si, premium = -1), "surplus premium of 'A'")
  bad(list(retention = Inf, lines = 2, sum_insured = si), "must be finite")
})
