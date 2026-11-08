#actual against expected development, Clark's growth curves and the chain ladder for inflation

test_that("actual vs expected compares the latest diagonal with the earlier triangle's chain ladder", {
  ave <- actual_vs_expected(raa_triangle)
  expect_s3_class(ave, "netsimr_reserve")
  C <- raa_triangle
  m <- nrow(C)
  latest <- rowSums(!is.na(C))
  earlier <- C
  earlier[cbind(seq_len(m), latest)] <- NA
  earlier <- earlier[latest >= 2, seq_len(ncol(C) - 1)]
  cl <- chain_ladder(earlier)
  #origin 1985 was at age 5 before the latest diagonal, and at 6 after it
  row <- ave$by_origin[ave$by_origin$origin == "1985", ]
  expect_equal(row$previous, C["1985", 5])
  expect_equal(row$expected, C["1985", 5] * (cl$factors[[5]] - 1))
  expect_equal(row$actual, C["1985", 6] - C["1985", 5])
  expect_equal(row$se, sqrt(C["1985", 5] * cl$sigma[[5]]^2 + C["1985", 5]^2 * cl$factor_se[[5]]^2))
  expect_equal(row$z, (row$actual - row$expected) / row$se)
  #the oldest origin has no factor for its last step, the newest a single value: neither is counted
  expect_true(is.na(ave$by_origin$expected[1]))
  expect_true(is.na(ave$by_origin$expected[m]))
  counted <- is.finite(ave$by_origin$expected)
  expect_equal(ave$total[["expected"]], sum(ave$by_origin$expected[counted]))
  expect_equal(ave$total[["actual"]], sum(ave$by_origin$actual[counted]))
  expect_gt(ave$total[["se"]], sqrt(sum(ave$by_origin$se[counted]^2)) * 0.99)
  expect_output(print(ave), "Actual vs expected")
  #arguments go to the chain ladder of the earlier triangle
  simple <- actual_vs_expected(raa_triangle, average = "simple")
  expect_false(isTRUE(all.equal(simple$total[["expected"]], ave$total[["expected"]])))
  expect_error(actual_vs_expected(matrix(c(1, 2, 3, NA), 2)), "at least two origins")
})

#a triangle drawn from Clark's model: a log-logistic curve and over-dispersed Poisson increments
clark_triangle <- function(m = 12, omega = 1.5, theta = 2.5, ultimate = 10000, phi = 20, seed = 4) {
  set.seed(seed)
  growth <- function(age) ifelse(age > 0, 1 / (1 + (theta / pmax(age, 1e-12))^omega), 0)
  C <- matrix(NA_real_, m, m, dimnames = list(origin = 2000 + seq_len(m), development = seq_len(m)))
  for (i in seq_len(m)) {
    k <- m - i + 1
    mean_increments <- ultimate * (growth(seq_len(k) - 0.5) - growth(seq_len(k) - 1.5))
    C[i, seq_len(k)] <- cumsum(phi * stats::rpois(k, mean_increments / phi))
  }
  C
}

test_that("Clark's LDF method fits its growth curve by maximum likelihood", {
  C <- clark_triangle()
  fit <- clark_growth(C)
  expect_s3_class(fit, "netsimr_reserve")
  expect_equal(fit$parameters[["omega"]], 1.5, tolerance = 0.15)
  expect_equal(fit$parameters[["theta"]], 2.5, tolerance = 0.15)
  #each ultimate is the latest claims over the share developed (its likelihood equation)
  latest <- C[cbind(seq_len(nrow(C)), rowSums(!is.na(C)))]
  expect_equal(fit$by_origin$latest, unname(latest))
  ages <- rowSums(!is.na(C))
  expect_equal(unname(fit$by_origin$ultimate * fit$growth(ages)), unname(latest), tolerance = 1e-6)
  #the fitted increments of each origin add up to its latest claims
  expect_equal(unname(rowSums(fit$fitted, na.rm = TRUE)), unname(latest), tolerance = 1e-6)
  #the log-likelihood is at its highest in omega and theta
  increments <- cbind(C[, 1], C[, -1] - C[, -ncol(C)])
  loglik <- function(omega, theta) {
    g <- function(a) ifelse(a > 0, 1 / (1 + (theta / pmax(a, 1e-12))^omega), 0)
    total <- 0
    for (i in seq_len(nrow(C))) {
      k <- ages[i]
      share <- g(seq_len(k) - 0.5) - g(seq_len(k) - 1.5)
      mu <- latest[i] / g(k - 0.5) * share
      total <- total + sum(increments[i, seq_len(k)] * log(mu) - mu)
    }
    total
  }
  best <- loglik(fit$parameters[["omega"]], fit$parameters[["theta"]])
  for (step in c(0.995, 1.005)) {
    expect_lt(loglik(fit$parameters[["omega"]] * step, fit$parameters[["theta"]]), best)
    expect_lt(loglik(fit$parameters[["omega"]], fit$parameters[["theta"]] * step), best)
  }
  #sigma^2 is the Pearson statistic over the degrees of freedom
  cells <- !is.na(C)
  expect_equal(fit$parameters[["df"]], sum(cells) - (nrow(C) + 2))
  pearson <- sum((increments[cells] - fit$fitted[cells])^2 / fit$fitted[cells])
  expect_equal(fit$parameters[["sigma2"]], pearson / fit$parameters[["df"]])
  #the standard errors: the process part is sigma^2 times the reserve
  expect_equal(fit$by_origin$process_se, sqrt(fit$parameters[["sigma2"]] * fit$by_origin$ibnr))
  expect_equal(fit$total[["se"]], sqrt(fit$total[["process_se"]]^2 + fit$total[["parameter_se"]]^2))
  expect_true(all(fit$by_origin$parameter_se > 0))
  expect_equal(fit$by_origin$cv, fit$by_origin$se / fit$by_origin$ibnr)
})

test_that("Clark's Cape Cod method has one loss ratio, and a truncation shortens the reserves", {
  premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
  cc <- clark_growth(raa_triangle, premium = premium, method = "cape cod")
  ages <- rowSums(!is.na(raa_triangle))
  latest <- raa_triangle[cbind(1:10, ages)]
  developed <- cc$growth(ages)
  expect_equal(cc$parameters[["loss_ratio"]], sum(latest) / sum(premium * developed), tolerance = 1e-8)
  expect_equal(cc$by_origin$ibnr, unname(premium * cc$parameters[["loss_ratio"]] * (1 - developed)), tolerance = 1e-6)
  expect_equal(cc$total[["ultimate_loss_ratio"]], cc$total[["ultimate"]] / sum(premium))
  expect_identical(nrow(cc$covariance), 3L)
  weibull <- clark_growth(raa_triangle, curve = "weibull")
  expect_match(weibull$method, "Weibull")
  expect_lt(weibull$total[["ibnr"]], clark_growth(raa_triangle)$total[["ibnr"]])
  truncated <- clark_growth(raa_triangle, truncation = 20)
  expect_lt(truncated$total[["ibnr"]], clark_growth(raa_triangle)$total[["ibnr"]])
  expect_equal(truncated$growth(20), 1)
  expect_error(clark_growth(raa_triangle, method = "cape cod"), "needs the premium")
  expect_error(clark_growth(raa_triangle, truncation = 5), "truncation")
  expect_error(clark_growth(raa_triangle, offset = 1), "offset")
  expect_output(print(cc), "Clark Cape Cod method")
})

test_that("the chain ladder for inflation restates the past and inflates the future", {
  #with no inflation it is the chain ladder
  plain <- inflation_adjusted_chain_ladder(raa_triangle, past = 0, future = 0)
  cl <- chain_ladder(raa_triangle)
  expect_equal(plain$by_origin$ibnr, cl$by_origin$ibnr)
  expect_equal(plain$total[["future_inflation"]], 0)
  #a small triangle by hand: 10% past inflation, 20% future
  C <- matrix(c(100, 150, 120, NA), 2, dimnames = list(origin = c("a", "b"), development = 1:2))
  r <- inflation_adjusted_chain_ladder(C, past = 0.1, future = 0.2)
  #origin a's first payment (calendar 1) is restated by 10%; b's first and a's second are today's
  restated <- matrix(c(110, 150, 110 + 20, NA), 2)
  expect_equal(unname(r$restated), restated)
  factor <- 130 / 110
  #b's second payment, 150 * (factor - 1) today, is paid a period ahead: 20% more
  expect_equal(r$by_origin$ibnr_today, c(0, 150 * (factor - 1)))
  expect_equal(r$by_origin$ibnr, c(0, 150 * (factor - 1) * 1.2))
  expect_equal(r$by_origin$ultimate, r$by_origin$latest + r$by_origin$ibnr)
  #higher future inflation, higher reserves; a rate per calendar step
  low <- inflation_adjusted_chain_ladder(raa_triangle, past = 0.05, future = 0.02)$total[["ibnr"]]
  high <- inflation_adjusted_chain_ladder(raa_triangle, past = 0.05, future = 0.10)$total[["ibnr"]]
  expect_lt(low, high)
  stepwise <- inflation_adjusted_chain_ladder(raa_triangle, past = rep(0.05, 9), future = 0.05)
  expect_equal(stepwise$total, inflation_adjusted_chain_ladder(raa_triangle, past = 0.05)$total)
  expect_error(inflation_adjusted_chain_ladder(raa_triangle, past = c(0.1, 0.2)), "steps")
  expect_error(inflation_adjusted_chain_ladder(raa_triangle, future = -2), "future")
  expect_output(print(stepwise), "claims inflation")
})
