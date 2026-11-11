#discounted reserves, the risk adjustment and the Solvency II standard formula

motor_paid <- function() {
  as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
}

test_that("the standard formula's parameters are those of the regulation", {
  expect_identical(nrow(sii_nonlife_segments), 12L)
  expect_equal(sii_nonlife_segments$premium_sd[c(6, 7, 8)], c(0.19, 0.083, 0.064))
  expect_equal(sii_nonlife_segments$reserve_sd[c(6, 7, 8)], c(0.172, 0.055, 0.22))
  expect_true(isSymmetric(unname(sii_nonlife_correlation)))
  expect_equal(unname(diag(sii_nonlife_correlation)), rep(1, 12))
  expect_equal(sii_nonlife_correlation[9, 11], 0.5)
  expect_equal(sii_nonlife_correlation[4, 12], 0.5)
  expect_true(all(sii_nonlife_correlation[upper.tri(sii_nonlife_correlation)] %in% c(0.25, 0.5)))
})

test_that("the standard formula combines premium and reserve risk, and the segments, as Articles 115 to 117 say", {
  one <- sii_premium_reserve_risk(data.frame(segment = 4, premium = 100, reserve = 50))
  expect_equal(one$total[["capital"]], 3 * sqrt((0.08 * 100)^2 + 0.08 * 0.10 * 100 * 50 + (0.10 * 50)^2))
  expect_equal(one$total[["diversification"]], 0)
  # two segments: 3 * sqrt(sum of CorrS sigma_s V_s sigma_t V_t)
  two <- sii_premium_reserve_risk(data.frame(segment = c(1, 2), premium = c(100, 60), reserve = c(80, 20)))
  sv <- two$by_segment$sd * two$by_segment$volume
  expect_equal(two$total[["capital"]], 3 * sqrt(sv[1]^2 + sv[2]^2 + 2 * 0.5 * sv[1] * sv[2]))
  expect_equal(two$total[["diversification"]], sum(3 * sv) - two$total[["capital"]])
  expect_equal(two$reserve_risk, 3 * sqrt((0.09 * 80)^2 + (0.08 * 20)^2 + 2 * 0.5 * 0.09 * 80 * 0.08 * 20))
  # the adjustment for non-proportional reinsurance, and geographical diversification
  np <- sii_premium_reserve_risk(data.frame(segment = 1, premium = 100, reserve = 0), np_adjustment = 1)
  expect_equal(np$total[["capital"]], 3 * 0.8 * 0.10 * 100)
  div <- sii_premium_reserve_risk(data.frame(segment = 1, premium = 100, reserve = 0, diversification = 0.6))
  expect_equal(div$total[["volume"]], 100 * (0.75 + 0.25 * 0.6))
  usp <- sii_premium_reserve_risk(data.frame(segment = 4, premium = 100, reserve = 0), premium_sd = c("4" = 0.05))
  expect_equal(usp$total[["capital"]], 3 * 0.05 * 100)
  expect_error(sii_premium_reserve_risk(data.frame(segment = 13, premium = 1, reserve = 1)), "1 to 12")
  expect_error(sii_premium_reserve_risk(data.frame(segment = 2, premium = 1, reserve = 1), np_adjustment = 2), "1, 4 and 5")
  expect_error(sii_premium_reserve_risk(data.frame(segment = 1, premium = -1, reserve = 1)), "at least 0")
})

test_that("the discounted reserves are the chain ladder's payments, discounted at their terms", {
  paid <- motor_paid()
  cl <- chain_ladder(paid)
  flat <- discount_reserves(paid, 0)
  expect_equal(flat$total[["reserve"]], cl$total[["ibnr"]])
  expect_equal(flat$total[["discounted"]], flat$total[["reserve"]])
  expect_equal(flat$by_origin$reserve, cl$by_origin$ibnr)
  be <- discount_reserves(cl, yield_curve = c(0.03, 0.04), timing = 0.5)
  cf <- be$cash_flows
  expect_equal(cf$term, seq_len(nrow(cf)) - 0.5)
  expect_equal(cf$spot_rate[1:3], c(0.03, 0.035, 0.04))
  expect_equal(cf$discount_factor, (1 + cf$spot_rate)^(-cf$term))
  expect_equal(be$total[["discounted"]], sum(cf$payments * cf$discount_factor))
  expect_equal(sum(cf$payments), cl$total[["ibnr"]])
  # payments at the end of each year are discounted more; quarterly periods less
  expect_lt(discount_reserves(cl, 0.03, timing = 1)$total[["discounted"]], discount_reserves(cl, 0.03, timing = 0)$total[["discounted"]])
  expect_gt(discount_reserves(cl, 0.03, period = 0.25)$total[["discounted"]], discount_reserves(cl, 0.03)$total[["discounted"]])
  # a tail is paid after the triangle
  tailed <- discount_reserves(paid, 0, tail = 1.05, tail_periods = 2)
  expect_equal(tailed$total[["reserve"]], chain_ladder(paid, tail = 1.05)$total[["ibnr"]])
  expect_output(print(be), "discounted")
  expect_error(discount_reserves(cl, -1), "above -1")
  expect_error(discount_reserves(cl, 0.03, timing = 2), "share of a period")
  # (an endless tail would spread its payments over infinitely many periods)
  expect_error(discount_reserves(paid, 0, tail = 1.05, tail_periods = Inf), "whole number of at least 1")
  expect_error(discount_reserves(paid, 0, tail = 1.05, tail_periods = 1.5), "whole number of at least 1")
})

test_that("the risk adjustment is a percentile of the bootstrap, or the cost of capital over the run-off", {
  paid <- motor_paid()
  boot <- bootstrap_chain_ladder(paid, n = 1000, seed = 1)
  be <- discount_reserves(paid, 0.03)
  scale <- be$total[["discounted"]] / be$total[["reserve"]]
  p <- risk_adjustment("percentile", boot = boot, level = 0.75, discounted = be)
  expect_equal(p$risk_adjustment, (stats::quantile(boot$total_reserves, 0.75, names = FALSE) - mean(boot$total_reserves)) * scale)
  expect_equal(p$confidence_level, 0.75)
  # cost of capital, by hand: the capital runs off with the reserve still to pay at the start of each year
  cf <- be$cash_flows
  outstanding <- (sum(cf$payments) - c(0, cumsum(cf$payments)[-nrow(cf)])) / sum(cf$payments)
  years <- seq_len(nrow(cf))
  coc <- risk_adjustment("cost of capital", boot = boot, discounted = be, capital = 1e6, coc_rate = 0.06)
  expect_equal(coc$risk_adjustment, sum(0.06 * 1e6 * outstanding * 1.03^(-years)))
  expect_true(coc$confidence_level > 0.5 && coc$confidence_level < 1)
  expect_true(is.na(risk_adjustment("cost of capital", discounted = be, capital = 1e6)$confidence_level))
  expect_error(risk_adjustment("percentile"), "needs the bootstrap")
  expect_error(risk_adjustment("cost of capital", discounted = be), "capital")
})
