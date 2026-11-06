#Claims reserving: the chain ladder with Mack's standard errors, the expected loss ratio methods,
#the bootstrap and building triangles. The reference values are those of the ChainLadder
#package's MackChainLadder() (version 0.2.21) on the same triangles and settings; the totals of
#the Taylor and Ashe triangle with Mack's rule are those of Mack's 1993 paper.

expect_mack <- function(fit, ultimate, se, total_se) {
  expect_equal(fit$by_origin$ultimate, ultimate, tolerance = 1e-8)
  expect_equal(fit$by_origin$se, se, tolerance = 1e-8)
  expect_equal(fit$total[["se"]], total_se, tolerance = 1e-10)
}

test_that("the chain ladder of the RAA triangle has Mack's standard errors", {
  fit <- chain_ladder(raa_triangle)
  expect_s3_class(fit, "netsimr_chain_ladder")
  expect_equal(unname(fit$factors), c(2.9993586513, 1.6235227538, 1.270888115, 1.1716746331, 1.1133848862, 1.0419346379,
                                      1.0332635538, 1.016936481, 1.0092165899), tolerance = 1e-10)
  expect_mack(fit,
              c(18834, 16857.9539, 24083.3709, 28703.1422, 28926.7363, 19501.1032, 17749.3026, 24019.1925, 16044.9841, 18402.4425),
              c(0, 142.9317, 592.1483, 712.8539, 1452.0903, 1994.9878, 2203.8385, 5354.3405, 6331.543, 24565.7757),
              26880.740330)
  expect_equal(fit$total[["process_se"]], 24917.265747, tolerance = 1e-10)
  expect_equal(fit$total[["parameter_se"]], 10084.843499, tolerance = 1e-10)
  expect_equal(fit$total[["ibnr"]], fit$total[["ultimate"]] - sum(fit$by_origin$latest))
  expect_equal(fit$total[["ibnr"]], 52135.2283, tolerance = 1e-8)
  #the factor of 9-10 has one link ratio: its sigma is extrapolated
  expect_match(fit$notes, "Sigma of 9-10", all = FALSE)
  #the projected triangle keeps the observed values and ends with the ultimate
  expect_identical(fit$full_triangle[, 1:10][!is.na(raa_triangle)], raa_triangle[!is.na(raa_triangle)])
  expect_equal(unname(fit$full_triangle[, "Ultimate"]), fit$by_origin$ultimate)
  #the cumulative factors to ultimate, and the share developed of each origin
  expect_equal(unname(fit$cdf), rev(cumprod(rev(c(unname(fit$factors), 1)))))
  expect_equal(fit$by_origin$developed, fit$by_origin$latest / fit$by_origin$ultimate)
})

test_that("Mack's rule gives the standard errors of Mack's paper for the Taylor and Ashe triangle", {
  fit <- chain_ladder(taylor_ashe_triangle, sigma = "Mack")
  expect_mack(fit,
              c(3901463, 5433718.8145, 5378826.2901, 5297905.8208, 4858199.639, 5111171.4577, 5660770.6201, 6784799.012,
                5642266.2633, 4969824.6944),
              c(0, 75535.0408, 121698.5616, 133548.853, 261406.4493, 411009.7039, 558316.8581, 875327.5119, 971257.8065,
                1363154.9117),
              2447094.860835)
  expect_equal(fit$total[["ibnr"]], 18680855.61, tolerance = 1e-9)
})

test_that("simple averages, a fitted tail, left out link ratios and recent periods match MackChainLadder", {
  expect_mack(chain_ladder(taylor_ashe_triangle, average = "simple", sigma = "Mack"),
              c(3901463, 5433718.8145, 5369820.5279, 5283340.0897, 4838368.371, 5124540.4532, 5710060.848, 6818274.0899,
                5664341.215, 5097235.9411),
              c(0, 81817.4698, 129868.1915, 142372.8258, 261454.3444, 431380.8556, 597193.7119, 1009595.8557, 1020971.3316,
                1363261.5393),
              2547153.726794)

  tail <- chain_ladder(raa_triangle, tail = "exponential")
  expect_equal(tail$tail, 1.0094357516, tolerance = 1e-9)
  expect_mack(tail,
              c(19011.7129, 17017.0214, 24310.6156, 28973.9779, 29199.6818, 19685.1107, 17916.7806, 24245.8316, 16196.3806,
                18576.0834),
              c(149.2059, 200.2565, 623.0468, 746.4404, 1479.3088, 2019.5891, 2229.2607, 5407.7101, 6392.7021, 24798.0078),
              27153.693539)

  exclude <- matrix(FALSE, 10, 9)
  exclude[2, 1] <- TRUE
  exclude[5, 3] <- TRUE
  left_out <- chain_ladder(raa_triangle, exclude = exclude, sigma = "Mack")
  expect_equal(unname(left_out$factors[c(1, 3)]), c(2.8167380196, 1.2410992856), tolerance = 1e-10)
  expect_false(any(left_out$used[exclude]))
  expect_mack(left_out,
              c(18834, 16857.9539, 24083.3709, 28703.1422, 28926.7363, 19501.1032, 17749.3026, 23456.1975, 15668.8996, 16876.9022),
              c(0, 206.2201, 623.3767, 747.1752, 1469.4571, 2001.8569, 2209.2421, 5618.8082, 6386.5421, 15664.009),
              19276.066790)

  recent <- chain_ladder(raa_triangle, periods = 5, sigma = "Mack")
  expect_equal(unname(recent$factors[1:2]), c(4.2338477635, 1.7482092806), tolerance = 1e-10)
  expect_mack(recent,
              c(18834, 16857.9539, 24083.3709, 28703.1422, 28926.7363, 19501.1032, 17802.596, 23603.8709, 16978.4938, 27487.939),
              c(0, 206.2201, 623.3767, 747.1752, 1469.4571, 2001.8569, 2279.7871, 3952.8168, 6278.9902, 19530.2777),
              22290.072827)
})

test_that("the chain ladder takes a triangle with more origins than development periods", {
  #three older origins, fully developed, above the Taylor and Ashe triangle
  older <- rbind(taylor_ashe_triangle[1, ] * 0.9, taylor_ashe_triangle[1, ] * 1.1, taylor_ashe_triangle[1, ] * 0.95)
  tall <- rbind(older, taylor_ashe_triangle)
  rownames(tall) <- 1:13
  fit <- chain_ladder(tall, sigma = "Mack")
  expect_equal(fit$by_origin$ibnr[1:4], rep(0, 4))
  #(MackChainLadder gives 2,073,312.37)
  expect_equal(fit$total[["se"]], 2073312.37, tolerance = 1e-8)
})

test_that("the tail curves fit the factors above 1 and extrapolate them", {
  factors <- 1 + 0.5 * exp(-0.7 * (1:9))
  exponential <- reserving_tail_curve(factors, "exponential")
  expect_equal(exponential$tail, prod(1 + 0.5 * exp(-0.7 * (10:109))), tolerance = 1e-12)
  expect_equal(exponential$fitted(3), factors[3])
  power <- 1 + 0.8 * (1:9)^-2.5
  expect_equal(reserving_tail_curve(power, "inverse power")$tail, prod(1 + 0.8 * (10:109)^-2.5), tolerance = 1e-12)
  #fitted from a later factor, the curve leaves out the earlier ones
  expect_identical(reserving_tail_curve(factors, from = 4)$used, 4:9)
  #factors that do not fall towards 1, or a tail over 2, are refused
  expect_error(reserving_tail_curve(1 + 0.01 * (1:9)), "do not fall towards 1")
  expect_error(reserving_tail_curve(1 + 2 * exp(-0.05 * (1:9))), "over 2")
  expect_error(reserving_tail_curve(c(1.5, 0.9, 0.95)), "at least two development factors above 1")
  #an entered tail factor scales every ultimate
  expect_equal(chain_ladder(raa_triangle, tail = 1.05)$by_origin$ultimate,
               chain_ladder(raa_triangle)$by_origin$ultimate * 1.05)
})

test_that("the chain ladder refuses triangles and settings it cannot use", {
  expect_error(chain_ladder("a"), "numeric matrix")
  expect_error(chain_ladder(matrix(1:3, 3, 1)), "two development periods")
  gap <- raa_triangle
  gap[2, 3] <- NA
  expect_error(chain_ladder(gap), "Origin 1982 has a gap")
  empty <- raa_triangle
  empty[10, 1] <- NA
  expect_error(chain_ladder(empty), "Origin 1990 has no values")
  expect_error(chain_ladder(raa_triangle[, 1:10][-1, ]), "last development period has no values")
  all_out <- matrix(FALSE, 10, 9)
  all_out[1, 9] <- TRUE
  expect_error(chain_ladder(raa_triangle, exclude = all_out), "9-10 has no link ratios")
  expect_error(chain_ladder(raa_triangle, exclude = matrix(FALSE, 2, 2)), "logical matrix")
  expect_error(chain_ladder(raa_triangle, periods = 0), "whole number")
  expect_error(chain_ladder(raa_triangle, tail = "spline"), "tail must be")
  expect_error(chain_ladder(raa_triangle, tail = -1), "tail must be")
  #a link ratio from claims of zero is left out, not divided by
  zero <- raa_triangle
  zero[2, 1] <- 0
  fit <- chain_ladder(zero)
  expect_true(is.na(fit$link_ratios[2, 1]))
  expect_true(all(is.finite(fit$factors)))
})

test_that("Bornhuetter-Ferguson, Benktander and Cape Cod follow their formulas", {
  premium <- seq(50000, 68000, by = 2000)
  cl <- chain_ladder(raa_triangle)
  latest <- cl$by_origin$latest
  developed <- 1 / cl$cdf[cl$latest_period]
  bf <- bornhuetter_ferguson(cl, premium, loss_ratio = 0.35)
  expect_equal(bf$by_origin$ultimate, unname(latest + (1 - developed) * premium * 0.35))
  expect_equal(bf$total[["ibnr"]], sum(bf$by_origin$ultimate - latest))
  expect_equal(bf$by_origin$ultimate_loss_ratio, bf$by_origin$ultimate / premium)
  #the same with the triangle and the chain ladder's settings
  expect_equal(bornhuetter_ferguson(raa_triangle, premium, 0.35)$by_origin, bf$by_origin)
  #a loss ratio for each origin
  ratios <- seq(0.3, 0.39, by = 0.01)
  expect_equal(bornhuetter_ferguson(cl, premium, ratios)$by_origin$prior_ultimate, premium * ratios)
  #Benktander: Bornhuetter-Ferguson with the Bornhuetter-Ferguson ultimate as the prior
  bk <- bornhuetter_ferguson(cl, premium, 0.35, iterations = 2)
  expect_identical(bk$method, "Benktander")
  expect_equal(bk$by_origin$ultimate, unname(latest + (1 - developed) * bf$by_origin$ultimate))
  #many iterations tend to the chain ladder
  expect_equal(bornhuetter_ferguson(cl, premium, 0.35, iterations = 2000)$by_origin$ultimate, cl$by_origin$ultimate,
               tolerance = 1e-6)

  cc <- cape_cod(cl, premium)
  loss_ratio <- sum(latest) / sum(premium * developed)
  expect_equal(cc$total[["loss_ratio"]], loss_ratio)
  expect_equal(cc$by_origin$ultimate, unname(latest + (1 - developed) * premium * loss_ratio))
  #with decay 0 each origin's loss ratio is its own: the chain ladder
  expect_equal(cape_cod(cl, premium, decay = 0)$by_origin$ultimate, cl$by_origin$ultimate)
  #with decay between, the loss ratios weigh the nearby origins most
  decayed <- cape_cod(cl, premium, decay = 0.5)$by_origin$loss_ratio
  weights <- 0.5^abs(outer(1:10, 1:10, "-"))
  expect_equal(decayed, as.vector((weights %*% latest) / (weights %*% (premium * developed))))

  expect_error(bornhuetter_ferguson(cl, premium[-1], 0.35), "one for each origin")
  expect_error(bornhuetter_ferguson(cl, premium, -0.1), "non-negative")
  expect_error(bornhuetter_ferguson(cl, premium, 0.35, iterations = 0), "iterations")
  expect_error(cape_cod(cl, premium, decay = 2), "decay")
})

test_that("the bootstrap is reproducible, keeps the session's random numbers and centres on the chain ladder", {
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  first <- bootstrap_chain_ladder(raa_triangle, n = 500, seed = 1)
  #the session's stream goes on as if the bootstrap had not run
  expect_identical(stats::runif(1), before)
  expect_identical(bootstrap_chain_ladder(raa_triangle, n = 500, seed = 1)$total_reserves, first$total_reserves)
  expect_false(identical(bootstrap_chain_ladder(raa_triangle, n = 500, seed = 2)$total_reserves, first$total_reserves))
  expect_equal(dim(first$reserves), c(500, 10))
  expect_equal(first$total_reserves, rowSums(first$reserves))
  #the oldest origin is fully developed: no reserve
  expect_true(all(first$reserves[, 1] == 0))
  expect_named(first$total, c("latest", "ultimate", "ibnr", "se", "cv", "p75", "p90", "p95", "p99", "p99.5"))

  #the scale parameter: the Pearson residuals' sum of squares over N - p (55 values, 19 parameters)
  cl <- chain_ladder(raa_triangle)
  fitted <- first$fitted
  observed <- !is.na(raa_triangle)
  increments <- cbind(raa_triangle[, 1], raa_triangle[, -1] - raa_triangle[, -10])
  pearson <- (increments - fitted) / sqrt(abs(fitted))
  expect_equal(first$scale, sum(pearson[observed]^2) / (55 - 19))
  expect_equal(first$residuals[observed], pearson[observed] * sqrt(55 / 36))

  #the mean reserve is near the chain ladder's; its standard deviation is that of the
  #over-dispersed Poisson model, near 3.0 million (ChainLadder's BootChainLadder gives 3.00
  #million with 20,000 simulations, against Mack's 2.44 million)
  big <- bootstrap_chain_ladder(taylor_ashe_triangle, n = 20000, seed = 3)
  mack <- chain_ladder(taylor_ashe_triangle)
  expect_equal(big$total[["ibnr"]], mack$total[["ibnr"]], tolerance = 0.03)
  expect_equal(big$total[["se"]], 3.0e6, tolerance = 0.06)
  expect_true(all(diff(unlist(big$total[c("p75", "p90", "p95", "p99", "p99.5")])) > 0))
  odp <- bootstrap_chain_ladder(taylor_ashe_triangle, n = 20000, process = "odp", seed = 3)
  expect_equal(odp$total[["ibnr"]], mack$total[["ibnr"]], tolerance = 0.03)
  #an over-dispersed Poisson draw is a multiple of the scale parameter
  expect_equal(odp$reserves[1:50, 10] / odp$scale, round(odp$reserves[1:50, 10] / odp$scale))
})

test_that("the bootstrap applies a tail factor and refuses what it cannot use", {
  plain <- bootstrap_chain_ladder(raa_triangle, n = 200, seed = 4)
  tailed <- bootstrap_chain_ladder(raa_triangle, n = 200, tail = 1.05, seed = 4)
  latest <- plain$by_origin$latest
  expect_equal(tailed$reserves, (rep(latest, each = 200) + plain$reserves) * 1.05 - rep(latest, each = 200))
  expect_error(bootstrap_chain_ladder(raa_triangle, n = 1), "at least 2")
  expect_error(bootstrap_chain_ladder(raa_triangle, n = 10, seed = 1.5), "seed")
  expect_error(bootstrap_chain_ladder(raa_triangle, n = 10, probs = 2), "probs")
  expect_error(bootstrap_chain_ladder(raa_triangle[9:10, 1:2], n = 10), "too few values")
})

test_that("a triangle is built from a long or a wide table", {
  long <- data.frame(year = c(2020, 2020, 2020, 2021, 2021, 2022), dev = c(1, 2, 3, 1, 2, 1), paid = c(10, 5, 1, 12, 6, 15))
  cumulative <- as_triangle(long, "year", "dev", "paid", cumulative = FALSE)
  expect_equal(unname(cumulative), matrix(c(10, 12, 15, 15, 18, NA, 16, NA, NA), 3))
  expect_identical(dimnames(cumulative), list(origin = c("2020", "2021", "2022"), development = c("1", "2", "3")))
  #payments of the same cell are added up, in any order; a cell of the past without any is zero
  payments <- data.frame(year = c(2022, 2020, 2020, 2020, 2021, 2020), dev = c(1, 1, 2, 1, 1, 3), paid = c(15, 4, 5, 6, 12, 1))
  expect_equal(unname(as_triangle(payments, "year", "dev", "paid", cumulative = FALSE)),
               matrix(c(10, 12, 15, 15, 12, NA, 16, NA, NA), 3))
  #cumulative values, development periods as ages in months
  ages <- data.frame(o = c("A", "A", "B"), age = c(12, 24, 12), v = c(10, 15, 12))
  expect_equal(unname(as_triangle(ages, "o", "age", "v")), matrix(c(10, 12, 15, NA), 2))
  expect_error(as_triangle(rbind(ages, ages[1, ]), "o", "age", "v"), "more than one value")
  expect_error(as_triangle(ages[-1, ], "o", "age", "v"), "no value at development period 12")
  expect_error(as_triangle(ages, "o", "age", "missing"), "no column missing")

  #the wide form, with the origin in a column
  wide <- data.frame(origin = c("2020", "2021"), d1 = c(10, 12), d2 = c(15, NA))
  expect_equal(unname(as_triangle(wide, "origin", NULL, NULL)), matrix(c(10, 12, 15, NA), 2))
  expect_equal(unname(as_triangle(wide, "origin", NULL, NULL, cumulative = FALSE)), matrix(c(10, 12, 25, NA), 2))
  #the simulated motor claims make a twelve-year triangle, the RAA triangle is its own wide form
  motor <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
  expect_equal(dim(motor), c(12, 12))
  expect_equal(sum(motor[cbind(1:12, 12:1)]), sum(simulated_motor_claims$paid))
  raa <- data.frame(origin = rownames(raa_triangle), raa_triangle, check.names = FALSE)
  expect_equal(unname(as_triangle(raa, "origin", NULL, NULL)), unname(raa_triangle))
})

test_that("a reserving result prints its table with a total", {
  printed <- utils::capture.output(print(chain_ladder(raa_triangle)))
  expect_match(printed[1], "Chain ladder")
  expect_match(printed, "Total 160,987", all = FALSE)
  premium <- seq(50000, 68000, by = 2000)
  expect_match(utils::capture.output(print(cape_cod(raa_triangle, premium))), "Total", all = FALSE)
  expect_match(utils::capture.output(print(bootstrap_chain_ladder(raa_triangle, n = 100, seed = 1))), "Total", all = FALSE)
})
