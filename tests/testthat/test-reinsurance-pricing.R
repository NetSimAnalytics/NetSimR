#excess of loss pricing: exposure curves, exposure rating, burning cost, Pareto rating,
#technical premium and credibility

test_that("the MBBEFD curves match the mbbefd package", {
  #values of mbbefd::ecMBBEFD() (version 0.8.14), for the Swiss Re curves c = 1.5, 3 and 5
  #and the special cases b = 1 and bg = 1
  x <- c(0.05, 0.2, 0.5, 0.8)
  expect_equal(swiss_re_curve(x, 1.5), c(0.119475628505683, 0.346846892486520, 0.634936774666991, 0.861275325208794), tolerance = 1e-12)
  expect_equal(swiss_re_curve(x, "Y3"), c(0.282670389358040, 0.549307865438635, 0.776880905373500, 0.920796396366605), tolerance = 1e-12)
  expect_equal(swiss_re_curve(x, "Lloyds"), c(0.569039817615682, 0.796716102755684, 0.927062059134251, 0.979763238467497), tolerance = 1e-12)
  expect_equal(mbbefd_curve(x, 1, 5), c(0.113282752559378, 0.365212388971971, 0.682606194485985, 0.891668149608153), tolerance = 1e-12)
  expect_equal(mbbefd_curve(x, 0.2, 5), c(0.096648956761765, 0.344025420402881, 0.690983005625053, 0.905067584634696), tolerance = 1e-12)
  expect_equal(mbbefd_curve(x, 2, 10), c(0.171191438688513, 0.447846670668845, 0.728596917341720, 0.905765340184491), tolerance = 1e-12)
})

test_that("an MBBEFD curve is the limited expected value of its distribution", {
  #G(x) = E[min(X, x)] / E[X] for the degree of loss X, whose survival is
  #(1 - b) / ((g - 1) b^(1 - x) + 1 - gb) below 1
  p <- swiss_re_parameters(3)
  survival <- function(t) (1 - p$b) / ((p$g - 1) * p$b^(1 - t) + 1 - p$g * p$b)
  lev <- function(x) stats::integrate(survival, 0, x, rel.tol = 1e-10)$value
  x <- c(0.1, 0.3, 0.7)
  expect_equal(mbbefd_curve(x, p$b, p$g), vapply(x, lev, numeric(1)) / lev(1), tolerance = 1e-8)
})

test_that("the MBBEFD curves run from 0 to 1, rise and take every special case", {
  x <- seq(0, 1, by = 0.05)
  for (c in c(0, 0.5, 1.5, 2, 3, 4, 5, 8)) {
    g <- swiss_re_curve(x, c)
    expect_equal(g[c(1, length(g))], c(0, 1))
    expect_true(all(diff(g) > 0))
  }
  #total losses only: g = 1 or b = 0
  expect_equal(mbbefd_curve(x, 3, 1), x)
  expect_equal(mbbefd_curve(x, 0, 10), x)
  #c = 0 is total losses only too
  expect_equal(swiss_re_curve(x, 0), x, tolerance = 1e-12)
  #x above 1 counts as 1, and the arguments are recycled
  expect_equal(mbbefd_curve(c(1.5, 1), 2, 10), c(1, 1))
  expect_length(mbbefd_curve(0.5, c(1, 2, 3), 10), 3)
  #the named curves and their parameters
  p <- swiss_re_parameters(c("Y1", "Y2", "Y3", "Y4", "Lloyds"))
  expect_equal(p$c, c(1.5, 2, 3, 4, 5))
  expect_equal(p$b, exp(3.1 - 0.15 * (1 + p$c) * p$c))
  expect_equal(p$g, exp((0.78 + 0.12 * p$c) * p$c))
  expect_equal(swiss_re_parameters("lloyd's")$c, 5)
  expect_error(swiss_re_parameters("Y5"), "names")
  expect_error(mbbefd_curve(-0.1, 2, 10), "x must be")
  expect_error(mbbefd_curve(0.5, -1, 10), "b must be")
  expect_error(mbbefd_curve(0.5, 2, 0.5), "g must be")
})

test_that("property exposure rating shares each band's expected loss by the curve", {
  profile <- data.frame(band = c("small", "large"), sum_insured = c(2e6, 1e7), premium = c(1e6, 5e5))
  rating <- property_exposure_rating(profile, deductible = 1e6, limit = 3e6, curve = "Y3", loss_ratio = 0.6)
  expected_share <- c(swiss_re_curve(1, 3) - swiss_re_curve(0.5, 3), swiss_re_curve(0.4, 3) - swiss_re_curve(0.1, 3))
  expect_equal(rating$bands$layer_share, expected_share)
  expect_equal(rating$bands$deductible_share, c(0.5, 0.1))
  expect_equal(rating$bands$exhaustion_share, c(1, 0.4))
  expect_equal(rating$bands$layer_loss, c(6e5, 3e5) * expected_share)
  expect_equal(rating$expected_loss, sum(rating$bands$layer_loss))
  expect_equal(rating$rate, rating$expected_loss / 1.5e6)
  expect_equal(rating$ground_up_loss, 9e5)
  expect_identical(rating$bands$band, c("small", "large"))
  expect_s3_class(rating, "netsimr_exposure_rating")
  #the loss ratio from the profile, and a curve given by b and g or by c
  profile$loss_ratio <- c(0.5, 0.7)
  by_column <- property_exposure_rating(profile, 1e6, 3e6, curve = 3)
  expect_equal(by_column$bands$expected_loss, c(5e5, 3.5e5))
  p <- swiss_re_parameters(3)
  expect_equal(property_exposure_rating(profile, 1e6, 3e6, curve = c(b = p$b, g = p$g))$expected_loss, by_column$expected_loss)
  #a layer above every sum insured takes nothing; one from 0 to above them takes all
  expect_equal(property_exposure_rating(profile, 2e7, 1e7)$expected_loss, 0)
  expect_equal(property_exposure_rating(profile, 0, Inf)$expected_loss, sum(profile$premium * profile$loss_ratio))
  expect_error(property_exposure_rating(profile[, c("band", "premium")], 1e6, 3e6), "sum_insured")
  expect_error(property_exposure_rating(profile[, c("sum_insured", "premium")], 1e6, 3e6), "loss_ratio")
  expect_error(property_exposure_rating(profile, 1e6, 0), "limit must be")
  expect_error(property_exposure_rating(profile, 1e6, 3e6, curve = "Y9"), "names")
  expect_output(print(rating), "Expected layer loss")
})

test_that("casualty exposure rating uses the limited expected value of the claim sizes", {
  profile <- data.frame(policy_limit = c(1e6, 5e6), premium = c(2e6, 1e6))
  rating <- casualty_exposure_rating(profile, deductible = 5e5, limit = 2e6, severity = "LogNormal",
                                     severity_params = c(meanlog = 11, sdlog = 1.8), loss_ratio = 0.7)
  #the limited expected value by integrating the survival function
  lev <- function(x) stats::integrate(function(t) stats::plnorm(t, 11, 1.8, lower.tail = FALSE), 0, x, rel.tol = 1e-10)$value
  share <- c((lev(1e6) - lev(5e5)) / lev(1e6), (lev(2.5e6) - lev(5e5)) / lev(5e6))
  expect_equal(rating$bands$layer_share, share, tolerance = 1e-7)
  expect_equal(rating$expected_loss, sum(c(1.4e6, 7e5) * rating$bands$layer_share))
  #the other claim size distributions, with the simulator's parameters
  gamma <- casualty_exposure_rating(profile, 5e5, 2e6, "Gamma", c(shape = 0.5, scale = 4e5), loss_ratio = 0.7)
  lev_gamma <- function(x) stats::integrate(function(t) stats::pgamma(t, 0.5, scale = 4e5, lower.tail = FALSE), 0, x)$value
  expect_equal(gamma$bands$layer_share[1], (lev_gamma(1e6) - lev_gamma(5e5)) / lev_gamma(1e6), tolerance = 1e-6)
  pareto <- casualty_exposure_rating(profile, 5e5, 2e6, "Pareto", c(alpha = 1.5, x_m = 1e5), loss_ratio = 0.7)
  expect_equal(pareto$bands$layer_share[2],
               (ParetoCappedMean(2.5e6, 1e5, 1.5) - ParetoCappedMean(5e5, 1e5, 1.5)) / ParetoCappedMean(5e6, 1e5, 1.5))
  exponential <- casualty_exposure_rating(profile, 5e5, 2e6, "Exponential", 1e-6, loss_ratio = 0.7)
  expect_equal(exponential$bands$layer_share[1], (exp(-0.5) - exp(-1)) / (1 - exp(-1)))
  #a policy limit under the deductible puts nothing in the layer
  low <- casualty_exposure_rating(data.frame(policy_limit = 4e5, premium = 1e6), 5e5, 2e6, "LogNormal", c(11, 1.8), 0.7)
  expect_equal(low$expected_loss, 0)
  expect_error(casualty_exposure_rating(profile, 5e5, 2e6, "Normal", c(1, 1), 0.7), "Unknown severity")
  expect_error(casualty_exposure_rating(profile, 5e5, 2e6, "LogNormal", c(11, -1), 0.7), "severity_params")
  expect_error(casualty_exposure_rating(data.frame(policy_limit = Inf, premium = 1), 0, 1, "Pareto", c(0.8, 1), 0.7),
               "infinite mean")
  expect_output(print(rating), "casualty")
})

test_that("the burning cost indexes, layers, develops and divides by the on-level premium", {
  losses <- data.frame(year = c(2020, 2020, 2021, 2022), amount = c(1.5e6, 4e5, 2e6, 9e6))
  premium <- data.frame(year = 2020:2022, premium = c(1e7, 1.2e7, 1.5e7))
  bc <- burning_cost(losses, premium, deductible = 1e6, limit = 4e6, inflation = 0.1, rating_year = 2023,
                     development = c(`2020` = 1, `2021` = 1.1, `2022` = 1.5), premium_index = c(1.2, 1.1, 1))
  index <- 1.1^c(3, 3, 2, 1)
  layer <- pmin(pmax(losses$amount * index - 1e6, 0), 4e6)
  expect_equal(bc$losses$indexed, losses$amount * index)
  expect_equal(bc$losses$layer, layer)
  by_year <- c(sum(layer[1:2]), layer[3] * 1.1, layer[4] * 1.5)
  expect_equal(bc$by_year$developed_losses, by_year)
  expect_equal(bc$by_year$onlevel_premium, c(1.2e7, 1.32e7, 1.5e7))
  expect_equal(bc$by_year$burning_cost, by_year / c(1.2e7, 1.32e7, 1.5e7))
  expect_equal(bc$by_year$layer_count, c(1, 1, 1))
  expect_equal(bc$by_year$losses, c(2, 1, 1))
  expect_equal(bc$rate, sum(by_year) / sum(c(1.2e7, 1.32e7, 1.5e7)))
  expect_true(is.na(bc$expected_loss))
  expect_equal(bc$n_layer_losses, 3)
  #equal weights, a left out year, the rating premium and an inflation index
  equal <- burning_cost(losses, premium, 1e6, 4e6, inflation = 0.1, rating_year = 2023, exclude = 2021,
                        weights = "equal", rating_premium = 2e7)
  rates <- c(sum(layer[1:2]) / 1e7, layer[4] / 1.5e7)
  expect_equal(equal$rate, mean(rates))
  expect_equal(equal$expected_loss, mean(rates) * 2e7)
  expect_equal(equal$by_year$included, c(TRUE, FALSE, TRUE))
  expect_equal(equal$by_year$at_rating_premium, equal$by_year$aggregate_losses * 2e7 / premium$premium)
  indexed <- burning_cost(losses, premium, 1e6, 4e6, inflation = c(`2020` = 100, `2021` = 110, `2022` = 121, `2023` = 133.1),
                          rating_year = 2023)
  expect_equal(indexed$losses$indexed, losses$amount * index)
  #the annual aggregate deductible and limit apply after the development
  agg <- burning_cost(losses, premium, 1e6, 4e6, aggregate_deductible = 5e5, aggregate_limit = 3e6, inflation = 0.1,
                      rating_year = 2023)
  expect_equal(agg$by_year$aggregate_losses, pmin(pmax(agg$by_year$developed_losses - 5e5, 0), 3e6))
  #premium as numbers named by year, and no losses at all
  named <- burning_cost(losses, c(`2020` = 1e7, `2021` = 1.2e7, `2022` = 1.5e7), 1e6, 4e6, inflation = 0.1, rating_year = 2023)
  expect_equal(named$rate, burning_cost(losses, premium, 1e6, 4e6, inflation = 0.1, rating_year = 2023)$rate)
  none <- burning_cost(losses[0, ], premium, 1e6, 4e6)
  expect_equal(none$rate, 0)
  expect_equal(none$rating_year, 2023)
  expect_output(print(bc), "Burning cost")
})

test_that("the burning cost warns of a threshold above the deductible and checks its inputs", {
  losses <- data.frame(year = c(2020, 2022), amount = c(2e6, 3e6))
  premium <- data.frame(year = 2020:2022, premium = c(1e7, 1e7, 1e7))
  expect_warning(burning_cost(losses, premium, 1e6, 4e6, inflation = 0.1, rating_year = 2025, threshold = 8e5),
                 "2020, 2021, 2022")
  expect_no_warning(burning_cost(losses, premium, 1e6, 4e6, inflation = 0.1, rating_year = 2025, threshold = 5e5))
  expect_error(burning_cost(data.frame(year = 2019, amount = 1), premium, 1e6, 4e6), "no premium: 2019")
  expect_error(burning_cost(data.frame(year = 2020, amount = -1), premium, 1e6, 4e6), "negative")
  expect_error(burning_cost(losses, data.frame(year = 2020:2022, premium = c(1, 0, 1)), 1e6, 4e6), "positive")
  expect_error(burning_cost(losses, premium, 1e6, 4e6, development = c(1, 1)), "one per year")
  expect_error(burning_cost(losses, premium, 1e6, 4e6, development = c(`2020` = 1)), "no value for 2021, 2022")
  expect_error(burning_cost(losses, premium, 1e6, 4e6, exclude = 2020:2022), "no years")
  expect_error(burning_cost(losses[, "year", drop = FALSE], premium, 1e6, 4e6), "amount")
  expect_error(burning_cost(losses, premium, 1e6, 4e6, inflation = c(`2020` = 1)), "no value")
})

test_that("the Pareto rating estimates alpha and the frequency above the threshold", {
  losses <- data.frame(year = c(2020, 2020, 2021, 2021, 2021, 2022), amount = c(1e6, 2.5e6, 8e5, 1.2e6, 6e5, 4e6))
  premium <- data.frame(year = 2020:2022, premium = c(1e7, 1e7, 2e7))
  pr <- pareto_rating(losses, premium, threshold = 7e5, deductible = 1e6, limit = 4e6, inflation = 0, rating_year = 2023,
                      development = c(1, 1, 2), rating_premium = 3e7)
  above <- losses$amount[losses$amount > 7e5]
  alpha <- (length(above) - 1) / sum(log(above / 7e5))
  expect_equal(pr$alpha, alpha)
  expect_equal(pr$n, 5)
  expect_equal(pr$by_year$large_losses, c(2, 2, 1))
  expect_equal(pr$frequency, (2 + 2 + 2) / 4e7)
  severity <- ParetoCappedMean(5e6, 7e5, alpha) - ParetoCappedMean(1e6, 7e5, alpha)
  expect_equal(pr$layer_severity, severity)
  expect_equal(pr$rate, 6 / 4e7 * severity)
  expect_equal(pr$expected_loss, pr$rate * 3e7)
  expect_equal(pr$expected_count, 6 / 4e7 * 3e7)
  #a given alpha, a left out year
  given <- pareto_rating(losses, premium, 7e5, 1e6, 4e6, exclude = 2022, alpha = 2)
  expect_equal(given$alpha, 2)
  expect_equal(given$alpha_estimated, 3 / sum(log(c(1e6, 2.5e6, 8e5, 1.2e6) / 7e5)))
  expect_equal(given$frequency, 4 / 2e7)
  #the expected part of a Pareto loss in the layer, by simulation
  set.seed(1)
  x <- 7e5 * stats::runif(4e5)^(-1 / alpha)
  expect_equal(mean(pmin(pmax(x - 1e6, 0), 4e6)), severity, tolerance = 0.02)
  expect_error(pareto_rating(losses, premium, 7e5, 5e5, 4e6), "at least the threshold")
  expect_error(pareto_rating(losses, premium, 3e6, 3e6, 4e6), "at least two losses")
  expect_output(print(pr), "Alpha")
  expect_output(print(given), "estimated")
})

test_that("on the example data, the experience ratings come close to the true rate", {
  #the losses are Pareto with alpha 1.7 above 500,000 in 2025 money, 0.16 per million of
  #on-level premium; the true rate of 4m xs 1m:
  truth <- 0.16 / 1e6 * (ParetoCappedMean(5e6, 5e5, 1.7) - ParetoCappedMean(1e6, 5e5, 1.7))
  development <- stats::setNames(xl_premium$development, xl_premium$year)
  index <- stats::setNames(xl_premium$rate_index, xl_premium$year)
  premium <- xl_premium[, c("year", "premium")]
  bc <- burning_cost(xl_large_losses, premium, 1e6, 4e6, inflation = 0.04, development = development,
                     premium_index = index, threshold = 5e5)
  #(the Pareto's threshold must be at least the reporting threshold indexed from 2015, about
  #740,000: below it the oldest years miss losses, and alpha comes out too low)
  pr <- pareto_rating(xl_large_losses, premium, 7.5e5, 1e6, 4e6, inflation = 0.04, development = development,
                      premium_index = index)
  biased <- pareto_rating(xl_large_losses, premium, 5e5, 1e6, 4e6, inflation = 0.04, development = development,
                          premium_index = index)
  expect_lt(biased$alpha, pr$alpha)
  expect_equal(bc$rate, truth, tolerance = 0.3)
  expect_equal(pr$rate, truth, tolerance = 0.3)
  expect_equal(pr$alpha, 1.7, tolerance = 0.15)
  expect_identical(names(xl_large_losses), c("claim", "year", "amount"))
  expect_true(all(xl_large_losses$amount >= 5e5))
  expect_identical(xl_premium$year, 2015:2024)
  expect_identical(names(xl_property_profile), c("band", "lower", "upper", "risks", "sum_insured", "premium"))
  expect_identical(names(xl_casualty_profile), c("policy_limit", "premium"))
})

test_that("the technical premium loads the expected loss and takes the reinstatement premium off", {
  tp <- technical_premium(1.2e6, 4e6, loading = 0.25, reinstatements = 1, reinstatement_rate = 1)
  expect_equal(tp$total_premium, 1.6e6)
  expect_equal(tp$used, 0.3)
  expect_equal(tp$upfront_premium, 1.6e6 / 1.3)
  expect_equal(tp$reinstatement_premium, 1.6e6 - 1.6e6 / 1.3)
  expect_equal(tp$upfront_premium + tp$upfront_premium * 1 * 0.3, tp$total_premium)
  expect_equal(tp$rate_on_line, tp$upfront_premium / 4e6)
  expect_equal(tp$payback, 4e6 / tp$upfront_premium)
  #no reinstatements; free ones; the reinstated cover used capped at the reinstatements
  none <- technical_premium(1.2e6, 4e6, loading = 0.2)
  expect_equal(none$upfront_premium, 1.5e6)
  expect_equal(none$reinstatement_premium, 0)
  expect_equal(technical_premium(1.2e6, 4e6, reinstatements = 2, reinstatement_rate = 0)$upfront_premium, 1.2e6)
  expect_equal(technical_premium(1e7, 4e6, reinstatements = 1)$used, 1)
  expect_equal(technical_premium(1e7, 4e6, reinstatements = Inf)$used, 2.5)
  expect_equal(technical_premium(1.2e6, 4e6, reinstatements = 1, used = 0.2)$upfront_premium, 1.2e6 / 1.2)
  expect_equal(technical_premium(0, 4e6)$payback, Inf)
  expect_error(technical_premium(1e6, 4e6, loading = 1), "below 1")
  expect_error(technical_premium(1e6, 4e6, reinstatements = 1, used = 2), "used must be")
  expect_error(technical_premium(-1, 4e6), "expected_loss")
})

test_that("the credibility weight is the square root of the losses over the full standard", {
  expect_equal(credibility_weight(c(0, 270.5, 1082, 5000)), c(0, 0.5, 1, 1))
  expect_equal(credibility_weight(25, full = 100), 0.5)
  expect_error(credibility_weight(-1), "n must be")
  expect_error(credibility_weight(1, full = 0), "full must be")
})
