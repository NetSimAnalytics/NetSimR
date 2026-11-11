#zero-inflated Poisson and Negative Binomial: the simulator's distributions and the fits of the Frequency tab

test_that("the zero-inflated moments are those of the mixture with a point at zero", {
  #mean (1 - p) m and variance (1 - p) (v + p m^2), for a base distribution of mean m and variance v
  expect_equal(distribution_moments(freq_dist_options, "Zero_Inflated_Poisson", c(4, 0.3)),
               c(mean = 0.7 * 4, sd = sqrt(0.7 * (4 + 0.3 * 16))))
  expect_equal(distribution_moments(freq_dist_options, "Zero_Inflated_Negative_Binomial", c(2, 1.5, 0.25)),
               c(mean = 0.75 * 3, sd = sqrt(0.75 * (3 * 2.5 + 0.25 * 9))))
  #no extra zeros: the base distribution
  expect_equal(distribution_moments(freq_dist_options, "Zero_Inflated_Poisson", c(4, 0)),
               distribution_moments(freq_dist_options, "Poisson", 4))
  expect_equal(distribution_moments(freq_dist_options, "Zero_Inflated_Negative_Binomial", c(2, 1.5, 0)),
               distribution_moments(freq_dist_options, "Negative_Binomial", c(2, 1.5)))
})

test_that("the zero-inflated counts are simulated with their moments and their zeros", {
  n <- 60000
  zip <- simulate_claims(n, "Zero-inflated Poisson", c(lambda = 4, zero_prob = 0.3), "Gamma", c(2, 500), seed = 1)$claim_counts
  expect_equal(mean(zip), 2.8, tolerance = 0.02)
  expect_equal(stats::sd(zip), sqrt(0.7 * (4 + 0.3 * 16)), tolerance = 0.02)
  expect_equal(mean(zip == 0), 0.3 + 0.7 * exp(-4), tolerance = 0.03)
  zinb <- simulate_claims(n, "zero_inflated_negative_binomial", c(2, 1.5, 0.25), "Gamma", c(2, 500), seed = 2)$claim_counts
  expect_equal(mean(zinb), 2.25, tolerance = 0.02)
  expect_equal(stats::sd(zinb), sqrt(0.75 * (7.5 + 0.25 * 9)), tolerance = 0.02)
  expect_equal(mean(zinb == 0), 0.25 + 0.75 * stats::dnbinom(0, size = 2, mu = 3), tolerance = 0.03)
})

test_that("a layer of zero-inflated counts is the same drawn from its large claims alone", {
  #gross = FALSE draws the claims above the deductible only (the split of the counts); gross = TRUE draws every claim
  for (case in list(list("Zero_Inflated_Poisson", c(4, 0.3)), list("Zero_Inflated_Negative_Binomial", c(2, 1.5, 0.25)))) {
    run <- function(gross, seed) {
      simulate_claims(60000, case[[1]], case[[2]], "Gamma", c(2, 500), seed = seed, eel_layer = "limited",
                      eel_deductible = 1500, eel_limit = 2000, gross = gross)
    }
    short <- run(FALSE, 3)
    full <- run(TRUE, 4)
    expect_equal(mean(short$total_claims), mean(full$total_claims), tolerance = 0.03)
    expect_equal(stats::sd(short$total_claims), stats::sd(full$total_claims), tolerance = 0.03)
    expect_equal(mean(short$claim_counts), mean(full$claim_counts), tolerance = 0.02)
    expect_equal(mean(short$total_claims == 0), mean(full$total_claims == 0), tolerance = 0.02)
  }
})

test_that("the parameters of the zero-inflated counts are validated", {
  run <- function(...) simulate_claims(10, ..., "Gamma", c(2, 500))
  expect_error(run("Zero_Inflated_Poisson", c(3, 1)), "less than 1")
  expect_error(run("Zero_Inflated_Poisson", c(3, -0.1)))
  expect_error(run("Zero_Inflated_Poisson", 3), "lambda, zero_prob", fixed = TRUE)
  expect_error(run("Zero_Inflated_Negative_Binomial", c(2, 1.5)), "r, beta, zero_prob", fixed = TRUE)
  expect_error(run("Zero_Inflated_Negative_Binomial", c(0, 1.5, 0.2)))
  #the ids of the inputs are not those of another distribution: a settings file holds each once
  ids <- unlist(lapply(freq_dist_options, function(d) d@paramIDs))
  expect_false(anyDuplicated(ids) > 0)
})

zero_inflated_counts <- function() {
  set.seed(11)
  n <- 4000
  list(zip = stats::rbinom(n, 1, 0.7) * stats::rpois(n, 4),
       zinb = stats::rbinom(n, 1, 0.75) * stats::rnbinom(n, size = 2, mu = 3),
       po = stats::rpois(n, 3), nb = stats::rnbinom(n, size = 1.5, mu = 4))
}

test_that("the zero-inflated Poisson fit is the maximum of its likelihood", {
  d <- zero_inflated_counts()
  fit <- fit_zip_mle(d$zip)
  expect_named(fit$estimate, c("lambda", "zero"))
  expect_true(fit$inflated)
  nll <- function(p) -sum(log(dft_zero_inflated_d(d$zip, stats::plogis(p[2]), function(x) stats::dpois(x, exp(p[1])))))
  direct <- stats::optim(c(log(3), 0), nll, control = list(reltol = 1e-14, maxit = 5000))
  expect_equal(unname(fit$estimate), c(exp(direct$par[1]), stats::plogis(direct$par[2])), tolerance = 1e-4)
  expect_equal(fit$loglik, -direct$value, tolerance = 1e-9)
  expect_equal(fit$aic, -2 * fit$loglik + 4)
  expect_equal(fit$bic, -2 * fit$loglik + 2 * log(length(d$zip)))
  #close to the parameters the counts were drawn with, within their standard errors
  expect_lt(abs(fit$estimate[["lambda"]] - 4), 4 * fit$sd[["lambda"]])
  expect_lt(abs(fit$estimate[["zero"]] - 0.3), 4 * fit$sd[["zero"]])
  expect_true(all(fit$sd > 0 & fit$sd < 0.1))
})

test_that("the zero-inflated Negative Binomial fit is the maximum of its likelihood", {
  d <- zero_inflated_counts()
  fit <- fit_zinb_mle(d$zinb)
  expect_named(fit$estimate, c("size", "mu", "zero"))
  expect_true(fit$inflated)
  expect_false(fit$capped)
  nll <- function(p) {
    -sum(log(dft_zero_inflated_d(d$zinb, stats::plogis(p[3]), function(x) stats::dnbinom(x, size = exp(p[1]), mu = exp(p[2])))))
  }
  direct <- stats::optim(c(log(2), log(3), stats::qlogis(0.2)), nll, control = list(reltol = 1e-14, maxit = 5000))
  expect_equal(unname(fit$estimate), c(exp(direct$par[1:2]), stats::plogis(direct$par[3])), tolerance = 2e-3)
  expect_gte(fit$loglik, -direct$value - 1e-6)
  expect_equal(fit$loglik, -direct$value, tolerance = 1e-7)
  expect_equal(fit$aic, -2 * fit$loglik + 6)
  expect_lt(abs(fit$estimate[["size"]] - 2), 4 * fit$sd[["size"]])
  expect_lt(abs(fit$estimate[["mu"]] - 3), 4 * fit$sd[["mu"]])
  expect_lt(abs(fit$estimate[["zero"]] - 0.25), 4 * fit$sd[["zero"]])
  expect_equal(dim(fit$cor), c(3L, 3L))
  #it beats the zero-inflated Poisson and the Negative Binomial on these counts
  expect_lt(fit$aic, fit_zip_mle(d$zinb)$aic)
  expect_lt(fit$aic, fit_nbinom_mle(d$zinb)$aic)
})

test_that("weights count as repeated rows in the zero-inflated fits", {
  d <- zero_inflated_counts()
  for (case in list(list(fit_zip_mle, d$zip), list(fit_zinb_mle, d$zinb))) {
    counts <- table(case[[2]])
    weighted <- case[[1]](as.numeric(names(counts)), as.numeric(counts))
    plain <- case[[1]](case[[2]])
    expect_equal(weighted$estimate, plain$estimate, tolerance = 1e-6)
    expect_equal(weighted$loglik, plain$loglik, tolerance = 1e-10)
    expect_true(weighted$weighted)
    expect_false(plain$weighted)
  }
})

test_that("counts without extra zeros give the fit of the base distribution, marked as such", {
  d <- zero_inflated_counts()
  zip <- fit_zip_mle(d$po)
  expect_false(zip$inflated)
  expect_equal(zip$estimate[["zero"]], 0)
  expect_equal(zip$estimate[["lambda"]], mean(d$po), tolerance = 1e-12)
  expect_equal(zip$loglik, fit_poisson_mle(d$po)$loglik, tolerance = 1e-10)
  expect_true(all(is.na(zip$sd)))
  #(fewer zeros than the Negative Binomial gives: a sample of one may have a few more, by chance)
  counts <- d$nb[-which(d$nb == 0)[1:150]]
  zinb <- fit_zinb_mle(counts)
  nb <- fit_nbinom_mle(counts)
  expect_false(zinb$inflated)
  expect_equal(unname(zinb$estimate), c(unname(nb$estimate), 0))
  expect_equal(zinb$loglik, nb$loglik, tolerance = 1e-10)
  expect_identical(zinb$n, length(counts))
  #a model of its own only with extra zeros (and, for the Negative Binomial, a size)
  expect_false(dft_count_fit_distinct(zip))
  expect_false(dft_count_fit_distinct(zinb))
  expect_true(dft_count_fit_distinct(fit_zip_mle(d$zip)))
  expect_false(dft_count_fit_distinct(NULL))
  expect_output(dft_print_count_fit(zip), "no more zeros than the Poisson")
})

test_that("rows with claims no more spread than a Poisson's cap the zero-inflated Negative Binomial", {
  x <- c(0, 0, 0, 5, 7, 3, 4, 6)
  zip <- fit_zip_mle(x)
  zinb <- fit_zinb_mle(x)
  expect_true(zinb$capped)
  expect_true(zinb$inflated)
  expect_equal(zinb$estimate[["size"]], dft_nbinom_max_size)
  expect_equal(zinb$estimate[["mu"]], zip$estimate[["lambda"]], tolerance = 1e-8)
  expect_equal(zinb$estimate[["zero"]], zip$estimate[["zero"]], tolerance = 1e-6)
  expect_equal(zinb$loglik, zip$loglik, tolerance = 1e-6)
  expect_false(dft_count_fit_distinct(zinb))
  #a single size of claim count, or no row with more than one claim: no truncated fit, no extra zeros
  expect_false(fit_zip_mle(c(0, 0, 1, 1, 1))$inflated)
  expect_false(fit_zinb_mle(c(0, 0, 1, 1, 1))$inflated)
  expect_error(fit_zip_mle(rep(0, 5)), "all zero")
  expect_error(fit_zinb_mle(rep(0, 5)), "all zero")
})

test_that("the fitted cdfs of the count models are those of their distributions", {
  d <- zero_inflated_counts()
  fit <- fit_zinb_mle(d$zinb)
  e <- fit$estimate
  cdf <- dft_count_fit_cdf(fit)
  expect_equal(cdf(-1), 0)
  expect_equal(cdf(0:6), e[["zero"]] + (1 - e[["zero"]]) * stats::pnbinom(0:6, size = e[["size"]], mu = e[["mu"]]))
  #the fitted share of zeros is the share of the data (the likelihood equation of the extra zeros)
  expect_equal(cdf(0), mean(d$zinb == 0), tolerance = 1e-6)
  zip <- fit_zip_mle(d$zip)
  expect_equal(dft_count_fit_cdf(zip)(0), mean(d$zip == 0), tolerance = 1e-8)
  expect_equal(dft_count_fit_cdf(fit_poisson_mle(d$po))(2), stats::ppois(2, mean(d$po)))
  nb <- fit_nbinom_mle(d$nb)
  expect_equal(dft_count_fit_cdf(nb)(3), stats::pnbinom(3, size = nb$estimate[["size"]], mu = nb$estimate[["mu"]]))
})

test_that("the zero-inflated fits are sent to the simulator with its parameters", {
  values <- dft_simulator_settings(list(model = "Zero-inflated Poisson", lambda = 4, zero = 0.3))
  expect_identical(values$freqDistr, "Zero_Inflated_Poisson")
  expect_equal(c(values$zip_lambda, values$zip_zero), c(4, 0.3))
  values <- dft_simulator_settings(list(model = "Zero-inflated Negative Binomial", size = 2, mu = 3, zero = 0.25))
  expect_identical(values$freqDistr, "Zero_Inflated_Negative_Binomial")
  expect_equal(c(values$zinb_r, values$zinb_beta, values$zinb_zero), c(2, 1.5, 0.25))
  #the simulator's moments from these settings are those of the fitted model
  expect_equal(freq_dist_options$Zero_Inflated_Negative_Binomial@moments_func(c(values$zinb_r, values$zinb_beta, values$zinb_zero))[1],
               0.75 * 3)
  #claims above a reporting threshold are a share of all claims: the counts of all claims are larger,
  #with the same extra zeros
  values <- dft_simulator_settings(list(model = "Zero-inflated Poisson", lambda = 4, zero = 0.3), frequency_share = 0.5)
  expect_equal(c(values$zip_lambda, values$zip_zero), c(8, 0.3))
  values <- dft_simulator_settings(list(model = "Zero-inflated Negative Binomial", size = 2, mu = 3, zero = 0.25),
                                   frequency_share = 0.5)
  expect_equal(c(values$zinb_r, values$zinb_beta, values$zinb_zero), c(2, 3, 0.25))
  expect_error(dft_simulator_settings(list(model = "Zero-inflated Poisson", lambda = 4, zero = 1)))
  expect_error(dft_simulator_settings(list(model = "Zero-inflated Poisson", lambda = 4, zero = NA_real_)))
  #the table of what is sent names the model and its three parameters
  rows <- dft_simulator_settings_rows(dft_simulator_settings(list(model = "Zero-inflated Negative Binomial", size = 2, mu = 3,
                                                                  zero = 0.25)))
  expect_identical(vapply(rows, `[[`, character(1), 1),
                   c("Frequency distribution", "r (shape)", "beta (scale)", "Probability of an extra zero"))
  expect_identical(rows[[1]][[2]], "Zero-inflated Negative Binomial")
  expect_true(all(c("Zero-inflated Poisson", "Zero-inflated Negative Binomial") %in% names(dft_export_frequency_models)))
})
