#the P-P plot and the conditional tail expectations of the Severity tab

#E[X | X > v] by numerical integration: v plus the integral of the survival function above v,
#over the survival at v, in units of `scale` (about the size of the excess), which integrate() needs
tail_mean_numeric <- function(survival, v, scale = v) {
  v + scale * stats::integrate(function(u) survival(v + scale * u) / survival(v), 0, Inf, rel.tol = 1e-8)$value
}

test_that("the tail means are those of the distributions, far into the tail too", {
  check <- function(name, params, survival, v, scale = v) {
    tail_mean <- dft_tail_mean(name, params)
    for (i in seq_along(v)) {
      expect_equal(tail_mean(v[i]), tail_mean_numeric(survival, v[i], rep_len(scale, length(v))[i]), tolerance = 1e-6,
                   label = paste(name, v[i]))
    }
  }
  check("Normal", c(Mean = 1000, `Std. dev.` = 300), function(q) pnorm(q, 1000, 300, lower.tail = FALSE), c(900, 1500, 2200),
        scale = c(300, 100, 50))
  check("LogNormal", c(mu = 8, sigma = 1.5), function(q) plnorm(q, 8, 1.5, lower.tail = FALSE), c(1000, 1e5, 1e6))
  check("Exponential", c(Mean = 500), function(q) pexp(q, 1 / 500, lower.tail = FALSE), c(10, 3000))
  check("Gamma", c(Scale = 400, Shape = 2.5), function(q) pgamma(q, shape = 2.5, scale = 400, lower.tail = FALSE),
        c(500, 5000, 15000))
  check("Weibull", c(Scale = 2000, Shape = 0.7), function(q) pweibull(q, 0.7, 2000, lower.tail = FALSE), c(100, 2e4, 1e5))
  check("Pareto", c(xm = 1000, alpha = 2.5), function(q) ifelse(q < 1000, 1, (1000 / q)^2.5), c(1500, 1e5))
  #a Pareto with alpha at most 1 has no mean, and an infinite CTE
  expect_equal(dft_tail_mean("Pareto", c(xm = 1000, alpha = 0.9))(5000), Inf)
  #a failed fit has none
  expect_null(dft_tail_mean("Gamma", c(Scale = NA, Shape = NA)))
})

test_that("the empirical CTE is the mean of the largest claims, the last in part", {
  expect_equal(dft_empirical_cte(1:100, c(0.9, 0.95, 0.99)), c(mean(91:100), mean(96:100), 100))
  #half a claim: the largest; one and a half: the largest and half the next
  expect_equal(dft_empirical_cte(1:10, c(0.95, 0.85)), c(10, (10 + 0.5 * 9) / 1.5))
  #200 * (1 - 0.995) is one claim, though a hair below it in floating point
  expect_equal(dft_empirical_cte(1:200, 0.995), 200)
  #the order of the claims does not matter
  set.seed(3)
  x <- rlnorm(1000, 7, 1.3)
  expect_equal(dft_empirical_cte(sample(x), 0.99), mean(sort(x, decreasing = TRUE)[1:10]))
})

test_that("a model's CTE is the mean claim above its percentile", {
  #the Exponential: the percentile -mean log(1 - p), plus the mean
  model <- list(name = "Exponential", params = c(Mean = 500), cdf = function(q) pexp(q, 1 / 500))
  p <- c(0.9, 0.99, 0.995)
  expect_equal(dft_model_cte(model, p, c(10, 5000)), -500 * log(1 - p) + 500, tolerance = 1e-9)
  #a failed fit has none
  expect_equal(dft_model_cte(list(name = "Gamma", params = c(Scale = NA, Shape = NA), cdf = NULL), p, 1:10), rep(NA_real_, 3))
  #above a reporting threshold: the percentile of the claims above it, and the mean claim above that
  set.seed(4)
  x <- rlnorm(3000, 8, 1.2)
  d <- 2000
  x <- x[x > d]
  lognormal <- dft_truncated_severity_models(x, d)[[2]]
  mu <- lognormal$params[["mu"]]
  sigma <- lognormal$params[["sigma"]]
  s_d <- plnorm(d, mu, sigma, lower.tail = FALSE)
  v <- qlnorm(1 - (1 - 0.99) * s_d, mu, sigma)
  expect_equal(dft_model_cte(lognormal, 0.99, x),
               tail_mean_numeric(function(q) plnorm(q, mu, sigma, lower.tail = FALSE), v), tolerance = 1e-6)
})

test_that("the P-P points are the empirical and fitted probabilities at the claims", {
  x <- c(5, 1, 3, 2)
  pp <- dft_pp_data(x, list(Uniform = function(q) punif(q, 0, 6)))
  expect_equal(pp$x, c(1, 2, 3, 5))
  expect_equal(pp$empirical, (1:4 - 0.5) / 4)
  expect_equal(pp$fitted$Uniform, c(1, 2, 3, 5) / 6)
  #with policy limits, the middle of each step of the Kaplan-Meier estimate
  km <- dft_kaplan_meier(c(1, 2, 3, 3, 4), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  pp <- dft_pp_data(c(1, 2, 3, 3, 4), list(Uniform = function(q) punif(q, 0, 4)), km)
  expect_equal(pp$x, km$x)
  expect_equal(pp$empirical, (c(0, km$cdf[-length(km$cdf)]) + km$cdf) / 2)
  #at most `points` claims, from the smallest to the largest
  pp <- dft_pp_data(1:2000, list(Uniform = function(q) punif(q, 0, 2001)), points = 500)
  expect_length(pp$x, 500)
  expect_equal(range(pp$x), c(1, 2000))
})

test_that("the Severity tab draws the P-P plot and gives the CTEs", {
  skip_server_on_cran()
  set.seed(10)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(sev = round(exp(rnorm(300, 7, 1.2)))), path, row.names = FALSE)
  testServer(distribution_fitting_tool_Server, {
    session$setInputs(file1 = list(datapath = path, name = "claims.csv"), data_includes_header = TRUE, sep = ",", quote = "\"",
                      dec = ".")
    session$setInputs(severity_var = "sev", severity_hist_bins = 20, sev_fit_log_scale = FALSE, execute_sev_analysis = 1)
    expect_match(output$sev_pp_plot$src, "^data:image/png;base64,")
    html <- output$sev_cte_table$html
    expect_match(html, "CTE 99.5%", fixed = TRUE)
    expect_match(html, "CTE 90%", fixed = TRUE)
    expect_match(html, netsimr_fmt(dft_empirical_cte(severity_data(), 0.9)), fixed = TRUE)
    expect_match(html, "rests on 1.5 claims", fixed = TRUE)
    #the LogNormal fit's CTE
    expect_match(html, netsimr_fmt(dft_model_cte(sev_models()[[2]], 0.99, severity_data())), fixed = TRUE)
    #with one policy limit for every claim: no empirical CTE
    session$setInputs(sev_limit_source = "value", sev_limit = 5000, execute_sev_analysis = 2)
    expect_true(sev_has_censoring())
    expect_match(output$sev_pp_plot$src, "^data:image/png;base64,")
    expect_match(output$sev_cte_table$html, "the claims give no CTE", fixed = TRUE)
  })
})
