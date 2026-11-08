#claim sizes censored at their policy limits

censored_claims <- function() {
  set.seed(3)
  loss <- stats::rlnorm(3000, 9, 1.4)
  limit <- sample(c(20000, 50000, 1e5), 3000, TRUE)
  list(paid = pmin(loss, limit), censored = loss >= limit)
}

test_that("the fits with censoring are the maximum likelihood fits of survival analysis", {
  x <- censored_claims()
  models <- dft_truncated_severity_models(x$paid, 0, x$censored)
  #the LogNormal and Weibull fits of survival::survreg() on the same claims (stored, so the
  #survival package is not needed)
  expect_equal(unname(models[[2]]$params), c(8.989224, 1.406001), tolerance = 1e-5)
  expect_equal(unname(models[[6]]$params), c(15000.37, 0.806079), tolerance = 1e-5)
  #ignoring the limits understates the tail
  naive <- c(mean(log(x$paid)), stats::sd(log(x$paid)))
  expect_lt(naive[2], models[[2]]$params[["sigma"]])
  #the Exponential and Pareto have closed forms: the claims below their limits over the exposure
  expect_equal(models[[3]]$params[["Mean"]], sum(x$paid) / sum(!x$censored))
  expect_equal(models[[5]]$params[["alpha"]], sum(!x$censored) / sum(log(x$paid / min(x$paid))))
  #without a threshold, the cdfs are of all claims
  expect_equal(models[[2]]$cdf(30000), stats::plnorm(30000, models[[2]]$params[[1]], models[[2]]$params[[2]]))
  expect_equal(models[[1]]$cdf(-1), stats::pnorm(-1, models[[1]]$params[[1]], models[[1]]$params[[2]]))
  #above a threshold too
  above <- x$paid > 5000
  both <- dft_truncated_severity_models(x$paid[above], 5000, x$censored[above])
  expect_equal(both[[2]]$cdf(5000), 0)
  expect_gt(both[[2]]$survival_at_threshold, 0)
  #without censoring, the threshold must still be positive
  expect_error(dft_truncated_severity_models(c(10, 20, 30), 0), "positive number")
  expect_error(dft_truncated_severity_models(c(10, 20, 30), 0, c(TRUE, TRUE, FALSE)), "below their policy limits")
})

test_that("the Kaplan-Meier estimate allows for the censored claims", {
  #four claims, the second censored at 2: at 1 one of four closes, at 3 one of the two still open
  km <- dft_kaplan_meier(c(1, 2, 3, 4), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(km$x, c(1, 3, 4))
  expect_equal(km$cdf, c(0.25, 1 - 0.75 * 0.5, 1))
  #without censoring it is the empirical cdf
  plain <- dft_kaplan_meier(c(5, 1, 3, 3), rep(FALSE, 4))
  expect_equal(plain$cdf, c(0.25, 0.75, 1))
  expect_equal(dft_km_quantiles(km, c(0.2, 0.5, 0.9)), c(1, 3, 4))
  #the largest gap is just before 3, where the estimate is still 0.25 and the uniform cdf 0.75
  expect_equal(dft_km_distance(km, function(q) stats::punif(q, 0, 4)), 0.5)
})

test_that("the Severity tab takes one limit or a column of limits", {
  skip_server_on_cran()
  x <- censored_claims()
  limits <- ifelse(x$censored, x$paid, 1e5)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(paid = round(x$paid, 2), policy_limit = limits), path, row.names = FALSE)
  testServer(distribution_fitting_tool_Server, {
    session$setInputs(file1 = list(datapath = path, name = "claims.csv"), data_includes_header = TRUE, sep = ",", quote = "\"",
                      dec = ".")
    session$setInputs(severity_var = "paid", sev_limit_source = "column", sev_limit_col = "policy_limit",
                      execute_sev_analysis = 1)
    expect_equal(sum(sev_censored()), sum(x$censored))
    expect_equal(unname(sev_models()[[2]]$params), c(8.989224, 1.406001), tolerance = 1e-3)
    html <- output$sev_param_summary$html
    expect_match(html, "Kaplan-Meier")
    expect_match(html, "policy limit")
    expect_match(output$sev_stats$html, "at their policy limits")
    expect_true(is.list(output$sev_fit_plot))
    expect_true(is.list(output$sev_qq_plot))
    #one limit for every claim
    session$setInputs(sev_limit_source = "value", sev_limit = 50000, execute_sev_analysis = 2)
    expect_equal(sum(sev_censored()), sum(round(x$paid, 2) >= 50000))
    #no limits: the plain fits, as before
    session$setInputs(sev_limit_source = "none", execute_sev_analysis = 3)
    expect_false(sev_has_censoring())
    expect_equal(unname(sev_models()[[2]]$params[[1]]), mean(log(round(x$paid, 2))))
  })
})
