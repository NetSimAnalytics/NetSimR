#a portfolio of lines joined by a copula

portfolio_lines <- function() {
  list(
    motor = list(frequency = "Poisson", frequency_params = 30, severity = "LogNormal", severity_params = c(8, 1)),
    property = list(frequency = "Poisson", frequency_params = 4, severity = "Pareto", severity_params = c(2, 20000))
  )
}

test_that("each line keeps its distribution and the ranks follow the Gaussian copula", {
  lines <- portfolio_lines()
  set.seed(9)
  before <- .Random.seed
  p <- simulate_portfolio(lines, correlation = 0.6, n = 20000, seed = 1)
  expect_identical(.Random.seed, before)
  expect_s3_class(p, "netsimr_portfolio")
  expect_identical(names(p$losses), c("motor", "property", "total"))
  expect_equal(p$losses$total, p$losses$motor + p$losses$property)
  #the marginals are those simulated on their own, only reordered
  independent <- simulate_portfolio(lines, correlation = 0, n = 20000, seed = 1)
  expect_equal(sort(p$losses$motor), sort(independent$losses$motor))
  expect_equal(sort(p$losses$property), sort(independent$losses$property))
  #Spearman's rho of a Gaussian copula is 6 / pi * asin(rho / 2) (ties at zero losses lower it a little)
  rho <- stats::cor(p$losses$motor, p$losses$property, method = "spearman")
  expect_equal(rho, 6 / pi * asin(0.6 / 2), tolerance = 0.05)
  expect_lt(abs(stats::cor(independent$losses$motor, independent$losses$property, method = "spearman")), 0.03)
  #the same seed, the same portfolio
  expect_identical(simulate_portfolio(lines, correlation = 0.6, n = 20000, seed = 1)$losses, p$losses)
  #dependence raises the total's VaR and lowers the diversification
  expect_gt(p$summary$var[3], independent$summary$var[3])
  expect_lt(p$diversification[["benefit"]], independent$diversification[["benefit"]])
})

test_that("the risk measures and the capital allocation add up", {
  p <- simulate_portfolio(portfolio_lines(), correlation = 0.3, n = 20000, level = 0.99, seed = 2)
  s <- p$summary
  total <- p$losses$total
  var99 <- stats::quantile(total, 0.99, names = FALSE)
  expect_equal(s$var[3], var99)
  expect_equal(s$tvar[3], mean(total[total >= var99]))
  #the lines' capitals add up to the total's TVaR, their shares to 1
  expect_equal(sum(s$capital[1:2]), s$capital[3])
  expect_equal(s$capital[3], s$tvar[3])
  expect_equal(sum(s$capital_share[1:2]), 1)
  expect_equal(p$diversification[["benefit"]], sum(s$var[1:2]) - s$var[3])
  expect_gt(p$diversification[["benefit"]], 0)
  expect_output(print(p), "Diversification benefit")
  expect_output(print(p), "VaR 99%")
})

test_that("a t copula makes joint extremes more likely than a Gaussian one", {
  set.seed(4)
  n <- 40000
  a <- stats::rexp(n)
  b <- stats::rexp(n)
  joint <- function(p) {
    top_a <- p$losses$a > stats::quantile(p$losses$a, 0.99)
    top_b <- p$losses$b > stats::quantile(p$losses$b, 0.99)
    mean(top_a & top_b)
  }
  gaussian <- simulate_portfolio(list(a = a, b = b), correlation = 0.5, n = n, seed = 3)
  t_copula <- simulate_portfolio(list(a = a, b = b), correlation = 0.5, copula = "t", df = 2, n = n, seed = 3)
  expect_gt(joint(t_copula), 1.5 * joint(gaussian))
  expect_identical(t_copula$copula, "t")
  expect_output(print(t_copula), "t copula")
})

test_that("the lines and the correlation are checked", {
  lines <- portfolio_lines()
  expect_error(simulate_portfolio(lines[1], n = 100), "at least two lines")
  expect_error(simulate_portfolio(unname(lines), n = 100), "own name")
  expect_error(simulate_portfolio(lines, correlation = 1, n = 100), "below 1")
  three <- c(lines, list(other = stats::rexp(100)))
  expect_error(simulate_portfolio(three, correlation = -0.6, n = 100), "above -0.5")
  bad <- matrix(c(1, 0.9, 0.9, 0.9, 1, -0.9, 0.9, -0.9, 1), 3)
  expect_error(simulate_portfolio(three, correlation = bad, n = 100), "positive definite")
  expect_error(simulate_portfolio(lines, correlation = matrix(c(1, 0.2, 0.3, 1), 2), n = 100), "symmetric")
  expect_error(simulate_portfolio(list(a = 1:5, b = 1:5), n = 100), "must be 100 finite numbers")
  expect_error(simulate_portfolio(list(a = list(n_sims = 5), b = 1:100), n = 100), "must not set n_sims")
  expect_error(simulate_portfolio(list(a = list(frequency = "Nope"), b = stats::rexp(100)), n = 100), "could not be simulated")
  expect_error(simulate_portfolio(lines, copula = "t", df = 0, n = 100), "df must be")
  expect_error(simulate_portfolio(lines, level = 1, n = 100), "level")
  #a matrix with the lines' names works as one number would
  R <- matrix(c(1, 0.4, 0.4, 1), 2)
  expect_equal(simulate_portfolio(lines, R, n = 2000, seed = 5)$losses, simulate_portfolio(lines, 0.4, n = 2000, seed = 5)$losses)
})
