#the largest single claim of each simulation, for occurrence exceedance (OEP)

test_that("largest_claim is each simulation's largest claim, 0 without claims, the totals unchanged", {
  a <- simulate_claims(3000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 1, largest_claims = TRUE, shortcuts = FALSE)
  b <- simulate_claims(3000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 1, shortcuts = FALSE)
  expect_false("largest_claim" %in% names(b))
  expect_identical(a$total_claims, b$total_claims)
  expect_true(all(a$largest_claim <= a$total_claims * (1 + 1e-12)))
  expect_identical(a$largest_claim == 0, a$claim_counts == 0)
  one <- a$claim_counts == 1
  expect_equal(a$largest_claim[one], a$total_claims[one])
})

test_that("the largest claims follow the theory: P(largest <= x) = exp(-lambda * S(x)) for Poisson counts", {
  big <- simulate_claims(100000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 2, largest_claims = TRUE)
  x <- c(20000, 50000, 100000)
  theory <- exp(-3 * stats::plnorm(x, 9, 1.5, lower.tail = FALSE))
  empirical <- vapply(x, function(v) mean(big$largest_claim <= v), numeric(1))
  expect_true(all(abs(empirical - theory) < 4 * sqrt(theory * (1 - theory) / 1e5)))
  #asking for them gives up the one-step totals, but the same distribution of totals
  gamma <- simulate_claims(20000, "Poisson", 5, "Gamma", c(shape = 2, scale = 1000), seed = 3, largest_claims = TRUE)
  expect_equal(mean(gamma$total_claims), 5 * 2 * 1000, tolerance = 0.02)
  expect_true(all(gamma$largest_claim <= gamma$total_claims * (1 + 1e-12)))
})

test_that("with a layer, largest_claim is the largest ceded claim, in both ways of drawing it", {
  layer <- list(20000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 3, eel_layer = "limited",
                eel_deductible = 20000, eel_limit = 50000, largest_claims = TRUE)
  full <- do.call(simulate_claims, layer)
  #drawing only the claims that reach the layer (gross = FALSE)
  fast <- do.call(simulate_claims, c(layer, list(gross = FALSE)))
  for (x in list(full, fast)) {
    expect_true(all(x$largest_claim >= 0 & x$largest_claim <= 50000))
    expect_true(all(x$largest_claim <= x$total_claims * (1 + 1e-12)))
  }
  expect_equal(mean(full$largest_claim), mean(fast$largest_claim), tolerance = 0.05)
})

test_that("largest claims are the same in parallel, are checked by simulate_claims(), and the app asks for them", {
  skip_on_cran()
  par <- simulate_claims(4000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 4, largest_claims = TRUE, parallel = TRUE,
                         chunk_size = 1000, workers = 2)
  seq <- simulate_claims(4000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 4, largest_claims = TRUE, chunk_size = 1000)
  expect_identical(par$largest_claim, seq$largest_claim)
  expect_error(simulate_claims(10, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 1, largest_claims = "yes"), "largest_claims")
  #the app asks for them unless a run expects more than 20 million claims
  expect_true(sim_wants_largest_claims(3, 50000))
  expect_true(sim_wants_largest_claims(NA, 50000))
  expect_false(sim_wants_largest_claims(1000, 50000))
})
