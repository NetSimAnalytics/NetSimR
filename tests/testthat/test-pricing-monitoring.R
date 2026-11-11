#on-level premiums, the indicated rate change and Buhlmann-Straub credibility

test_that("the parallelogram method gives the areas of the triangles", {
  # +10% in mid-2023 on annual policies: 2023 earns an eighth of its premium at the new rate
  o <- on_level_premium(c(100, 100, 100), 2022:2024, data.frame(date = 2023.5, change = 0.10))
  expect_equal(o$average_level, c(1, 1 + 0.1 / 8, 1 + 0.1 * 7 / 8))
  expect_equal(o$on_level_factor, 1.1 / o$average_level)
  expect_equal(attr(o, "current_level"), 1.1)
  # a change on 1 January: the year of the change earns half at the new rate
  jan <- on_level_premium(100, 2023, data.frame(date = as.Date("2023-01-01"), change = 0.2))
  expect_equal(jan$average_level, 1.1)
  # six-month policies written evenly: of the year's 0.5 years of earned cover, those written after a change
  # mid-2023 earn a triangle of 0.5^2 / 2 = 0.125, a quarter
  six <- on_level_premium(100, 2023, data.frame(date = 2023.5, change = 0.1), term = 0.5)
  expect_equal(six$average_level, 1 + 0.1 * 0.25)
  # several changes compound
  two <- on_level_premium(100, 2026, data.frame(date = c(2024, 2025), change = c(0.1, -0.05)))
  expect_equal(two$average_level, 1.1 * 0.95)
  expect_error(on_level_premium(100, 2023, data.frame(date = 2023, change = -1)), "above -1")
})

test_that("the indicated rate change trends the losses to the new rates' claims", {
  r <- indicated_rate_change(c(70, 75), c(100, 100), 2023:2024, trend = 0.05, effective = 2026,
                             expense_ratio = 0.25, profit = 0.05)
  expect_equal(r$target, 2027)
  expect_equal(r$by_year$trend_factor, 1.05^(2027 - c(2023.5, 2024.5)))
  expect_equal(r$experience_loss_ratio, mean(c(70, 75) * r$by_year$trend_factor / 100))
  expect_equal(r$indicated_change, r$experience_loss_ratio / 0.7 - 1)
  blended <- indicated_rate_change(c(70, 75), c(100, 100), 2023:2024, effective = as.Date("2026-01-01"), credibility = 0.4,
                                   complement = 0.65)
  expect_equal(blended$loss_ratio, 0.4 * blended$experience_loss_ratio + 0.6 * 0.65)
  expect_error(indicated_rate_change(70, 100, 2023, effective = 2026, credibility = 0.5), "complement")
  expect_error(indicated_rate_change(70, 100, 2023, effective = 2026, expense_ratio = 0.9, profit = 0.1), "permissible")
  expect_error(indicated_rate_change(70, 100, "2023", effective = 2026), "accident years, as numbers")
})

test_that("Buhlmann-Straub credibility matches the actuar package", {
  set.seed(1)
  d <- data.frame(g = rep(1:5, each = 4), w = stats::runif(20, 50, 150))
  d$x <- stats::rnorm(20, rep(c(0.6, 0.7, 0.65, 0.8, 0.55), each = 4), 0.05 * sqrt(100 / d$w))
  bs <- buhlmann_straub(d$x, d$w, d$g)
  # actuar::cm(~group, ...) on the same data (stored, so that actuar is not needed)
  expect_equal(bs$by_group$estimate, c(0.5847835, 0.717533, 0.6923688, 0.7809309, 0.5355902), tolerance = 1e-6)
  expect_equal(bs$by_group$credibility, c(0.9506254, 0.9565154, 0.9349261, 0.9530111, 0.9578972), tolerance = 1e-6)
  expect_equal(bs$collective_mean, 0.6622413, tolerance = 1e-6)
  expect_equal(c(bs$s2, bs$a), c(0.2405722, 0.01124584), tolerance = 1e-6)
  # groups that differ no more than chance get no credibility
  flat <- buhlmann_straub(rep(c(1, 2, 1, 2), 3), rep(1, 12), rep(c("a", "b", "c"), each = 4))
  expect_equal(flat$by_group$credibility, rep(0, 3))
  expect_equal(flat$by_group$estimate, rep(1.5, 3))
  expect_error(buhlmann_straub(1:3, 1:3, rep("a", 3)), "two groups")
})
