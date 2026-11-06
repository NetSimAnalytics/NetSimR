#The one-year view (Merz and Wuthrich) and the Munich chain ladder. The reference values are
#those of the ChainLadder package's CDR() and MunichChainLadder() (version 0.2.21); the total
#one-year standard error of the Taylor and Ashe triangle is that of Merz and Wuthrich's paper.

test_that("the one-year standard errors of the RAA triangle are those of ChainLadder's CDR()", {
  mw <- merz_wuthrich(raa_triangle)
  expect_s3_class(mw, "netsimr_reserve")
  expect_equal(mw$by_origin$one_year_se,
               c(0, 142.931716231, 569.953604381, 377.706411250, 1299.245669330, 1667.888183159, 1185.712866084,
                 4691.118569929, 4706.975145506, 23610.351930674), tolerance = 1e-9)
  expect_equal(mw$total[["one_year_se"]], 25166.302526438, tolerance = 1e-11)
  #until ultimate, Mack's standard error; the one-year error is below it
  expect_equal(mw$by_origin$mack_se, chain_ladder(raa_triangle)$by_origin$se)
  expect_true(all(mw$by_origin$one_year_se <= mw$by_origin$mack_se + 1e-9))
  #the origin with one year left has all its risk in that year
  expect_equal(mw$by_origin$one_year_se[2], mw$by_origin$mack_se[2])
})

test_that("the one-year standard error of the Taylor and Ashe triangle is Merz and Wuthrich's", {
  mw <- merz_wuthrich(taylor_ashe_triangle, sigma = "Mack")
  expect_equal(mw$total[["one_year_se"]], 1778967.6633576, tolerance = 1e-11)
  expect_equal(mw$by_origin$one_year_se[10], 1029924.9909764, tolerance = 1e-11)
  #with older origins fully developed above it: as ChainLadder (2.11e-10 apart, rounding)
  tall <- rbind(taylor_ashe_triangle[1, ] * 0.9, taylor_ashe_triangle)
  rownames(tall) <- 0:10
  expect_equal(merz_wuthrich(tall, sigma = "Mack")$by_origin$one_year_se[1:2], c(0, 0))
})

test_that("the Munich chain ladder of Quarg and Mack's triangles is that of ChainLadder", {
  mcl <- munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)
  expect_equal(mcl$by_origin$ultimate_paid,
               c(2131, 2384.84209151, 4553.62362114, 6069.50929270, 4878.95038298, 4598.99574631, 7504.57586034),
               tolerance = 1e-10)
  expect_equal(mcl$by_origin$ultimate_incurred,
               c(2174, 2443.22239968, 4634.35789464, 6182.34740706, 4957.80540639, 4672.40178216, 7655.37761111),
               tolerance = 1e-10)
  expect_equal(unname(mcl$lambda), c(0.636021466425, 0.436187132012), tolerance = 1e-10)
  expect_equal(mcl$q, c(0.532582216809, 0.848862137203, 0.927596439169, 0.945073538982, 0.949174351276,
                        0.959879206212, 0.980220791168), tolerance = 1e-10)
  #the projections keep the observed values
  expect_identical(mcl$paid_full[!is.na(quarg_mack_paid)], quarg_mack_paid[!is.na(quarg_mack_paid)])
  #the paid and incurred ultimates are closer than the chain ladders'
  gap <- function(paid, incurred) abs(paid - incurred) / incurred
  expect_lt(gap(mcl$total[["ultimate_paid"]], mcl$total[["ultimate_incurred"]]),
            gap(mcl$total[["chain_ladder_paid"]], mcl$total[["chain_ladder_incurred"]]))
  expect_equal(mcl$by_origin$chain_ladder_paid, chain_ladder(quarg_mack_paid)$by_origin$ultimate)
})

test_that("the Munich chain ladder needs matching positive triangles", {
  expect_error(munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred[-7, ]), "same cells")
  negative <- quarg_mack_paid
  negative[1, 1] <- -5
  expect_error(munich_chain_ladder(negative, quarg_mack_incurred), "positive")
  #the simulated motor claims: paid and incurred of the same cells
  paid <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
  incurred <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "incurred", cumulative = FALSE)
  mcl <- munich_chain_ladder(paid, incurred)
  expect_equal(nrow(mcl$by_origin), 12)
  expect_match(utils::capture.output(print(mcl)), "Total", all = FALSE)
})
