#The helpers of the reserving tool: reading an uploaded table, the ultimates of each method, the
#tables and the charts

test_that("an uploaded table in the long form gives the triangle and each origin's premium", {
  df <- data.frame(ay = c("2021", "2021", "2022"), dev = c("1", "2", "1"), paid = c("10,5", "4", "12"),
                   premium = c("100", "100", "120"), stringsAsFactors = FALSE)
  #a decimal comma, as the Data tab's setting reads it
  x <- rsv_upload_input(df, "long", "ay", "dev", "paid", "premium", cumulative = FALSE, dec = ",")
  expect_equal(unname(x$triangle), matrix(c(10.5, 12, 14.5, NA), 2))
  expect_equal(x$premium, c(100, 120))
  #the premium must be one per origin
  df$premium[2] <- "90"
  expect_error(rsv_upload_input(df, "long", "ay", "dev", "paid", "premium", cumulative = FALSE, dec = ","),
               "more than one value for origin 2021")
  expect_error(rsv_upload_input(df, "long", "ay", "dev", "dev"), "different column")
  expect_error(rsv_upload_input(df, "long", "nope", "dev", "paid"), "origin column")
  expect_error(rsv_upload_input(df, "long", "ay", NULL, "paid"), "development period and value")
  #no premium column: no premiums
  expect_null(rsv_upload_input(df, "long", "ay", "dev", "paid", cumulative = FALSE, dec = ",")$premium)
})

test_that("an uploaded table in the wide form takes every other numeric column as a development period", {
  df <- data.frame(year = c(2020, 2021, 2022), premium = c(50, 60, 70), `12` = c(10, 12, 15), `24` = c(16, 18, NA),
                   `36` = c(17, NA, NA), note = c("a", "b", "c"), check.names = FALSE)
  x <- rsv_upload_input(df, "wide", "year", premium = "premium")
  expect_identical(colnames(x$triangle), c("12", "24", "36"))
  expect_equal(unname(x$triangle), unname(as.matrix(df[c("12", "24", "36")])))
  expect_equal(x$premium, c(50, 60, 70))
  expect_error(rsv_upload_input(df[c("year", "12")], "wide", "year"), "at least two numeric development period columns")
})

test_that("each method's ultimate and the selected one", {
  cl <- chain_ladder(raa_triangle)
  premium <- seq(50000, 68000, by = 2000)
  methods <- c(rep("cl", 7), "bf", "bk", "cc")
  r <- rsv_method_results(cl, premium, rep(0.35, 10), decay = 1, selected = methods)
  bf <- bornhuetter_ferguson(cl, premium, 0.35)
  expect_equal(r$bornhuetter_ferguson, bf$by_origin$ultimate)
  expect_equal(r$benktander, bornhuetter_ferguson(cl, premium, 0.35, iterations = 2)$by_origin$ultimate)
  expect_equal(r$cape_cod, cape_cod(cl, premium)$by_origin$ultimate)
  expect_equal(r$ultimate, c(cl$by_origin$ultimate[1:7], r$bornhuetter_ferguson[8], r$benktander[9], r$cape_cod[10]))
  expect_equal(r$ibnr, r$ultimate - r$latest)
  expect_identical(r$method[8:10], c("Bornhuetter-Ferguson", "Benktander", "Cape Cod"))
  #Mack's standard error belongs to the chain ladder only
  expect_equal(r$mack_se[1:7], cl$by_origin$se[1:7])
  expect_true(all(is.na(r$mack_se[8:10])))
  expect_equal(attr(r, "cape_cod_loss_ratio"), cape_cod(cl, premium)$total[["loss_ratio"]])

  #without an origin's premium, its expected loss ratio methods and every origin's Cape Cod are missing
  premium[9] <- NA
  r <- rsv_method_results(cl, premium, rep(0.35, 10), decay = 1, selected = methods)
  expect_true(is.na(r$bornhuetter_ferguson[9]) && is.na(r$ultimate[9]))
  expect_true(all(is.na(r$cape_cod)))
  expect_equal(r$bornhuetter_ferguson[8], bf$by_origin$ultimate[8])
  expect_true(is.na(r$ultimate_loss_ratio[9]))
})

test_that("the link ratio table has a button for each ratio, and marks those left out", {
  exclude <- matrix(FALSE, 10, 9)
  exclude[2, 1] <- TRUE
  cl <- chain_ladder(raa_triangle, exclude = exclude, periods = 5)
  outside <- !reserving_recent_ratios(cl$link_ratios, cl$latest_period, 5)
  html <- as.character(rsv_link_ratio_table(cl, exclude, outside))
  expect_equal(lengths(regmatches(html, gregexpr("<button", html))), 45)
  expect_match(html, "class=\"rsv-ratio rsv-excluded\" data-row=\"2\" data-col=\"1\" aria-pressed=\"true\"", fixed = TRUE)
  expect_match(html, "40.425, left out. Include it.", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("rsv-outside", html))), sum(outside & !exclude & !is.na(cl$link_ratios)))
  #the latest origin has no link ratios, and no row
  expect_no_match(html, "<td>1990</td>", fixed = TRUE)
  #the selected factors, the tail, and the pattern from each period to ultimate
  expect_match(html, paste0("<td>", formatC(cl$factors[1], format = "f", digits = 3), "</td>"), fixed = TRUE)
  expect_match(html, paste0("<td>", formatC(cl$cdf[1], format = "f", digits = 3), "</td>"), fixed = TRUE)
  expect_match(html, "<td>1.0000</td>", fixed = TRUE)
})

test_that("a triangle table shows the projected values apart", {
  cl <- chain_ladder(raa_triangle)
  html <- as.character(rsv_triangle_table(cl$full_triangle, cbind(is.na(raa_triangle), TRUE)))
  expect_equal(lengths(regmatches(html, gregexpr("rsv-projected", html))), 45 + 10)
  expect_match(html, "<td>18,834</td>", fixed = TRUE)
  expect_match(html, "<th>Ultimate</th>", fixed = TRUE)
})

test_that("the reserving charts draw in both themes", {
  cl <- chain_ladder(raa_triangle, tail = "exponential")
  boot <- bootstrap_chain_ladder(raa_triangle, n = 200, seed = 1)
  r <- rsv_method_results(cl, seq(50000, 68000, by = 2000), rep(0.35, 10), 1, rep("cl", 10))
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  for (dark in c(FALSE, TRUE)) {
    grDevices::png(file, width = 800, height = 500)
    expect_no_error({
      rsv_development_chart(cl, dark)
      rsv_link_ratio_chart(cl, dark)
      rsv_tail_chart(cl, dark)
      rsv_ultimate_chart(r$origin, r$latest, r$ibnr, list(`Chain ladder` = r$chain_ladder, `Cape Cod` = r$cape_cod), dark)
      rsv_bootstrap_chart(boot$total_reserves, boot$total[["ibnr"]], c(`75th percentile` = boot$total[["p75"]]), dark)
      for (by in c("origin", "development", "calendar")) rsv_residual_chart(boot$residuals, by, dark)
    })
    grDevices::dev.off()
  }
})

test_that("the examples load with their premiums", {
  motor <- rsv_example_input("motor")
  expect_equal(dim(motor$triangle), c(12, 12))
  expect_equal(motor$premium, unique(simulated_motor_claims[c("accident_year", "earned_premium")])$earned_premium)
  expect_null(rsv_example_input("raa")$premium)
  expect_identical(rsv_example_input("taylor_ashe")$triangle, taylor_ashe_triangle)
  expect_identical(unname(rsv_examples), c("raa", "taylor_ashe", "motor", "quarg_mack"))
  #the paid and incurred examples have both triangles, in the same cells
  for (id in c("motor", "quarg_mack")) {
    x <- rsv_example_input(id)
    expect_identical(is.na(x$incurred), is.na(x$triangle))
  }
  expect_identical(rsv_example_input("quarg_mack")$incurred, quarg_mack_incurred)
  expect_null(rsv_example_input("raa")$incurred)
})

test_that("an uploaded table gives an incurred triangle from its incurred column", {
  df <- data.frame(ay = c(2021, 2021, 2022), dev = c(1, 2, 1), paid = c(10, 4, 12), incurred = c(20, 2, 25))
  x <- rsv_upload_input(df, "long", "ay", "dev", "paid", cumulative = FALSE, incurred = "incurred")
  expect_equal(unname(x$incurred), matrix(c(20, 25, 22, NA), 2))
  expect_error(rsv_upload_input(df, "long", "ay", "dev", "paid", incurred = "paid"), "different column")
  #an incurred amount missing where a paid one is
  short <- df
  short$incurred[3] <- NA
  expect_error(rsv_upload_input(short, "long", "ay", "dev", "paid", cumulative = FALSE, incurred = "incurred"),
               "same origins and development periods")
})

test_that("the link ratios left out are kept by origin and factor", {
  ratios <- reserving_link_ratios(raa_triangle)
  excluded <- matrix(FALSE, nrow(ratios), ncol(ratios), dimnames = dimnames(ratios))
  excluded["1982", "1-2"] <- TRUE
  excluded["1985", "3-4"] <- TRUE
  labels <- rsv_exclusion_labels(excluded)
  expect_identical(labels, c("1982 | 1-2", "1985 | 3-4"))
  expect_identical(rsv_exclusion_matrix(labels, ratios), excluded)
  #labels of another triangle, or of a ratio it does not have, are passed over
  expect_false(any(rsv_exclusion_matrix(c("2014 | 1-2", "1990 | 1-2", "garbled"), ratios)))
  expect_identical(rsv_exclusion_labels(excluded & FALSE), character(0))
})
