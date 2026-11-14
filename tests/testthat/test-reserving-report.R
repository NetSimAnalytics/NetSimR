#the reserving tool's report and its comparison of tail factors

test_that("a tail curve is extrapolated over the periods asked for", {
  short <- chain_ladder(raa_triangle, tail = "inverse power", tail_periods = 10)
  long <- chain_ladder(raa_triangle, tail = "inverse power")
  expect_identical(long$tail_curve$periods, 100)
  expect_lt(short$tail, long$tail)
  expect_equal(long$tail, chain_ladder(raa_triangle, tail = "inverse power", tail_periods = 100)$tail)
  #by hand: the product of the fitted factors of the next ten periods
  curve <- short$tail_curve
  expect_equal(short$tail, prod(curve$fitted(10:19)))
  expect_error(chain_ladder(raa_triangle, tail = "exponential", tail_periods = 0), "tail_periods")
})

test_that("the report holds the figures of the tool, with the bootstrap once it has run", {
  skip_server_on_cran()
  testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "motor", tail_method = "exponential", tail_from = "1",
                      tail_periods = 50, average = "volume", sigma = "log-linear")
    tails <- tail_options()
    expect_identical(tails$method[1], "No tail")
    expect_equal(tails$tail[2], cl()$tail)
    expect_equal(tails$ibnr[2], cl()$total[["ibnr"]])
    expect_true(all(is.finite(tails$tail)))
    expect_match(output$tail_comparison$html, "Clark log-logistic")
    report <- output$download_report
    html <- paste(readLines(report, warn = FALSE), collapse = "\n")
    for (section in c("Key results", "Data and settings", "Development factors", "Ultimates", "Uncertainty",
                      "More methods", "Paid and incurred", "Tail factors compared")) {
      expect_match(html, section, fixed = TRUE)
    }
    expect_match(html, netsimr_amount(cl()$total[["ibnr"]]), fixed = TRUE)
    expect_match(html, "The bootstrap was not run", fixed = TRUE)
    #every chart in both themes
    expect_equal(lengths(regmatches(html, gregexpr("class=\"chart-light\"", html))),
                 lengths(regmatches(html, gregexpr("class=\"chart-dark\"", html))))
    session$setInputs(boot_n = 500, boot_seed = 1, boot_tail = TRUE, boot_process = "gamma", run_bootstrap = 1)
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    expect_match(html, "Distribution of the total IBNR", fixed = TRUE)
    expect_match(html, netsimr_amount(current_boot()$total[["p99.5"]]), fixed = TRUE)
  })
})

test_that("the report says which parts it leaves out, and why", {
  skip_server_on_cran()
  testServer(reserving_tool_Server, {
    #the RAA triangle has no incurred claims, and the bootstrap has not run
    session$setInputs(data_source = "example", example = "raa", tail_method = "none", tail_from = "1",
                      average = "volume", sigma = "log-linear")
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    expect_match(html, "Not in this report", fixed = TRUE)
    expect_match(html, "Munich chain ladder:</strong> The Munich chain ladder needs incurred claims", fixed = TRUE)
    expect_match(html, "Risk adjustment:</strong> The percentile risk adjustment needs the bootstrap", fixed = TRUE)
  })
})

test_that("the section of what a report leaves out lists the errors among its parts", {
  expect_null(netsimr_report_omitted(list(Clark = 1, Munich = NULL)))
  section <- netsimr_report_omitted(list(Clark = 1, Munich = simpleError("needs incurred claims"),
                                         Bootstrap = netsimr_report_part(stop(""))))
  html <- as.character(section$content)
  expect_identical(section$id, "not-included")
  expect_match(html, "<strong>Munich:</strong> needs incurred claims", fixed = TRUE)
  expect_match(html, "<strong>Bootstrap:</strong> not available.", fixed = TRUE)
  expect_no_match(html, "Clark", fixed = TRUE)
})
