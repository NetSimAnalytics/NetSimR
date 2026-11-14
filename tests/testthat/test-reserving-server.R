#the reserving tool's server, driven through shiny::testServer(): it runs on every platform and on
#CRAN, unlike the browser tests (test-app-reserving.R). The test session has no page, so updates
#the server sends to inputs go nowhere; where the page would set an input in return, the test
#sets it

#the text of a rendered UI output
ui_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(x$html)))

motor_triangle <- function(value = "paid") {
  as_triangle(simulated_motor_claims, "accident_year", "development_year", value, cumulative = FALSE)
}
motor_premium <- function() unique(simulated_motor_claims[c("accident_year", "earned_premium")])$earned_premium

test_that("the server fits the example's chain ladder with the development settings", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa")
    expect_identical(triangle(), raa_triangle)
    expect_null(data_input()$premium)
    expect_equal(cl()$total[["se"]], chain_ladder(raa_triangle)$total[["se"]])
    expect_match(ui_text(output$data_overview), "160,987")
    expect_match(ui_text(output$development_stats), "52,135")
    #simple averages of the latest 5 periods, with a fitted tail and Mack's rule
    session$setInputs(average = "simple", periods = "5", tail_method = "exponential", sigma = "Mack")
    expected <- chain_ladder(raa_triangle, average = "simple", periods = 5, tail = "exponential", sigma = "Mack")
    expect_equal(cl()$total, expected$total)
    expect_match(as.character(output$link_ratio_table$html), "rsv-outside", fixed = TRUE)
    #an entered tail factor, and one the server refuses
    session$setInputs(tail_method = "factor", tail_factor = 1.05)
    expect_equal(cl()$tail, 1.05)
    session$setInputs(tail_factor = -1)
    expect_error(cl(), "tail factor above 0")
    #a curve fitted from a later factor
    session$setInputs(tail_method = "exponential", tail_from = "3")
    expect_identical(cl()$tail_curve$from, 3L)
    expect_match(ui_text(output$projected_table), "Ultimate")
    expect_match(ui_text(output$development_notes), "simple averages")
  })
})

test_that("a clicked link ratio is left out, and brought back", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa")
    cl()
    session$setInputs(ratio_toggle = list(row = 2, col = 1))
    expect_true(excluded()[2, 1])
    exclude <- matrix(FALSE, 10, 9)
    exclude[2, 1] <- TRUE
    expect_equal(cl()$factors, chain_ladder(raa_triangle, exclude = exclude)$factors)
    expect_match(ui_text(output$development_stats), "Ratios left out 1 of 45")
    #clicked again, it is back
    session$setInputs(ratio_toggle = list(row = 2, col = 1))
    expect_false(excluded()[2, 1])
    #a position outside the triangle is ignored
    session$setInputs(ratio_toggle = list(row = 99, col = 1))
    expect_false(any(excluded()))
    session$setInputs(ratio_toggle = list(row = 3, col = 2))
    session$setInputs(include_all = 1)
    expect_false(any(excluded()))
    #another triangle starts with every link ratio
    session$setInputs(ratio_toggle = list(row = 3, col = 2))
    session$setInputs(example = "taylor_ashe")
    cl()
    expect_false(any(excluded()))
    #reading the same triangle again keeps them
    session$setInputs(ratio_toggle = list(row = 3, col = 2))
    session$setInputs(app_theme = "dark")
    expect_true(excluded()[3, 2])
  })
})

test_that("each origin's method, premium and loss ratio give its ultimate", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "motor", prior_loss_ratio = 70, decay = 1, method_all = "cl")
    cl_motor <- chain_ladder(motor_triangle())
    #before the By origin card shows, the data's premiums and the settings for all origins
    r <- results()
    expect_equal(r$premium, motor_premium())
    expect_equal(r$ultimate, cl_motor$by_origin$ultimate)
    expect_equal(r$cape_cod, cape_cod(cl_motor, motor_premium())$by_origin$ultimate)
    #the inputs of the last origin: Bornhuetter-Ferguson with its own premium and loss ratio
    g <- generation()
    inputs <- list(13e6, 65, "bf")
    names(inputs) <- paste0(c("premium_", "prior_", "method_"), g, "_12")
    do.call(session$setInputs, inputs)
    premium <- motor_premium()
    premium[12] <- 13e6
    bf <- bornhuetter_ferguson(cl_motor, premium, c(rep(0.7, 11), 0.65))$by_origin$ultimate[12]
    expect_equal(results()$ultimate[12], bf)
    expect_match(ui_text(output$ultimate_stats), "Mixed")
    expect_match(ui_text(output$ultimate_table), "Bornhuetter-Ferguson")
    #without a premium, the origin has no ultimate, and the table says why
    do.call(session$setInputs, stats::setNames(list(NA), paste0("premium_", g, "_12")))
    expect_true(is.na(results()$ultimate[12]))
    expect_match(ui_text(output$ultimate_table), "No ultimate for 2025")
    expect_match(ui_text(output$ultimate_table), "Cape Cod every origin")
    #the downloads
    results_file <- utils::read.csv(output$download_results)
    expect_equal(nrow(results_file), 12)
    expect_true("cdf" %in% names(results_file))
    expect_equal(nrow(utils::read.csv(output$download_triangle)), 12)
  })
})

test_that("the RAA triangle has no premiums, so its expected loss ratio methods say so", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa", prior_loss_ratio = 70, method_all = "bf")
    r <- results()
    expect_true(all(is.na(r$premium)))
    expect_true(all(is.na(r$ultimate)))
    expect_match(ui_text(output$ultimate_stats), "Needs every origin's premium")
    expect_error(output$loss_ratio_plot, "Enter premiums")
    expect_match(ui_text(output$data_overview), "None")
  })
})

test_that("the Paid & incurred tab and the one-year view give the package's results", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa")
    expect_error(munich(), "needs incurred claims")
    mw <- merz_wuthrich(raa_triangle)
    expect_equal(one_year()$total, mw$total)
    expect_match(ui_text(output$one_year_table), "25,166")
    expect_match(ui_text(output$uncertainty_stats), "25,166")
    expect_match(ui_text(output$mack_table), "26,881")

    session$setInputs(example = "quarg_mack")
    expect_equal(munich()$by_origin, munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)$by_origin)
    expect_match(ui_text(output$munich_stats), "Paid ultimate")
    expect_match(ui_text(output$munich_table), "7,505")
    expect_match(ui_text(output$data_overview), "With incurred claims")
    expect_equal(nrow(utils::read.csv(output$download_munich)), 7)
  })
})

test_that("the bootstrap runs on the triangle showing, and asks to run again for another", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa", boot_n = 500, boot_seed = 3, boot_process = "gamma",
                      boot_tail = FALSE)
    expect_error(current_boot(), "Click Run bootstrap")
    session$setInputs(run_bootstrap = 1)
    expected <- bootstrap_chain_ladder(raa_triangle, n = 500, seed = 3)
    expect_equal(current_boot()$total, expected$total)
    expect_match(ui_text(output$bootstrap_table), "500 simulations")
    expect_equal(utils::read.csv(output$download_bootstrap, check.names = FALSE)$total, expected$total_reserves)
    #the tail of the Development tab, when asked
    session$setInputs(tail_method = "factor", tail_factor = 1.02, boot_tail = TRUE, run_bootstrap = 2)
    expect_equal(current_boot()$tail, 1.02)
    #a number of simulations out of range
    session$setInputs(boot_n = 50, run_bootstrap = 3)
    expect_error(boot(), "from 100 to 100,000")
    #another triangle
    session$setInputs(boot_n = 500, run_bootstrap = 4)
    current_boot()
    session$setInputs(example = "taylor_ashe")
    expect_error(current_boot(), "run the bootstrap again")
  })
})

test_that("an uploaded table is read with the chosen columns", {
  skip_server_on_cran()
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  utils::write.csv(simulated_motor_claims, file, row.names = FALSE)
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "upload", file1 = list(datapath = file, name = "motor.csv"), data_includes_header = TRUE,
                      sep = ",", dec = ".", quote = "\"", layout = "long", origin_col = "accident_year",
                      development_col = "development_year", value_col = "paid", incurred_col = "incurred",
                      premium_col = "earned_premium", cumulative = "incremental")
    expect_equal(unname(triangle()), unname(motor_triangle()))
    expect_equal(unname(data_input()$incurred), unname(motor_triangle("incurred")))
    expect_equal(data_premium(), motor_premium())
    expect_match(ui_text(output$data_overview), "motor.csv")
    #a column chosen twice
    session$setInputs(value_col = "accident_year")
    expect_error(data_input(), "different column")
    #the wide layout of the RAA triangle
    wide <- tempfile(fileext = ".csv")
    on.exit(unlink(wide), add = TRUE)
    utils::write.csv(data.frame(origin = rownames(raa_triangle), raa_triangle, check.names = FALSE), wide, row.names = FALSE,
                     na = "")
    session$setInputs(file1 = list(datapath = wide, name = "raa.csv"), layout = "wide", origin_col = "origin",
                      premium_col = "none", cumulative = "cumulative")
    expect_equal(unname(triangle()), unname(raa_triangle))
  })
})

test_that("a saved settings file holds the session's choices, by origin", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "motor", average = "simple", periods = "6",
                      tail_method = "exponential", tail_from = "1", sigma = "log-linear", prior_loss_ratio = 65,
                      decay = 1, method_all = "cl", boot_n = 2000, boot_process = "odp", boot_tail = TRUE, boot_seed = 9,
                      origin_col = "")
    cl()
    session$setInputs(ratio_toggle = list(row = 8, col = 1))
    g <- generation()
    do.call(session$setInputs, stats::setNames(list("bf", 13e6), paste0(c("method_", "premium_"), g, "_12")))
    saved <- read_settings_file(output$save_settings, rsv_settings_tool)
    v <- saved$values
    expect_identical(saved$version, as.numeric(rsv_settings_version))
    expect_identical(v$example, "motor")
    expect_identical(v$periods, "6")
    expect_identical(v$boot_process, "odp")
    expect_identical(v$excluded_ratios, "2021 | 1-2")
    expect_identical(v$origin_labels, as.character(2014:2025))
    expect_identical(v$origin_method[12], "bf")
    expect_equal(v$origin_premium[12], 13e6)
    expect_equal(v$origin_prior, rep(65, 12))
    #an empty column select is not saved
    expect_null(v$origin_col)
  })
})

test_that("a loaded settings file sets the triangle it names, its link ratios and each origin's choices", {
  skip_server_on_cran()
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  write_settings_file(list(data_source = "example", example = "motor", average = "simple", periods = "6",
                           tail_method = "exponential", prior_loss_ratio = 65, method_all = "cl",
                           excluded_ratios = "2021 | 1-2", origin_labels = as.character(2014:2025),
                           origin_premium = c(motor_premium()[-12], 13e6), origin_prior = rep(65, 12),
                           origin_method = c(rep("cl", 11), "bf")),
                      file, rsv_settings_tool, rsv_settings_version)
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa", prior_loss_ratio = 70, method_all = "cl")
    cl()
    session$setInputs(load_settings = list(datapath = file, name = "motor.txt"))
    #another triangle is to come: its periods and link ratios wait for it
    pending <- loaded$pending
    expect_identical(pending$exclusions, "2021 | 1-2")
    expect_identical(pending$periods, "6")
    #the page sets the inputs the server updated
    session$setInputs(example = "motor", average = "simple", tail_method = "exponential", prior_loss_ratio = 65)
    cl()
    expect_true(excluded()["2021", "1-2"])
    expect_null(loaded$pending)
    session$setInputs(periods = "6")
    exclude <- matrix(FALSE, 12, 11)
    exclude[8, 1] <- TRUE
    expect_equal(cl()$total, chain_ladder(motor_triangle(), average = "simple", periods = 6, exclude = exclude,
                                          tail = "exponential")$total)
    #the loss ratio for all origins changed with the file, which kept each origin's own values
    v <- origin_values()
    expect_identical(v$method[12], "bf")
    expect_equal(v$premium[12], 13e6)
    expect_equal(v$prior, rep(0.65, 12))
    expect_false(is.null(loaded_origins()))
    #a later change of the loss ratio for all origins sets it for every origin again
    session$setInputs(prior_loss_ratio = 60)
    expect_null(loaded_origins())
  })
})

test_that("a loaded settings file for the triangle showing sets its link ratios at once, and a bad file is refused", {
  skip_server_on_cran()
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  write_settings_file(list(data_source = "example", example = "raa", excluded_ratios = c("1982 | 1-2", "1990 | 1-2")),
                      file, rsv_settings_tool, rsv_settings_version)
  bad <- tempfile(fileext = ".txt")
  on.exit(unlink(bad), add = TRUE)
  write_settings_file(list(example = "raa"), bad, sim_settings_tool, sim_settings_version)
  newer <- tempfile(fileext = ".txt")
  on.exit(unlink(newer), add = TRUE)
  write_settings_file(list(example = "raa"), newer, rsv_settings_tool, rsv_settings_version + 1)
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa")
    cl()
    session$setInputs(load_settings = list(datapath = file, name = "raa.txt"))
    #(the 1990 label names no link ratio and is passed over)
    expect_identical(which(excluded()), 2L)
    expect_null(loaded$pending)
    #a file of another tool, or of a newer version, changes nothing
    session$setInputs(load_settings = list(datapath = bad, name = "simulator.txt"))
    expect_identical(which(excluded()), 2L)
    session$setInputs(load_settings = list(datapath = newer, name = "newer.txt"))
    expect_identical(which(excluded()), 2L)
  })
})

test_that("the charts of the reserving tool render", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "motor", app_theme = "light", tail_method = "exponential",
                      prior_loss_ratio = 70, method_all = "cl", decay = 1, boot_n = 200, boot_seed = 1, run_bootstrap = 1)
    for (id in c("development_plot", "link_ratio_plot", "tail_plot", "ultimate_plot", "loss_ratio_plot",
                 "munich_plot", "munich_ratio_plot", "bootstrap_plot", "residual_origin_plot",
                 "residual_development_plot", "residual_calendar_plot")) {
      expect_match(output[[id]]$src, "^data:image/png;base64,", label = id)
    }
    #without a tail curve, the tail chart explains itself
    session$setInputs(tail_method = "none")
    expect_error(output$tail_plot, "Choose an exponential or inverse power tail curve")
  })
})

test_that("the More methods tab gives the actual vs expected, Clark's curve and the chain ladder for inflation", {
  skip_server_on_cran()
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "raa")
    expect_equal(ave()$total, actual_vs_expected(raa_triangle)$total)
    expect_match(ui_text(output$ave_table), "Actual / expected")
    expect_match(output$ave_plot$src, "^data:image/png;base64,")
    #Clark's LDF method, then the Cape Cod method, which needs premiums the RAA example lacks
    expect_equal(clark()$total, clark_growth(raa_triangle)$total)
    expect_match(ui_text(output$clark_table), "omega")
    expect_match(output$clark_plot$src, "^data:image/png;base64,")
    session$setInputs(clark_curve = "weibull", clark_truncation = 20)
    expect_equal(clark()$total, clark_growth(raa_triangle, "weibull", truncation = 20)$total)
    session$setInputs(clark_method = "cape cod")
    expect_error(clark(), "needs every origin's premium")
    #inflation of 5% in the past and 8% in the future
    session$setInputs(inflation_past = 5, inflation_future = 8)
    expect_equal(inflation()$total, inflation_adjusted_chain_ladder(raa_triangle, 0.05, 0.08)$total)
    expect_match(ui_text(output$inflation_table), "today's money")
    stats <- ui_text(output$more_stats)
    expect_match(stats, "Actual vs expected")
    expect_match(stats, "of future inflation")
  })
  #the motor example has premiums, so the Cape Cod method runs
  shiny::testServer(reserving_tool_Server, {
    session$setInputs(data_source = "example", example = "motor", clark_method = "cape cod")
    expect_match(clark()$method, "Cape Cod")
    expect_equal(clark()$total[["ibnr"]], clark_growth(triangle(), premium = motor_premium(), method = "cape cod")$total[["ibnr"]])
  })
})
