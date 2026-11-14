#Browser tests of the reserving tool (helpers in helper-app.R)

#waits until an output's text matches a JavaScript regular expression; on a timeout, the
#error shows the text it had
rsv_wait_for_text <- function(app, id, pattern) {
  #the pattern is written as a JavaScript one, /.../ with an optional i, and matched in R
  ignore_case <- grepl("/i$", pattern)
  regex <- sub("^/(.*)/i?$", "\\1", pattern)
  tryCatch(
    app_wait_for(function() grepl(regex, app_text(app, paste0("#", id)), perl = TRUE, ignore.case = ignore_case),
                 paste("#", id, "to match", pattern), timeout = 60),
    error = function(e) stop("#", id, " did not match ", pattern, "; it reads: ", app_text(app, paste0("#", id)), call. = FALSE)
  )
}

rsv_wait_for_plot <- function(app, id) {
  app$wait_for_js(sprintf("(function () { var img = document.querySelector('#%s img'); return !!img && img.naturalWidth > 0; })()", id),
                  timeout = 60 * 1000)
  image <- app_plot_image(app, id)
  expect_gt(image$opaque + image$partial, 1000)
  expect_false(app$get_js(sprintf("document.getElementById('%s').classList.contains('shiny-output-error')", id)))
}

#the id of an input of the By origin card, by its kind and origin ("method", "premium", "prior")
rsv_origin_input <- function(app, kind, origin) {
  app$get_js(sprintf("(function () {
    var label = Array.from(document.querySelectorAll('#origin_inputs label')).find(function (l) {
      return l.innerText.trim().indexOf(%s) === 0;
    });
    return label ? label.getAttribute('for') : null;
  })()", app_js_string(paste(c(method = "Method of", premium = "Premium of", prior = "A priori loss ratio of")[[kind]], origin))))
}

#a row of a results table, as text
rsv_row <- function(app, id, first_cell) {
  unlist(app$get_js(sprintf("(function () {
    var row = Array.from(document.querySelectorAll('#%s tr')).find(function (r) {
      return r.cells.length > 0 && r.cells[0].innerText.trim() === %s;
    });
    return row ? Array.from(row.cells).map(function (c) { return c.innerText.trim(); }) : null;
  })()", id, app_js_string(first_cell))))
}

test_that("the reserving tool fits the chain ladder, leaves out a link ratio the user clicks and fits a tail", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-development")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  rsv_wait_for_text(app, "data_overview", "/160,987/")
  expect_match(app_text(app, "#triangle_table"), "1990 2,063", fixed = TRUE)

  app_goto(app, "development")
  rsv_wait_for_text(app, "development_stats", "/52,135/")
  rsv_wait_for_plot(app, "development_plot")

  #leaving out the 1982 ratio from 1 to 2 (40.4) changes the first factor, as chain_ladder() does
  exclude <- matrix(FALSE, 10, 9)
  exclude[2, 1] <- TRUE
  expected <- chain_ladder(raa_triangle, exclude = exclude)
  app_click(app, selector = ".rsv-ratio[data-row='2'][data-col='1']")
  rsv_wait_for_text(app, "development_stats", sprintf("/%s/", netsimr_amount(expected$total[["ibnr"]])))
  expect_match(app_text(app, "#development_stats"), "RATIOS LEFT OUT 1 of 45", ignore.case = TRUE)
  expect_identical(app$get_js("document.querySelector(\".rsv-ratio[data-row='2'][data-col='1']\").getAttribute('aria-pressed')"), "true")
  expect_identical(rsv_row(app, "link_ratio_table", "Selected")[2], formatC(expected$factors[[1]], format = "f", digits = 3))

  #the link ratio chart, and every ratio back in
  app$set_inputs(development_tabs = "Link ratios", wait_ = FALSE)
  rsv_wait_for_plot(app, "link_ratio_plot")
  app_click(app, "include_all")
  rsv_wait_for_text(app, "development_stats", "/52,135/")

  #a fitted exponential tail, as MackChainLadder(RAA, tail = TRUE)
  app_set(app, tail_method = "exponential")
  rsv_wait_for_text(app, "development_stats", "/1\\.0094/")
  expect_match(app_text(app, "#development_stats"), netsimr_amount(chain_ladder(raa_triangle, tail = "exponential")$total[["ibnr"]]),
               fixed = TRUE)
  app$set_inputs(development_tabs = "Tail curve", wait_ = FALSE)
  rsv_wait_for_plot(app, "tail_plot")
  expect_app_logs_clean(app)
})

test_that("the ultimates follow each origin's method, premium and a priori loss ratio", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-ultimates")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_set(app, example = "motor")
  rsv_wait_for_text(app, "data_overview", "/120,431,000/")
  app_goto(app, "ultimates")
  rsv_wait_for_text(app, "ultimate_table", "/2025/")

  triangle <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
  premium <- unique(simulated_motor_claims[c("accident_year", "earned_premium")])$earned_premium
  cl <- chain_ladder(triangle)
  expect_identical(rsv_row(app, "ultimate_table", "2025")[9], netsimr_amount(cl$by_origin$ultimate[12]))
  expect_identical(rsv_row(app, "ultimate_table", "2025")[7], netsimr_amount(cape_cod(cl, premium)$by_origin$ultimate[12]))

  #2025 by Bornhuetter-Ferguson, with its own premium and loss ratio
  args <- list(app, "bf", 13e6, 65)
  names(args) <- c("", rsv_origin_input(app, "method", "2025"), rsv_origin_input(app, "premium", "2025"),
                   rsv_origin_input(app, "prior", "2025"))
  do.call(app_set, args)
  premium[12] <- 13e6
  bf <- bornhuetter_ferguson(cl, premium, c(rep(0.7, 11), 0.65))$by_origin$ultimate[12]
  rsv_wait_for_text(app, "ultimate_table", sprintf("/%s/", netsimr_amount(bf)))
  row <- rsv_row(app, "ultimate_table", "2025")
  expect_identical(row[c(8, 9)], c("Bornhuetter-Ferguson", netsimr_amount(bf)))
  expect_identical(row[11], netsimr_percent(bf / 13e6))
  expect_match(app_text(app, "#ultimate_stats"), "Mixed", fixed = TRUE)

  #one method for all origins
  app_set(app, method_all = "cc")
  rsv_wait_for_text(app, "ultimate_stats", "/Cape Cod/")
  expect_identical(rsv_row(app, "ultimate_table", "2025")[8], "Cape Cod")

  rsv_wait_for_plot(app, "ultimate_plot")
  app$set_inputs(ultimate_tabs = "Loss ratios", wait_ = FALSE)
  rsv_wait_for_plot(app, "loss_ratio_plot")

  results <- utils::read.csv(app_download(app, "download_results"))
  expect_equal(nrow(results), 12)
  expect_true(all(c("origin", "chain_ladder", "bornhuetter_ferguson", "cape_cod", "method", "ultimate", "ibnr") %in% names(results)))
  expect_equal(results$premium[12], 13e6)
  expect_app_logs_clean(app)
})

test_that("the bootstrap with a seed gives bootstrap_chain_ladder()'s reserves", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-bootstrap")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "uncertainty")
  rsv_wait_for_text(app, "mack_table", "/26,881/")
  expect_match(app_text(app, "#bootstrap_table"), "Click Run bootstrap", fixed = TRUE)

  app_set(app, boot_n = 1000, boot_seed = 7, boot_process = "odp")
  app_click(app, "run_bootstrap")
  rsv_wait_for_text(app, "bootstrap_table", "/Total/")
  expected <- bootstrap_chain_ladder(raa_triangle, n = 1000, process = "odp", seed = 7)
  row <- rsv_row(app, "bootstrap_table", "Total")
  expect_identical(row[2:3], netsimr_amount(unname(expected$total[c("ibnr", "se")])))
  expect_identical(row[9], netsimr_amount(expected$total[["p99.5"]]))
  rsv_wait_for_plot(app, "bootstrap_plot")
  app$set_inputs(uncertainty_tabs = "by calendar period", wait_ = FALSE)
  rsv_wait_for_plot(app, "residual_calendar_plot")

  simulations <- utils::read.csv(app_download(app, "download_bootstrap"), check.names = FALSE)
  expect_equal(nrow(simulations), 1000)
  expect_equal(simulations$total, expected$total_reserves)

  #another triangle: the bootstrap must run again
  app_goto(app, "data")
  app_set(app, example = "taylor_ashe")
  app_goto(app, "uncertainty")
  rsv_wait_for_text(app, "bootstrap_table", "/run the bootstrap again/")
  expect_app_logs_clean(app)
})

test_that("an uploaded table of payments makes the triangle, with the premiums", {
  skip_if_no_app_browser()
  csv <- file.path(tempdir(), "netsimr_motor_payments.csv")
  utils::write.csv(simulated_motor_claims, csv, row.names = FALSE)
  app <- start_netsimr_app("reserving", "reserving-upload")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_set(app, data_source = "upload")
  app_upload(app, "file1", csv)
  #the columns are guessed from their names
  app_wait_for_inputs(app, list(origin_col = "accident_year", development_col = "development_year", value_col = "paid",
                                premium_col = "earned_premium"), "the guessed columns")
  app_set(app, cumulative = "incremental")
  rsv_wait_for_text(app, "data_overview", sprintf("/%s/", netsimr_amount(sum(simulated_motor_claims$paid))))
  expect_match(app_text(app, "#data_overview"), "120,431,000", fixed = TRUE)
  expect_match(app_text(app, "#data_overview"), "2014 to 2025", fixed = TRUE)
  #a layout that does not fit the file says why
  app_set(app, value_col = "accident_year")
  rsv_wait_for_text(app, "triangle_table", "/different column/")
  expect_app_logs_clean(app)
})

test_that("the link ratios read as text, and their buttons stand out on hover, in both themes", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-contrast")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "development")
  rsv_wait_for_text(app, "link_ratio_table", "/Selected/")
  app_click(app, selector = ".rsv-ratio[data-row='2'][data-col='1']")
  app$wait_for_js("document.querySelectorAll('.rsv-excluded').length === 1", timeout = 30 * 1000)
  for (mode in c("light", "dark")) {
    app_theme(app, mode)
    colours <- app$get_js("(function () {
      var card = document.querySelector('#link_ratio_table').closest('.card');
      return {card: getComputedStyle(card).backgroundColor,
              used: getComputedStyle(document.querySelector('.rsv-ratio:not(.rsv-excluded)')).color,
              left_out: getComputedStyle(document.querySelector('.rsv-excluded')).color};
    })()")
    card <- app_colour(colours$card)
    expect_gte(app_contrast(app_colour(colours$used, card), card), 4.5)
    expect_gte(app_contrast(app_colour(colours$left_out, card), card), 4.5)
  }
  expect_app_logs_clean(app)
})

test_that("the Paid & incurred tab gives the Munich chain ladder, and the Uncertainty tab the one-year view", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-munich")
  on.exit(app$stop(), add = TRUE)
  #the RAA triangle has no incurred claims
  app_goto(app, "munich")
  rsv_wait_for_text(app, "munich_table", "/needs incurred claims/")

  #the one-year view of the RAA triangle
  app_goto(app, "uncertainty")
  rsv_wait_for_text(app, "one_year_table", "/Total/")
  mw <- merz_wuthrich(raa_triangle)
  expect_identical(rsv_row(app, "one_year_table", "Total")[3], netsimr_amount(mw$total[["one_year_se"]]))
  expect_match(app_text(app, "#uncertainty_stats"), netsimr_amount(mw$total[["one_year_se"]]), fixed = TRUE)

  #the Quarg and Mack example
  app_goto(app, "data")
  app_set(app, example = "quarg_mack")
  rsv_wait_for_text(app, "data_overview", "/With incurred claims/")
  app_goto(app, "munich")
  rsv_wait_for_text(app, "munich_table", "/Total/")
  mcl <- munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)
  row <- rsv_row(app, "munich_table", "7")
  expect_identical(row[c(7, 8)], netsimr_amount(unlist(mcl$by_origin[7, c("ultimate_paid", "ultimate_incurred")], use.names = FALSE)))
  rsv_wait_for_plot(app, "munich_plot")
  app$set_inputs(munich_tabs = "Paid to incurred", wait_ = FALSE)
  rsv_wait_for_plot(app, "munich_ratio_plot")
  expect_equal(nrow(utils::read.csv(app_download(app, "download_munich"))), 7)
  expect_app_logs_clean(app)
})

test_that("saved settings bring back the data, the factors, the link ratios left out and each origin's choices", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reserving", "reserving-save")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_set(app, example = "motor")
  rsv_wait_for_text(app, "data_overview", "/120,431,000/")
  #the link ratios left out: the one clicked, and those before the latest 6 periods
  motor <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
  exclude <- matrix(FALSE, 12, 11)
  exclude[8, 1] <- TRUE
  fit <- chain_ladder(motor, average = "simple", periods = 6, exclude = exclude, tail = "exponential")
  left_out <- sprintf("/RATIOS LEFT OUT\\s*%d of/i", sum(!is.na(fit$link_ratios) & !fit$used))
  app_goto(app, "development")
  app_set(app, average = "simple", tail_method = "exponential")
  app_set(app, periods = "6")
  app_click(app, selector = ".rsv-ratio[data-row='8'][data-col='1']")
  rsv_wait_for_text(app, "development_stats", left_out)
  rsv_wait_for_text(app, "development_stats", sprintf("/%s/", netsimr_amount(fit$total[["ibnr"]])))
  expected_development <- app_text(app, "#development_stats")
  app_goto(app, "ultimates")
  rsv_wait_for_text(app, "ultimate_table", "/2025/")
  app_set(app, prior_loss_ratio = 65)
  args <- list(app, "bf", 13e6)
  names(args) <- c("", rsv_origin_input(app, "method", "2025"), rsv_origin_input(app, "premium", "2025"))
  do.call(app_set, args)
  rsv_wait_for_text(app, "ultimate_stats", "/Mixed/")
  expected_table <- app_text(app, "#ultimate_table")
  app_goto(app, "data")
  file <- app_download(app, "save_settings")
  saved <- read_settings_file(file, rsv_settings_tool)$values
  expect_identical(saved$excluded_ratios, "2021 | 1-2")
  expect_identical(saved$origin_method[12], "bf")

  #a new session, with the RAA example, loads them
  fresh <- start_netsimr_app("reserving", "reserving-load")
  on.exit(fresh$stop(), add = TRUE)
  app_goto(fresh, "data")
  rsv_wait_for_text(fresh, "data_overview", "/160,987/")
  app_upload(fresh, "load_settings", file)
  app_wait_for_inputs(fresh, list(example = "motor", average = "simple", tail_method = "exponential", prior_loss_ratio = 65),
                      "the loaded settings")
  app_goto(fresh, "development")
  rsv_wait_for_text(fresh, "development_stats", left_out)
  app_wait_for_inputs(fresh, list(periods = "6"), "the loaded periods")
  expect_identical(app_text(fresh, "#development_stats"), expected_development)
  app_goto(fresh, "ultimates")
  rsv_wait_for_text(fresh, "ultimate_stats", "/Mixed/")
  expect_identical(app_text(fresh, "#ultimate_table"), expected_table)
  expect_app_logs_clean(fresh)
})
