#Browser tests of the portfolio capital tool (helpers in helper-app.R)

pfo_wait_for_text <- function(app, id, regex) {
  tryCatch(
    app_wait_for(function() grepl(regex, app_text(app, paste0("#", id)), perl = TRUE),
                 paste("#", id, "to match", regex), timeout = 60),
    error = function(e) stop("#", id, " did not match ", regex, "; it reads: ", app_text(app, paste0("#", id)), call. = FALSE)
  )
}

pfo_wait_for_plot <- function(app, id) {
  app$wait_for_js(sprintf("(function () { var img = document.querySelector('#%s img'); return !!img && img.naturalWidth > 0; })()", id),
                  timeout = 60 * 1000)
  image <- app_plot_image(app, id)
  expect_gt(image$opaque + image$partial, 1000)
}

test_that("the portfolio tool sets the lines and their dependence, and measures the capital", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("portfolio", "portfolio")
  on.exit(app$stop(), add = TRUE)

  app_goto(app, "lines")
  pfo_wait_for_text(app, "lines_table", "Negative Binomial, mean 8")
  #a fourth line, and a new distribution with its own parameters
  app_set(app, n_lines = 4)
  app$wait_for_js("!!document.getElementById('line_name_4')", timeout = 30000)
  app_set(app, line_severity_4 = "Gamma")
  app_wait_for_inputs(app, list(line_s1_4 = 2, line_s2_4 = 5000), "the Gamma defaults")
  pfo_wait_for_text(app, "lines_stats", "(?i)lines\\s+4")
  expect_no_horizontal_scroll(app, "The Lines tab")

  app_goto(app, "dependence")
  app_set(app, corr_mode = "matrix")
  app$wait_for_js("!!document.getElementById('corr_3_4')", timeout = 30000)
  app_set(app, corr_1_2 = 0.9, corr_1_3 = 0.9, corr_2_3 = -0.9)
  pfo_wait_for_text(app, "dependence_stats", "Not valid")
  app_set(app, corr_2_3 = 0.8)
  pfo_wait_for_text(app, "dependence_stats", "Valid")

  app_goto(app, "capital")
  app_set(app, n_sims = 5000)
  app_click(app, "run")
  pfo_wait_for_text(app, "capital_stats", "(?i)diversification")
  expect_match(app_text(app, "#capital_table"), "Marine")
  pfo_wait_for_plot(app, "capital_plot")
  app$set_inputs(capital_tabs = "Dependence", wait_ = FALSE)
  pfo_wait_for_plot(app, "rank_plot")
  report <- app_download(app, "download_report")
  expect_true(app_bytes_contain(app_file_bytes(report), "Portfolio Capital Report"))
  expect_app_logs_clean(app)
})
