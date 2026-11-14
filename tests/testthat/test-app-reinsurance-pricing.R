#Browser tests of the reinsurance pricing tool (helpers in helper-app.R)

#waits until an output's text matches a regular expression; on a timeout, the error shows
#the text it had
xlp_wait_for_text <- function(app, id, regex) {
  tryCatch(
    app_wait_for(function() grepl(regex, app_text(app, paste0("#", id)), perl = TRUE),
                 paste("#", id, "to match", regex), timeout = 60),
    error = function(e) stop("#", id, " did not match ", regex, "; it reads: ", app_text(app, paste0("#", id)), call. = FALSE)
  )
}

xlp_wait_for_plot <- function(app, id) {
  app$wait_for_js(sprintf("(function () { var img = document.querySelector('#%s img'); return !!img && img.naturalWidth > 0; })()", id),
                  timeout = 60 * 1000)
  image <- app_plot_image(app, id)
  expect_gt(image$opaque + image$partial, 1000)
  expect_false(app$get_js(sprintf("document.getElementById('%s').classList.contains('shiny-output-error')", id)))
}

#the burning cost of the example for a layer, as the server finds it
xlp_expected_rate <- function(deductible, limit, reinstatements = 1) {
  burning_cost(xl_large_losses, xl_premium[, c("year", "premium")], deductible, limit,
               aggregate_limit = (reinstatements + 1) * limit, inflation = 0.04, rating_year = 2025,
               development = stats::setNames(xl_premium$development, xl_premium$year),
               premium_index = stats::setNames(xl_premium$rate_index, xl_premium$year), threshold = 5e5)$rate
}

test_that("the reinsurance pricing tool rates the experience and the exposure, and prices the layer", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("reinsurance", "reinsurance-pricing")
  on.exit(app$stop(), add = TRUE)

  app_goto(app, "experience")
  xlp_wait_for_text(app, "experience_stats", netsimr_percent(xlp_expected_rate(1e6, 4e6), 2))
  expect_match(app_text(app, "#burning_cost_table"), "2015")
  xlp_wait_for_plot(app, "burning_cost_plot")
  app$set_inputs(experience_tabs = "Pareto fit", wait_ = FALSE)
  xlp_wait_for_plot(app, "pareto_plot")

  #another layer, set on the Layer tab, rates again
  app_goto(app, "layer")
  app_set(app, deductible = 2e6)
  xlp_wait_for_text(app, "layer_stats", "4,000,000 xs 2,000,000")
  xlp_wait_for_text(app, "glance_table", netsimr_percent(xlp_expected_rate(2e6, 4e6), 2))
  xlp_wait_for_plot(app, "layer_losses_plot")

  #the casualty example, on its claim size distribution
  app_goto(app, "exposure")
  xlp_wait_for_text(app, "exposure_table", "(?i)of sum insured")
  app_set(app, profile_example = "casualty")
  xlp_wait_for_text(app, "exposure_table", "(?i)policy limit")
  expected <- casualty_exposure_rating(xl_casualty_profile, 2e6, 4e6, "LogNormal", c(10, 2), 0.55)
  xlp_wait_for_text(app, "exposure_stats", netsimr_percent(expected$rate, 2))
  app$set_inputs(exposure_tabs = "Curve", wait_ = FALSE)
  xlp_wait_for_plot(app, "curve_plot")

  #the pricing: the credibility button sets the weight of the experience
  app_goto(app, "pricing")
  xlp_wait_for_text(app, "pricing_stats", "50% Burning cost")
  app_set(app, full_credibility = 100)
  app_click(app, "use_credibility")
  app_wait_for(function() !identical(app_inputs(app, "experience_weight")$experience_weight, 50L) &&
                 !identical(app_inputs(app, "experience_weight")$experience_weight, 50),
               "the credibility weight", timeout = 30)
  xlp_wait_for_plot(app, "method_plot")
  expect_match(app_text(app, "#pricing_table"), "Blended")
  expect_app_logs_clean(app)
})
