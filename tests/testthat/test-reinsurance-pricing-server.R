#the reinsurance pricing tool's server, driven through shiny::testServer(): it runs on every
#platform and on CRAN, unlike the browser tests (test-app-reinsurance-pricing.R). The test session
#has no page, so updates the server sends to inputs go nowhere; where the page would set an input
#in return, the test sets it

#the text of a rendered UI output
xlp_ui_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(x$html)))

#the page's starting inputs
xlp_page_inputs <- function(session, ...) {
  inputs <- list(deductible = 1e6, limit = 4e6, reinstatements = 1, reinstatement_rate = 100, aggregate_deductible = 0,
                 rating_year = 2025, rating_premium = 6e7, loading = 25, loss_source = "example", first_year = NA,
                 last_year = NA, inflation = 4, threshold = 5e5, weights = "premium", pareto_threshold = 7.5e5,
                 pareto_alpha = NA, profile_source = "example", profile_example = "property", loss_ratio = 55,
                 curve = "Y3", severity = "LogNormal", severity_p1 = 10, severity_p2 = 2,
                 experience_method = "burning_cost", experience_weight = 50, full_credibility = 1082)
  do.call(session$setInputs, utils::modifyList(inputs, list(...)))
}

xlp_example_burning_cost <- function(...) {
  burning_cost(xl_large_losses, xl_premium[, c("year", "premium")], 1e6, 4e6, aggregate_limit = 8e6, inflation = 0.04,
               rating_year = 2025, development = stats::setNames(xl_premium$development, xl_premium$year),
               premium_index = stats::setNames(xl_premium$rate_index, xl_premium$year), threshold = 5e5,
               rating_premium = 6e7, ...)
}

test_that("the server rates the example's experience with the layer and each year's factors", {
  skip_server_on_cran()
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session)
    expected <- xlp_example_burning_cost()
    expect_equal(bc()$rate, expected$rate)
    expect_equal(bc()$by_year, expected$by_year)
    expect_identical(years(), 2015:2024)
    expect_match(xlp_ui_text(output$experience_stats), "Losses in the layer")
    expect_match(xlp_ui_text(output$burning_cost_table), netsimr_percent(expected$rate, 2), fixed = TRUE)
    expect_match(xlp_ui_text(output$year_inputs), "Rate index")
    #the Pareto with the example's factors
    pr <- pareto_rating(xl_large_losses, xl_premium[, c("year", "premium")], 7.5e5, 1e6, 4e6, inflation = 0.04,
                        rating_year = 2025, development = stats::setNames(xl_premium$development, xl_premium$year),
                        premium_index = stats::setNames(xl_premium$rate_index, xl_premium$year), rating_premium = 6e7)
    expect_equal(pareto()$rate, pr$rate)
    expect_match(xlp_ui_text(output$pareto_note), "losses above 750,000")
    #a year's premium, index and development typed in, and a year left out
    g <- generation()
    do.call(session$setInputs, stats::setNames(list(5e7, 1.1, 2, FALSE),
                                               paste0("year_", c("premium", "index", "development", "include"), "_", g, "_10")))
    premium <- xl_premium[, c("year", "premium")]
    premium$premium[10] <- 5e7
    changed <- burning_cost(xl_large_losses, premium, 1e6, 4e6, aggregate_limit = 8e6, inflation = 0.04, rating_year = 2025,
                            development = stats::setNames(c(xl_premium$development[-10], 2), xl_premium$year),
                            premium_index = stats::setNames(c(xl_premium$rate_index[-10], 1.1), xl_premium$year),
                            threshold = 5e5, exclude = 2024, rating_premium = 6e7)
    expect_equal(bc()$rate, changed$rate)
    expect_match(xlp_ui_text(output$burning_cost_table), "Left out: 2024")
    #fewer years: the inputs get new ids, and the losses outside are counted
    session$setInputs(first_year = 2018, last_year = 2024)
    expect_gt(generation(), g)
    expect_identical(nrow(bc()$by_year), 7L)
    expect_match(xlp_ui_text(output$burning_cost_table), "losses outside the years 2018 to 2024 left out")
    #equal weights, a given alpha, aggregate terms and unlimited reinstatements
    session$setInputs(weights = "equal", pareto_alpha = 2, aggregate_deductible = 1e6, reinstatements = NA)
    expect_identical(bc()$weights, "equal")
    expect_equal(pareto()$alpha, 2)
    expect_equal(bc()$aggregate_limit, Inf)
    expect_equal(bc()$aggregate_deductible, 1e6)
    #a threshold above the deductible stops the Pareto, not the burning cost
    session$setInputs(pareto_threshold = 2e6)
    expect_s3_class(pareto(), "error")
    expect_true(is.finite(bc()$rate))
    expect_match(xlp_ui_text(output$experience_stats), "Not available")
  })
})

test_that("the experience needs every year's premium, and a valid layer", {
  skip_server_on_cran()
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session)
    g <- generation()
    do.call(session$setInputs, stats::setNames(list(NA), paste0("year_premium_", g, "_3")))
    expect_error(bc(), "subject premium of each year in the By year card: 2017")
    do.call(session$setInputs, stats::setNames(list(4e7), paste0("year_premium_", g, "_3")))
    expect_true(is.finite(bc()$rate))
    session$setInputs(limit = 0)
    expect_error(layer(), "limit of the layer")
    session$setInputs(limit = 4e6, reinstatements = 1.5)
    expect_error(layer(), "whole number")
    session$setInputs(reinstatements = 1, loading = 100)
    expect_error(rating(), "loading")
    session$setInputs(loading = 25, first_year = 2020, last_year = 2019)
    expect_error(years(), "first year")
    #the threshold warning shows under the table
    session$setInputs(first_year = 2015, last_year = 2024, threshold = 9e5)
    expect_match(xlp_ui_text(output$burning_cost_table), "reporting threshold, indexed to 2025, is above the deductible")
  })
})

test_that("an uploaded file of losses is read with the chosen columns", {
  skip_server_on_cran()
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  utils::write.csv(data.frame(uw_year = xl_large_losses$year, gross_loss = xl_large_losses$amount, ref = xl_large_losses$claim),
                   file, row.names = FALSE)
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session, loss_source = "upload")
    session$setInputs(loss_file = list(datapath = file, name = "losses.csv"), loss_header = TRUE, loss_sep = ",", loss_dec = ".",
                      loss_year_col = "uw_year", loss_amount_col = "gross_loss")
    expect_equal(losses()$amount, xl_large_losses$amount)
    #an upload has no premiums until they are typed in
    expect_error(bc(), "subject premium of each year")
    g <- generation()
    premiums <- stats::setNames(as.list(xl_premium$premium), paste0("year_premium_", g, "_", 1:10))
    do.call(session$setInputs, premiums)
    expected <- burning_cost(xl_large_losses, xl_premium[, c("year", "premium")], 1e6, 4e6, aggregate_limit = 8e6,
                             inflation = 0.04, rating_year = 2025, threshold = 5e5, rating_premium = 6e7)
    expect_equal(bc()$rate, expected$rate)
    session$setInputs(loss_amount_col = "uw_year")
    expect_error(losses(), "different columns")
  })
})

test_that("the server rates the exposure of the example profiles and an uploaded one", {
  skip_server_on_cran()
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  utils::write.csv(data.frame(name = c("a", "b"), tsi = c(2e6, 1e7), gnpi = c(1e6, 5e5), lr = c(0.5, 0.7)), file, row.names = FALSE)
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session)
    expected <- property_exposure_rating(xl_property_profile, 1e6, 4e6, "Y3", 0.55)
    expect_equal(exposure()$expected_loss, expected$expected_loss)
    expect_identical(output$profile_type, "property")
    expect_match(xlp_ui_text(output$exposure_table), "Layer, of sum insured")
    expect_match(xlp_ui_text(output$exposure_stats), "Exposure rate")
    #other curves
    session$setInputs(curve = "c", curve_c = 4)
    expect_equal(exposure()$expected_loss, property_exposure_rating(xl_property_profile, 1e6, 4e6, 4, 0.55)$expected_loss)
    session$setInputs(curve = "bg", curve_b = 2, curve_g = 10)
    expect_equal(exposure()$curve$b, 2)
    session$setInputs(curve_g = 0.5)
    expect_error(exposure(), "Enter g")
    #the casualty example, with each claim size distribution
    session$setInputs(curve = "Y3", profile_example = "casualty")
    expect_identical(output$profile_type, "casualty")
    expect_equal(exposure()$expected_loss,
                 casualty_exposure_rating(xl_casualty_profile, 1e6, 4e6, "LogNormal", c(10, 2), 0.55)$expected_loss)
    expect_match(xlp_ui_text(output$exposure_table), "Policy limit")
    session$setInputs(severity = "Pareto", severity_p1 = 1.5, severity_p2 = 1e5)
    expect_equal(exposure()$severity$id, "Pareto")
    session$setInputs(severity = "Exponential", severity_p1 = 1e-5)
    expect_equal(exposure()$severity$params[["rate"]], 1e-5)
    #an uploaded property profile, with its loss ratio column
    session$setInputs(profile_source = "upload", profile_type = "property", profile_header = TRUE, profile_sep = ",",
                      profile_dec = ".", profile_file = list(datapath = file, name = "profile.csv"), band_col = "name",
                      size_col = "tsi", premium_col = "gnpi", loss_ratio_col = "lr")
    up <- exposure()
    expect_equal(up$bands$loss_ratio, c(0.5, 0.7))
    expect_identical(up$bands$band, c("a", "b"))
    session$setInputs(loss_ratio_col = "none")
    expect_equal(exposure()$bands$loss_ratio, c(0.55, 0.55))
    session$setInputs(premium_col = "tsi")
    expect_error(exposure(), "different column")
  })
})

test_that("the pricing blends the rates and gives the technical premium", {
  skip_server_on_cran()
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session)
    p <- pricing()
    bc_rate <- xlp_example_burning_cost()$rate
    exposure_rate <- property_exposure_rating(xl_property_profile, 1e6, 4e6, "Y3", 0.55)$rate
    expect_equal(p$rate[p$method == "Blended"], 0.5 * bc_rate + 0.5 * exposure_rate)
    blended_loss <- (0.5 * bc_rate + 0.5 * exposure_rate) * 6e7
    #the reinstated cover from the burning cost's years at the rating premium, scaled to the expected loss
    years <- xlp_example_burning_cost()
    used <- mean(pmin(years$by_year$at_rating_premium * blended_loss / years$expected_loss, 4e6)) / 4e6
    tp <- technical_premium(blended_loss, 4e6, loading = 0.25, reinstatements = 1, reinstatement_rate = 1, used = used)
    expect_equal(p$upfront_premium[p$method == "Blended"], tp$upfront_premium)
    expect_lt(used, blended_loss / 4e6)
    expect_equal(p$upfront_premium + p$reinstatement_premium, p$expected_loss / 0.75)
    #no reinstatements: no reinstatement premium; without the burning cost, the expected loss over the limit
    session$setInputs(reinstatements = 0)
    expect_equal(pricing()$reinstatement_premium, rep(0, 4))
    session$setInputs(reinstatements = 1)
    expect_match(xlp_ui_text(output$pricing_stats), netsimr_amount(tp$upfront_premium), fixed = TRUE)
    expect_match(xlp_ui_text(output$pricing_table), "Reinstatement premium")
    expect_match(xlp_ui_text(output$glance_table), "Burning cost")
    expect_match(xlp_ui_text(output$layer_stats), "4,000,000 xs 1,000,000")
    #the Pareto as the experience, all weight on it
    session$setInputs(experience_method = "pareto", experience_weight = 100)
    p <- pricing()
    expect_equal(p$rate[p$method == "Blended"], p$rate[p$method == "Pareto"])
    #without the exposure rating, the blend is the experience rate
    session$setInputs(experience_method = "burning_cost", experience_weight = 30, curve = "bg", curve_g = 0.5)
    p <- pricing()
    expect_equal(p$rate[p$method == "Blended"], bc_rate)
    expect_match(p$note[p$method == "Blended"], "exposure rating is not available")
    expect_true(is.na(p$rate[p$method == "Exposure"]))
    #the downloads
    session$setInputs(curve = "Y3")
    csv <- utils::read.csv(output$download_pricing)
    expect_identical(csv$method, c("Burning cost", "Pareto", "Exposure", "Blended"))
    expect_equal(nrow(utils::read.csv(output$download_burning_cost)), 10)
    expect_equal(nrow(utils::read.csv(output$download_losses)), nrow(xl_large_losses))
    expect_equal(nrow(utils::read.csv(output$download_exposure)), 9)
  })
})

test_that("the credibility button sets the weight of the experience", {
  skip_server_on_cran()
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session, full_credibility = 100)
    n <- bc()$n_layer_losses
    expect_identical(method_rates()$experience_n, n)
    expect_equal(credibility_weight(n, 100), sqrt(n / 100))
    #(the slider update goes nowhere in the test session; the button must not fail)
    expect_no_error(session$setInputs(use_credibility = 1))
    session$setInputs(full_credibility = NA, use_credibility = 2)
    expect_true(is.finite(method_rates()$Blended$rate))
  })
})

test_that("the settings file keeps the layer, the options and each year's values, and loads them back", {
  skip_server_on_cran()
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session, deductible = 2e6, curve = "Y4", severity = "Gamma", severity_p1 = 0.5, severity_p2 = 2e5)
    g <- generation()
    do.call(session$setInputs, stats::setNames(list(5e7, FALSE), paste0("year_", c("premium", "include"), "_", g, "_10")))
    saved <- read_settings_file(output$save_settings, xlp_settings_tool)
    v <- saved$values
    expect_identical(saved$version, as.numeric(xlp_settings_version))
    expect_equal(v$deductible, 2e6)
    expect_identical(v$curve, "Y4")
    expect_equal(v$year_labels, 2015:2024)
    expect_equal(v$year_premium[10], 5e7)
    expect_identical(v$year_include[10], FALSE)
    expect_null(v$loss_year_col)
    file.copy(output$save_settings, file)
  })
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session)
    bc()
    session$setInputs(load_settings = list(datapath = file, name = "pricing.txt"))
    #the years showing are the file's: their values apply at once (the test sets what the page would)
    saved <- loaded_by_year()
    expect_equal(saved$premium[10], 5e7)
    expect_false(saved$include[10])
    #the claim size distribution changed with the file: its defaults do not overwrite the file's parameters
    expect_identical(own_updates$waiting("severity"), "Gamma")
    session$setInputs(deductible = 2e6, curve = "Y4", severity = "Gamma", severity_p1 = 0.5, severity_p2 = 2e5)
    expect_length(own_updates$waiting("severity"), 0)
    #new years take their defaults from the file
    session$setInputs(first_year = 2016)
    expect_equal(year_values()$premium[9], 5e7)
    expect_false(year_values()$include[9])
    #a file of another tool changes nothing
    bad <- tempfile(fileext = ".txt")
    write_settings_file(list(deductible = 1), bad, rsv_settings_tool, rsv_settings_version)
    session$setInputs(load_settings = list(datapath = bad, name = "reserving.txt"))
    expect_equal(loaded_by_year()$premium[10], 5e7)
    unlink(bad)
  })
})

test_that("the charts of the reinsurance pricing tool render", {
  skip_server_on_cran()
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session, app_theme = "dark")
    for (id in c("burning_cost_plot", "losses_plot", "layer_losses_plot", "pareto_plot", "band_plot", "curve_plot", "method_plot")) {
      expect_match(output[[id]]$src, "^data:image/png;base64,", label = id)
    }
    session$setInputs(profile_example = "casualty", app_theme = "light")
    expect_match(output$curve_plot$src, "^data:image/png;base64,")
  })
})

test_that("the report holds the pricing, both ratings and, once run, the programmes", {
  skip_server_on_cran()
  shiny::testServer(reinsurance_pricing_tool_Server, {
    xlp_page_inputs(session)
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    for (text in c("Reinsurance Pricing Report", "Technical premium", "Burning cost by year", "Pareto fit",
                   "Bands of the risk profile", "Exposure curve", "Swiss Re Y3")) {
      expect_match(html, text, fixed = TRUE)
    }
    expect_no_match(html, "Best value programme", fixed = TRUE)
    # (every method rates the layer: no warnings)
    expect_no_match(html, "class=\"callout callout-warning\"", fixed = TRUE)
    session$setInputs(prog_frequency = "Poisson", prog_severity = "LogNormal", prog_f1 = 200, prog_s1 = 11, prog_s2 = 1.5,
                      prog_n = 2000, prog_level = "0.995", prog_loading = 25, prog_coc = 10, prog_seed = 1, run_programmes = 1)
    html <- paste(readLines(output$download_report, warn = FALSE), collapse = "\n")
    for (text in c("Best value programme", "Surplus 4 lines", "Net result by return period")) {
      expect_match(html, text, fixed = TRUE)
    }
    # each chart is drawn twice, for the light and the dark theme
    expect_identical(lengths(regmatches(html, gregexpr("data:image/png", html, fixed = TRUE))), 16L)
  })
})
