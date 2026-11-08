#writes a report to a temporary file and returns its text
report_text <- function(settings, results) {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  write_simulation_report(file, settings, results)
  expect_true(file.exists(file))
  paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

has_section <- function(html, id) grepl(paste0('id="', id, '"'), html, fixed = TRUE)

expect_self_contained <- function(html) {
  expect_true(startsWith(html, "<!DOCTYPE html>"))
  expect_true(grepl("<title>Simulation Report</title>", html, fixed = TRUE))
  expect_true(grepl("<style", html, fixed = TRUE))
  expect_true(grepl("data:image/png;base64,", html, fixed = TRUE))
  #nothing is loaded from elsewhere (the footer's link to the website loads nothing)
  expect_false(grepl("src=\"https?://", html))
  expect_false(grepl("<link[^>]+href=", html))
  expect_false(grepl("url\\(['\"]?https?://", html))
  expect_false(grepl("<script[^>]+src=", html))
  links <- regmatches(html, gregexpr("href=\"https?://[^\"]*\"", html))[[1]]
  expect_identical(links, "href=\"https://netsimanalytics.github.io/NetSimR/\"")
  for (id in c("key-results", "model-settings", "statistics", "charts", "how-to-read")) {
    expect_true(has_section(html, id), label = paste("section", id))
  }
}

test_that("a run without reinsurance gives a self-contained report", {
  settings <- base_settings(numOfSimulations = 1000)
  results <- do.call(simulate_function, settings)
  html <- report_text(settings, results)
  expect_self_contained(html)
  expect_true(has_section(html, "frequency"))
  expect_false(has_section(html, "gross-net"))
  expect_false(has_section(html, "layer-metrics"))
})

test_that("a layered run reports gross versus net and the layer metrics", {
  settings <- layered_settings(numOfSimulations = 1000)
  results <- do.call(simulate_function, settings)
  html <- report_text(settings, results)
  expect_self_contained(html)
  expect_true(has_section(html, "gross-net"))
  expect_true(has_section(html, "layer-metrics"))
  expect_true(has_section(html, "frequency"))
})

test_that("a numeric vector of totals is accepted", {
  settings <- base_settings(numOfSimulations = 1000)
  totals <- do.call(simulate_function, settings)$total_claims
  html <- report_text(settings, totals)
  expect_self_contained(html)
  expect_false(has_section(html, "frequency"))
})

#the report's text without the embedded images, whose base64 could contain any letters
report_words <- function(html) gsub("data:image/png;base64,[A-Za-z0-9+/=]+", "", html)

test_that("infinite totals are left out of the charts with a note, and the tables show them", {
  #a Pareto alpha this small passes the checks, but half of the totals overflow to Inf;
  #the report used to fail with "need finite 'ylim' values"
  settings <- base_settings(numOfSimulations = 1000, seedValue = 1, sevDistr = "Pareto", sev_params = c(0.002, 100))
  results <- do.call(simulate_function, settings)
  n_infinite <- sum(is.infinite(results$total_claims))
  expect_gt(n_infinite, 0)
  expect_lt(n_infinite, 1000)
  html <- report_text(settings, results)
  expect_self_contained(html)
  words <- report_words(html)
  expect_match(words, paste(formatC(n_infinite, format = "d", big.mark = ","),
                            "simulations \\([0-9.]+%\\) had infinite totals and are left out of the charts"))
  expect_match(words, "Mean</div>\\s*<div class=\"kpi-value\">Inf</div>")
  expect_false(grepl("NaN", words, fixed = TRUE))

  #an excluded layer: gross and net are infinite together, so ceded (Inf - Inf) is unknown
  #and its column blank, where it used to show NaN or statistics of the other rows only
  excluded <- utils::modifyList(settings, list(reinsuranceStructureEEL = "Exclude Layer",
                                               reinsurance_structure_eel_deductible_amount = 1000,
                                               reinsurance_structure_eel_limit_amount = 5000))
  words <- report_words(report_text(excluded, do.call(simulate_function, excluded)))
  expect_true(has_section(words, "gross-net"))
  expect_match(words, "amount (infinite minus infinite) is unknown and the Ceded column is left blank", fixed = TRUE)
  expect_false(grepl("NaN", words, fixed = TRUE))

  #every total infinite: the charts say so instead of failing
  settings <- base_settings(numOfSimulations = 100, freqDistr = "Fixed_number_of_Counts", freq_params = 1,
                            sevDistr = "Fixed_Severity", sev_params = 1, paretoSlice = TRUE, pareto_slice_times = 1,
                            slice_pareto_alphas = 1e-300, slice_pareto_x_ms = 0.5)
  results <- do.call(simulate_function, settings)
  expect_true(all(is.infinite(results$total_claims)))
  expect_match(report_words(report_text(settings, results)), "100 simulations (100.0%) had infinite totals", fixed = TRUE)
})

test_that("negative zero totals are shown as 0", {
  #no claims under a Normal severity with a negative mean give totals of -0
  settings <- base_settings(numOfSimulations = 1000, sevDistr = "Normal", sev_params = c(-1000, 300))
  words <- report_words(report_text(settings, do.call(simulate_function, settings)))
  expect_false(grepl(">-0(\\.0+)?<", words))
})

test_that("empty totals are an error", {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  expect_error(write_simulation_report(file, base_settings(), numeric(0)))
  expect_error(write_simulation_report(file, base_settings(), data.frame(total_claims = c(NA_real_, NA_real_))))
  expect_error(write_simulation_report(file, base_settings(), data.frame(gross_claims = 1:5)), "total_claims")
})

test_that("the report's settings show each layer's limit before its deductible", {
  #the order a layer is quoted in ("limit xs deductible"), as on the simulator's page
  settings <- base_settings(numOfSimulations = 300, seedValue = 1,
                            reinsuranceStructureEEL = "Limited Layer",
                            reinsurance_structure_eel_deductible_amount = 100, reinsurance_structure_eel_limit_amount = 400,
                            reinsuranceStructureAL = "Limited Layer",
                            reinsurance_structure_al_deductible_amount = 200, reinsurance_structure_al_limit_amount = 1000)
  html <- report_text(settings, do.call(simulate_function, settings))
  cards <- strsplit(html, '<div class="setting-card">', fixed = TRUE)[[1]]
  for (title in c("Each &amp; every loss (EEL)", "Aggregate layer (AL)")) {
    card <- grep(title, cards, fixed = TRUE, value = TRUE)
    expect_length(card, 1)
    limit <- regexpr("<dt>Limit</dt>", card, fixed = TRUE)[1]
    deductible <- regexpr("<dt>Deductible</dt>", card, fixed = TRUE)[1]
    expect_gt(limit, 0)
    expect_gt(deductible, 0)
    expect_lt(limit, deductible)
  }
})

test_that("the report and the Compare tab show the Pareto's x_m before alpha, and each slice's threshold first", {
  #as the simulator asks for them; the parameters are still passed as c(alpha, x_m)
  settings <- base_settings(numOfSimulations = 300, seedValue = 1, sevDistr = "Pareto", sev_params = c(2.5, 1000),
                            paretoSlice = TRUE, pareto_slice_times = 2,
                            slice_pareto_alphas = c(2, 1.6), slice_pareto_x_ms = c(50000, 250000))
  html <- report_text(settings, do.call(simulate_function, settings))
  text <- gsub(intToUtf8(160), " ", gsub("<[^>]+>", " ", html), fixed = TRUE)
  expect_match(text, "x_m (minimum) = 1,000, alpha (shape) = 2.5", fixed = TRUE)
  expect_match(text, paste0("From 50,000, ", intToUtf8(945), " 2.00"), fixed = TRUE)
  expect_match(text, paste0("From 250,000, ", intToUtf8(945), " 1.60"), fixed = TRUE)
  expect_match(sim_tab_describe_settings(settings), "Pareto (x_m = 1000, alpha = 2.5)", fixed = TRUE)
  #a distribution without a display order keeps its own
  settings <- base_settings(numOfSimulations = 300, seedValue = 1, sev_params = c(6, 1))
  expect_match(sim_tab_describe_settings(settings), "Log-Normal (mu = 6, sigma = 1)", fixed = TRUE)
})

test_that("the report shows the largest single claim (OEP) beside the period total (AEP)", {
  settings <- layered_settings(numOfSimulations = 3000, largest_claims = TRUE)
  results <- do.call(simulate_function, settings)
  expect_true("largest_claim" %in% names(results))
  html <- report_text(settings, results)
  text <- gsub("<[^>]+>", " ", html)
  expect_match(text, "Largest claim (OEP)", fixed = TRUE)
  expect_match(text, "before the reinstatement limit and the aggregate layer", fixed = TRUE)
  #the OEP column at 1 in 100 is the 99th percentile of the largest claims
  oep_99 <- stats::quantile(results$largest_claim[!is.na(results$total_claims)], 0.99, names = FALSE)
  expect_match(html, format(round(oep_99, 2), big.mark = ",", nsmall = 2), fixed = TRUE)
  #without the column, nothing about OEP
  plain <- report_text(layered_settings(numOfSimulations = 3000), do.call(simulate_function, layered_settings(numOfSimulations = 3000)))
  #(the visible text: the charts are embedded as base64, where any three letters can occur)
  expect_false(grepl("(OEP)", gsub("<[^>]+>", " ", plain), fixed = TRUE))
})

test_that("the report says how a run was processed", {
  settings <- base_settings(numOfSimulations = 1000)
  results <- do.call(simulate_function, settings)
  expect_match(report_text(settings, results), "Single process", fixed = TRUE)
  expect_match(report_text(utils::modifyList(settings, list(multiprocessing = TRUE, workers = 12L)), results),
               "Parallel, 12 workers", fixed = TRUE)
  expect_match(report_text(utils::modifyList(settings, list(multiprocessing = TRUE)), results), "Parallel<", fixed = TRUE)
})
