#reinstatement premiums: the exported calculation, the column simulate_function() and
#simulate_claims() add, the layer metrics and the report, and the simulator's settings

test_that("reinstatement_premium() prices the reinstatements used pro rata to the amount", {
  #one rate for every reinstatement: the premium is the rate times the reinstatements used
  expect_equal(reinstatement_premium(c(0, 0.25, 1, 1.5, 2), premium = 1e5), c(0, 25000, 1e5, 150000, 2e5))
  expect_equal(reinstatement_premium(1.5, premium = 1e5, rates = 0.5), 75000)
  #one rate per reinstatement: the first is used up before the second
  expect_equal(reinstatement_premium(c(0.5, 1, 1.5, 2), premium = 1e5, rates = c(1, 0.5)),
               c(50000, 1e5, 125000, 150000))
  expect_true(is.na(reinstatement_premium(NA_real_, premium = 1e5)))
  expect_length(reinstatement_premium(numeric(0), premium = 1e5), 0)
  expect_error(reinstatement_premium(-1, premium = 1e5), "non-negative")
  expect_error(reinstatement_premium(1, premium = -5), "premium")
  expect_error(reinstatement_premium(1, premium = c(1, 2)), "premium")
  expect_error(reinstatement_premium(1, premium = 1e5, rates = -0.5), "rates")
  expect_error(reinstatement_premium(2.5, premium = 1e5, rates = c(1, 0.5)), "2 reinstatements")
})

test_that("simulate_claims() and simulate_function() add the premium of the reinstatements used", {
  args <- list(5000, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 1, eel_layer = "limited",
               eel_deductible = 20000, eel_limit = 50000, eel_reinstatements = 2)
  plain <- do.call(simulate_claims, args)
  priced <- do.call(simulate_claims, c(args, list(eel_premium = 10000, eel_reinstatement_rates = c(1, 0.5))))
  expect_false("reinstatement_premium" %in% names(plain))
  #the premium changes nothing else
  expect_equal(priced[names(plain)], plain)
  #an independent count: reinstatement k is used by the share of the k-th limit of recoveries
  recoveries <- pmin(priced$total_claims, 3 * 50000)
  naive <- 10000 * (pmin(recoveries, 50000) + 0.5 * pmin(pmax(recoveries - 50000, 0), 50000)) / 50000
  expect_equal(priced$reinstatement_premium, naive, tolerance = 1e-10)
  expect_gt(mean(priced$reinstatement_premium), 0)
  #a premium needs limited reinstatements, and is checked like the other settings
  expect_error(simulate_claims(100, "Poisson", 3, "LogNormal", c(9, 1.5), seed = 1, eel_layer = "limited",
                               eel_deductible = 1, eel_limit = 2, eel_premium = 5), "needs eel_reinstatements")
  expect_error(do.call(simulate_claims, c(args, list(eel_premium = -1))), "EEL Upfront Premium")
  expect_error(do.call(simulate_claims, c(args, list(eel_premium = 1, eel_reinstatement_rates = c(1, 1, 1)))),
               "one for each of the 2 reinstatements")
  #simulate_function() ignores a premium without limited reinstatements, as the app passes the field's
  #last value whatever the structure
  unlimited <- run_simulation(numOfSimulations = 200, reinsuranceStructureEEL = "Limited Layer",
                              reinsurance_structure_eel_deductible_amount = 1000,
                              reinsurance_structure_eel_limit_amount = 5000,
                              reinsurance_structure_eel_premium = 10000)
  expect_false("reinstatement_premium" %in% names(unlimited))
  #an empty premium field (NA) is no premium
  empty <- do.call(simulate_function, layered_settings(numOfSimulations = 200, reinsurance_structure_eel_premium = NA))
  expect_false("reinstatement_premium" %in% names(empty))
})

test_that("the layer metrics and the report give the premium figures a layer is quoted on", {
  settings <- layered_settings(numOfSimulations = 3000, reinsurance_structure_eel_premium = 1500,
                               reinsurance_structure_eel_reinstatement_rates = 1)
  results <- do.call(simulate_function, settings)
  layer <- summarise_simulation(settings, results)$layer
  expect_identical(layer$upfront_premium, 1500)
  expect_equal(layer$rate_on_line, 1500 / settings$reinsurance_structure_eel_limit_amount)
  expect_equal(layer$expected_reinstatement_premium, mean(results$reinstatement_premium))
  expect_equal(layer$expected_total_premium, 1500 + mean(results$reinstatement_premium))
  expect_equal(layer$expected_loss_ratio, mean(results$total_claims) / layer$expected_total_premium)
  #without a premium the figures are absent
  plain <- summarise_simulation(layered_settings(numOfSimulations = 3000),
                                do.call(simulate_function, layered_settings(numOfSimulations = 3000)))$layer
  expect_true(is.na(plain$upfront_premium))

  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  write_simulation_report(file, settings, results)
  text <- gsub("<[^>]+>", " ", paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = " "))
  for (metric in c("Upfront premium", "Rate on line", "Expected reinstatement premium", "Expected total premium",
                   "Expected loss ratio", "Reinstatement rates")) {
    expect_match(text, metric, fixed = TRUE)
  }
  expect_match(text, "100%", fixed = TRUE)
})

test_that("the simulator saves, loads and remembers the premium and the rate", {
  ids <- sim_settings_input_ids()
  expect_true(all(c("reinsurance_structure_eel_premium", "reinsurance_structure_eel_reinstatement_rate_pct") %in% ids))
  plan_ids <- function(inputs) vapply(sim_settings_plan(inputs), `[[`, character(1), "id")
  inputs <- list(reinsuranceStructureEEL = "Limited Layer", reinsurance_structure_eel_deductible_amount = 1000,
                 reinsurance_structure_eel_limit_amount = 5000, reinsuranceStructureLimitedReinstatements = TRUE,
                 reinsuranceStructureReinstatementLimit = 2, reinsurance_structure_eel_premium = 1500,
                 reinsurance_structure_eel_reinstatement_rate_pct = 50)
  expect_true(all(c("reinsurance_structure_eel_premium", "reinsurance_structure_eel_reinstatement_rate_pct") %in%
                    plan_ids(inputs)))
  #a file saved before reinstatement premiums loads without them
  older <- inputs[setdiff(names(inputs), c("reinsurance_structure_eel_premium",
                                           "reinsurance_structure_eel_reinstatement_rate_pct"))]
  expect_false(any(c("reinsurance_structure_eel_premium", "reinsurance_structure_eel_reinstatement_rate_pct") %in%
                     plan_ids(older)))

  shiny::testServer(shiny_simulator_server, {
    set_page_inputs(session, reinsuranceStructureEEL = "Limited Layer")
    session$setInputs(reinsuranceStructureLimitedReinstatements = TRUE)
    html <- paste(as.character(output$reinsuranceStructureReinstatementLimit_ui$html), collapse = "")
    expect_match(html, 'id="reinsurance_structure_eel_premium"[^>]*sim-amount-input')
    #the rate starts at 100%
    expect_match(html, 'id="reinsurance_structure_eel_reinstatement_rate_pct"[^>]*value="100"')
  })
})
