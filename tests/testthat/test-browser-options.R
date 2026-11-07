#in a web browser (webR, as on the package's website) the tools leave out what needs worker
#processes or a database

test_that("the tools know when they run in a web browser", {
  expect_false(netsimr_in_browser())
})

test_that("in a web browser the simulator has no processing choice and the GLM tool no database", {
  skip_if_not_installed("htmltools")
  page_text <- function(ui) as.character(htmltools::renderTags(ui(NULL))$html)
  expect_match(page_text(shiny_simulator_ui), "id=\"processing\"", fixed = TRUE)
  expect_match(page_text(GLMFittingToolUI), "value=\"Database\"", fixed = TRUE)
  local_mocked_bindings(netsimr_in_browser = function() TRUE)
  expect_no_match(page_text(shiny_simulator_ui), "id=\"processing\"", fixed = TRUE)
  glm <- page_text(GLMFittingToolUI)
  expect_no_match(glm, "value=\"Database\"", fixed = TRUE)
  expect_match(glm, "value=\"CSV File\"", fixed = TRUE)
})

test_that("in a web browser the simulator runs without workers, even when a settings file asks for them", {
  local_mocked_bindings(netsimr_in_browser = function() TRUE)
  shiny::testServer(shiny_simulator_server, {
    session$setInputs(freqDistr = "Poisson", lamda = 3, sevDistr = "LogNormal", mu = 6, sigma = 1.5,
                      numberOfSimulations = 500, seedSetBinary = TRUE, seedValue = 1, processing = "parallel",
                      pareto_slice_times = 0, sevCapBinary = FALSE, sevTruncateAtZero = FALSE,
                      reinsuranceStructureEEL = "No Reinsurance Structure", reinsuranceStructureAL = "No Reinsurance Structure")
    #no workers were started for the switch
    expect_null(get("workers", envir = environment(stop_parallel_workers)))
    session$setInputs(RunSimulations = 1)
    expect_false(last_run()$settings$multiprocessing)
    expect_equal(nrow(simulated_data$data), 500)
  })
})
