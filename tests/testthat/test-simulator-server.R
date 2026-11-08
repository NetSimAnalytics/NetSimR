#the simulator server, driven through shiny::testServer(): failed runs and the parallel workers

#inputs for a small sequential run without reinsurance; named arguments override them
set_run_inputs <- function(session, ...) {
  inputs <- list(
    freqDistr = "Poisson", lamda = 3,
    sevDistr = "LogNormal", mu = 6, sigma = 1.5,
    numberOfSimulations = 500,
    seedSetBinary = TRUE, seedValue = 1,
    processing = "single",
    pareto_slice_times = 0,
    sevCapBinary = FALSE, sevTruncateAtZero = FALSE,
    reinsuranceStructureEEL = "No Reinsurance Structure",
    reinsuranceStructureAL = "No Reinsurance Structure"
  )
  do.call(session$setInputs, utils::modifyList(inputs, list(...)))
}

#a huge (finite) Poisson mean passes the settings checks but makes the run itself fail;
#an infinite one is now stopped by the checks
run_failing <- function(session, run) {
  expect_message(
    suppressWarnings(session$setInputs(lamda = 1e300, RunSimulations = run)),
    "Simulation failed"
  )
}

#testServer() copies plain server variables when it starts; read the current value through
#the environment of a function defined in the server
current_settings <- function(server_function) get("simulation_settings", envir = environment(server_function))

#the socket connections open in this session (the workers' ones), by number and description
open_sockets <- function() {
  connections <- showConnections(all = TRUE)
  is_socket <- connections[, "class"] == "sockconn"
  paste(rownames(connections)[is_socket], connections[is_socket, "description"])
}

#the session's workers, read through the environment of a function defined in the server
#(testServer() copies plain server variables when it starts)
session_workers <- function(server_function) get("workers", envir = environment(server_function))

#the process ids of a cluster's workers
worker_pids <- function(cluster) sort(unlist(parallel::clusterCall(cluster, Sys.getpid)))

test_that("a failed run keeps the previous results", {
  skip_server_on_cran()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session)
    session$setInputs(RunSimulations = 1)
    first <- last_run()
    expect_equal(first$id, 1L)
    expect_equal(nrow(simulated_data$data), 500)
    expect_identical(current_settings(stop_parallel_workers), first$settings)

    run_failing(session, run = 2)
    expect_identical(last_run(), first)
    expect_identical(simulated_data$data, first$data)
    expect_identical(current_settings(stop_parallel_workers), first$settings)

    #the next successful run replaces them
    session$setInputs(lamda = 2, RunSimulations = 3)
    expect_equal(last_run()$id, 2L)
    expect_equal(current_settings(stop_parallel_workers)$freq_params, 2)
    expect_identical(simulated_data$data, last_run()$data)
  })
})

test_that("a sequential session starts no workers", {
  skip_server_on_cran()
  sockets_before <- open_sockets()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session)
    session$setInputs(RunSimulations = 1)
    run_failing(session, run = 2)
    expect_null(session_workers(stop_parallel_workers))
  })
  expect_identical(open_sockets(), sockets_before)
})

test_that("workers started by the app stay warm through a failed run and are shut down with the session", {
  skip_server_on_cran()
  skip_on_cran()
  old_options <- options(mc.cores = 2)
  on.exit(options(old_options), add = TRUE)
  sockets_before <- open_sockets()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session)
    session$setInputs(processing = "parallel")
    cluster <- session_workers(stop_parallel_workers)
    expect_length(cluster, 2)
    pids <- worker_pids(cluster)
    worker_sockets <- open_sockets()
    #an error raised by the simulation leaves the workers healthy: a failed run used to shut
    #them down, and the next run paid the start-up cost again
    run_failing(session, run = 1)
    expect_identical(session_workers(stop_parallel_workers), cluster)
    expect_identical(worker_pids(cluster), pids)
    expect_identical(open_sockets(), worker_sockets)
    #the next run reuses the same workers and gives the results of a sequential run
    session$setInputs(lamda = 3, RunSimulations = 2)
    expect_equal(nrow(simulated_data$data), 500)
    expect_identical(worker_pids(cluster), pids)
    sequential <- do.call(simulate_function, utils::modifyList(last_run()$settings, list(multiprocessing = FALSE)))
    expect_identical(simulated_data$data, sequential)
    expect_identical(open_sockets(), worker_sockets)
  })
  #the end of the session shuts the workers down
  expect_identical(open_sockets(), sockets_before)
})

test_that("Auto keeps small runs in the session and starts the workers once a run pays for them", {
  skip_server_on_cran()
  skip_on_cran()
  old_options <- options(mc.cores = 2)
  on.exit(options(old_options), add = TRUE)
  sockets_before <- open_sockets()
  #(testServer() runs its code on copies of the test's variables, so the plan is kept in an environment)
  plan <- new.env()
  plan$workers <- 0L
  real_plan <- netsimr_parallel_plan
  local_mocked_bindings(netsimr_parallel_plan = function(...) if (plan$workers > 0) plan$workers else real_plan(...))
  shiny::testServer(shiny_simulator_server, {
    #two chunks of a small run: Auto runs them here and starts nothing
    set_run_inputs(session, processing = "auto", numberOfSimulations = 20000)
    session$setInputs(RunSimulations = 1)
    expect_null(session_workers(stop_parallel_workers))
    expect_false(last_run()$settings$multiprocessing)
    expect_identical(open_sockets(), sockets_before)
    #a run that pays: the workers are started for it and kept
    plan$workers <- 2L
    session$setInputs(RunSimulations = 2)
    cluster <- session_workers(stop_parallel_workers)
    expect_length(cluster, 2)
    expect_true(last_run()$settings$multiprocessing)
    expect_identical(last_run()$settings$workers, 2L)
    sequential <- do.call(simulate_function, utils::modifyList(last_run()$settings, list(multiprocessing = FALSE)))
    expect_identical(simulated_data$data, sequential)
    #the next run reuses them
    pids <- worker_pids(cluster)
    session$setInputs(RunSimulations = 3)
    expect_identical(session_workers(stop_parallel_workers), cluster)
    expect_identical(worker_pids(cluster), pids)
    #Single runs here even with warm workers
    session$setInputs(processing = "single", RunSimulations = 4)
    expect_false(last_run()$settings$multiprocessing)
  })
  expect_identical(open_sockets(), sockets_before)
})

test_that("a worker that dies during a run is replaced by fresh workers on the next run", {
  skip_server_on_cran()
  skip_on_cran()
  old_options <- options(mc.cores = 2)
  on.exit(options(old_options), add = TRUE)
  sockets_before <- open_sockets()
  #every chunk ends its worker's process while running
  real_run_chunks <- run_chunks_on_workers
  die <- function(i) tools::pskill(Sys.getpid())
  environment(die) <- globalenv()
  dying_workers <- function(cluster, run_chunk, chunk_seeds, chunks = seq_along(chunk_seeds)) real_run_chunks(cluster, die, chunk_seeds, chunks)
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session, processing = "parallel")
    pids <- worker_pids(session_workers(stop_parallel_workers))
    #the app shuts its workers down, leaving no connection open
    with_mocked_bindings(
      expect_message(session$setInputs(RunSimulations = 1), "Simulation failed"),
      run_chunks_on_workers = dying_workers
    )
    expect_null(session_workers(stop_parallel_workers))
    expect_identical(open_sockets(), sockets_before)
    #no connection is left for the garbage collector to close with a warning
    expect_no_warning(gc(), message = "closing unused connection")
    #the next run starts fresh workers and succeeds on them
    session$setInputs(RunSimulations = 2)
    expect_equal(nrow(simulated_data$data), 500)
    cluster <- session_workers(stop_parallel_workers)
    expect_length(cluster, 2)
    expect_false(any(worker_pids(cluster) %in% pids))
  })
  expect_identical(open_sockets(), sockets_before)
})

test_that("the compare tab draws the return-period chart for two stored runs, in both themes", {
  skip_server_on_cran()
  last_run <- shiny::reactiveVal(NULL)
  make_run <- function(id, ...) {
    settings <- base_settings(numOfSimulations = 1000, seedValue = id, ...)
    list(id = id, settings = settings, data = do.call(simulate_function, settings), finished = Sys.time())
  }
  shiny::testServer(sim_compare_tab_server, args = list(last_run = last_run), {
    last_run(make_run(1L))
    session$flushReact()
    last_run(make_run(2L, freq_params = 4))
    session$flushReact()
    #the chart inputs are debounced
    session$elapse(500)
    light <- output$return_period_chart
    expect_match(light$src, "^data:image/png;base64,")
    expect_match(light$alt, "return period")

    session$setInputs(app_theme = "dark")
    dark <- output$return_period_chart
    expect_match(dark$src, "^data:image/png;base64,")
    expect_false(identical(dark$src, light$src))
  })
})

test_that("the return-period chart fits six runs with long names in a narrow plot", {
  entries <- lapply(1:6, function(id) {
    settings <- base_settings(numOfSimulations = 500, seedValue = id)
    sim_tab_compare_entry(list(id = id, settings = settings, data = do.call(simulate_function, settings),
                               finished = Sys.time()))
  })
  names <- c(paste("A very long run name that will not fit on one line of the legend", 1:2), paste("Run", 3:6))
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  for (dark in c(FALSE, TRUE)) {
    expect_no_error(shiny::plotPNG(function() sim_tab_return_period_plot(entries, names, dark = dark),
                                   filename = file, width = 320, height = 400, bg = "transparent"))
    expect_gt(file.size(file), 0)
  }
})

test_that("the compare chart's y axis spans negative runs, and infinite totals are left out", {
  #the axis used to run from zero to the largest loss, so a run of negative totals was invisible
  ticks <- sim_tab_y_ticks(c(-2779, -100, 0, Inf, 35000))
  expect_true(all(is.finite(ticks)))
  expect_lte(min(ticks), -2779)
  expect_gte(max(ticks), 35000)
  expect_true(0 %in% ticks)
  expect_equal(range(sim_tab_y_ticks(c(0, 0))), c(0, 1))
  expect_equal(range(sim_tab_y_ticks(c(Inf, NA))), c(0, 1))

  entry_of <- function(id, ...) {
    settings <- base_settings(numOfSimulations = 1000, seedValue = id, ...)
    sim_tab_compare_entry(list(id = id, settings = settings, data = do.call(simulate_function, settings),
                               finished = Sys.time()))
  }
  negative <- entry_of(1, sevDistr = "Normal", sev_params = c(-1000, 300))
  positive <- entry_of(2)
  infinite <- entry_of(3, sevDistr = "Pareto", sev_params = c(0.002, 100))
  expect_true(any(is.infinite(infinite$curve$value)))
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 700, height = 450)
  on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
  sim_tab_return_period_plot(list(negative, positive, infinite), c("Negative", "Positive", "Infinite"))
  usr <- graphics::par("usr")
  expect_lte(usr[3], min(negative$curve$value))
  expect_gte(usr[4], max(positive$curve$value))

  #infinite totals show as Inf in the metrics, and their standard deviation as a dash
  expect_identical(sim_tab_fmt_amount(infinite$metrics$mean), "Inf")
  expect_identical(sim_tab_fmt_amount(infinite$metrics$sd), intToUtf8(8212))
  expect_identical(sim_tab_fmt_amount(-0), "0.00")
  expect_false(grepl("NaN", as.character(sim_tab_metrics_table(list(negative, infinite), c("A", "B"))), fixed = TRUE))
  #the x axis reaches 1 in 1,000,000, the longest return period of the largest run
  expect_true(all(c(2e5, 5e5, 1e6) %in% return_period_axis_ticks))
})

test_that("the message about empty fields follows the fields and goes once they are filled in", {
  skip_server_on_cran()
  shiny::testServer(shiny_simulator_server, {
    set_run_inputs(session, lamda = NA, mu = NA)
    session$setInputs(RunSimulations = 1)
    expect_length(missing_notice$listed, 2)
    #filling in one field leaves the other listed; filling in both removes the message
    session$setInputs(lamda = 3)
    expect_length(missing_notice$listed, 1)
    expect_match(missing_notice$listed, "Severity")
    session$setInputs(mu = 6)
    expect_null(missing_notice$listed)
    #once it has timed out, emptying a field does not bring it back before the next run
    session$setInputs(mu = NA, RunSimulations = 2)
    expect_length(missing_notice$listed, 1)
    missing_notice$until <- Sys.time() - 1
    session$setInputs(lamda = NA)
    expect_null(missing_notice$listed)
    session$setInputs(RunSimulations = 3)
    expect_length(missing_notice$listed, 2)
  })
})
