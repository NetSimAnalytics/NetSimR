#Browser tests of the claims simulator app (helpers in helper-app.R)

sim_chart_id <- "compare-return_period_chart"

#loads a built-in example and waits until the server has (some of) its values
sim_load_example <- function(app, label, expected) {
  app_set(app, settingsIO_example = label)
  app_click(app, "settingsIO_load_example")
  app_wait_for_notification(app, "Loaded example")
  app_wait_for_inputs(app, expected, paste0("the values of the example '", label, "'"))
  app_clear_notifications(app)
}

#runs the simulation: the button is disabled on click and enabled again when the run ends
sim_run <- function(app) {
  app_click(app, "RunSimulations")
  app$wait_for_js("document.getElementById('RunSimulations').disabled === false", timeout = 120 * 1000)
  app_idle(app)
}

#opens the Report tab and waits for the report of the latest run, or its error message;
#returns the text of the report frame and of the tab
sim_report <- function(app) {
  app_goto(app, "report")
  app$wait_for_js("(function () {
    var frame = document.querySelector('iframe.sim-report-frame');
    if (frame && frame.contentDocument && frame.contentDocument.body &&
        frame.contentDocument.body.innerText.length > 200) return true;
    var view = document.getElementById('report-report_view');
    return !!view && /could not be created/.test(view.innerText);
  })()", timeout = 120 * 1000)
  frame <- app$get_js("(function () {
    var frame = document.querySelector('iframe.sim-report-frame');
    return frame && frame.contentDocument && frame.contentDocument.body ? frame.contentDocument.body.innerText : '';
  })()")
  list(frame = paste(unlist(frame), collapse = " "), view = app_text(app, "#report-report_view"))
}

sim_plot_width <- function(app) {
  width <- app$get_js(sprintf("(function () { var img = document.querySelector('#%s img'); return img ? img.naturalWidth : 0; })()", sim_chart_id))
  as.numeric(unlist(width))
}

test_that("the simulator runs an example, builds the report and compares the runs in a chart", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("simulator", "simulator-run")
  on.exit(app$stop(), add = TRUE)
  expect_app_logs_clean(app)

  app_goto(app, "simulator")
  sim_load_example(app, "Motor: excess of loss layer",
                   list(freqDistr = "Poisson", lamda = 5, mu = 9, sigma = 1.3, seedValue = 1,
                        reinsuranceStructureEEL = "Limited Layer", reinsurance_structure_eel_limit_amount = 200000))
  #two small runs, so the Compare tab has a comparison
  app_set(app, numberOfSimulations = 4000)
  sim_run(app)
  app_set(app, numberOfSimulations = 3000)
  sim_run(app)
  expect_false(any(grepl("could not|error", app_notifications(app), ignore.case = TRUE)))

  #the report of the latest run is built
  report <- sim_report(app)
  expect_match(report$frame, "Key results", fixed = TRUE)
  expect_match(report$frame, "3,000", fixed = TRUE)
  expect_no_match(report$view, "could not be created", fixed = TRUE)

  #both runs are stored, and the table and the chart compare them
  app_goto(app, "compare")
  app$wait_for_js(sprintf(
    "document.querySelectorAll('.sim-run-item').length === 2 && document.querySelector('#%s img') !== null", sim_chart_id
  ), timeout = 60 * 1000)
  expect_match(app_text(app, ".sim-compare-table"), "Mean", fixed = TRUE)

  #the chart is redrawn in the dark theme's colours
  app_theme(app, "light")
  light_src <- app_plot_src(app, sim_chart_id)
  app_theme(app, "dark")
  app_wait_for(function() {
    src <- app_plot_src(app, sim_chart_id)
    nzchar(src) && !identical(src, light_src)
  }, "the chart to be redrawn in the dark theme")
  dark <- app_plot_image(app, sim_chart_id)
  bytes <- app_png_bytes(dark$src)
  expect_identical(bytes[1:8], app_png_signature)
  expect_equal(app_png_width(bytes), dark$width)
  #the browser decodes the PNG and counts its pixels by alpha. The background is left
  #transparent, not painted over, so the page's dark card shows through; and the text and
  #lines are anti-aliased with semi-transparent edge pixels. Windows' default png() device
  #draws every pixel either fully transparent or fully opaque (no semi-transparent pixel at
  #all), which smears the light text of the dark theme: R/plot_device.R asks for the cairo
  #device there, and this fails if it no longer does.
  pixels <- dark$width * dark$height
  expect_gt(dark$clear / pixels, 0.5)
  expect_gt(dark$partial, 1000)

  #the chart is redrawn for a new width (not the old image scaled)
  app$set_window_size(600, 900)
  app_wait_for(function() {
    width <- sim_plot_width(app)
    width > 0 && width != dark$width
  }, "the chart to be redrawn at the new window width")
  app_idle(app)
  resized <- app_plot_image(app, sim_chart_id)
  expect_lt(resized$width, dark$width)
  expect_lt(abs(resized$width / resized$pixelRatio - resized$clientWidth), 3)
  expect_gt(resized$partial, 0)

  expect_app_logs_clean(app)
})

test_that("simulator settings saved to a text file load back, and an .rds file is refused", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("simulator", "simulator-settings")
  on.exit(app$stop(), add = TRUE)

  app_goto(app, "simulator")
  expect_match(app$get_js("document.getElementById('settingsIO_load').getAttribute('accept')"), ".txt", fixed = TRUE)
  #wait for every value this test changes next: the example's seed field is drawn by the server,
  #so its value arrives in the loader's second stage. The values are typed straight after they
  #arrive: the loader once sent the example's values back over them (on slow CI machines)
  sim_load_example(app, "Motor: excess of loss layer",
                   list(lamda = 5, numberOfSimulations = 50000, seedValue = 1,
                        reinsuranceStructureEEL = "Limited Layer", reinsuranceStructureReinstatementLimit = 2))
  app_set(app, numberOfSimulations = 12345, lamda = 6.5, seedValue = 2024)

  #the settings are saved as text
  saved <- app_download(app, "settingsIO_save")
  expect_identical(basename(saved), "claims_simulator_settings.txt")
  expect_identical(readLines(saved, n = 1, warn = FALSE), "NetSimRSettings: claims simulator")
  fields <- read.dcf(saved, all = TRUE)
  ids <- setdiff(names(fields), c("NetSimRSettings", "SettingsVersion"))
  before <- app_inputs(app, ids)
  #the inputs that have a value (the fields of other distributions are empty)
  before <- before[vapply(before, function(x) !is.null(x) && !all(is.na(x)), logical(1))]
  expect_true(all(c("freqDistr", "lamda", "sevDistr", "mu", "numberOfSimulations", "seedValue",
                    "reinsuranceStructureEEL", "reinsurance_structure_eel_limit_amount") %in% names(before)))
  expect_equal(before$numberOfSimulations, 12345)
  expect_equal(before$lamda, 6.5)
  expect_equal(before$seedValue, 2024)

  #other settings, then the saved file restores every input
  sim_load_example(app, "Property: aggregate cover",
                   list(freqDistr = "Negative_Binomial", sevDistr = "Gamma", reinsuranceStructureAL = "Limited Layer"))
  app_upload(app, "settingsIO_load", saved)
  app_wait_for_notification(app, "Settings loaded from 'claims_simulator_settings.txt'")
  app_wait_for_inputs(app, before, "the saved settings to be restored")
  app_clear_notifications(app)

  #an .rds settings file, as NetSimR 0.3.0 and earlier saved them, is refused
  rds <- file.path(tempdir(), "netsimr_simulator_settings.rds")
  saveRDS(list(format = "NetSimR simulator settings", version = 2, saved = Sys.time(),
               inputs = list(freqDistr = "Binomial", n = 9, p = 0.1)), rds)
  app_upload(app, "settingsIO_load", rds)
  note <- app_wait_for_notification(app, "netsimr_simulator_settings[.]rds")
  expect_match(note, "not a NetSimR settings file", fixed = TRUE)
  expect_match(note, ".txt file", fixed = TRUE)
  app_idle(app, 1000)
  after <- app_inputs(app, names(before))
  expect_true(all(mapply(app_same_value, after, before)))

  expect_app_logs_clean(app)
})

test_that("a run with totals of both +Inf and -Inf and an aggregate exclude layer builds its report", {
  skip_if_no_app_browser()
  #Binomial(10, 0.5) claims from a Normal with mean and standard deviation 1e308 overflow
  #to +Inf and -Inf; the aggregate layer excludes 1e308 in excess of 1e300
  settings <- file.path(tempdir(), "netsimr_simulator_settings_1e308.txt")
  writeLines(c(
    "NetSimRSettings: claims simulator",
    "SettingsVersion: 3",
    "freqDistr: \"Binomial\"",
    "sevDistr: \"Normal\"",
    "lamda: NA",
    "n: 10L",
    "p: 0.5",
    "normal_mean: 1e+308",
    "normal_sd: 1e+308",
    "numberOfSimulations: 3000L",
    "seedSetBinary: TRUE",
    "seedValue: 1L",
    "multiprocessingBinary: FALSE",
    "pareto_slice_times: 0L",
    paste0("slice_pareto_param_", 1:12, ": NA"),
    "sevCapBinary: FALSE",
    "sevTruncateAtZero: FALSE",
    "reinsuranceStructureEEL: \"No Reinsurance Structure\"",
    "reinsuranceStructureAL: \"Exclude Layer\"",
    "reinsurance_structure_al_dedctible_amount: 1e+300",
    "reinsurance_structure_al_limit_amount: 1e+308"
  ), settings)

  app <- start_netsimr_app("simulator", "simulator-infinite")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "simulator")
  app_upload(app, "settingsIO_load", settings)
  app_wait_for_notification(app, "Settings loaded")
  app_wait_for_inputs(app, list(
    freqDistr = "Binomial", n = 10, p = 0.5, sevDistr = "Normal", normal_mean = 1e308, normal_sd = 1e308,
    sevTruncateAtZero = FALSE, numberOfSimulations = 3000, seedSetBinary = TRUE, seedValue = 1,
    reinsuranceStructureAL = "Exclude Layer", reinsurance_structure_al_dedctible_amount = 1e300,
    reinsurance_structure_al_limit_amount = 1e308
  ), "the settings of the file")
  app_clear_notifications(app)
  sim_run(app)

  report <- sim_report(app)
  expect_no_match(report$view, "could not be created", fixed = TRUE)
  expect_false(any(grepl("could not be", app_notifications(app), fixed = TRUE)))
  expect_match(report$frame, "Key results", fixed = TRUE)
  #the run is the case of the bug: totals of +Inf and of -Inf, whose sums are undefined
  expect_match(report$frame, "-Inf", fixed = TRUE)
  expect_match(report$frame, "NaN", fixed = TRUE)

  expect_app_logs_clean(app)
})

test_that("amounts typed into the simulator show thousands separators and reach the server as numbers", {
  skip_if_no_app_browser()
  app <- start_netsimr_app("simulator", "simulator-amounts")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "simulator")
  cdp <- app$get_chromote_session()
  #types as a person does: select the field's text, type, then leave the field with Tab
  type_amount <- function(id, text) {
    app$run_js(sprintf("var e = document.getElementById(%s); e.focus(); e.select();", app_js_string(id)))
    if (nzchar(text)) {
      cdp$Input$insertText(text = text)
    } else {
      cdp$Input$dispatchKeyEvent(type = "keyDown", key = "Delete", code = "Delete", windowsVirtualKeyCode = 46)
    }
    cdp$Input$dispatchKeyEvent(type = "keyDown", key = "Tab", code = "Tab", windowsVirtualKeyCode = 9)
    cdp$Input$dispatchKeyEvent(type = "keyUp", key = "Tab", code = "Tab", windowsVirtualKeyCode = 9)
    app_idle(app)
  }
  shown <- function(id) app$get_js(sprintf("document.getElementById(%s).value", app_js_string(id)))
  server <- function(id) app$get_values(input = id)$input[[id]]

  #the starting value, then a typed one with and without separators
  expect_identical(shown("numberOfSimulations"), "50,000")
  type_amount("numberOfSimulations", "1234567")
  expect_identical(shown("numberOfSimulations"), "1,234,567")
  expect_equal(server("numberOfSimulations"), 1234567)
  type_amount("numberOfSimulations", "1,500,000")
  expect_equal(server("numberOfSimulations"), 1500000)
  #spaces group the digits too, as in much of Europe
  type_amount("numberOfSimulations", "2 500 000")
  expect_identical(shown("numberOfSimulations"), "2,500,000")
  expect_equal(server("numberOfSimulations"), 2500000)
  #a decimal comma is not a thousands separator: 2,50 was read as 250, a hundred times too much
  type_amount("numberOfSimulations", "2,50")
  expect_identical(shown("numberOfSimulations"), "2,50")
  expect_true(is.na(server("numberOfSimulations")))
  #Enter shows the separators, as leaving the field does
  app$run_js("var e = document.getElementById('numberOfSimulations'); e.focus(); e.select();")
  cdp$Input$insertText(text = "75000")
  cdp$Input$dispatchKeyEvent(type = "keyDown", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13)
  cdp$Input$dispatchKeyEvent(type = "keyUp", key = "Enter", code = "Enter", windowsVirtualKeyCode = 13)
  app_idle(app)
  expect_identical(shown("numberOfSimulations"), "75,000")
  expect_equal(server("numberOfSimulations"), 75000)
  #an empty field, or text that is not a number, is NA, and the text is left to correct
  type_amount("numberOfSimulations", "")
  expect_true(is.na(server("numberOfSimulations")))
  type_amount("numberOfSimulations", "abc")
  expect_identical(shown("numberOfSimulations"), "abc")
  expect_true(is.na(server("numberOfSimulations")))
  #negative amounts and decimals
  app_set(app, sevDistr = "Normal")
  type_amount("normal_mean", "-2500")
  expect_identical(shown("normal_mean"), "-2,500")
  expect_equal(server("normal_mean"), -2500)
  type_amount("normal_sd", "1234.5")
  expect_identical(shown("normal_sd"), "1,234.5")
  expect_equal(server("normal_sd"), 1234.5)
  #a layer's amounts, and an example loaded with updateNumericInput()
  app_set(app, reinsuranceStructureEEL = "Limited Layer")
  type_amount("reinsurance_structure_eel_limit_amount", "400000")
  expect_identical(shown("reinsurance_structure_eel_limit_amount"), "400,000")
  expect_equal(server("reinsurance_structure_eel_limit_amount"), 400000)
  #the severity cap
  app_set(app, sevCapBinary = TRUE)
  type_amount("sev_cap_amount", "5000000")
  expect_identical(shown("sev_cap_amount"), "5,000,000")
  expect_equal(server("sev_cap_amount"), 5e6)
  #the slices' thresholds; removing the first slice moves the second's values up, formatted
  app_click(app, "add_pareto_slice")
  app_idle(app)
  app_click(app, "add_pareto_slice")
  app_idle(app)
  type_amount("slice_pareto_param_2", "50000")
  type_amount("slice_pareto_param_4", "250000")
  expect_identical(shown("slice_pareto_param_2"), "50,000")
  expect_equal(server("slice_pareto_param_4"), 250000)
  app_click(app, "remove_pareto_slice_1")
  app$wait_for_js("document.getElementById('slice_pareto_param_2').value === '250,000'", timeout = 30 * 1000)
  expect_equal(server("slice_pareto_param_2"), 250000)
  expect_identical(shown("slice_pareto_param_4"), "")
  expect_true(is.na(server("slice_pareto_param_4")))
  sim_load_example(app, "Motor: excess of loss layer",
                   list(numberOfSimulations = 50000, reinsurance_structure_eel_limit_amount = 200000))
  expect_identical(shown("numberOfSimulations"), "50,000")
  expect_identical(shown("reinsurance_structure_eel_limit_amount"), "200,000")

  expect_app_logs_clean(app)
})
