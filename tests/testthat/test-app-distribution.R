#Browser test of the distribution fitting tool (helpers in helper-app.R)

dft_wait_for_plot <- function(app, id) {
  app$wait_for_js(sprintf("(function () { var img = document.querySelector('#%s img'); return !!img && img.naturalWidth > 0; })()", id),
                  timeout = 60 * 1000)
  image <- app_plot_image(app, id)
  expect_identical(app_png_bytes(image$src)[1:8], app_png_signature)
  #something is drawn, and the output is not an error message
  expect_gt(image$opaque + image$partial, 1000)
  expect_false(app$get_js(sprintf("document.getElementById('%s').classList.contains('shiny-output-error')", id)))
  image
}

test_that("the distribution tool fits a Negative Binomial to overdispersed counts and draws the charts", {
  skip_if_no_app_browser()
  csv <- app_claims_csv()
  counts <- utils::read.csv(csv)$claim_count
  #the maximum likelihood Negative Binomial: its mean is the sample mean, and its size
  #maximises the likelihood
  mu <- mean(counts)
  nll <- function(log_size) -sum(stats::dnbinom(counts, size = exp(log_size), mu = mu, log = TRUE))
  size <- exp(stats::optimize(nll, c(-5, 10), tol = 1e-10)$minimum)
  expect_gt(stats::var(counts) / mu, 1.2)

  app <- start_netsimr_app("distribution", "distribution-counts")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_upload(app, "file1", csv)
  app$wait_for_js("/2,000/.test((document.getElementById('data_overview') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)

  app_goto(app, "frequency")
  app_set(app, counts_var = "claim_count")
  app_click(app, "execute_freq_analysis")
  #(the tiles' labels are in capitals, which innerText keeps)
  app$wait_for_js("/suggested model/i.test((document.getElementById('freq_stats') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)
  app_idle(app)

  #the suggested model is the Negative Binomial, fitted with its size (not capped at 1e8)
  expect_match(tolower(app_text(app, "#freq_stats")), "suggested model negative binomial", fixed = TRUE)
  params <- app_text(app, "#selected_freq_params")
  shown <- regmatches(params, regexec("Negative Binomial[^0-9]*? r ([0-9][0-9,]*[.]?[0-9]*)", params))[[1]]
  expect_length(shown, 2)
  expect_equal(as.numeric(gsub(",", "", shown[2])), size, tolerance = 0.005)
  expect_no_match(params, "r is set to", fixed = TRUE)

  #the charts are drawn as images
  dft_wait_for_plot(app, "freq_fit_plot")
  app$set_inputs(freq_results_tabs = "Histogram", wait_ = FALSE)
  dft_wait_for_plot(app, "count_hist")

  expect_app_logs_clean(app)
})

test_that("a slider's track, handle and value stand out in the dark and the light theme", {
  #the handle was drawn in the card's own colour with no border, so it could not be seen in
  #either theme; the track was 1.7:1 against the card in the dark theme; a control needs 3:1
  #and the value above the handle, as text, 4.5:1
  skip_if_no_app_browser()
  app <- start_netsimr_app("distribution", "distribution-sliders")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "severity")
  for (mode in c("dark", "light")) {
    app_theme(app, mode)
    parts <- app$get_js("(function () {
      var irs = document.getElementById('severity_hist_bins').parentElement.querySelector('.irs');
      var card = irs.closest('.card');
      function part(sel) {
        var s = getComputedStyle(irs.querySelector(sel));
        return {bg: s.backgroundColor, text: s.color,
                edge: parseFloat(s.borderTopWidth) > 0 ? s.borderTopColor : 'rgba(0, 0, 0, 0)'};
      }
      return {card: getComputedStyle(card).backgroundColor,
              line: part('.irs-line'), handle: part('.irs-handle'), value: part('.irs-single')};
    })()")
    card <- app_colour(parts$card)
    #a part stands out by its fill or by its edge, whichever is the clearer
    shows <- function(p) max(app_contrast(app_colour(p$bg, beneath = card), card),
                             app_contrast(app_colour(p$edge, beneath = card), card))
    expect_gte(shows(parts$line), 3)
    expect_gte(shows(parts$handle), 3)
    value <- app_colour(parts$value$bg, beneath = card)
    expect_gte(app_contrast(app_colour(parts$value$text, beneath = value), value), 4.5)
  }
  expect_app_logs_clean(app)
})

test_that("fitted models saved on the 'To the simulator' tab load into the simulator", {
  skip_if_no_app_browser()
  set.seed(11)
  csv <- file.path(tempdir(), "netsimr_fit_to_simulator.csv")
  utils::write.csv(data.frame(claim_count = stats::rnbinom(1500, size = 2, mu = 3),
                              amount = round(exp(stats::rnorm(1500, 8, 1.2)))), csv, row.names = FALSE)
  claims <- utils::read.csv(csv)

  app <- start_netsimr_app("distribution", "distribution-export")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_upload(app, "file1", csv)
  app$wait_for_js("/1,500/.test((document.getElementById('data_overview') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)

  #a model whose tab has not been run says so, and offers nothing to save
  app_goto(app, "export")
  app_set(app, export_severity = "LogNormal - Pareto - Pareto")
  app$wait_for_js("/Run the analysis on the Sliced severity tab/.test(document.getElementById('export_summary').innerText)",
                  timeout = 30 * 1000)
  expect_false(app$get_js("!!document.getElementById('export_settings')"))

  app_goto(app, "frequency")
  app_set(app, counts_var = "claim_count")
  app_click(app, "execute_freq_analysis")
  app$wait_for_js("/suggested model/i.test((document.getElementById('freq_stats') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)
  app_goto(app, "sliced")
  app_set(app, sliced_sev_var = "amount")
  app_click(app, "execute_sliced_sev_analysis")
  app_idle(app)
  points <- unlist(app_inputs(app, c("slicing_point_left", "slicing_point_right")))

  app_goto(app, "export")
  app_set(app, export_frequency = "Negative Binomial")
  app$wait_for_js("/Slice 2 alpha/.test(document.getElementById('export_summary').innerText)", timeout = 30 * 1000)
  saved <- app_download(app, "export_settings")
  expect_identical(basename(saved), "fitted_claims_simulator_settings.txt")
  read <- sim_settings_read(saved)
  expect_true(sim_settings_validate(read$inputs, read$version))
  values <- read$inputs

  #the Negative Binomial as the Frequency tab fits it
  nb <- fit_nbinom_mle(claims$claim_count)
  expect_identical(values$freqDistr, "Negative_Binomial")
  expect_equal(c(values$r, values$beta), unname(c(nb$estimate[["size"]], nb$estimate[["mu"]] / nb$estimate[["size"]])),
               tolerance = 1e-10)
  #a LogNormal body with two slices at the slicing points, the top alpha the tail's maximum likelihood
  expect_identical(values$sevDistr, "LogNormal")
  expect_true(all(is.finite(c(values$mu, values$sigma))))
  expect_identical(values$pareto_slice_times, 2L)
  expect_equal(c(values$slice_pareto_param_2, values$slice_pareto_param_4), unname(points))
  tail <- claims$amount[claims$amount > points[2]]
  expect_equal(values$slice_pareto_param_3, length(tail) / sum(log(tail / points[2])), tolerance = 1e-12)
  expect_app_logs_clean(app)

  #the simulator sets each of them
  sim <- start_netsimr_app("simulator", "simulator-fitted")
  on.exit(sim$stop(), add = TRUE)
  app_goto(sim, "simulator")
  app_upload(sim, "settingsIO_load", saved)
  app_wait_for_notification(sim, "Settings loaded")
  app_wait_for_inputs(sim, values, "the fitted settings to be loaded")
  expect_app_logs_clean(sim)
})

test_that("claims reported above a threshold are fitted from zero, and their frequency is sent for all claims", {
  skip_if_no_app_browser()
  set.seed(12)
  csv <- file.path(tempdir(), "netsimr_fit_above_threshold.csv")
  #the claims above 3,000 of LogNormal(8, 1.2) claims, with counts of those claims
  amounts <- round(exp(stats::rnorm(6000, 8, 1.2)))
  amounts <- amounts[amounts > 3000][1:2000]
  utils::write.csv(data.frame(claim_count = stats::rpois(2000, 1.5), amount = amounts), csv, row.names = FALSE)
  claims <- utils::read.csv(csv)

  app <- start_netsimr_app("distribution", "distribution-threshold")
  on.exit(app$stop(), add = TRUE)
  app_goto(app, "data")
  app_upload(app, "file1", csv)
  app$wait_for_js("/2,000/.test((document.getElementById('data_overview') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)

  app_goto(app, "frequency")
  app_set(app, counts_var = "claim_count")
  app_click(app, "execute_freq_analysis")
  app$wait_for_js("/suggested model/i.test((document.getElementById('freq_stats') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)

  app_goto(app, "severity")
  app_set(app, severity_var = "amount", sev_threshold = 3000)
  #the field shows the threshold with its separator
  expect_identical(app$get_js("document.getElementById('sev_threshold').value"), "3,000")
  app_click(app, "execute_sev_analysis")
  app$wait_for_js("/Mean above\\s+threshold/i.test((document.getElementById('sev_param_summary') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)
  table <- app_text(app, "#sev_param_summary")
  expect_match(table, "reporting threshold of 3,000", fixed = TRUE)
  expect_match(table, "The Pareto and the generalised Pareto start at the threshold", fixed = TRUE)
  #the LogNormal of the claims from zero, not of the claims above the threshold
  fit <- dft_truncated_severity_models(claims$amount, 3000)[[2]]
  expect_match(table, netsimr_fmt(fit$params[["mu"]]), fixed = TRUE)
  expect_lt(abs(fit$params[["mu"]] - 8), 0.15)

  app_goto(app, "export")
  app_set(app, export_frequency = "Poisson", export_severity = "LogNormal")
  app$wait_for_js("!!document.getElementById('export_counts_above_threshold')", timeout = 30 * 1000)
  expect_match(app_text(app, "#export_summary"), "frequency is of all claims from zero", fixed = TRUE)
  values <- sim_settings_read(app_download(app, "export_settings"))$inputs
  expect_equal(c(values$mu, values$sigma), unname(fit$params), tolerance = 1e-8)
  #the Poisson of the counts above the threshold, divided by the share of claims above it
  expect_equal(values$lamda, mean(claims$claim_count) / fit$survival_at_threshold, tolerance = 1e-8)

  #counts of all claims are sent as they are
  app_set(app, export_counts_above_threshold = FALSE)
  app$wait_for_js("/must count all of them/.test(document.getElementById('export_summary').innerText)", timeout = 30 * 1000)
  values <- sim_settings_read(app_download(app, "export_settings", filename = file.path(tempdir(), "unscaled.txt")))$inputs
  expect_equal(values$lamda, mean(claims$claim_count), tolerance = 1e-12)

  #without a threshold the Severity tab fits all the claims again
  app_goto(app, "severity")
  app$run_js("var e = document.getElementById('sev_threshold'); e.value = ''; $(e).trigger('change');")
  app_wait_for(function() {
    d <- app_inputs(app, "sev_threshold")$sev_threshold
    is.null(d) || all(is.na(d))
  }, "the threshold to be cleared")
  app_click(app, "execute_sev_analysis")
  app$wait_for_js("/smallest claim/.test((document.getElementById('sev_param_summary') || {innerText: ''}).innerText)",
                  timeout = 60 * 1000)
  expect_no_match(app_text(app, "#sev_param_summary"), "above threshold", fixed = TRUE)

  expect_app_logs_clean(app)
})
