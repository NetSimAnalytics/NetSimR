# Writes the reports of the website's worked example (vignettes/articles/worked-example.Rmd):
# each of the six tools run on the package's example data, as a visitor can with a few
# clicks, and its report saved. The tools' servers are driven without a browser, with
# shiny::testServer(): the test session has no page, so the script sets the inputs to the
# values the page starts with, and those the page would set when the server asks it to. Each
# step is the tool's example with its starting settings, but for what the article names.
#
#   Rscript pkgdown/example-reports.R <output directory>
#
# The reports are those of the installed NetSimR (or, in a session that has loaded the
# package from source, of that source).
args <- commandArgs(TRUE)
out <- if (length(args) >= 1) args[1] else "docs/reports"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

if (!"NetSimR" %in% loadedNamespaces()) library(NetSimR)
library(shiny)
ns <- asNamespace("NetSimR")
internal <- function(name) get(name, envir = ns)
# the report of a download handler's output, copied to the output directory
keep <- function(file, name) {
  if (!file.copy(file, file.path(out, name), overwrite = TRUE)) stop("could not write ", name)
  cat(sprintf("%-28s %6.0f kB\n", name, file.size(file.path(out, name)) / 1024))
}

# ---- NetGLM: a Poisson model of the motor policies' claim counts ----------------------------
testServer(internal("GLMFittingToolServer"), {
  session$setInputs(try_example = 1)
  session$setInputs(response_variable = "claim_count", glm_distribution = "poisson", link_function = "log",
                    offset = "exposure", offset_log = TRUE, weights = "None",
                    formula = "ns(age, df = 4) + region + vehicle + log(vehicle_value)",
                    holdout_share = "0.2", holdout_seed = 1, fit_model = 1)
  if (!is.null(fit_result()$error)) stop("the GLM did not fit: ", fit_result()$error)
  keep(output$download_glm_report, "netglm-report.html")
})

# ---- NetDisFit: the liability claims, fitted with their policy limits -----------------------
fit_file <- file.path(tempdir(), "fitted_claims.txt")
testServer(internal("distribution_fitting_tool_Server"), {
  session$setInputs(data_source = "example", example = "liability", data_includes_header = TRUE, sep = ",", dec = ".",
                    quote = "\"", sev_limit_source = "column", sev_fit_log_scale = TRUE, severity_hist_bins = 20)
  session$setInputs(severity_var = "amount", sev_limit_col = "policy_limit", execute_sev_analysis = 1)
  keep(output$download_report, "netdisfit-report.html")
  # the closest fit, sent on to the simulator
  table <- sev_table()
  closest <- table$rows[[table$best]][[1]]
  cat("closest severity:", closest, "\n")
  session$setInputs(export_frequency = "none", export_severity = closest)
  file.copy(output$export_settings, fit_file, overwrite = TRUE)
})
fit <- internal("read_settings_file")(fit_file, internal("sim_settings_tool"))$values

# ---- NetSimulator: the book at a limit of 1,000,000, with an excess of loss layer -----------
# the fitted claim size, capped at the highest policy limit, with the claims of a year (2,500
# in four years) and a layer of 750,000 excess of 250,000
severity <- internal("sev_dist_options")[[fit$sevDistr]]
settings <- list(
  numOfSimulations = 20000, seedSetBinary = TRUE, seedValue = 1, multiprocessing = FALSE,
  freqDistr = "Poisson", freq_params = 625,
  sevDistr = fit$sevDistr, sev_params = vapply(severity@paramIDs, function(id) as.numeric(fit[[id]]), numeric(1)),
  paretoSlice = FALSE, sevCapBinary = TRUE, sev_cap_amount = 1e6,
  reinsuranceStructureEEL = "Limited Layer", reinsurance_structure_eel_deductible_amount = 250000,
  reinsurance_structure_eel_limit_amount = 750000, reinsuranceStructureLimitedReinstatements = FALSE,
  reinsuranceStructureAL = "No Reinsurance Structure"
)
report <- file.path(tempdir(), "simulation_report.html")
internal("write_simulation_report")(report, settings, do.call(internal("simulate_function"), settings))
keep(report, "netsimulator-report.html")

# ---- NetTreaty: the example layer priced, and the programmes compared -----------------------
testServer(internal("reinsurance_pricing_tool_Server"), {
  session$setInputs(deductible = 1e6, limit = 4e6, reinstatements = 1, reinstatement_rate = 100, aggregate_deductible = 0,
                    rating_year = 2025, rating_premium = 6e7, loading = 25, loss_source = "example", first_year = NA,
                    last_year = NA, inflation = 4, threshold = 5e5, weights = "premium", pareto_threshold = 7.5e5,
                    pareto_alpha = NA, profile_source = "example", profile_example = "property", loss_ratio = 55,
                    curve = "Y3", severity = "LogNormal", severity_p1 = 10, severity_p2 = 2,
                    experience_method = "burning_cost", experience_weight = 50, full_credibility = 1082)
  session$setInputs(prog_frequency = "Poisson", prog_severity = "LogNormal", prog_f1 = 200, prog_s1 = 11, prog_s2 = 1.5,
                    prog_n = 20000, prog_level = "0.995", prog_loading = 25, prog_coc = 10, prog_seed = 1,
                    run_programmes = 1)
  current_programmes()
  keep(output$download_report, "nettreaty-report.html")
})

# ---- NetCapital: three lines joined by a Gaussian copula -------------------------------------
testServer(internal("portfolio_tool_Server"), {
  session$setInputs(line_source = "model", n_lines = 3, copula = "gaussian", copula_df = 4, corr_mode = "all",
                    corr_all = 0.25, n_sims = 20000, level = "0.995", seed = 1)
  session$setInputs(run = 1)
  current()
  keep(output$download_report, "netcapital-report.html")
})

# ---- NetReserve: the motor triangle, with the bootstrap and IFRS 17 --------------------------
testServer(internal("reserving_tool_Server"), {
  session$setInputs(data_source = "example", example = "motor", tail_method = "none", tail_from = "1",
                    tail_periods = 100, average = "volume", sigma = "log-linear", prior_loss_ratio = 70, decay = 1,
                    yield_curve = "3.0, 3.1, 3.2, 3.3, 3.4", period_length = "1", payment_timing = "0.5",
                    ra_method = "percentile", ra_level = 75, coc_rate = 6, ra_capital = "sii", sii_segment = "1")
  session$setInputs(boot_n = 5000, boot_seed = 1, boot_tail = TRUE, boot_process = "gamma", run_bootstrap = 1)
  keep(output$download_report, "netreserve-report.html")
})

# on the website, each report's page counts its visits with GoatCounter, as the rest of the
# website does (_pkgdown.yml): without cookies or personal data. A report saved from a tool
# counts nothing.
goatcounter <- paste0("<script data-goatcounter=\"https://netsimanalytics.goatcounter.com/count\" ",
                      "async src=\"https://gc.zgo.at/count.js\"></script>")
for (page in list.files(out, pattern = "-report\\.html$", full.names = TRUE)) {
  html <- readLines(page, warn = FALSE, encoding = "UTF-8")
  head_end <- grep("</head>", html, fixed = TRUE)[1]
  if (is.na(head_end)) stop("no </head> in ", page)
  html[head_end] <- sub("</head>", paste0(goatcounter, "</head>"), html[head_end], fixed = TRUE)
  writeLines(html, page, useBytes = TRUE)
}

cat("Wrote the worked example's reports to", out, "\n")
