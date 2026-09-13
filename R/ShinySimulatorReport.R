#' Styles for the simulation report
#'
#' @noRd
simulation_report_css <- "
:root {
  --r-navy: #0b1f3d;
  --r-blue: #2563eb;
  --r-blue-soft: #eff6ff;
  --r-red: #dc2626;
  --r-border: #e2e8f0;
  --r-muted: #64748b;
  --r-text: #0f172a;
  --r-label: #334155;
  --r-shadow: 0 1px 2px rgba(15, 23, 42, 0.04), 0 6px 18px rgba(15, 23, 42, 0.05);
}

* { box-sizing: border-box; }

html { scroll-behavior: smooth; }

body {
  margin: 0;
  font-family: 'Inter', 'Segoe UI', system-ui, -apple-system, Roboto, 'Helvetica Neue', Arial, sans-serif;
  color: var(--r-text);
  background: #f1f5f9;
  font-size: 15px;
  line-height: 1.55;
  -webkit-font-smoothing: antialiased;
}

/* ---------- Layout ---------- */
.report-layout {
  max-width: 1240px;
  margin: 0 auto;
  padding: 24px;
  display: grid;
  grid-template-columns: 210px minmax(0, 1fr);
  gap: 28px;
  align-items: start;
}

.report-nav {
  position: sticky;
  top: 24px;
  background: #ffffff;
  border: 1px solid var(--r-border);
  border-radius: 14px;
  box-shadow: var(--r-shadow);
  padding: 12px;
}

.report-nav-title {
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--r-muted);
  padding: 4px 10px 8px 10px;
}

.report-nav ul {
  list-style: none;
  margin: 0;
  padding: 0;
}

.report-nav a {
  display: block;
  padding: 7px 10px;
  border-radius: 8px;
  color: var(--r-label);
  text-decoration: none;
  font-size: 0.9rem;
  font-weight: 600;
}

.report-nav a:hover,
.report-nav a:focus-visible {
  background: var(--r-blue-soft);
  color: var(--r-blue);
}

section {
  scroll-margin-top: 16px;
}

/* ---------- Header banner ---------- */
.report-header {
  background:
    radial-gradient(circle at 88% 15%, rgba(96, 165, 250, 0.35), transparent 45%),
    linear-gradient(135deg, #0b1f3d 0%, #13315c 60%, #1d4ed8 140%);
  color: #ffffff;
  border-radius: 18px;
  padding: 28px 32px;
  margin-bottom: 8px;
  box-shadow: var(--r-shadow);
}

.report-eyebrow {
  color: #93c5fd;
  font-size: 0.78rem;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: 0.1em;
  margin: 0 0 8px 0;
}

.report-header h1 {
  color: #ffffff;
  font-weight: 800;
  font-size: 2.1rem;
  line-height: 1.15;
  letter-spacing: -0.02em;
  margin: 0;
}

.report-date {
  color: rgba(255, 255, 255, 0.75);
  font-size: 0.92rem;
  font-weight: 500;
  margin: 8px 0 0 0;
}

/* ---------- Headings ---------- */
h2 {
  font-weight: 800;
  font-size: 1.35rem;
  line-height: 1.3;
  letter-spacing: -0.01em;
  margin: 36px 0 14px 0;
  padding-bottom: 8px;
  border-bottom: 1px solid var(--r-border);
}

h3 {
  font-size: 1rem;
  font-weight: 700;
  color: var(--r-label);
  margin: 0 0 10px 0;
}

.section-intro {
  color: var(--r-muted);
  margin: -4px 0 14px 0;
}

/* ---------- Headline figures ---------- */
.kpi-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
}

.kpi {
  background: #ffffff;
  border: 1px solid var(--r-border);
  border-top: 3px solid var(--r-blue);
  border-radius: 14px;
  padding: 14px 16px;
  box-shadow: var(--r-shadow);
}

.kpi.kpi-tail {
  border-top-color: var(--r-red);
}

.kpi-label {
  font-size: 0.72rem;
  font-weight: 800;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--r-muted);
}

.kpi-value {
  font-size: 1.4rem;
  font-weight: 800;
  margin-top: 4px;
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.kpi-note {
  font-size: 0.8rem;
  color: var(--r-muted);
  margin-top: 2px;
}

/* ---------- Settings ---------- */
.settings-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 12px;
}

.setting-card {
  background: #ffffff;
  border: 1px solid var(--r-border);
  border-radius: 14px;
  padding: 14px 16px;
  box-shadow: var(--r-shadow);
}

.setting-title {
  display: flex;
  align-items: center;
  gap: 8px;
  font-weight: 800;
  margin-bottom: 10px;
}

.setting-title::before {
  content: '';
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--r-blue);
}

.setting-card dl {
  display: grid;
  grid-template-columns: auto 1fr;
  gap: 6px 14px;
  margin: 0;
}

.setting-card dt {
  font-weight: 600;
  font-size: 0.88rem;
  color: var(--r-muted);
}

.setting-card dd {
  margin: 0;
  text-align: right;
  font-weight: 600;
  font-variant-numeric: tabular-nums;
}

.setting-off {
  color: var(--r-muted);
  font-weight: 500;
}

/* ---------- Tables ---------- */
.two-col {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
  gap: 16px;
  align-items: start;
}

.table-card {
  background: #ffffff;
  border: 1px solid var(--r-border);
  border-radius: 14px;
  padding: 14px 16px 8px 16px;
  box-shadow: var(--r-shadow);
  overflow-x: auto;
}

.report-table {
  width: 100%;
  border-collapse: collapse;
  font-variant-numeric: tabular-nums;
  font-size: 0.92rem;
}

.report-table th,
.report-table td {
  padding: 7px 8px;
  text-align: left;
}

.report-table th {
  font-size: 0.72rem;
  font-weight: 800;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--r-muted);
  border-bottom: 1px solid var(--r-border);
}

.report-table td {
  border-top: 1px solid #f1f5f9;
}

.report-table .num {
  text-align: right;
}

.report-table tbody tr:hover td {
  background: #f8fafc;
}

/* ---------- Charts ---------- */
.chart-card {
  background: #ffffff;
  border: 1px solid var(--r-border);
  border-radius: 14px;
  padding: 16px 18px 10px 18px;
  margin-bottom: 16px;
  box-shadow: var(--r-shadow);
}

.chart-card img {
  display: block;
  width: 100%;
  height: auto;
}

.chart-note {
  color: var(--r-muted);
  font-size: 0.85rem;
  margin: 0 0 8px 0;
}

/* ---------- Notes and footer ---------- */
.callout {
  background: var(--r-blue-soft);
  border: 1px solid #bfdbfe;
  border-left: 4px solid var(--r-blue);
  border-radius: 12px;
  padding: 14px 18px;
}

.callout ul {
  margin: 0;
  padding-left: 1.1rem;
}

.callout li + li {
  margin-top: 4px;
}

.report-footer {
  color: var(--r-muted);
  font-size: 0.85rem;
  text-align: center;
  margin: 32px 0 8px 0;
}

/* ---------- Responsive and print ---------- */
@media (max-width: 900px) {
  .report-layout {
    grid-template-columns: 1fr;
    padding: 16px;
    gap: 16px;
  }

  .report-nav {
    position: static;
  }

  .report-nav ul {
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
  }
}

@media (max-width: 767px) {
  .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
  .report-header { padding: 22px 20px; }
  .report-header h1 { font-size: 1.6rem; }
}

@media (max-width: 420px) {
  .kpi-grid { grid-template-columns: 1fr; }
}

@media print {
  .report-nav { display: none; }
  .report-layout { display: block; padding: 0; }
  body { background: #ffffff; }
  .kpi, .setting-card, .table-card, .chart-card { box-shadow: none; break-inside: avoid; }
  .report-header { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
}
"

#' Write the simulation report as a self-contained HTML file
#'
#' Builds the page with htmltools and embeds the charts as PNG images, so the
#' report needs neither pandoc nor rmarkdown.
#'
#' @param file Path of the HTML file to write.
#' @param settings The list of \code{simulate_function} arguments used for the run.
#' @param total_claims Numeric vector of simulated total claims.
#' @param generated Time stamp shown in the report.
#' @return The path of the written file, invisibly.
#' @noRd
write_simulation_report <- function(file, settings, total_claims, generated = Sys.time()) {
  s <- settings
  claims <- as.numeric(unlist(total_claims))
  claims <- claims[!is.na(claims)]
  if (length(claims) == 0) stop("There are no simulated totals to report.", call. = FALSE)

  div <- htmltools::div
  tags <- htmltools::tags

  #special characters built from code points keep this file ASCII-only
  dash <- intToUtf8(8212)
  nbsp <- intToUtf8(160)
  alpha <- intToUtf8(945)
  middot <- intToUtf8(183)

  # ---------- formatting helpers ----------
  is_blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))
  fmt_num <- function(x, digits = 2) {
    if (is_blank(x)) return(dash)
    formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
  }
  fmt_int <- function(x) if (is_blank(x)) dash else formatC(round(as.numeric(x)), format = "d", big.mark = ",")
  fmt_pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
  #headline amounts use one decimal style for the whole report, set by the scale of the results
  amount_digits <- if (max(abs(claims)) >= 1000) 0 else 2
  fmt_amount <- function(x) if (is_blank(x)) dash else fmt_num(x, amount_digits)
  #settings amounts are formatted on their own scale
  fmt_setting <- function(x) if (is_blank(x)) dash else fmt_num(x, if (abs(as.numeric(x)) >= 1000) 0 else 2)
  fmt_return_period <- function(p) paste("1 in", formatC(round(1 / (1 - p)), format = "d", big.mark = ","))

  known <- function(options, id) !is_blank(id) && is.character(id) && id %in% names(options)
  distr_label <- function(options, id) {
    if (known(options, id)) return(options[[id]]@distr_label)
    if (!is_blank(id)) return(gsub("_", " ", id))
    dash
  }
  param_text <- function(values, options, id) {
    values <- unlist(values)
    if (is_blank(values)) return(dash)
    shown <- sub("\\.?0+$", "", fmt_num(values, 4))
    labels <- if (known(options, id)) options[[id]]@param_labels else NULL
    if (length(labels) == length(values)) {
      #non-breaking spaces keep each "name = value" pair on one line
      paste(paste(labels, shown, sep = paste0(nbsp, "=", nbsp)), collapse = ", ")
    } else {
      paste(shown, collapse = ", ")
    }
  }

  # ---------- statistics ----------
  quantile_of <- function(p) unname(stats::quantile(claims, p))
  #TVaR as the average of the worst (1 - p) share of simulations; unlike averaging
  #everything at or above VaR, this stays correct when many totals tie (e.g. at zero)
  claims_desc <- sort(claims, decreasing = TRUE)
  tvar_of <- function(p) mean(claims_desc[seq_len(max(1, ceiling(length(claims_desc) * (1 - p))))])
  claims_mean <- mean(claims)
  claims_sd <- if (length(claims) > 1) stats::sd(claims) else NA
  var995 <- quantile_of(0.995)
  tvar995 <- tvar_of(0.995)
  cv_text <- if (isTRUE(claims_mean != 0) && !is.na(claims_sd)) fmt_num(claims_sd / claims_mean, 3) else dash

  # ---------- charts ----------
  col_blue <- "#3b82f6"
  col_blue_dark <- "#1d4ed8"
  col_navy <- "#0f172a"
  col_red <- "#dc2626"
  col_grid <- "#e2e8f0"
  col_muted <- "#64748b"

  chart_par <- function(mar = c(4.2, 5.6, 1, 1)) {
    graphics::par(mar = mar, mgp = c(3.4, 0.7, 0), las = 1, col.axis = col_muted,
                  col.lab = "#334155", fg = col_muted, cex.axis = 0.85, cex.lab = 0.95)
  }
  axis_labels <- function(at) {
    if (max(abs(at), na.rm = TRUE) >= 100) formatC(at, format = "f", digits = 0, big.mark = ",") else format(at)
  }
  x_axis <- function(at) graphics::axis(1, at = at, labels = axis_labels(at), col = col_grid, col.ticks = col_grid)

  #layers that are rarely hit produce many zero totals; a single bar at zero would flatten
  #the histogram and box plot, so those charts show the non-zero totals and state the zero share
  zero_share <- mean(claims == 0)
  drop_zeros <- zero_share >= 0.2 && any(claims > 0)
  plot_claims <- if (drop_zeros) claims[claims > 0] else claims
  zero_note <- if (drop_zeros) {
    paste0(" ", fmt_pct(zero_share), " of simulations had zero total claims and are left out of this chart.")
  } else {
    ""
  }

  #draw into a temporary PNG at twice screen resolution and embed it as a data URI
  chart_image <- function(draw, height, alt) {
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path), add = TRUE)
    res <- 192
    grDevices::png(path, width = 9 * res, height = height * res, res = res)
    tryCatch(draw(), finally = grDevices::dev.off())
    tags$img(src = base64enc::dataURI(file = path, mime = "image/png"), alt = alt)
  }

  draw_histogram <- function() {
    chart_par()
    h <- graphics::hist(plot_claims, breaks = 80, plot = FALSE)
    y_top <- max(h$counts) * 1.1
    graphics::plot(h, col = NA, border = NA, main = "", xlab = "Total claims", ylab = "Simulations",
                   axes = FALSE, ylim = c(0, y_top))
    y_at <- pretty(c(0, y_top))
    graphics::abline(h = y_at, col = col_grid, lwd = 0.8)
    graphics::plot(h, col = col_blue, border = "white", add = TRUE)
    x_axis(pretty(h$breaks))
    graphics::axis(2, at = y_at, labels = formatC(y_at, format = "d", big.mark = ","), lwd = 0)
    graphics::abline(v = claims_mean, col = col_navy, lty = 2, lwd = 1.8)
    graphics::abline(v = var995, col = col_red, lty = 2, lwd = 1.8)
    graphics::legend("topright", bty = "n", cex = 0.85, text.col = "#334155", lty = 2, lwd = 1.8,
                     col = c(col_navy, col_red),
                     legend = c(paste("Mean", fmt_amount(claims_mean)), paste("VaR 99.5%", fmt_amount(var995))))
  }

  draw_boxplot <- function() {
    chart_par(mar = c(4.2, 1.5, 0.6, 1))
    graphics::boxplot(plot_claims, horizontal = TRUE, axes = FALSE, frame.plot = FALSE,
                      col = "#dbeafe", border = col_blue_dark, medcol = col_navy, medlwd = 2.4,
                      whisklty = 1, staplelwd = 1.2, outpch = 16, outcex = 0.35,
                      outcol = grDevices::adjustcolor(col_blue_dark, alpha.f = 0.25),
                      xlab = "Total claims")
    x_axis(pretty(range(plot_claims)))
  }

  draw_cdf <- function() {
    chart_par()
    cdf_probs <- seq(0, 1, length.out = 1001)
    cdf_x <- stats::quantile(claims, cdf_probs, names = FALSE)
    graphics::plot(cdf_x, cdf_probs, type = "n", axes = FALSE, main = "",
                   xlab = "Total claims", ylab = "Cumulative probability", ylim = c(0, 1))
    graphics::abline(h = seq(0, 1, 0.25), col = col_grid, lwd = 0.8)
    graphics::abline(v = var995, col = col_red, lty = 2, lwd = 1.5)
    graphics::lines(cdf_x, cdf_probs, type = "s", col = col_blue, lwd = 2.4)
    x_axis(pretty(range(cdf_x)))
    graphics::axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"), lwd = 0)
  }

  # ---------- building blocks ----------
  tile <- function(label, value, note = NULL, class = "kpi") {
    div(class = class,
        div(class = "kpi-label", label),
        div(class = "kpi-value", value),
        if (!is.null(note)) div(class = "kpi-note", note))
  }

  off <- function(text = "Not applied") tags$span(class = "setting-off", text)

  setting_card <- function(title, rows) {
    rows <- rows[!vapply(rows, is.null, logical(1))]
    div(
      class = "setting-card",
      div(class = "setting-title", title),
      tags$dl(lapply(names(rows), function(key) htmltools::tagList(tags$dt(key), tags$dd(rows[[key]]))))
    )
  }

  report_table <- function(df, numeric_cols) {
    cell_class <- function(j) if (j %in% numeric_cols) "num" else NULL
    tags$table(
      class = "report-table",
      tags$thead(tags$tr(lapply(seq_along(df), function(j) tags$th(class = cell_class(j), names(df)[j])))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(lapply(seq_along(df), function(j) tags$td(class = cell_class(j), df[i, j])))
      }))
    )
  }

  report_section <- function(id, title, ...) tags$section(id = id, tags$h2(title), ...)

  has_deductible <- function(structure) isTRUE(structure %in% c("Unlimited Layer", "Limited Layer", "Exclude Layer"))
  has_limit <- function(structure) isTRUE(structure %in% c("Limited Layer", "Exclude Layer"))
  no_structure <- function(structure) is_blank(structure) || identical(structure, "No Reinsurance Structure")

  # ---------- sections ----------
  key_results <- report_section(
    "key-results", "Key results",
    tags$p(class = "section-intro",
           "Total claims per simulated period, after the tail adjustments and reinsurance structures below."),
    div(
      class = "kpi-grid",
      tile("Mean", fmt_amount(claims_mean), paste(fmt_int(length(claims)), "simulations")),
      tile("Median", fmt_amount(quantile_of(0.5))),
      tile("Standard deviation", fmt_amount(claims_sd), if (cv_text != dash) paste("CV", cv_text)),
      tile("VaR 99.5%", fmt_amount(var995), fmt_return_period(0.995), class = "kpi kpi-tail"),
      tile("TVaR 99.5%", fmt_amount(tvar995), "Average beyond VaR 99.5%", class = "kpi kpi-tail"),
      tile("Maximum", fmt_amount(max(claims)), "Largest simulated total")
    )
  )

  slice_count <- if (isTRUE(s$paretoSlice) && !is_blank(s$pareto_slice_times)) as.integer(s$pareto_slice_times) else 0L
  slice_rows <- if (slice_count > 0) {
    alphas <- unlist(s$slice_pareto_alphas)
    x_ms <- unlist(s$slice_pareto_x_ms)
    stats::setNames(
      lapply(seq_len(slice_count), function(j) {
        paste0(alpha, " ", fmt_num(alphas[j], 2), " from ", fmt_setting(x_ms[j]))
      }),
      paste("Pareto slice", seq_len(slice_count))
    )
  } else {
    list("Pareto slices" = off())
  }

  eel <- s$reinsuranceStructureEEL
  al <- s$reinsuranceStructureAL

  model_settings <- report_section(
    "model-settings", "Model settings",
    div(
      class = "settings-grid",
      setting_card("Simulation", list(
        "Simulations" = fmt_int(s$numOfSimulations),
        "Seed" = if (isTRUE(s$seedSetBinary)) paste("Fixed at", s$seedValue) else off("Random"),
        "Processing" = if (isTRUE(s$multiprocessing)) "Parallel" else "Single process"
      )),
      setting_card("Frequency", list(
        "Distribution" = distr_label(freq_dist_options, s$freqDistr),
        "Parameters" = param_text(s$freq_params, freq_dist_options, s$freqDistr)
      )),
      setting_card("Severity", list(
        "Distribution" = distr_label(sev_dist_options, s$sevDistr),
        "Parameters" = param_text(s$sev_params, sev_dist_options, s$sevDistr)
      )),
      setting_card("Tail adjustments", c(
        slice_rows,
        list("Severity cap" = if (isTRUE(s$sevCapBinary)) fmt_setting(s$sev_cap_amount) else off())
      )),
      setting_card("Each & every loss (EEL)", list(
        "Structure" = if (no_structure(eel)) off("None") else eel,
        "Deductible" = if (has_deductible(eel)) fmt_setting(s$reinsurance_structure_eel_dedctible_amount),
        "Limit" = if (has_limit(eel)) fmt_setting(s$reinsurance_structure_eel_limit_amount),
        "Reinstatements" = if (isTRUE(eel == "Limited Layer")) {
          if (isTRUE(s$reinsuranceStructureLimitedReinstatements)) {
            paste("Up to", fmt_int(s$reinsuranceStructureReinstatementLimit))
          } else {
            off("Unlimited")
          }
        }
      )),
      setting_card("Aggregate layer (AL)", list(
        "Structure" = if (no_structure(al)) off("None") else al,
        "Deductible" = if (has_deductible(al)) fmt_setting(s$reinsurance_structure_al_dedctible_amount),
        "Limit" = if (has_limit(al)) fmt_setting(s$reinsurance_structure_al_limit_amount)
      ))
    )
  )

  summary_stats <- data.frame(
    Metric = c("Simulations", "Mean", "Median", "Standard deviation", "Coefficient of variation", "Minimum", "Maximum"),
    Value = c(fmt_int(length(claims)), fmt_num(claims_mean), fmt_num(quantile_of(0.5)), fmt_num(claims_sd),
              cv_text, fmt_num(min(claims)), fmt_num(max(claims))),
    stringsAsFactors = FALSE
  )
  probs <- c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 0.995)
  tail_table <- data.frame(
    Percentile = fmt_pct(probs),
    `Return period` = vapply(probs, fmt_return_period, character(1)),
    VaR = vapply(probs, function(p) fmt_num(quantile_of(p)), character(1)),
    TVaR = vapply(probs, function(p) fmt_num(tvar_of(p)), character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  statistics <- report_section(
    "statistics", "Statistics and tail risk",
    div(
      class = "two-col",
      div(class = "table-card", tags$h3("Summary statistics"), report_table(summary_stats, 2)),
      div(class = "table-card", tags$h3("Percentiles, VaR and TVaR"), report_table(tail_table, c(3, 4)))
    )
  )

  charts <- report_section(
    "charts", "Charts",
    div(class = "chart-card",
        tags$h3("Distribution of total claims"),
        tags$p(class = "chart-note", paste0("Dashed lines mark the mean and the 99.5% VaR of all simulations.", zero_note)),
        chart_image(draw_histogram, 4.2, "Histogram of total claims")),
    div(class = "chart-card",
        tags$h3("Spread and outliers"),
        tags$p(class = "chart-note", paste0("The box covers the middle 50% of simulations; dots are outliers.", zero_note)),
        chart_image(draw_boxplot, 2.3, "Box plot of total claims")),
    div(class = "chart-card",
        tags$h3("Cumulative distribution"),
        tags$p(class = "chart-note", "Share of simulations with total claims at or below each amount."),
        chart_image(draw_cdf, 4.2, "Cumulative distribution of total claims"))
  )

  reading_guide <- report_section(
    "how-to-read", "How to read this report",
    div(class = "callout", tags$ul(
      tags$li("A large gap between the median and the tail percentiles points to a heavy-tailed outcome."),
      tags$li("VaR is the loss exceeded only with the stated probability. TVaR is the average loss in those worst cases, so it is always at least as large as VaR."),
      tags$li("The return period shows the same probability as a frequency: 99.5% corresponds to a 1 in 200 year event."),
      tags$li("Figures reflect total claims after any tail adjustments and reinsurance structures listed under Model settings.")
    ))
  )

  sections <- list(
    c("key-results", "Key results"),
    c("model-settings", "Model settings"),
    c("statistics", "Statistics and tail risk"),
    c("charts", "Charts"),
    c("how-to-read", "How to read this report")
  )
  nav <- tags$nav(
    class = "report-nav",
    `aria-label` = "Contents",
    div(class = "report-nav-title", "Contents"),
    tags$ul(lapply(sections, function(x) tags$li(tags$a(href = paste0("#", x[1]), x[2]))))
  )

  generated_text <- format(generated, "%d %B %Y, %H:%M")
  version_text <- tryCatch(paste0(" ", utils::packageVersion("NetSimR")), error = function(e) "")

  body <- tags$body(
    div(
      class = "report-layout",
      nav,
      tags$main(
        tags$header(
          class = "report-header",
          tags$p(class = "report-eyebrow", "NetSimR Claims & Reinsurance Simulator"),
          tags$h1("Simulation Report"),
          tags$p(class = "report-date", paste("Generated", generated_text))
        ),
        key_results,
        model_settings,
        statistics,
        charts,
        reading_guide,
        tags$footer(class = "report-footer",
                    paste0("Generated with NetSimR", version_text, " ", middot, " ", generated_text))
      )
    )
  )

  #the head is written by hand: htmltools moves tags$head() content out when rendering to text,
  #which would drop the title and the styles
  html <- enc2utf8(paste0(
    "<!DOCTYPE html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>Simulation Report</title>\n",
    "<style>", simulation_report_css, "</style>\n",
    "</head>\n",
    as.character(body), "\n",
    "</html>\n"
  ))
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(file)
}
