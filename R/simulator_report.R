#' Ticks for a chart axis of numbers of simulations
#'
#' Whole numbers only, so a chart of a few simulations is not labelled 0, 0, 0, 0, 0, 1.
#' @param top The top of the axis.
#' @return Increasing whole-number tick values from zero.
#' @noRd
sim_report_count_ticks <- function(top) {
  at <- pretty(c(0, top))
  #pretty() steps such as 0.2 are not exact, so 5 * 0.2 is compared with a tolerance
  at <- round(at[abs(at - round(at)) < 1e-8])
  if (length(at) < 2) at <- c(0, max(1, ceiling(top)))
  at
}

#' Range of amounts a report chart spans
#'
#' A single value (every total equal, e.g. one simulation or a fixed claim amount) is
#' widened to 12 per cent either side of it (0.5 either side of zero), so it is drawn as a
#' narrow bar or step in the middle of the chart rather than across the whole axis.
#' @param x Finite amounts.
#' @return The range to draw, two numbers.
#' @noRd
sim_report_amount_range <- function(x) {
  x_range <- range(x)
  if (x_range[1] != x_range[2]) return(x_range)
  half <- if (x_range[1] != 0) abs(x_range[1]) * 0.01 else 0.5
  widened <- x_range[1] + c(-12, 12) * half
  if (all(is.finite(widened))) widened else x_range
}

#' Totals the report's histogram draws
#'
#' Layers that are rarely hit produce many zero totals; a single bar at zero would flatten
#' the histogram, so from a fifth of zeros up the chart leaves the zeros out and the report
#' states their share. Only the zeros go: negative totals (a Normal severity with a negative
#' mean) belong to the distribution the chart describes and stay in it.
#' @param x Finite totals.
#' @param zero_share Share of the simulations whose total is zero.
#' @return A list with \code{values}, the totals to draw, and \code{zeros_dropped}, whether
#'   the zeros were left out.
#' @noRd
sim_report_histogram_totals <- function(x, zero_share = mean(x == 0)) {
  zeros_dropped <- isTRUE(zero_share >= 0.2) && any(x != 0)
  list(values = if (zeros_dropped) x[x != 0] else x, zeros_dropped = zeros_dropped)
}

#' Write the simulation report as a self-contained HTML file
#'
#' Builds the page with shiny's HTML tag functions and embeds the charts as PNG images, so the
#' report needs neither pandoc nor rmarkdown.
#'
#' @param file Path of the HTML file to write.
#' @param settings The list of \code{simulate_function} arguments used for the run.
#' @param results The data frame returned by \code{simulate_function}. A numeric
#'   vector of total claims is also accepted.
#' @param generated Time stamp shown in the report.
#' @return The path of the written file, invisibly.
#' @noRd
write_simulation_report <- function(file, settings, results, generated = Sys.time()) {
  s <- settings
  #every figure in the report comes from the summary; the raw columns only feed the charts
  summary <- summarise_simulation(settings, results)
  claims <- summary$totals
  n <- summary$n
  #simulations whose total is undefined (NaN) are left out of every figure, and said so
  n_undefined <- summary$undefined
  if (is.numeric(results)) results <- data.frame(total_claims = results)
  results <- as.data.frame(results)
  column_or_null <- function(name) if (name %in% names(results)) as.numeric(results[[name]]) else NULL
  #the claim counts of every simulation are defined, so the frequency chart uses them all
  counts <- column_or_null("claim_counts")
  #each period's largest single claim, for occurrence exceedance (OEP), on the simulations the
  #other figures use (those with a defined total)
  largest <- column_or_null("largest_claim")
  if (!is.null(largest)) {
    largest <- largest[!is.na(as.numeric(results$total_claims))]
    largest <- largest[!is.na(largest)]
    if (!length(largest)) largest <- NULL
  }
  results <- results[!is.na(results$total_claims), , drop = FALSE]
  gross <- column_or_null("gross_claims")

  div <- shiny::div
  tags <- shiny::tags

  #special characters built from code points keep this file ASCII-only
  dash <- intToUtf8(8212)
  nbsp <- intToUtf8(160)
  alpha <- intToUtf8(945)
  middot <- intToUtf8(183)

  # ---------- formatting helpers ----------
  is_blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x))
  #amounts and shares as the Compare tab writes them
  fmt_num <- sim_fmt_amount
  fmt_int <- function(x) if (is_blank(x)) dash else formatC(round(as.numeric(x)), format = "d", big.mark = ",")
  fmt_pct <- sim_fmt_percent
  #small probabilities get an extra decimal so they do not round to zero
  fmt_prob <- function(p) {
    if (is_blank(p)) return(dash)
    if (p > 0 && p < 0.01) fmt_pct(p, 2) else fmt_pct(p)
  }
  #headline amounts use one decimal style for the whole report, set by the scale of the
  #modelled totals as on the Compare tab, so a small ceded series under a large gross one
  #keeps its decimals (the results are unrounded; small amounts get enough decimals not to
  #show as zero); the gross totals are formatted on their own scale in their table
  amount_digits <- display_digits(claims)
  fmt_amount <- function(x) if (is_blank(x)) dash else fmt_num(x, amount_digits)
  #settings amounts are formatted on their own scale
  fmt_setting <- function(x) if (is_blank(x)) dash else fmt_num(x, display_digits(x))
  fmt_return_period <- function(p) paste("1 in", formatC(round(1 / (1 - p)), format = "d", big.mark = ","))
  #a count with its noun, e.g. "1 simulation" and "2 simulations"
  count_of <- function(k, noun) paste(fmt_int(k), if (isTRUE(k == 1)) noun else paste0(noun, "s"))

  known <- function(options, id) !is_blank(id) && is.character(id) && id %in% names(options)
  distr_label <- function(options, id) {
    if (known(options, id)) return(options[[id]]@distr_label)
    if (!is_blank(id)) return(gsub("_", " ", id))
    dash
  }
  param_text <- function(values, options, id) {
    values <- unlist(values)
    if (is_blank(values)) return(dash)
    #four decimals, or more for a small parameter (an Exponential rate of 1e-5, say, which
    #four would show as 0), with the trailing zeros stripped
    shown <- vapply(values, function(v) sub("\\.?0+$", "", fmt_num(v, max(4, display_digits(v)))), character(1))
    labels <- if (known(options, id)) options[[id]]@param_labels else NULL
    if (length(labels) == length(values)) {
      #in the order the app shows them (the Pareto's x_m before alpha); non-breaking spaces
      #keep each "name = value" pair on one line
      order <- param_display_positions(options[[id]])
      paste(paste(labels[order], shown[order], sep = paste0(nbsp, "=", nbsp)), collapse = ", ")
    } else {
      paste(shown, collapse = ", ")
    }
  }

  # ---------- statistics ----------
  #the figures are computed by summarise_simulation; the charts quantile the raw vectors themselves
  st <- summary$stats
  #the charts and tables ask for quantiles of the same few vectors (a million values each in a
  #large run), so each is sorted once and kept; the quantiles are those of stats::quantile()
  sorted_vectors <- list()
  quantile_of_vec <- function(x, p) {
    if (anyNA(x)) return(stats::quantile(x, p, names = FALSE))
    for (kept in sorted_vectors) if (identical(kept$x, x)) return(sorted_quantile(kept$sorted, p))
    sorted <- sort(x)
    sorted_vectors[[length(sorted_vectors) + 1]] <<- list(x = x, sorted = sorted)
    sorted_quantile(sorted, p)
  }
  claims_mean <- st$mean
  claims_sd <- st$sd
  var995 <- st$var995
  tvar995 <- st$tvar995
  cv_text <- if (!is.na(st$cv)) fmt_num(st$cv, 3) else dash

  # ---------- what the modelled totals represent ----------
  eel <- s$reinsuranceStructureEEL
  al <- s$reinsuranceStructureAL
  #layers give the losses ceded to them; exclusions leave the retained (net) losses
  role <- summary$role
  modelled_label <- summary$modelled_label
  has_gross <- !is.null(summary$gross)

  # ---------- charts ----------
  #each chart is drawn once per theme; pal holds the palette currently being drawn
  palettes <- list(
    light = list(bg = "#ffffff", blue = "#3b82f6", blue_dark = "#1d4ed8", navy = "#0f172a", red = "#dc2626",
                 grey = "#94a3b8", grid = "#e2e8f0", muted = "#64748b", label = "#334155"),
    dark = list(bg = "#111a2b", blue = "#60a5fa", blue_dark = "#93c5fd", navy = "#e2e8f0", red = "#f87171",
                grey = "#8391a7", grid = "#26324a", muted = "#94a3b8", label = "#cbd5e1")
  )
  pal <- palettes$light

  chart_par <- function(mar = c(4.2, 5.6, 1, 1)) {
    graphics::par(mar = mar, mgp = c(3.4, 0.7, 0), las = 1, col.axis = pal$muted,
                  col.lab = pal$label, fg = pal$muted, cex.axis = 0.85, cex.lab = 0.95)
  }
  axis_labels <- axis_amount_labels
  x_axis <- function(at) graphics::axis(1, at = at, labels = axis_labels(at), col = pal$grid, col.ticks = pal$grid)
  count_ticks <- sim_report_count_ticks
  amount_range <- sim_report_amount_range

  #infinite totals (draws that overflow, e.g. from a Pareto severity with a tiny alpha)
  #cannot be drawn: the histogram leaves them out and the curves stop where they start
  finite_claims <- claims[is.finite(claims)]
  n_infinite <- n - length(finite_claims)
  #totals of +Inf and of -Inf together make the figures that include both (the mean) undefined
  both_infinities <- any(claims == Inf) && any(claims == -Inf)
  infinite_note <- if (n_infinite > 0) {
    paste0(count_of(n_infinite, "simulation"), " (", fmt_prob(n_infinite / n), ") had ",
           if (n_infinite == 1) "an infinite total and is" else "infinite totals and are", " left out of the charts.")
  }
  #the reason for undefined totals depends on where they arose (see summarise_simulation):
  #claims of +Inf and -Inf in one simulation leave the gross total undefined too, while a
  #structure turns an infinite gross amount into infinity minus infinity
  undefined_text <- if (n_undefined > 0) {
    cause <- summary$undefined_cause
    claims_reason <- "claims overflowed to both +Inf and -Inf, whose sum is undefined"
    structure_reason <- paste("an infinite amount met a layer with an infinite deductible or an excluded layer",
                              "with an infinite limit, which leaves infinity minus infinity")
    reason <- if (is.null(cause) || cause[["unknown"]] > 0) {
      paste0(", for example because ", claims_reason, ", or because ", structure_reason, ".")
    } else if (cause[["structures"]] == 0) {
      paste0(": the ", claims_reason, ".")
    } else if (cause[["claims"]] == 0) {
      paste0(": ", structure_reason, ".")
    } else {
      paste0(". In ", fmt_int(cause[["claims"]]), " of them the ", claims_reason, "; in ",
             fmt_int(cause[["structures"]]), " ", structure_reason, ".")
    }
    paste0(
      fmt_int(n_undefined), " of ", count_of(n + n_undefined, "simulation"), " had ",
      if (n_undefined == 1) "an undefined (NaN) total" else "undefined (NaN) totals", reason, " ",
      if (n_undefined == 1) "It is" else "They are", " left out of every figure and chart in this report, ",
      "which describe the other ", count_of(n, "simulation"), "."
    )
  }

  #layers that are rarely hit produce many zero totals, which would flatten the histogram,
  #so it leaves them out (and only them, see sim_report_histogram_totals) and states their share
  zero_share <- st$zero_share
  histogram_totals <- sim_report_histogram_totals(finite_claims, zero_share)
  plot_claims <- histogram_totals$values
  zero_note <- if (histogram_totals$zeros_dropped) {
    paste0(" ", fmt_pct(zero_share), " of simulations had zero total claims and are left out of this chart.")
  } else {
    ""
  }

  #draw each chart once per theme into a temporary PNG at twice screen resolution and embed
  #both as data URIs; the page shows the one that matches the current theme. The device
  #arguments (R/plot_device.R) ask Windows for the cairo device, which anti-aliases
  #a chart of the report (netsimr_report_chart()), each theme's copy drawn with its palette
  chart_card <- function(draw, title, note, alt, height = 4.2, width = 9) {
    netsimr_report_chart(function(dark) {
      pal <<- palettes[[if (dark) "dark" else "light"]]
      on.exit(pal <<- palettes$light)
      draw()
    }, alt, title = title, note = note, height = height, width = width)
  }

  #a chart with nothing finite to draw shows a short message instead
  draw_no_data <- function(message) {
    chart_par()
    graphics::plot.new()
    graphics::text(0.5, 0.5, message, col = pal$muted, cex = 0.95)
  }

  draw_histogram <- function() {
    if (length(plot_claims) == 0) return(draw_no_data("Every total is infinite, so there is nothing to draw."))
    chart_par()
    x_lim <- amount_range(plot_claims)
    #every total equal: one bar a twelfth of the axis wide
    breaks <- if (diff(range(plot_claims)) == 0 && diff(x_lim) > 0) mean(x_lim) + c(-1, 1) * diff(x_lim) / 24 else 80
    h <- graphics::hist(plot_claims, breaks = breaks, plot = FALSE)
    if (length(breaks) == 1) x_lim <- range(h$breaks)
    y_top <- max(h$counts) * 1.1
    graphics::plot(h, col = NA, border = NA, main = "", xlab = modelled_label, ylab = "Simulations",
                   axes = FALSE, xlim = x_lim, ylim = c(0, y_top))
    y_at <- count_ticks(y_top)
    graphics::abline(h = y_at, col = pal$grid, lwd = 0.8)
    graphics::plot(h, col = pal$blue, border = pal$bg, add = TRUE)
    x_axis(pretty(x_lim))
    graphics::axis(2, at = y_at, labels = formatC(y_at, format = "d", big.mark = ","), lwd = 0)
    #an infinite mean or VaR has no place on the axis; the legend still gives it
    if (is.finite(claims_mean)) graphics::abline(v = claims_mean, col = pal$navy, lty = 2, lwd = 1.8)
    if (is.finite(var995)) graphics::abline(v = var995, col = pal$red, lty = 2, lwd = 1.8)
    graphics::legend("topright", bty = "n", cex = 0.85, text.col = pal$label, lty = 2, lwd = 1.8,
                     col = c(pal$navy, pal$red),
                     legend = c(paste("Mean", fmt_amount(claims_mean)), paste("VaR 99.5%", fmt_amount(var995))))
  }

  #return periods are only drawn where at least 10 simulations lie beyond them
  max_return_period <- n / 10
  #series: a named list of list(values, col, lty, lwd); the names become the legend labels.
  #ylab names what the losses are, as the other charts' axes do (e.g. "Ceded", not "Total claims")
  draw_return_periods <- function(series, ylab) {
    rps <- exp(seq(log(2), log(max_return_period), length.out = 300))
    ys <- lapply(series, function(x) quantile_of_vec(x$values, 1 - 1 / rps))
    #infinite losses are left out of the limits; the curves stop where they start
    y_range <- range(c(0, unlist(ys)), finite = TRUE)
    #totals that are all zero still need an axis
    if (y_range[1] == y_range[2]) y_range[2] <- 1
    y_at <- pretty(y_range)
    y_labels <- axis_labels(y_at)
    #widen the left margin for long axis labels so the axis title does not overlap them
    left <- max(5.6, 1.6 + 0.62 * max(nchar(y_labels)))
    chart_par(mar = c(4.2, left, 1, 1))
    graphics::plot(range(rps), y_range, type = "n", log = "x", axes = FALSE, main = "",
                   xlab = "Return period", ylab = "", ylim = y_range)
    graphics::title(ylab = ylab, line = left - 1.3)
    ticks <- return_period_axis_ticks[return_period_axis_ticks <= max_return_period]
    graphics::abline(v = ticks, col = pal$grid, lwd = 0.8)
    graphics::abline(h = y_at, col = pal$grid, lwd = 0.8)
    show_200 <- 200 <= max_return_period
    if (show_200) graphics::abline(v = 200, col = pal$red, lty = 2, lwd = 1.5)
    for (i in rev(seq_along(series))) {
      finite <- is.finite(ys[[i]])
      graphics::lines(rps[finite], ys[[i]][finite], col = pal[[series[[i]]$col]], lwd = series[[i]]$lwd, lty = series[[i]]$lty)
    }
    if (!any(is.finite(unlist(ys)))) {
      #centred on the log scale of the x axis
      graphics::text(sqrt(2 * max_return_period), mean(y_range), "The losses are infinite at every return period shown.",
                     col = pal$muted, cex = 0.95)
    }
    graphics::axis(1, at = ticks, labels = paste("1 in", formatC(ticks, format = "d", big.mark = ",")),
                   col = pal$grid, col.ticks = pal$grid, cex.axis = 0.8)
    graphics::axis(2, at = y_at, labels = y_labels, lwd = 0)
    graphics::legend("topleft", bty = "n", cex = 0.85, text.col = pal$label,
                     legend = c(names(series), if (show_200) "1 in 200 (99.5%)"),
                     col = c(vapply(series, function(x) pal[[x$col]], ""), if (show_200) pal$red),
                     lty = c(vapply(series, function(x) x$lty, 1), if (show_200) 2),
                     lwd = c(vapply(series, function(x) x$lwd, 1), if (show_200) 1.5))
  }

  draw_cdf <- function() {
    cdf_probs <- seq(0, 1, length.out = 1001)
    cdf_x <- quantile_of_vec(claims, cdf_probs)
    #with infinite totals the curve stops below 100%, at the share of finite totals
    finite <- is.finite(cdf_x)
    if (!any(finite)) return(draw_no_data("Every total is infinite, so there is nothing to draw."))
    cdf_x <- cdf_x[finite]
    cdf_probs <- cdf_probs[finite]
    x_lim <- amount_range(cdf_x)
    chart_par()
    graphics::plot(cdf_x, cdf_probs, type = "n", axes = FALSE, main = "",
                   xlab = modelled_label, ylab = "Cumulative probability", xlim = x_lim, ylim = c(0, 1))
    graphics::abline(h = seq(0, 1, 0.25), col = pal$grid, lwd = 0.8)
    if (is.finite(var995)) graphics::abline(v = var995, col = pal$red, lty = 2, lwd = 1.5)
    #the step runs from the left edge of the chart to the right one, so a single total
    #reads as a step at that total; totals of -Inf lie left of every amount drawn
    graphics::lines(c(x_lim[1], cdf_x, x_lim[2]), c(mean(claims == -Inf), cdf_probs, cdf_probs[length(cdf_probs)]),
                    type = "s", col = pal$blue, lwd = 2.4)
    x_axis(pretty(x_lim))
    graphics::axis(2, at = seq(0, 1, 0.25), labels = paste0(seq(0, 100, 25), "%"), lwd = 0)
  }

  draw_counts <- function() {
    chart_par(mar = c(4.2, 5.6, 1, 1))
    whole_counts <- round(counts)
    top <- max(1, ceiling(quantile_of_vec(whole_counts, 0.995)))
    if (top <= 60) {
      #one bar per claim count, with the rare highest counts grouped into the last bar
      grouped <- pmin(whole_counts, top)
      #(tabulate() counts a million whole numbers far faster than table(), which makes a factor)
      heights <- if (all(grouped >= 0)) {
        as.numeric(tabulate(grouped + 1, nbins = top + 1))
      } else {
        as.numeric(table(factor(grouped, levels = 0:top)))
      }
      bar_labels <- as.character(0:top)
      if (any(whole_counts > top)) bar_labels[length(bar_labels)] <- paste0(top, "+")
      y_top <- max(heights) * 1.1
      y_at <- count_ticks(y_top)
      graphics::barplot(heights, col = NA, border = NA, axes = FALSE, ylim = c(0, y_top),
                        xlab = "Claims per period", ylab = "Simulations")
      graphics::abline(h = y_at, col = pal$grid, lwd = 0.8)
      centres <- graphics::barplot(heights, col = pal$blue, border = NA, axes = FALSE,
                                   ylim = c(0, y_top), add = TRUE, axisnames = FALSE)
      #label every few bars, and always the last one so a grouped "k+" bar is named
      step <- max(1, ceiling(length(heights) / 16))
      shown <- seq(1, length(heights), by = step)
      #R drops axis labels that would overlap, so leave room before the last one
      if (length(heights) - max(shown) < step) shown <- shown[-length(shown)]
      shown <- c(shown, length(heights))
      graphics::axis(1, at = centres[shown], labels = bar_labels[shown], tick = FALSE, cex.axis = 0.8)
    } else {
      h <- graphics::hist(whole_counts, breaks = 40, plot = FALSE)
      y_top <- max(h$counts) * 1.1
      y_at <- count_ticks(y_top)
      graphics::plot(h, col = NA, border = NA, main = "", axes = FALSE, ylim = c(0, y_top),
                     xlab = "Claims per period", ylab = "Simulations")
      graphics::abline(h = y_at, col = pal$grid, lwd = 0.8)
      graphics::plot(h, col = pal$blue, border = pal$bg, add = TRUE)
      x_axis(pretty(h$breaks))
    }
    graphics::axis(2, at = y_at, labels = formatC(y_at, format = "d", big.mark = ","), lwd = 0)
  }

  # ---------- building blocks ----------
  off <- function(text = "Not applied") tags$span(class = "setting-off", text)

  report_table <- function(df, numeric_cols = integer(0), explain_cols = integer(0)) {
    cell_class <- function(j) {
      if (j %in% numeric_cols) return("num")
      if (j %in% explain_cols) return("explain")
      NULL
    }
    tags$table(
      class = "report-table",
      tags$thead(tags$tr(lapply(seq_along(df), function(j) tags$th(class = cell_class(j), names(df)[j])))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(lapply(seq_along(df), function(j) tags$td(class = cell_class(j), df[i, j])))
      }))
    )
  }

  #a section of the page (netsimr_write_report())
  report_section <- function(id, title, ...) list(id = id, title = title, content = tagList(...))

  has_deductible <- function(structure) isTRUE(structure %in% c("Unlimited Layer", "Limited Layer", "Exclude Layer"))
  has_limit <- function(structure) isTRUE(structure %in% c("Limited Layer", "Exclude Layer"))
  #an upfront premium prices the reinstatements of a limited layer with limited reinstatements
  priced_reinstatements <- isTRUE(eel == "Limited Layer") && isTRUE(s$reinsuranceStructureLimitedReinstatements) &&
    is.numeric(s$reinsurance_structure_eel_premium) && length(s$reinsurance_structure_eel_premium) == 1 &&
    isTRUE(is.finite(s$reinsurance_structure_eel_premium))
  no_structure <- function(structure) is_blank(structure) || identical(structure, "No Reinsurance Structure")

  # ---------- key results ----------
  key_intro <- switch(
    role,
    gross = "Total claims per simulated period, after any tail adjustments.",
    ceded = "Losses ceded to the reinsurance layers per simulated period.",
    net = "Retained (net) losses per simulated period, after the excluded layers are removed.",
    mixed = "Totals per simulated period after the reinsurance structures below."
  )
  key_results <- report_section(
    "key-results", "Key results",
    tags$p(class = "section-intro", key_intro),
    if (n_undefined > 0) div(class = "callout callout-warning", undefined_text),
    if (n_infinite > 0) div(class = "callout callout-warning", paste0(
      fmt_int(n_infinite), " of ", if (n_undefined > 0) "the ", count_of(n, "simulation"),
      if (n_undefined > 0) " with a defined total", " had ", if (n_infinite == 1) "an infinite total" else "infinite totals",
      if (both_infinities) paste0(" (", fmt_int(sum(claims == Inf)), " of +Inf and ", fmt_int(sum(claims == -Inf)), " of -Inf)"),
      ", so the figures that include ", if (n_infinite == 1) "it" else "them", " are infinite",
      if (both_infinities) ", or undefined (shown as a dash) where they add +Inf to -Inf, as the mean does",
      ". This happens when claim draws overflow, e.g. with ",
      if (any(claims == -Inf)) "a Normal severity with a huge standard deviation." else "a very small Pareto alpha."
    )),
    netsimr_report_tiles(list(
      Mean = list(value = fmt_amount(claims_mean),
                  note = if (n_undefined > 0) paste(fmt_int(n), "of", count_of(n + n_undefined, "simulation")) else count_of(n, "simulation")),
      Median = fmt_amount(st$median),
      `Standard deviation` = list(value = fmt_amount(claims_sd), note = if (cv_text != dash) paste("CV", cv_text)),
      `VaR 99.5%` = list(value = fmt_amount(var995), note = fmt_return_period(0.995)),
      `TVaR 99.5%` = list(value = fmt_amount(tvar995), note = "Average of the worst 0.5%"),
      Maximum = list(value = fmt_amount(st$max), note = "Largest simulated total")
    ), tail = c("VaR 99.5%", "TVaR 99.5%"))
  )

  # ---------- model settings ----------
  slice_count <- if (isTRUE(s$paretoSlice) && !is_blank(s$pareto_slice_times)) as.integer(s$pareto_slice_times) else 0L
  slice_rows <- if (slice_count > 0) {
    alphas <- unlist(s$slice_pareto_alphas)
    x_ms <- unlist(s$slice_pareto_x_ms)
    stats::setNames(
      lapply(seq_len(slice_count), function(j) {
        #the threshold first, as the app asks for it
        paste0("From ", fmt_setting(x_ms[j]), ", ", alpha, " ", fmt_num(alphas[j], 2))
      }),
      paste("Pareto slice", seq_len(slice_count))
    )
  } else {
    list("Pareto slices" = off())
  }

  model_settings <- report_section(
    "model-settings", "Model settings",
    netsimr_report_settings(list(
      Simulation = list(
        "Simulations" = fmt_int(s$numOfSimulations),
        "Seed" = if (isTRUE(s$seedSetBinary)) paste("Fixed at", fmt_int(s$seedValue)) else off("Random"),
        "Processing" = if (!isTRUE(s$multiprocessing)) {
          "Single process"
        } else if (is.numeric(s$workers) && length(s$workers) == 1) {
          paste("Parallel,", count_of(s$workers, "worker"))
        } else {
          "Parallel"
        }
      ),
      Frequency = list(
        "Distribution" = distr_label(freq_dist_options, s$freqDistr),
        "Parameters" = param_text(s$freq_params, freq_dist_options, s$freqDistr)
      ),
      Severity = list(
        "Distribution" = distr_label(sev_dist_options, s$sevDistr),
        "Parameters" = param_text(s$sev_params, sev_dist_options, s$sevDistr),
        #only the Normal distribution can be truncated at zero; older settings lists lack the field
        "Negative claims" = if (isTRUE(s$sevTruncateAtZero) && identical(s$sevDistr, "Normal")) "Truncated at zero"
      ),
      `Tail adjustments` = c(
        slice_rows,
        list("Severity cap" = if (isTRUE(s$sevCapBinary)) fmt_setting(s$sev_cap_amount) else off())
      ),
      `Each & every loss (EEL)` = list(
        "Structure" = if (no_structure(eel)) off("None") else eel,
        #the limit first, as a layer is quoted ("limit xs deductible")
        "Limit" = if (has_limit(eel)) fmt_setting(s$reinsurance_structure_eel_limit_amount),
        "Deductible" = if (has_deductible(eel)) fmt_setting(s$reinsurance_structure_eel_deductible_amount),
        "Reinstatements" = if (isTRUE(eel == "Limited Layer")) {
          if (isTRUE(s$reinsuranceStructureLimitedReinstatements)) {
            paste("Up to", fmt_int(s$reinsuranceStructureReinstatementLimit))
          } else {
            off("Unlimited")
          }
        },
        #the premium that prices them, when there is one
        "Upfront premium" = if (priced_reinstatements) fmt_setting(s$reinsurance_structure_eel_premium),
        "Reinstatement rates" = if (priced_reinstatements) {
          rates <- s$reinsurance_structure_eel_reinstatement_rates
          if (is.null(rates)) rates <- 1
          paste(paste0(signif(100 * rates, 6), "%"), collapse = ", ")
        }
      ),
      `Aggregate layer (AL)` = list(
        "Structure" = if (no_structure(al)) off("None") else al,
        "Limit" = if (has_limit(al)) fmt_setting(s$reinsurance_structure_al_limit_amount),
        "Deductible" = if (has_deductible(al)) fmt_setting(s$reinsurance_structure_al_deductible_amount)
      )
    ))
  )

  # ---------- gross, ceded and net ----------
  gross_section <- NULL
  if (has_gross) {
    columns <- summary$gross$series
    gross_table <- summary$gross$table
    #the share of the gross mean is a percentage; every other row is an amount, formatted on
    #the scale of its own column (the modelled column's is the headline style), so a rarely
    #hit layer's ceded figures keep the decimals the gross totals do not need
    column_digits <- lapply(columns, display_digits)
    format_cell <- function(metric, value, digits) {
      if (metric == "Share of gross mean") {
        if (is.na(value)) dash else fmt_pct(value)
      } else {
        fmt_num(value, digits)
      }
    }
    formatted <- lapply(names(columns), function(k) {
      vapply(seq_len(nrow(gross_table)), function(i) {
        format_cell(gross_table$metric[i], gross_table[[k]][i], column_digits[[k]])
      }, character(1))
    })
    comparison <- data.frame(
      Metric = gross_table$metric,
      stats::setNames(formatted, names(columns)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    gross_intro <- switch(
      role,
      ceded = "The structures are reinsurance layers, so the modelled totals are the ceded losses. Net is gross minus ceded.",
      net = "The structures exclude layers, so the modelled totals are the retained (net) losses. Ceded is gross minus net.",
      mixed = "The structures mix layers and exclusions, so the totals are shown as they come out of the structures, next to gross."
    )
    #when gross and the modelled total are both infinite, their difference is unknown; its
    #column is blank and its curve left out
    unknown_note <- NULL
    known <- !vapply(columns, anyNA, logical(1))
    if (isTRUE(summary$gross$unknown > 0)) {
      #the column of gross minus the modelled totals: ceded for exclusions, otherwise the third
      unknown_name <- if (role == "net") names(columns)[2] else names(columns)[3]
      unknown <- summary$gross$unknown
      unknown_note <- paste0(
        count_of(unknown, "simulation"), " had infinite gross and ", tolower(modelled_label),
        " totals, so ", if (unknown == 1) "its" else "their", " ", tolower(unknown_name),
        " amount (infinite minus infinite) is unknown and the ", unknown_name, " column is left blank."
      )
    }
    #claims of +Inf and -Inf in one simulation leave its gross total undefined (NaN)
    if (isTRUE(summary$gross$undefined > 0)) {
      blank <- names(columns)[!known]
      undefined_gross <- summary$gross$undefined
      unknown_note <- paste(unknown_note, paste0(
        count_of(undefined_gross, "simulation"), " had ",
        if (undefined_gross == 1) "an undefined (NaN) gross total" else "undefined (NaN) gross totals",
        ", as the claims overflowed to both +Inf and -Inf, so the ", paste(blank, collapse = " and "),
        if (length(blank) == 1) " column is" else " columns are", " left blank."
      ))
    }
    comparison_chart <- if (max_return_period >= 5) {
      styles <- list(
        list(col = "grey", lty = 2, lwd = 2.2),
        list(col = "blue", lty = 1, lwd = 2.6),
        list(col = "navy", lty = 1, lwd = 2.2)
      )
      comparison_series <- stats::setNames(
        lapply(seq_along(columns), function(i) c(list(values = columns[[i]]), styles[[i]])),
        names(columns)
      )[known]
      chart_card(function() draw_return_periods(comparison_series, "Loss"),
                 paste(paste(names(comparison_series), collapse = ", "), "by return period"),
                 "How the structures change the loss at each return period, on a log scale.",
                 "Gross, ceded and net losses by return period")
    }
    gross_section <- report_section(
      "gross-net", "Gross, ceded and net",
      tags$p(class = "section-intro", gross_intro),
      div(
        class = "table-card",
        report_table(comparison, numeric_cols = seq(2, ncol(comparison))),
        tags$p(class = "table-note",
               "Gross is after tail adjustments and the severity cap, before reinsurance. Each column's percentiles come from its own simulations, so net VaR is not gross VaR minus ceded VaR."),
        if (!is.null(unknown_note)) tags$p(class = "table-note", unknown_note)
      ),
      if (!is.null(comparison_chart)) div(style = "margin-top: 16px;", comparison_chart)
    )
  }

  # ---------- layer metrics ----------
  layer_section <- NULL
  if (!is.null(summary$layer)) {
    layer <- summary$layer
    metric_rows <- list(
      c("Chance the layers are hit", fmt_prob(layer$hit_prob), "Share of simulations with any ceded loss"),
      c("Average loss when hit", fmt_amount(layer$avg_loss_when_hit), "Mean ceded loss over the simulations that hit the layers"),
      c("Expected loss", fmt_amount(layer$expected_loss), "Mean ceded loss over all simulations")
    )
    #loss on line uses the aggregate limit when there is one, otherwise the each-and-every-loss limit
    if (!is.na(layer$loss_on_line)) {
      metric_rows <- c(metric_rows, list(
        c("Loss on line", fmt_pct(layer$loss_on_line, 2), paste("Expected loss as a share of", layer$line_limit_name))
      ))
    }
    #the most the layers can pay in one period
    if (is.finite(layer$capacity)) {
      metric_rows <- c(metric_rows, list(
        c("Aggregate capacity", fmt_setting(layer$capacity), "Most the layers can pay in one period"),
        c("Chance the layers are exhausted", fmt_prob(layer$exhaust_prob), "Share of simulations that use the full capacity")
      ))
    } else {
      metric_rows <- c(metric_rows, list(
        c("Aggregate capacity", "Unlimited", "No aggregate limit or reinstatement limit applies")
      ))
    }
    if (!is.na(layer$reinstatement_limit)) {
      metric_rows <- c(metric_rows, list(
        c("Average reinstatements used", fmt_num(layer$reinstatements_avg, 2), paste("Out of", fmt_int(layer$reinstatement_limit), "available")),
        c("Chance all reinstatements are used", fmt_prob(layer$reinstatements_all_used_prob), "Share of simulations that use every reinstatement")
      ))
    }
    #with an upfront premium, the reinstatements used are priced
    if (isTRUE(is.finite(layer$upfront_premium))) {
      metric_rows <- c(metric_rows, list(
        c("Upfront premium", fmt_amount(layer$upfront_premium), "The premium of the each-and-every-loss layer"),
        if (!is.na(layer$rate_on_line)) {
          c("Rate on line", fmt_pct(layer$rate_on_line, 2), "Upfront premium as a share of the each-and-every-loss limit")
        },
        c("Expected reinstatement premium", fmt_amount(layer$expected_reinstatement_premium),
          "Mean premium for the reinstatements used, pro rata to the amount reinstated"),
        c("Expected total premium", fmt_amount(layer$expected_total_premium),
          "Upfront premium plus the expected reinstatement premium"),
        if (!is.na(layer$expected_loss_ratio)) {
          c("Expected loss ratio", fmt_pct(layer$expected_loss_ratio, 1), "Expected loss as a share of the expected total premium")
        }
      ))
      metric_rows <- metric_rows[!vapply(metric_rows, is.null, logical(1))]
    }
    metrics <- as.data.frame(do.call(rbind, metric_rows), stringsAsFactors = FALSE)
    names(metrics) <- c("Metric", "Value", "What it means")
    layer_section <- report_section(
      "layer-metrics", "Layer metrics",
      tags$p(class = "section-intro", "Figures a reinsurance price is usually built on, for the layers described under Model settings."),
      div(class = "table-card", report_table(metrics, numeric_cols = 2, explain_cols = 3))
    )
  }

  # ---------- statistics, tail risk and accuracy ----------
  summary_stats <- data.frame(
    Metric = c("Simulations", "Mean", "Median", "Standard deviation", "Coefficient of variation", "Minimum", "Maximum"),
    Value = c(fmt_int(n), fmt_num(claims_mean), fmt_num(st$median), fmt_num(claims_sd),
              cv_text, fmt_num(st$min), fmt_num(st$max)),
    stringsAsFactors = FALSE
  )
  #the statistics rest on the simulations with a defined total; the others are counted apart
  if (n_undefined > 0) {
    summary_stats$Metric[1] <- "Simulations with a defined total"
    summary_stats <- rbind(summary_stats[1, ], data.frame(
      Metric = "Undefined (NaN) totals, left out", Value = fmt_int(n_undefined), stringsAsFactors = FALSE
    ), summary_stats[-1, ])
  }

  #a percentile is only listed when at least 10 simulations lie beyond it
  percentiles <- summary$percentiles
  tail_table <- data.frame(
    Percentile = fmt_pct(percentiles$prob),
    `Return period` = vapply(percentiles$prob, fmt_return_period, character(1)),
    VaR = vapply(percentiles$var, fmt_num, character(1)),
    TVaR = vapply(percentiles$tvar, fmt_num, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  #the largest single claim at the same return periods (occurrence exceedance)
  if (!is.null(largest)) {
    tail_table$`Largest claim (OEP)` <- vapply(quantile_of_vec(largest, percentiles$prob), fmt_num, character(1))
  }
  tail_note <- if (summary$percentiles_dropped) {
    tags$p(class = "table-note", paste0(
      "Return periods beyond ", fmt_return_period(max(percentiles$prob)),
      " are left out: fewer than 10 simulations would lie beyond them. More simulations extend the table."
    ))
  }

  #accuracy: standard error of the mean with a normal-approximation range, and a
  #distribution-free 95% range for VaR 99.5% from the order statistics around it
  standard_error <- st$se
  tail_count <- st$tail_count995
  accuracy <- data.frame(
    Metric = c("Standard error of the mean", "95% range for the mean", "95% range for VaR 99.5%",
               "Simulations at or beyond VaR 99.5%"),
    Value = c(
      fmt_amount(standard_error),
      if (is.na(standard_error)) dash else paste(fmt_amount(st$mean_ci[1]), "to", fmt_amount(st$mean_ci[2])),
      paste(fmt_amount(st$var995_ci[1]), "to", fmt_amount(st$var995_ci[2])),
      fmt_int(st$beyond_var995)
    ),
    stringsAsFactors = FALSE
  )
  accuracy_warning <- if (tail_count < 50) {
    div(class = "callout callout-warning", paste0(
      "TVaR 99.5% is the average of only ", fmt_int(tail_count), if (tail_count == 1) " simulation" else " simulations",
      " (the worst 0.5%), so the tail figures are uncertain. ",
      "At least 10,000 simulations are recommended for 99.5% figures."
    ))
  }

  statistics <- report_section(
    "statistics", "Statistics and tail risk",
    div(
      class = "two-col",
      div(class = "table-card", tags$h3("Summary statistics"), report_table(summary_stats, 2)),
      div(class = "table-card", tags$h3("Percentiles, VaR and TVaR"), report_table(tail_table, seq(3, ncol(tail_table))), tail_note)
    ),
    div(
      class = "table-card",
      tags$h3("Simulation accuracy"),
      report_table(accuracy, 2),
      tags$p(class = "table-note",
             "The ranges show how much the figures could move from simulation noise alone. Narrow ranges mean the run was large enough."),
      tags$p(class = "table-note", paste0(
        "The range for the mean is a normal approximation (the mean plus or minus 1.96 standard errors), ",
        "which is rough for heavy-tailed totals; when no total is negative it is kept at zero or above. ",
        "The count at or beyond VaR 99.5% includes totals equal to VaR, so ties (e.g. many zero totals) can make it large."
      ))
    ),
    accuracy_warning
  )

  # ---------- claim frequency ----------
  frequency_section <- NULL
  if (!is.null(summary$frequency)) {
    frequency <- summary$frequency
    frequency_stats <- data.frame(
      Metric = c("Average claims per period", "Standard deviation", "Chance of no claims", "99th percentile", "Maximum"),
      Value = c(
        fmt_num(frequency$mean, 2),
        fmt_num(frequency$sd, 2),
        fmt_prob(frequency$p_zero),
        fmt_int(frequency$p99),
        fmt_int(frequency$max)
      ),
      stringsAsFactors = FALSE
    )
    frequency_section <- report_section(
      "frequency", "Claim frequency",
      tags$p(class = "section-intro", "Simulated number of claims per period, before severity and reinsurance."),
      div(
        class = "two-col",
        div(class = "table-card", report_table(frequency_stats, 2)),
        chart_card(draw_counts, "Claims per period", NULL, "Bar chart of claims per period", height = 3.6, width = 6)
      )
    )
  }

  # ---------- charts ----------
  return_period_card <- if (max_return_period >= 5) {
    #colours are palette names, so each theme's copy of the chart uses its own colours
    modelled_series <- stats::setNames(list(list(values = claims, col = "blue", lty = 1, lwd = 2.6)), modelled_label)
    #with the largest claims, the period total (aggregate exceedance, AEP) and the largest single
    #claim (occurrence exceedance, OEP)
    if (!is.null(largest)) {
      modelled_series <- list(
        "Period total (AEP)" = list(values = claims, col = "blue", lty = 1, lwd = 2.6),
        "Largest single claim (OEP)" = list(values = largest, col = "navy", lty = 2, lwd = 2)
      )
    }
    chart_card(function() draw_return_periods(modelled_series, modelled_label), "Losses by return period",
               paste0(
                 "The loss expected once in each number of periods, on a log scale.",
                 " Return periods are shown up to ", fmt_return_period(1 - 1 / max_return_period), ".",
                 if (!is.null(largest)) paste(
                   " The largest single claim is after the each-and-every-loss structure, before the",
                   "reinstatement limit and the aggregate layer, which act on the period's total."
                 )
               ),
               "Losses by return period")
  }

  charts <- report_section(
    "charts", "Charts",
    if (n_undefined > 0) div(class = "callout callout-warning", paste0(
      count_of(n_undefined, "simulation"), " had ",
      if (n_undefined == 1) "an undefined (NaN) total and is" else "undefined (NaN) totals and are",
      " left out of the charts."
    )),
    if (!is.null(infinite_note)) div(class = "callout callout-warning", infinite_note),
    chart_card(draw_histogram, "Distribution of total claims",
               paste0("Dashed lines mark the mean and the 99.5% VaR of all simulations.", zero_note),
               "Histogram of total claims"),
    return_period_card,
    chart_card(draw_cdf, "Cumulative distribution", "Share of simulations with total claims at or below each amount.",
               "Cumulative distribution of total claims")
  )

  # ---------- reading guide ----------
  reading_guide <- report_section(
    "how-to-read", "How to read this report",
    div(class = "callout", tags$ul(
      tags$li("A large gap between the median and the tail percentiles points to a heavy-tailed outcome."),
      tags$li(paste0(
        "VaR is the loss exceeded only with the stated probability. TVaR is the average loss in those worst cases, ",
        "so it is always at least as large as VaR: TVaR 99.5% is the mean of the worst 0.5% of the simulations, ",
        "rounded up to a whole number (here ", fmt_int(st$tail_count995), ")."
      )),
      tags$li("The return period shows the same probability as a frequency: 99.5% corresponds to a 1 in 200 year event."),
      if (has_gross) tags$li("Gross is before reinsurance. Ceded is what the layers pay, and net is what remains."),
      if (!no_structure(eel) && !no_structure(al)) {
        tags$li("The aggregate layer works on each period's total after the each-and-every-loss structure: the aggregate deductible comes off first, then the aggregate limit and the reinstatement capacity cap what is left.")
      },
      tags$li("The accuracy ranges reflect simulation noise only, not uncertainty in the chosen distributions or parameters."),
      tags$li("Figures reflect the tail adjustments and reinsurance structures listed under Model settings.")
    ))
  )

  # ---------- page ----------
  netsimr_write_report(file, "Simulation Report", "NetSimulator by NetSimR",
                       list(key_results, model_settings, gross_section, layer_section, statistics, frequency_section,
                            charts, reading_guide),
                       generated = generated)
}
