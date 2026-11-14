#' Run the reinsurance pricing application
#'
#' @description Opens NetTreaty, a shiny application for pricing an excess of loss
#'   layer: it rates the layer on its loss experience, with the burning cost and a
#'   Pareto model of the large losses, and on its exposure, with a risk profile and
#'   an MBBEFD exposure curve (property) or a claim size distribution (casualty),
#'   blends the two with a credibility weight, and gives the technical premium,
#'   with reinstatements, rate on line and payback.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{burning_cost}}, \code{\link{pareto_rating}},
#'   \code{\link{property_exposure_rating}}, \code{\link{casualty_exposure_rating}}
#'   and \code{\link{technical_premium}}, the methods of the application;
#'   \code{\link{run_shiny_simulator}}, \code{\link{run_shiny_distribution_fitting_tool}},
#'   \code{\link{run_shiny_glm_fitting_tool}} and \code{\link{run_shiny_reserving_tool}},
#'   the package's other applications.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_reinsurance_pricing_tool()
#' }
run_shiny_reinsurance_pricing_tool <- function() {
  shinyApp(ui = reinsurance_pricing_tool_UI, server = reinsurance_pricing_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the reinsurance pricing tool.

# The example risk profiles, by the label the Exposure tab shows.
xlp_profile_examples <- c(
  "Property: bands of sum insured" = "property",
  "Casualty: premium by policy limit" = "casualty"
)

# The exposure curves of the property rating, by label.
xlp_curves <- c("Swiss Re Y1 (c = 1.5)" = "Y1", "Swiss Re Y2 (c = 2)" = "Y2", "Swiss Re Y3 (c = 3)" = "Y3",
                "Swiss Re Y4 (c = 4)" = "Y4", "Lloyd's (c = 5)" = "Lloyds", "Swiss Re, any c" = "c",
                "MBBEFD, b and g" = "bg")

# The claim size distributions of the casualty rating, with the labels and defaults
# of their parameters.
xlp_severities <- list(
  LogNormal = list(label = "LogNormal", params = c("meanlog", "sdlog"), defaults = c(10, 2)),
  Gamma = list(label = "Gamma", params = c("shape", "scale"), defaults = c(0.5, 200000)),
  Exponential = list(label = "Exponential", params = "rate", defaults = 1e-5),
  Pareto = list(label = "Pareto", params = c("alpha", "x_m (minimum)"), defaults = c(1.5, 100000))
)

# The colours of the methods in the Pricing tab's chart.
xlp_method_colours <- c("Burning cost" = "#3b82f6", "Pareto" = "#10b981", "Exposure" = "#8b5cf6", "Blended" = "#e11d48")

# The colours of the pricing charts, for the light or dark theme.
xlp_chart_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(accent = "#fb7185", accent_soft = "#fb71858c", layer = "#fb718533", muted = "#94a3b8")
  } else {
    list(accent = "#e11d48", accent_soft = "#e11d4866", layer = "#e11d481f", muted = "#64748b")
  }
}

# A layer as "4,000,000 xs 1,000,000", an unlimited one as "Unlimited xs ...".
xlp_layer_label <- function(deductible, limit) {
  paste(if (is.finite(limit)) netsimr_amount(limit) else "Unlimited", "xs", netsimr_amount(deductible))
}

# The losses and their years from an uploaded table, as numbers.
xlp_upload_losses <- function(df, year, amount, dec = ".") {
  has <- function(column) !is.null(column) && nzchar(column) && column %in% names(df)
  if (!has(year) || !has(amount)) stop("Choose the year and amount columns.", call. = FALSE)
  if (identical(year, amount)) stop("Choose different columns for the year and the amount.", call. = FALSE)
  years <- netsimr_as_numeric(df[[year]], dec)
  amounts <- netsimr_as_numeric(df[[amount]], dec)
  keep <- !is.na(years) | !is.na(amounts)
  years <- years[keep]
  amounts <- amounts[keep]
  if (length(years) == 0) stop("The file has no losses.", call. = FALSE)
  if (anyNA(years) || any(years != round(years))) stop("Every loss needs a year, a whole number.", call. = FALSE)
  if (anyNA(amounts)) stop("Every loss needs an amount.", call. = FALSE)
  if (any(amounts < 0)) stop("The loss amounts must not be negative.", call. = FALSE)
  data.frame(year = years, amount = amounts)
}

# The risk profile from an uploaded table: the band label (optional), the sum insured
# or policy limit, the premium and the loss ratio (optional) of each row.
xlp_upload_profile <- function(df, type, size, premium, loss_ratio = NULL, band = NULL, dec = ".") {
  has <- function(column) !is.null(column) && nzchar(column) && !identical(column, "none") && column %in% names(df)
  size_label <- if (identical(type, "casualty")) "policy limit" else "sum insured"
  if (!has(size) || !has(premium)) stop("Choose the ", size_label, " and premium columns.", call. = FALSE)
  chosen <- c(size, premium, if (has(loss_ratio)) loss_ratio, if (has(band)) band)
  if (anyDuplicated(chosen)) stop("Choose a different column for each of the profile's values.", call. = FALSE)
  sizes <- netsimr_as_numeric(df[[size]], dec)
  premiums <- netsimr_as_numeric(df[[premium]], dec)
  keep <- !is.na(sizes) | !is.na(premiums)
  if (!any(keep)) stop("The profile has no rows.", call. = FALSE)
  if (anyNA(sizes[keep]) || any(sizes[keep] <= 0)) stop("Every row needs a positive ", size_label, ".", call. = FALSE)
  if (anyNA(premiums[keep]) || any(premiums[keep] < 0)) stop("Every row needs a premium of at least 0.", call. = FALSE)
  out <- data.frame(band = if (has(band)) as.character(df[[band]])[keep] else netsimr_amount(sizes[keep]), stringsAsFactors = FALSE)
  out[[if (identical(type, "casualty")) "policy_limit" else "sum_insured"]] <- sizes[keep]
  out$premium <- premiums[keep]
  if (has(loss_ratio)) {
    ratios <- netsimr_as_numeric(df[[loss_ratio]], dec)[keep]
    if (anyNA(ratios) || any(ratios < 0)) stop("Every row needs a loss ratio of at least 0, as a share such as 0.65.", call. = FALSE)
    out$loss_ratio <- ratios
  }
  out
}

# The example risk profile of a type, with a band label and a description.
xlp_example_profile <- function(type) {
  if (identical(type, "casualty")) {
    profile <- NetSimR::xl_casualty_profile
    profile <- data.frame(band = netsimr_amount(profile$policy_limit), profile, stringsAsFactors = FALSE)
    return(list(profile = profile, type = "casualty",
                description = "A simulated liability portfolio: the premium written at each policy limit."))
  }
  profile <- NetSimR::xl_property_profile
  list(profile = profile[, c("band", "sum_insured", "premium")], type = "property", data = profile,
       description = "A simulated property portfolio: bands of sum insured with their risks, average sum insured and premium.")
}

# The indexed losses of each year against the layer: a band shaded between the
# deductible and the exhaustion point, the losses as markers (those in the layer
# filled).
xlp_losses_chart <- function(detail, years, deductible, limit, dark = FALSE) {
  colours <- xlp_chart_colours(dark)
  plot_colours <- netsimr_plot_colours(dark)
  top <- deductible + limit
  values <- detail$indexed
  ylim <- netsimr_zero_range(c(values, deductible, if (is.finite(top)) top))
  legend <- list(list(label = "Loss in the layer", colour = colours$accent, type = "marker", pch = 19),
                 list(label = "Below the layer", colour = plot_colours$muted, type = "marker", pch = 1),
                 list(label = "Layer", colour = colours$layer, type = "bar"))
  netsimr_plot_frame(c(0.4, length(years) + 0.6), ylim, "Year", "Loss in the money of the rating year", dark,
                 legend = legend, x_labels = years)
  graphics::rect(0, deductible, length(years) + 1, min(top, ylim[2]), col = colours$layer, border = NA)
  graphics::abline(h = deductible, col = colours$accent, lty = 2, lwd = 1.2)
  if (is.finite(top) && top <= ylim[2]) graphics::abline(h = top, col = colours$accent, lty = 2, lwd = 1.2)
  x <- match(detail$year, years)
  # losses of the same year spread out a little, so they do not hide each other
  offset <- stats::ave(seq_along(x), x, FUN = function(i) (seq_along(i) - (length(i) + 1) / 2) * min(0.5 / length(i), 0.08))
  inside <- values > deductible
  graphics::points(x[!inside] + offset[!inside], values[!inside], pch = 1, col = plot_colours$muted, cex = 0.9, lwd = 1.5)
  graphics::points(x[inside] + offset[inside], values[inside], pch = 19, col = colours$accent, cex = 0.95)
  invisible(NULL)
}

# The share of the large losses above each size, observed and fitted by the Pareto.
xlp_pareto_chart <- function(losses, threshold, alpha, dark = FALSE) {
  x <- sort(losses)
  n <- length(x)
  grid <- exp(seq(log(threshold), log(max(x) * 1.5), length.out = 200))
  series <- list(
    list(name = "Losses (share above)", x = c(threshold, x), y = c(1, (n - seq_len(n)) / n), colour = netsimr_palette[1], step = TRUE),
    list(name = paste0("Pareto, alpha ", formatC(alpha, format = "f", digits = 2)), x = grid, y = (threshold / grid)^alpha,
         colour = xlp_chart_colours(dark)$accent)
  )
  netsimr_line_chart(series, "Loss in the money of the rating year", "Share of large losses above", dark, x_log = TRUE,
                 ylim = c(0, 1.02))
}

# The exposure curve of the property rating: the share of the expected loss below a
# deductible of each share of the sum insured, against total losses only.
xlp_property_curve_chart <- function(curve, dark = FALSE) {
  x <- seq(0, 1, length.out = 201)
  series <- list(
    list(name = curve$label, x = x, y = mbbefd_curve(x, curve$b, curve$g), colour = xlp_chart_colours(dark)$accent),
    list(name = "Total losses only", x = x, y = x, colour = "muted", lwd = 1.5)
  )
  netsimr_line_chart(series, "Deductible as a share of the sum insured", "Share of the expected loss below it", dark,
                 ylim = c(0, 1.02))
}

# The limited expected value of the casualty claim size distribution, with the layer.
xlp_casualty_curve_chart <- function(lev, label, limits, deductible, limit, dark = FALSE) {
  top <- max(limits, if (is.finite(limit)) deductible + limit else deductible)
  x <- seq(0, top, length.out = 201)
  series <- list(list(name = label, x = x, y = lev(x), colour = xlp_chart_colours(dark)$accent))
  netsimr_line_chart(series, "Claim size cap", "Limited expected value", dark,
                 vlines = c(deductible, if (is.finite(limit)) deductible + limit))
}

# The claim count and claim size distributions of the Programmes tab's gross portfolio, by
# the name compare_reinsurance() takes: each with its id in the simulator (and in the
# simulator's settings files, which NetDisFit's Export tab writes), the labels of its
# parameters, in the order of simulate_claims(), and their defaults (a mean claim of about
# 185,000 and about 200 claims a year, as the LogNormal and the Poisson the tab starts with).
xlp_prog_frequencies <- list(
  Poisson = list(id = "Poisson", params = "Mean (lambda)", defaults = 200),
  `Negative Binomial` = list(id = "Negative_Binomial", params = c("r (shape)", "beta (scale)"), defaults = c(100, 2)),
  `Zero-inflated Poisson` = list(id = "Zero_Inflated_Poisson", params = c("Mean (lambda)", "Extra zeros (probability)"),
                                 defaults = c(220, 0.1)),
  `Zero-inflated Negative Binomial` = list(id = "Zero_Inflated_Negative_Binomial",
                                           params = c("r (shape)", "beta (scale)", "Extra zeros (probability)"),
                                           defaults = c(110, 2, 0.1))
)
xlp_prog_severities <- list(
  LogNormal = list(id = "LogNormal", params = c("meanlog", "sdlog"), defaults = c(11, 1.5)),
  Gamma = list(id = "Gamma", params = c("shape", "scale"), defaults = c(0.5, 4e5)),
  Pareto = list(id = "Pareto", params = c("alpha", "x_m (minimum)"), defaults = c(2, 1e5)),
  Exponential = list(id = "Exponential", params = "rate", defaults = 5e-6),
  Weibull = list(id = "Weibull", params = c("shape", "scale"), defaults = c(0.7, 1.5e5)),
  Burr = list(id = "Burr", params = c("alpha", "gamma", "scale"), defaults = c(2, 1.2, 2e5)),
  `Generalised Pareto` = list(id = "Generalised_Pareto", params = c("location", "scale", "shape"), defaults = c(0, 1.2e5, 0.3)),
  Normal = list(id = "Normal", params = c("mean", "sd"), defaults = c(2e5, 5e4))
)

# The gross portfolio of the Programmes tab from the values of a simulator settings file,
# such as the one NetDisFit's Export tab saves: the `frequency` and the `severity` (each a
# `name` of the lists above and its `params`; NULL when the file leaves it out or the tab
# does not simulate it), the Pareto `slices` (a data frame of thresholds and alphas, or
# NULL) and `notes` on what was left out.
xlp_fit_from_settings <- function(v) {
  number <- function(id) {
    x <- v[[id]]
    if (is.numeric(x) && length(x) == 1 && is.finite(x)) as.numeric(x) else NA_real_
  }
  notes <- character(0)
  pick <- function(id, models, options, what) {
    if (!(is.character(id) && length(id) == 1 && !is.na(id))) return(NULL)
    at <- match(id, vapply(models, function(m) m$id, character(1)))
    if (is.na(at)) {
      notes <<- c(notes, paste0("this tab has no ", gsub("_", " ", id), " ", what))
      return(NULL)
    }
    params <- vapply(options[[id]]@paramIDs, number, numeric(1))
    if (anyNA(params)) {
      notes <<- c(notes, paste0("the file does not give every parameter of the ", names(models)[at], " ", what))
      return(NULL)
    }
    list(name = names(models)[at], params = unname(params))
  }
  frequency <- pick(v$freqDistr, xlp_prog_frequencies, freq_dist_options, "claim count")
  severity <- pick(v$sevDistr, xlp_prog_severities, sev_dist_options, "claim size")
  # the slices belong to the severity: without it they are left out
  slices <- NULL
  k <- number("pareto_slice_times")
  if (!is.null(severity) && is.finite(k) && k >= 1) {
    j <- seq_len(min(k, max_number_of_pareto_slices))
    # (the alpha in the odd fields, the threshold in the even ones)
    slices <- data.frame(threshold = vapply(paste0("slice_pareto_param_", 2 * j), number, numeric(1)),
                         alpha = vapply(paste0("slice_pareto_param_", 2 * j - 1), number, numeric(1)), row.names = NULL)
    usable <- !anyNA(slices) && all(slices$threshold > 0) && all(slices$alpha > 0) && all(diff(slices$threshold) > 0)
    if (!usable) {
      notes <- c(notes, "the Pareto slices of the file are not usable")
      slices <- NULL
    }
  }
  if (!is.null(severity) && isTRUE(v$sevCapBinary)) notes <- c(notes, "the severity cap of the file is not applied")
  list(frequency = frequency, severity = severity, slices = slices, notes = notes)
}

# The settings file of the reinsurance pricing tool (see write_settings_file()): the
# tool, the version and the inputs it keeps with their kind. "column" inputs refer to
# columns of an uploaded file and are applied once a file with that column is read;
# each year's premium, rate index, development and inclusion are kept by year.
xlp_settings_tool <- "Reinsurance pricing tool"
xlp_settings_version <- 1L
xlp_settings_inputs <- c(
  deductible = "numeric", limit = "numeric", reinstatements = "numeric", reinstatement_rate = "numeric",
  aggregate_deductible = "numeric", rating_year = "numeric", rating_premium = "numeric", loading = "numeric",
  loss_source = "radio", loss_header = "switch", loss_sep = "radio", loss_dec = "radio",
  loss_year_col = "column", loss_amount_col = "column", first_year = "numeric", last_year = "numeric",
  inflation = "numeric", threshold = "numeric", weights = "radio", pareto_threshold = "numeric", pareto_alpha = "numeric",
  profile_source = "radio", profile_example = "select", profile_type = "radio", profile_header = "switch",
  profile_sep = "radio", profile_dec = "radio", band_col = "column", size_col = "column", premium_col = "column",
  loss_ratio_col = "column", loss_ratio = "numeric", curve = "select", curve_c = "numeric", curve_b = "numeric",
  curve_g = "numeric", severity = "select", severity_p1 = "numeric", severity_p2 = "numeric",
  experience_method = "radio", experience_weight = "slider", full_credibility = "numeric",
  prog_frequency = "select", prog_severity = "select", prog_f1 = "numeric", prog_f2 = "numeric", prog_f3 = "numeric",
  prog_s1 = "numeric", prog_s2 = "numeric", prog_s3 = "numeric", prog_n = "numeric", prog_level = "select",
  prog_loading = "numeric", prog_coc = "numeric", prog_seed = "numeric"
)
