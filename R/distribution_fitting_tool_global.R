#' Run the distribution fitting application
#'
#' @description Opens NetDisFit, a shiny application that fits frequency and
#'   severity distributions to an uploaded claims file, compares them with the
#'   data, and splices Pareto tails onto a LogNormal body or fits a piecewise
#'   Pareto at chosen slicing points, ready for the NetSimR simulator.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{run_shiny_simulator}} and
#'   \code{\link{run_shiny_glm_fitting_tool}}, two of the package's other applications;
#'   \code{\link{SlicedLNormParetoMean}}, \code{\link{SlicedLNormParetoCappedMean}},
#'   \code{\link{SlicedGammaParetoMean}} and \code{\link{SlicedGammaParetoCappedMean}}
#'   for the moments of the sliced distributions the tool fits.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_distribution_fitting_tool()
#' }
run_shiny_distribution_fitting_tool <- function(){
  shinyApp(ui = distribution_fitting_tool_UI, server = distribution_fitting_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the distribution fitting tool: its examples and settings, and the data of
# its tables and charts. The fits are in R/distribution_fitting.R and R/heavy_tails.R; the
# charts, tables and data reading it shares with the other tools are in R/tool_charts.R,
# R/tool_tables.R and R/tool_data.R.

# The example data sets of the Data tab: motor policies with claim counts and exposure
# (for the Frequency tab) and liability claims at their policy limits (for the Severity tab).
dft_examples <- c("Motor policies: claim counts and exposure" = "motor",
                  "Liability claims: amounts and policy limits" = "liability")

# The settings file of the tool: the inputs it keeps, each with its kind (how it is restored).
# A column is chosen once data with that column is read; the slicing points and the piecewise
# thresholds are kept too, and set when their analysis runs.
dft_settings_tool <- "Distribution fitting tool"
dft_settings_version <- 1L
dft_settings_inputs <- c(
  data_source = "radio", example = "select", data_includes_header = "switch", sep = "radio", dec = "radio",
  quote = "radio",
  counts_var = "column", counts_weighted_var = "switch", counts_weights_var = "column", count_hist_bins = "slider",
  FreqDistri = "radio",
  severity_var = "column", sev_threshold = "numeric", sev_limit_source = "radio", sev_limit = "numeric",
  sev_limit_col = "column", sev_fit_log_scale = "switch", severity_hist_bins = "slider",
  sliced_sev_var = "column", sev_cens_fit_log_scale = "switch",
  piecewise_pareto_var = "column", num_pareto_slices = "slider", piecewise_pareto_fit_log_scale = "switch",
  export_frequency = "select", export_severity = "select"
)

# Rounds a slider bound to three significant digits, down or up, so that the
# bound still includes the data.
dft_nice_bound <- function(x, up = FALSE) {
  if (!is.finite(x) || x == 0) return(x)
  step <- 10^(floor(log10(abs(x))) - 2)
  if (up) ceiling(x / step) * step else floor(x / step) * step
}

# The top of a slider that places a slicing point or threshold among the
# claims: the largest claim once the top half a percent are left out, rounded
# up, rather than the largest claim itself. Claims are heavy tailed, and with
# the largest claim as the top the body of the distribution took less than a
# pixel of the bar (3,000 claims around 1,000 and one of 5,000,000: the first
# user click snapped to 380,000 or to the bottom). Leaving out whole claims
# rather than taking the 99.5th percentile keeps the largest claim as the top
# of fewer than two hundred claims, where half a percent is not a claim at
# all; a point above the top can still be typed.
dft_slider_top <- function(x) {
  sorted <- sort(x)
  if (!length(sorted)) return(NA_real_)
  dft_nice_bound(sorted[length(sorted) - floor(length(sorted) * 0.005)], up = TRUE)
}

# Parameter cell: a muted parameter name followed by its value.
dft_param_cell <- function(name, value, digits = 4) {
  if (is.null(name) || identical(name, "")) return(tags$span(class = "dft-na", netsimr_dash))
  tagList(tags$span(class = "dft-param-name", name), netsimr_fmt(value, digits))
}

# Grid of claim sizes for drawing fitted cdfs: evenly spaced from zero, or on
# a log scale from the smallest claim, up to the largest claim.
dft_severity_grid <- function(claims, log_scale = FALSE, n = 500) {
  top <- max(claims)
  if (log_scale) {
    bottom <- min(claims[claims > 0])
    if (bottom >= top) return(top)
    exp(seq(log(bottom), log(top), length.out = n))
  } else {
    seq(0, top, length.out = n)
  }
}

# Points at which to draw the empirical cdf as a step line: every distinct
# claim, or evenly spaced quantiles when there are more than max_points.
dft_ecdf_points <- function(claims, max_points = 2000) {
  points <- sort(unique(claims))
  if (length(points) > max_points) {
    points <- unique(stats::quantile(claims, seq(0, 1, length.out = max_points), type = 1, names = FALSE))
  }
  points
}

# Piecewise Pareto helpers. They cover the case the tool uses (no truncation,
# no reporting thresholds, no censoring, unit weights) with the same arithmetic
# as Pareto::PiecewisePareto_ML_Estimator_Alpha() and Pareto::pPiecewisePareto(),
# so the Pareto package is not needed, except that a loss equal to the first
# threshold counts in the first layer.

# Maximum likelihood fits of the Frequency tab, with the fields of
# fitdistrplus::fitdist() that the tool uses: estimate, sd, cor, loglik, aic,
# bic and n (the number of rows, as fitdist() counts it for the BIC). Weights
# are case weights, as in fitdist(): a row of weight 3 counts as three
# observations, so the log-likelihood is the weighted sum of the log densities
# and the standard errors come from the information of the total weight.

# Zero-inflated counts: zero with probability `zero`, and otherwise a count of
# the base distribution (which gives zeros of its own). The fits return the
# fields of the two above, with `inflated`: FALSE when the data has no more zeros
# than the base distribution fitted to it gives, so that the best zero
# probability is 0 and the fit is the base distribution's.
#
# The likelihood splits in two: whether a row has claims, with probability
# (1 - zero) * (1 - p0), p0 the base distribution's probability of no claim; and
# the number of claims of a row with claims, which follows the base distribution
# truncated at zero whatever `zero` is. So the base parameters are fitted to the
# rows with claims alone, and `zero` then sets the probability of a row with
# claims to their share of the rows: zero = 1 - share / (1 - p0). When that is
# not above 0 the maximum is on the boundary zero = 0. The standard errors come
# from the observed information of the whole likelihood, by differences.

# Data of the charts. Each function returns what its chart draws, so that it
# can be checked without drawing; `cdfs` is a named list of the fitted cdfs.

# Frequency histogram: the share of the observations (of the weight, with
# weights) in about `bins` bins of consecutive counts, and the probability of
# each bin under each fitted model. The last bin ends at the largest count.
# When the smallest count is above zero, a first bin holds the counts below
# it: the data has none there, but a fitted model may put some of its
# probability there (zero-truncated counts), which is the lack of fit the
# chart is there to show.
dft_count_hist_data <- function(counts, weights = NULL, bins = 20, cdfs = list()) {
  w <- if (is.null(weights)) rep(1, length(counts)) else weights
  lowest <- min(counts)
  width <- max(1, ceiling((max(counts) - lowest + 1) / max(1, bins)))
  starts <- seq(lowest, max(counts), by = width)
  ends <- starts + width - 1
  ends[length(ends)] <- max(counts)
  bin <- factor((counts - lowest) %/% width + 1, levels = seq_along(starts))
  observed <- as.numeric(tapply(w, bin, sum))
  observed[is.na(observed)] <- 0
  if (lowest > 0) {
    starts <- c(0, starts)
    ends <- c(lowest - 1, ends)
    observed <- c(0, observed)
  }
  # (format = "d" gives "NA" from 2^31 up)
  label <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  labels <- ifelse(starts == ends, label(starts), paste0(label(starts), "-", label(ends)))
  list(labels = labels, observed = observed / sum(w),
       fitted = lapply(cdfs, function(cdf) cdf(ends) - cdf(starts - 1)))
}

# Frequency cdf chart: the (weighted) empirical cdf and the fitted cdfs at
# every count from 0 to the largest, or at 5,000 of them.
dft_count_cdf_data <- function(counts, weights = NULL, cdfs = list()) {
  top <- max(counts)
  points <- if (top <= 5000) seq(0, top) else unique(round(seq(0, top, length.out = 5000)))
  list(x = points, empirical = empirical_cdf_at(points, counts, weights),
       fitted = lapply(cdfs, function(cdf) cdf(points)))
}

# Claim size cdf chart: the empirical cdf at its steps and the fitted cdfs on a
# grid (evenly or log spaced) that includes the extra points, the slicing
# points or thresholds where spliced cdfs change shape.
dft_severity_cdf_data <- function(claims, cdfs = list(), log_scale = FALSE, extra_points = NULL) {
  points <- dft_ecdf_points(claims)
  grid <- sort(unique(c(dft_severity_grid(claims, log_scale), extra_points)))
  list(x = points, empirical = empirical_cdf_at(points, claims), grid = grid,
       fitted = lapply(cdfs, function(cdf) cdf(grid)))
}

# Claim size histogram: about `bins` bins with round breaks, as hist() makes them.
dft_severity_hist_data <- function(claims, bins = 20) {
  h <- graphics::hist(claims, breaks = max(1, bins), plot = FALSE)
  list(breaks = h$breaks, counts = h$counts)
}

# Mean excess chart: the empirical mean excess at the steps of the empirical
# cdf, leaving out the largest claim, which no claim exceeds.
dft_mean_excess_data <- function(claims) {
  points <- dft_ecdf_points(claims)
  mean_excess <- mean_excess_at(points, claims)
  defined <- is.finite(mean_excess)
  list(x = points[defined], y = mean_excess[defined])
}

# Series of a cdf chart for netsimr_line_chart(): the empirical cdf as a step
# line, then each fitted cdf in its colour; step draws the fitted cdfs as
# steps too (counts).
dft_cdf_series <- function(data, colours = netsimr_palette[seq_along(data$fitted)], step = FALSE) {
  x <- if (is.null(data$grid)) data$x else data$grid
  fitted <- Map(function(name, y, colour) list(name = name, x = x, y = y, colour = colour, step = step),
                names(data$fitted), data$fitted, colours)
  c(list(list(name = "Empirical", x = data$x, y = data$empirical, colour = "empirical", step = TRUE, lwd = 2.5)),
    unname(fitted))
}

# ---------------------------------------------------------------- To the simulator

# The models the tool can send to the simulator, as the "To the simulator" tab
# offers them: the label shown, and the tab whose fit each one uses.
dft_export_frequency_models <- c("Poisson" = "frequency", "Negative Binomial" = "frequency",
                                 "Zero-inflated Poisson" = "frequency",
                                 "Zero-inflated Negative Binomial" = "frequency")
dft_export_severity_models <- c(
  "Normal" = "severity", "LogNormal" = "severity", "Exponential" = "severity",
  "Gamma" = "severity", "Pareto" = "severity", "Weibull" = "severity", "Burr" = "severity",
  "Generalised Pareto" = "severity",
  "LogNormal - Pareto" = "sliced", "LogNormal - Pareto - Pareto" = "sliced",
  "Piecewise Pareto" = "piecewise"
)

# The settings that put a fitted model into the claims simulator, as a settings
# file of the simulator holds them (sim_settings_input_ids()), so that the file
# loads with its "Load settings" button.
#
# `frequency` is NULL (the simulator's frequency is left as it is) or a list with
# `model` ("Poisson" or "Negative Binomial", or either after "Zero-inflated ")
# and its fitted parameters: `lambda`, or `size` and `mu` as fitdistrplus and
# the Frequency tab give them; the simulator's Negative Binomial is a
# Poisson-Gamma mixture with shape r and scale beta, so r = size and beta = mu /
# size. A zero-inflated model also has `zero`, its probability of an extra zero.
#
# `severity` is NULL or a list with `model` ("Normal", "LogNormal",
# "Exponential", "Gamma", "Pareto", "Weibull", "Burr" or "Generalised Pareto"),
# its parameters (`mean` and `sd`; `mu` and `sigma`; `mean`; `shape` and `scale`;
# `xm` and `alpha`; `shape` and `scale`; `alpha`, `gamma` and `scale`;
# `location`, `scale` and `shape`) and optionally
# `slices`, a data frame of increasing thresholds `x_m` and their `alpha`: above
# each threshold the claims follow a Pareto with that alpha, which is the
# simulator's splice (pareto_splice()) and the tool's sliced and piecewise
# models alike. A severity without slices sets the number of slices to 0.
#
# `frequency_share` is the share of all claims that the counts count: for a
# severity fitted to claims reported above a threshold, the counts are of those
# claims, a share S(d) of all claims, while the simulator draws all claims from
# its severity. Each claim being reported with probability p, a Poisson(lambda)
# count of all claims gives a Poisson(p lambda) count of reported ones, and a
# Negative Binomial(r, beta) one a Negative Binomial(r, p beta); so lambda and
# beta are divided by the share. A period of extra zeros has no claims of either
# kind, so the probability of an extra zero stays as it is.
#
# Every parameter must be a positive, finite number (a Normal mean, a LogNormal
# mu and a Generalised Pareto shape any finite number, and a Generalised Pareto
# location zero or more); otherwise it stops with a message naming the parameter.
dft_simulator_settings <- function(frequency = NULL, severity = NULL, frequency_share = 1) {
  if (!(is.numeric(frequency_share) && length(frequency_share) == 1 && is.finite(frequency_share) &&
        frequency_share > 0 && frequency_share <= 1)) {
    stop("The share of claims that the counts count must be above 0 and at most 1.", call. = FALSE)
  }
  values <- list()
  check <- function(x, what, positive = TRUE) {
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || (positive && x <= 0)) {
      stop(what, " is ", if (is.numeric(x) && length(x) == 1 && is.finite(x)) netsimr_fmt(x) else "not a finite number",
           ", which the simulator does not accept", if (positive) "; it needs a positive number" else "", ".", call. = FALSE)
    }
    unname(x)
  }
  check_zero <- function(x) {
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0 || x >= 1) {
      stop("The probability of an extra zero must be at least 0 and below 1.", call. = FALSE)
    }
    unname(x)
  }
  if (!is.null(frequency)) {
    values <- switch(
      frequency$model,
      "Poisson" = list(freqDistr = "Poisson", lamda = check(frequency$lambda, "The Poisson lambda") / frequency_share),
      "Negative Binomial" = {
        size <- check(frequency$size, "The Negative Binomial size")
        list(freqDistr = "Negative_Binomial", r = size,
             beta = check(frequency$mu, "The Negative Binomial mean") / size / frequency_share)
      },
      "Zero-inflated Poisson" = list(
        freqDistr = "Zero_Inflated_Poisson",
        zip_lambda = check(frequency$lambda, "The zero-inflated Poisson lambda") / frequency_share,
        zip_zero = check_zero(frequency$zero)
      ),
      "Zero-inflated Negative Binomial" = {
        size <- check(frequency$size, "The zero-inflated Negative Binomial size")
        list(freqDistr = "Zero_Inflated_Negative_Binomial", zinb_r = size,
             zinb_beta = check(frequency$mu, "The zero-inflated Negative Binomial mean") / size / frequency_share,
             zinb_zero = check_zero(frequency$zero))
      },
      stop("The simulator has no ", frequency$model, " frequency.", call. = FALSE)
    )
  }
  if (!is.null(severity)) {
    sev <- switch(
      severity$model,
      "Normal" = list(sevDistr = "Normal", normal_mean = check(severity$mean, "The Normal mean", positive = FALSE),
                      normal_sd = check(severity$sd, "The Normal standard deviation")),
      "LogNormal" = list(sevDistr = "LogNormal", mu = check(severity$mu, "The LogNormal mu", positive = FALSE),
                         sigma = check(severity$sigma, "The LogNormal sigma")),
      "Exponential" = list(sevDistr = "Exponential", rate = 1 / check(severity$mean, "The Exponential mean")),
      "Gamma" = list(sevDistr = "Gamma", shape = check(severity$shape, "The Gamma shape"),
                     scale = check(severity$scale, "The Gamma scale")),
      #(x_m before alpha, as the simulator shows them)
      "Pareto" = list(sevDistr = "Pareto", x_m = check(severity$xm, "The Pareto x_m"),
                      alpha = check(severity$alpha, "The Pareto alpha")),
      "Weibull" = list(sevDistr = "Weibull", weibull_shape = check(severity$shape, "The Weibull shape"),
                       weibull_scale = check(severity$scale, "The Weibull scale")),
      "Burr" = list(sevDistr = "Burr", burr_alpha = check(severity$alpha, "The Burr alpha"),
                    burr_gamma = check(severity$gamma, "The Burr gamma"),
                    burr_scale = check(severity$scale, "The Burr scale")),
      "Generalised Pareto" = {
        location <- check(severity$location, "The Generalised Pareto location", positive = FALSE)
        if (location < 0) stop("The Generalised Pareto location is negative, which the simulator does not accept.", call. = FALSE)
        list(sevDistr = "Generalised_Pareto", gpd_location = location,
             gpd_scale = check(severity$scale, "The Generalised Pareto scale"),
             gpd_shape = check(severity$shape, "The Generalised Pareto shape", positive = FALSE))
      },
      stop("The simulator has no ", severity$model, " severity.", call. = FALSE)
    )
    slices <- severity$slices
    k <- if (is.null(slices)) 0L else nrow(slices)
    if (k > max_number_of_pareto_slices) {
      stop("The simulator takes at most ", max_number_of_pareto_slices, " Pareto slices; this model has ", k, ".", call. = FALSE)
    }
    if (k > 1 && any(diff(slices$x_m) <= 0)) stop("The slice thresholds must increase.", call. = FALSE)
    sev$pareto_slice_times <- k
    #the simulator's slice fields: alpha in the odd ones, the threshold in the even ones (the
    #threshold written first, as the simulator shows it)
    for (j in seq_len(k)) {
      sev[[paste0("slice_pareto_param_", 2 * j)]] <- check(slices$x_m[j], paste("Slice", j, "threshold"))
      sev[[paste0("slice_pareto_param_", 2 * j - 1)]] <- check(slices$alpha[j], paste("Slice", j, "alpha"))
    }
    values <- c(values, sev)
  }
  values
}

# The piecewise Pareto of the Piecewise Pareto tab, from thresholds `t` (the
# first the smallest claim) and their alphas, as the simulator's severity: a
# Pareto from the first threshold, with a slice at each later one.
dft_piecewise_severity <- function(t, alpha) {
  k <- length(t)
  list(model = "Pareto", xm = t[1], alpha = alpha[1],
       slices = if (k > 1) data.frame(x_m = t[-1], alpha = alpha[-1]))
}

# A summary of exported settings for the "To the simulator" tab: one row per
# setting, its label in the simulator and its value.
dft_simulator_settings_rows <- function(values) {
  labels <- c(
    freqDistr = "Frequency distribution", lamda = "lambda (mean claims)", r = "r (shape)", beta = "beta (scale)",
    zip_lambda = "lambda (mean of the Poisson)", zip_zero = "Probability of an extra zero",
    zinb_r = "r (shape)", zinb_beta = "beta (scale)", zinb_zero = "Probability of an extra zero",
    sevDistr = "Severity distribution", normal_mean = "Mean", normal_sd = "Standard deviation",
    mu = "mu", sigma = "sigma", rate = "rate", shape = "shape", scale = "scale",
    x_m = "x_m (minimum)", alpha = "alpha (shape)", pareto_slice_times = "Number of Pareto slices",
    weibull_shape = "Shape", weibull_scale = "Scale",
    burr_alpha = "alpha (first shape)", burr_gamma = "gamma (second shape)", burr_scale = "Scale",
    gpd_location = "Location (threshold)", gpd_scale = "Scale", gpd_shape = "Shape (xi)"
  )
  shown <- c(Negative_Binomial = "Negative Binomial", Zero_Inflated_Poisson = "Zero-inflated Poisson",
             Zero_Inflated_Negative_Binomial = "Zero-inflated Negative Binomial",
             Generalised_Pareto = "Generalised Pareto")
  rows <- list()
  for (id in names(values)) {
    value <- values[[id]]
    slice <- regmatches(id, regexec("^slice_pareto_param_([0-9]+)$", id))[[1]]
    label <- if (length(slice) == 2) {
      i <- as.integer(slice[2])
      paste("Slice", (i + 1) %/% 2, if (i %% 2 == 1) "alpha" else "threshold (x_m)")
    } else labels[[id]]
    text <- if (is.character(value)) (if (value %in% names(shown)) shown[[value]] else value) else netsimr_fmt(value, 6)
    rows[[length(rows) + 1]] <- list(label, text)
  }
  rows
}

# ------------------------------------------------- Claims reported above a threshold

# The points of a Q-Q plot: the empirical quantiles of the claims at `points`
# probabilities between 0.5% and 99.5% and further into the tail (up to the
# (n - 0.5)/n quantile, beside the largest claim), with each model's quantiles
# at the same probabilities. A point on the diagonal is a quantile the model
# gets right.
dft_qq_data <- function(claims, cdfs, points = 200) {
  n <- length(claims)
  p <- sort(unique(c(seq(0.005, 0.995, length.out = points),
                     if (n >= 200) c(0.999, 1 - 0.5 / n))))
  p <- p[p > 0.5 / n & p < 1 - 0.5 / n | p == 1 - 0.5 / n]
  empirical <- stats::quantile(claims, p, names = FALSE, type = 7)
  list(p = p, empirical = empirical,
       fitted = lapply(cdfs, function(cdf) dft_model_quantiles(cdf, p, claims)))
}

# The points of a P-P plot: at each claim size, the empirical probability of a
# claim up to it against each model's cdf there. A point on the diagonal is a
# probability the model gets right. The empirical probability of the i-th of n
# sorted claims is (i - 0.5) / n, the middle of its step; with policy limits it
# is the middle of each step of the Kaplan-Meier estimate `km`
# (dft_kaplan_meier()), at the claims below their limits. At most `points`
# claims are kept, evenly spread over the steps.
dft_pp_data <- function(claims, cdfs, km = NULL, points = 500) {
  if (is.null(km)) {
    x <- sort(claims)
    empirical <- (seq_along(x) - 0.5) / length(x)
  } else {
    x <- km$x
    empirical <- (c(0, km$cdf[-length(km$cdf)]) + km$cdf) / 2
  }
  if (length(x) > points) {
    keep <- unique(round(seq(1, length(x), length.out = points)))
    x <- x[keep]
    empirical <- empirical[keep]
  }
  list(x = x, empirical = empirical, fitted = lapply(cdfs, function(cdf) cdf(x)))
}

# The levels of the conditional tail expectations in the Severity tab.
dft_cte_levels <- c(0.9, 0.95, 0.99, 0.995)


# The note under the Severity tab's table on the fits above a threshold that
# failed (the `failed` messages of dft_truncated_severity_models()): those that
# run to the edge of their parameters together, any other failure as it is.
dft_failed_fits_note <- function(failed) {
  if (length(failed) == 0) return("")
  names <- sub(" fit( above the threshold| with the policy limits)? failed: .*$", "", failed)
  edge <- grepl("heavier tail than any", failed, fixed = TRUE)
  either <- function(x) if (length(x) == 1) x else paste(paste(x[-length(x)], collapse = ", "), "or", x[length(x)])
  paste0(
    if (any(edge)) paste0(" No ", either(names[edge]), " fits: the claims above the threshold have a heavier tail than any ",
                          either(names[edge]), ", whose fit runs to the edge of its parameters."),
    if (any(!edge)) paste0(" ", paste0(failed[!edge], ".", collapse = " "))
  )
}
