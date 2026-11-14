#' Run the portfolio capital application
#'
#' @description Opens NetCapital, a shiny application for the capital of a
#'   portfolio of several lines of business: each line's annual losses are
#'   simulated from a frequency and a severity, or uploaded (simulated by the
#'   claims simulator or another model), and joined by a Gaussian or t copula
#'   with a correlation for every pair of lines or a matrix. It gives the value at
#'   risk and tail value at risk of each line and of the total, the
#'   diversification benefit, each line's share of the capital (co-TVaR), charts
#'   and a report.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{simulate_portfolio}}, the function behind the
#'   application; \code{\link{run_shiny_simulator}}, whose simulated losses it can
#'   take, and the package's other applications.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_portfolio_tool()
#' }
run_shiny_portfolio_tool <- function() {
  shinyApp(ui = portfolio_tool_UI, server = portfolio_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the portfolio tool.

# The most lines a portfolio of the tool can have.
pfo_max_lines <- 8L

# The claim count and claim size distributions a line can have, by their names in
# simulate_claims(), with the labels and defaults of their parameters.
pfo_frequencies <- list(
  Poisson = list(params = "Mean (lambda)", defaults = 10),
  `Negative Binomial` = list(params = c("r (shape)", "beta (scale)"), defaults = c(4, 2.5))
)
pfo_severities <- list(
  LogNormal = list(params = c("meanlog", "sdlog"), defaults = c(8, 1.2)),
  Gamma = list(params = c("shape", "scale"), defaults = c(2, 5000)),
  Pareto = list(params = c("alpha", "x_m (minimum)"), defaults = c(1.8, 20000)),
  Exponential = list(params = "rate", defaults = 1e-4)
)

# The lines of a new portfolio: motor, property and liability.
pfo_default_lines <- data.frame(
  name = c("Motor", "Property", "Liability", "Marine", "Health", "Line 6", "Line 7", "Line 8"),
  frequency = c("Poisson", "Poisson", "Negative Binomial", "Poisson", "Poisson", "Poisson", "Poisson", "Poisson"),
  f1 = c(50, 5, 4, 3, 200, 10, 10, 10),
  f2 = c(NA, NA, 2, NA, NA, NA, NA, NA),
  severity = c("LogNormal", "Pareto", "LogNormal", "Pareto", "Gamma", "LogNormal", "LogNormal", "LogNormal"),
  s1 = c(8, 1.8, 9, 1.5, 2, 8, 8, 8),
  s2 = c(1, 20000, 1.5, 50000, 500, 1.2, 1.2, 1.2),
  stringsAsFactors = FALSE
)

# The levels of the value at risk the tool offers.
pfo_levels <- c("99.5% (1 in 200)" = "0.995", "99% (1 in 100)" = "0.99", "99.9% (1 in 1,000)" = "0.999",
                "90% (1 in 10)" = "0.9")

# The colours of the portfolio charts, for the light or dark theme.
pfo_chart_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(accent = "#4ade80", accent_soft = "#4ade808c", points = "#4ade8040", var = "#f87171", muted = "#94a3b8")
  } else {
    list(accent = "#15803d", accent_soft = "#15803d59", points = "#15803d2e", var = "#dc2626", muted = "#64748b")
  }
}

# The correlation matrix of the lines from the inputs: one correlation for every pair, or the
# values above the diagonal of a matrix, by row (r12, r13, ..., r23, ...).
pfo_correlation_matrix <- function(k, upper) {
  R <- diag(k)
  if (length(upper) == 1) {
    R[upper.tri(R)] <- upper
  } else {
    R[lower.tri(R)] <- upper
    R <- t(R)
  }
  R[lower.tri(R)] <- t(R)[lower.tri(R)]
  R
}

# Each line's losses from an uploaded table: the chosen columns, as numbers; a line must
# have a finite loss in every row.
pfo_upload_lines <- function(df, columns, dec = ".") {
  columns <- intersect(columns, names(df))
  if (length(columns) < 2) stop("Choose at least two columns, one per line.", call. = FALSE)
  values <- lapply(columns, function(column) netsimr_as_numeric(df[[column]], dec))
  bad <- columns[vapply(values, function(v) anyNA(v) || any(!is.finite(v)), logical(1))]
  if (length(bad) > 0) {
    stop("Every row needs a loss in each line's column: ", paste0("'", bad, "'", collapse = ", "),
         if (length(bad) == 1) " has" else " have", " missing or non-numeric values.", call. = FALSE)
  }
  if (nrow(df) < 100) stop("The file needs at least 100 simulations, one per row.", call. = FALSE)
  stats::setNames(values, columns)
}

# Capital by line: each line's value at risk on its own (bars) and its share of the
# portfolio's TVaR (co-TVaR, a line).
pfo_capital_chart <- function(summary, level, dark = FALSE) {
  colours <- pfo_chart_colours(dark)
  lines <- summary[summary$line != "total", ]
  label <- paste0(format(100 * level), "%")
  netsimr_category_chart(lines$line, bars = list(name = paste("Value at risk on its own,", label), values = lines$var,
                                             colour = colours$accent_soft),
                     lines = list(list(name = paste("Allocated capital (co-TVaR),", label), values = lines$capital,
                                       colour = netsimr_palette[1])),
                     x_title = "Line", y_title = "Amount", dark = dark)
}

# The distribution of the portfolio's total losses, with its mean, value at risk and tail
# value at risk.
pfo_total_chart <- function(total, var, tvar, dark = FALSE) {
  colours <- pfo_chart_colours(dark)
  h <- graphics::hist(total, breaks = 80, plot = FALSE)
  netsimr_histogram_chart(h$breaks, h$counts, "Total annual loss", "Simulations", dark)
  graphics::abline(v = mean(total), col = colours$muted, lty = 2, lwd = 1.5)
  graphics::abline(v = c(var, tvar), col = colours$var, lty = c(2, 3), lwd = 1.8)
  graphics::legend("topright", bty = "n", cex = 0.8, text.col = netsimr_plot_colours(dark)$font, lty = c(2, 2, 3), lwd = 1.8,
                   col = c(colours$muted, colours$var, colours$var),
                   legend = c(paste("Mean", netsimr_amount(mean(total))), paste("VaR", netsimr_amount(var)), paste("TVaR", netsimr_amount(tvar))))
  invisible(NULL)
}

# The dependence of two lines: each simulation's rank of their losses, as shares from 0 to 1.
# Independent lines fill the square evenly; dependent ones gather along the diagonal, and a t
# copula's in its corners. At most 5,000 simulations are drawn.
pfo_rank_chart <- function(x, y, x_name, y_name, dark = FALSE) {
  colours <- pfo_chart_colours(dark)
  n <- length(x)
  u <- (rank(x, ties.method = "random") - 0.5) / n
  v <- (rank(y, ties.method = "random") - 0.5) / n
  shown <- if (n > 5000) round(seq(1, n, length.out = 5000)) else seq_len(n)
  netsimr_plot_frame(c(0, 1), c(0, 1), paste("Rank of", x_name), paste("Rank of", y_name), dark)
  graphics::points(u[shown], v[shown], pch = 16, cex = 0.45, col = colours$points)
  graphics::rect(0.95, 0.95, 1, 1, border = colours$var, lwd = 1.2)
  invisible(NULL)
}

# The settings file of the portfolio tool (see write_settings_file()): the inputs it keeps
# with their kind; each line's name, distributions and parameters, the correlation matrix
# and the uploaded columns are kept as vectors.
pfo_settings_tool <- "Portfolio capital tool"
pfo_settings_version <- 1L
pfo_settings_inputs <- c(
  line_source = "radio", n_lines = "numeric", upload_header = "switch", upload_sep = "radio", upload_dec = "radio",
  copula = "radio", copula_df = "numeric", corr_mode = "radio", corr_all = "numeric", n_sims = "numeric",
  level = "select", seed = "numeric"
)
