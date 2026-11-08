#' Run the claims reserving application
#'
#' @description Opens NetReserve, a shiny application for claims reserving: it
#'   loads a claims triangle (an example, or a file with a row per origin and
#'   development period or a row per origin), lets the user choose the
#'   development factors (their average, the calendar periods used, link ratios
#'   left out, a tail factor or curve), compares the chain ladder,
#'   Bornhuetter-Ferguson, Benktander and Cape Cod ultimates and loss ratios of
#'   each origin, and measures the uncertainty of the reserves with Mack's
#'   standard errors and a bootstrap.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{chain_ladder}}, \code{\link{bornhuetter_ferguson}},
#'   \code{\link{cape_cod}} and \code{\link{bootstrap_chain_ladder}}, the methods
#'   of the application; \code{\link{run_shiny_simulator}},
#'   \code{\link{run_shiny_distribution_fitting_tool}} and
#'   \code{\link{run_shiny_glm_fitting_tool}}, the package's other applications.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_reserving_tool()
#' }
run_shiny_reserving_tool <- function() {
  shinyApp(ui = reserving_tool_UI, server = reserving_tool_Server, onStart = shiny_tool_on_start)
}

# Internal helpers for the reserving tool.

# The example data sets, by the label the Data tab shows.
rsv_examples <- c(
  "RAA: incurred claims, 1981 to 1990" = "raa",
  "Taylor and Ashe: paid claims" = "taylor_ashe",
  "Simulated motor: paid and incurred claims, premiums" = "motor",
  "Quarg and Mack: paid and incurred claims" = "quarg_mack"
)

# The methods of the Ultimates tab, by label, and their colours in the charts (none
# of them the orange of the IBNR).
rsv_methods <- c("Chain ladder" = "cl", "Bornhuetter-Ferguson" = "bf", "Benktander" = "bk", "Cape Cod" = "cc")
rsv_method_colours <- c("Chain ladder" = "#3b82f6", "Bornhuetter-Ferguson" = "#10b981", "Benktander" = "#ec4899",
                        "Cape Cod" = "#8b5cf6", "A priori" = "empirical")

# An example: its triangle, its premiums and incurred triangle (NULL when it has
# none), its data as a table and a description.
rsv_example_input <- function(id) {
  if (identical(id, "motor")) {
    data <- NetSimR::simulated_motor_claims
    triangle <- as_triangle(data, "accident_year", "development_year", "paid", cumulative = FALSE)
    incurred <- as_triangle(data, "accident_year", "development_year", "incurred", cumulative = FALSE)
    premium <- data$earned_premium[match(rownames(triangle), as.character(data$accident_year))]
    return(list(triangle = triangle, incurred = incurred, premium = premium, data = data,
                description = paste("Simulated paid claims of a motor portfolio, by accident and development year,",
                                    "with each year's earned premium and incurred claims: incremental amounts, one row each.")))
  }
  if (identical(id, "quarg_mack")) {
    triangle <- NetSimR::quarg_mack_paid
    incurred <- NetSimR::quarg_mack_incurred
    data <- data.frame(origin = rep(rownames(triangle), ncol(triangle)),
                       development = rep(seq_len(ncol(triangle)), each = nrow(triangle)),
                       paid = as.vector(triangle), incurred = as.vector(incurred))
    data <- data[!is.na(data$paid), ]
    rownames(data) <- NULL
    return(list(triangle = triangle, incurred = incurred, premium = NULL, data = data,
                description = paste("Cumulative paid claims of Quarg and Mack (2004), with their incurred claims,",
                                    "the example of the Munich chain ladder.")))
  }
  triangle <- if (identical(id, "taylor_ashe")) NetSimR::taylor_ashe_triangle else NetSimR::raa_triangle
  description <- if (identical(id, "taylor_ashe")) {
    "Cumulative paid claims of Taylor and Ashe (1983), the triangle of Mack's 1993 paper."
  } else {
    "Cumulative incurred claims of the Reinsurance Association of America, in thousands, as in Mack (1994)."
  }
  data <- data.frame(origin = rownames(triangle), triangle, check.names = FALSE, stringsAsFactors = FALSE)
  list(triangle = triangle, incurred = NULL, premium = NULL, data = data, description = description)
}

# The triangle, premiums and incurred triangle of an uploaded table. Long: a row
# per origin and development period, with the origin, development, value and
# (optionally) incurred and premium columns. Wide: a row per origin, the origin
# column, an optional premium column and every other numeric column a development
# period, in their order (no incurred triangle).
rsv_upload_input <- function(df, layout, origin, development = NULL, value = NULL, premium = NULL,
                             cumulative = TRUE, dec = ".", incurred = NULL) {
  number <- function(column) dft_as_numeric(df[[column]], dec)
  has <- function(column) !is.null(column) && nzchar(column) && column %in% names(df)
  if (!has(origin)) stop("Choose the origin column.", call. = FALSE)
  premium_column <- if (has(premium)) premium
  incurred_triangle <- NULL
  if (identical(layout, "long")) {
    if (!has(development) || !has(value)) stop("Choose the development period and value columns.", call. = FALSE)
    incurred_column <- if (has(incurred)) incurred
    chosen <- c(origin, development, value, incurred_column, premium_column)
    if (length(unique(chosen)) < length(chosen)) {
      stop("Choose a different column for each of the origin, development period, amounts and premium.", call. = FALSE)
    }
    triangle <- reserving_long_triangle(df[[origin]], number(development), number(value), cumulative)
    if (!is.null(incurred_column)) {
      incurred_triangle <- reserving_long_triangle(df[[origin]], number(development), number(incurred_column), cumulative)
      if (!identical(dimnames(incurred_triangle), dimnames(triangle)) ||
          !identical(is.na(incurred_triangle), is.na(triangle))) {
        stop("The incurred amounts must cover the same origins and development periods as the paid amounts.", call. = FALSE)
      }
    }
  } else {
    numbers <- dft_numeric_columns(df, dec)
    columns <- setdiff(names(df), c(origin, premium_column))
    columns <- columns[columns %in% numbers]
    if (length(columns) < 2) stop("The table needs at least two numeric development period columns besides the origin.", call. = FALSE)
    values <- vapply(columns, function(column) number(column), numeric(nrow(df)))
    if (!is.matrix(values)) values <- matrix(values, nrow = 1, dimnames = list(NULL, columns))
    keep <- !is.na(df[[origin]]) & nzchar(trimws(as.character(df[[origin]])))
    triangle <- reserving_wide_triangle(values[keep, , drop = FALSE], as.character(df[[origin]])[keep], columns, cumulative)
  }
  premiums <- NULL
  if (!is.null(premium_column)) {
    amounts <- number(premium_column)
    labels <- as.character(df[[origin]])
    premiums <- vapply(rownames(triangle), function(o) {
      x <- unique(amounts[labels == o & !is.na(amounts)])
      if (length(x) > 1) stop("The premium column has more than one value for origin ", o, ".", call. = FALSE)
      if (length(x) == 0) NA_real_ else x
    }, numeric(1), USE.NAMES = FALSE)
  }
  list(triangle = triangle, incurred = incurred_triangle, premium = premiums)
}

# Formats amounts in full with thousands separators, ratios with three decimals and
# shares as percentages; a dash for a missing value.
rsv_amount <- function(x) {
  out <- rep(dft_dash, length(x))
  ok <- is.finite(x)
  out[ok] <- formatC(round(x[ok]), format = "f", digits = 0, big.mark = ",")
  out
}

rsv_ratio <- function(x, digits = 3) {
  out <- rep(dft_dash, length(x))
  ok <- is.finite(x)
  out[ok] <- formatC(x[ok], format = "f", digits = digits)
  out
}

rsv_percent <- function(x, digits = 1) {
  out <- rep(dft_dash, length(x))
  ok <- is.finite(x)
  out[ok] <- paste0(formatC(100 * x[ok], format = "f", digits = digits), "%")
  out
}

# A triangle as an HTML table: the origins down the side, the development periods
# across, the observed values as they are and the projected ones (where
# `projected` is TRUE) muted. The table is written as one string, as for
# dft_data_preview().
rsv_triangle_table <- function(values, projected = NULL, format = rsv_amount, note = NULL) {
  if (is.null(projected)) projected <- matrix(FALSE, nrow(values), ncol(values))
  text <- matrix(format(as.vector(values)), nrow(values))
  text[is.na(values)] <- ""
  classes <- ifelse(projected, " class=\"rsv-projected\"", "")
  body <- vapply(seq_len(nrow(values)), function(i) {
    paste0("<tr><td>", dft_escape(rownames(values)[i]), "</td>",
           paste0("<td", classes[i, ], ">", text[i, ], "</td>", collapse = ""), "</tr>")
  }, character(1))
  header <- paste0("<th>Origin</th>", paste0("<th>", dft_escape(colnames(values)), "</th>", collapse = ""))
  HTML(paste0("<div class=\"dft-table-wrap rsv-triangle\"><table class=\"dft-table\"><thead><tr>", header,
              "</tr></thead><tbody>", paste(body, collapse = ""), "</tbody></table></div>",
              if (!is.null(note)) paste0("<p class=\"dft-table-note\">", dft_escape(note), "</p>")))
}

# The link ratios as an HTML table of buttons, one per ratio, that leave it out
# of the development factors or bring it back (the page's script sends the click
# as input$ratio_toggle); under them, the averages of all the ratios, the selected
# factors and the pattern to ultimate. `outside` marks the ratios before the
# calendar periods used.
rsv_link_ratio_table <- function(cl, excluded, outside) {
  ratios <- cl$link_ratios
  m <- nrow(ratios)
  d <- ncol(ratios)
  available <- !is.na(ratios)
  # (an origin with no link ratios, such as the latest, has no row)
  body <- vapply(which(rowSums(available) > 0), function(i) {
    cells <- vapply(seq_len(d), function(j) {
      if (!available[i, j]) return("<td></td>")
      left_out <- isTRUE(excluded[i, j])
      state <- if (left_out) "left out" else if (outside[i, j]) "before the periods used" else "used"
      class <- paste0("rsv-ratio", if (left_out) " rsv-excluded" else if (outside[i, j]) " rsv-outside")
      label <- paste0("Link ratio of ", rownames(ratios)[i], " from ", colnames(ratios)[j], ": ",
                      rsv_ratio(ratios[i, j]), ", ", state, ". ", if (left_out) "Include it." else "Leave it out.")
      paste0("<td><button type=\"button\" class=\"", class, "\" data-row=\"", i, "\" data-col=\"", j,
             "\" aria-pressed=\"", if (left_out) "true" else "false", "\" aria-label=\"", dft_escape(label), "\">",
             rsv_ratio(ratios[i, j]), "</button></td>")
    }, character(1))
    paste0("<tr><td>", dft_escape(rownames(ratios)[i]), "</td>", paste(cells, collapse = ""), "<td></td></tr>")
  }, character(1))
  C <- cl$triangle
  from <- ifelse(available, C[, -ncol(C), drop = FALSE], 0)
  to <- ifelse(available, C[, -1, drop = FALSE], 0)
  volume <- colSums(to) / colSums(from)
  simple <- colSums(ifelse(available, ratios, 0)) / colSums(available)
  footer_row <- function(label, values, class = NULL) {
    paste0("<tr", if (!is.null(class)) paste0(" class=\"", class, "\""), "><td>", label, "</td>",
           paste0("<td>", values, "</td>", collapse = ""), "</tr>")
  }
  developed <- 1 / cl$cdf
  footer <- paste0(
    footer_row("Volume-weighted, all", c(rsv_ratio(volume), "")),
    footer_row("Simple average, all", c(rsv_ratio(simple), "")),
    footer_row("Selected", c(rsv_ratio(cl$factors), rsv_ratio(cl$tail, 4)), "rsv-selected"),
    footer_row("To ultimate", rsv_ratio(cl$cdf, 3)),
    footer_row("Developed", rsv_percent(developed))
  )
  header <- paste0("<th>Origin</th>", paste0("<th>", dft_escape(colnames(ratios)), "</th>", collapse = ""), "<th>Tail</th>")
  HTML(paste0(
    "<div class=\"dft-table-wrap rsv-ratios\"><table class=\"dft-table\"><thead><tr>", header,
    "</tr></thead><tbody>", paste(body, collapse = ""), "</tbody><tfoot>", footer, "</tfoot></table></div>"
  ))
}

# The script of the link ratio buttons: a click sends the ratio's position.
rsv_ratio_js <- "
(function () {
  document.addEventListener('click', function (event) {
    var button = event.target.closest ? event.target.closest('.rsv-ratio') : null;
    if (!button || !window.Shiny || !window.Shiny.setInputValue) return;
    window.Shiny.setInputValue('ratio_toggle',
      {row: Number(button.getAttribute('data-row')), col: Number(button.getAttribute('data-col'))},
      {priority: 'event'});
  });
})();
"

# The colours of the reserving charts, for the light or dark theme.
rsv_chart_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(accent = "#fb923c", accent_soft = "#fb923c8c", latest = "#64748b", ibnr = "#f97316",
         oldest = "#fed7aa", newest = "#ea580c")
  } else {
    list(accent = "#ea580c", accent_soft = "#ea580c66", latest = "#94a3b8", ibnr = "#ea580c",
         oldest = "#fdba74", newest = "#9a3412")
  }
}

# The cumulative claims of each origin by development period, the oldest origin
# lightest; solid where observed and dashed where projected.
rsv_development_chart <- function(cl, dark = FALSE) {
  full <- cl$full_triangle[, seq_len(ncol(cl$triangle)), drop = FALSE]
  latest <- cl$latest_period
  colours <- rsv_chart_colours(dark)
  m <- nrow(full)
  d <- ncol(full)
  lines_colours <- grDevices::colorRampPalette(c(colours$oldest, colours$newest))(m)
  legend <- list(list(label = "Observed", colour = colours$newest, type = "line"),
                 list(label = "Projected", colour = colours$newest, type = "line", lty = 2))
  dft_plot_frame(c(0.7, d + 0.3), dft_zero_range(full), "Development period", "Cumulative claims", dark,
                 legend = legend, x_labels = colnames(full))
  for (i in seq_len(m)) {
    graphics::lines(seq_len(latest[i]), full[i, seq_len(latest[i])], col = lines_colours[i], lwd = 2)
    if (latest[i] < d) {
      graphics::lines(latest[i]:d, full[i, latest[i]:d], col = lines_colours[i], lwd = 2, lty = 2)
    }
  }
  invisible(NULL)
}

# The link ratios of each development period: those used as filled markers, those
# left out as open ones, and the selected factor as a line. The axis covers the
# ratios used and the factors; ratios beyond it sit at its edge as triangles.
rsv_link_ratio_chart <- function(cl, dark = FALSE) {
  ratios <- cl$link_ratios
  used <- cl$used
  colours <- rsv_chart_colours(dark)
  plot_colours <- dft_plot_colours(dark)
  d <- ncol(ratios)
  shown <- c(ratios[used], cl$factors)
  ylim <- dft_line_range(shown)
  legend <- list(list(label = "Used", colour = colours$accent, type = "marker", pch = 19),
                 list(label = "Left out", colour = plot_colours$muted, type = "marker", pch = 1),
                 list(label = "Selected factor", colour = dft_model_palette[1], type = "point"))
  dft_plot_frame(c(0.5, d + 0.5), ylim, "Development factor", "Link ratio", dark, legend = legend,
                 x_labels = colnames(ratios))
  for (j in seq_len(d)) {
    for (state in c("left out", "used")) {
      rows <- which(!is.na(ratios[, j]) & (if (state == "used") used[, j] else !used[, j]))
      if (length(rows) == 0) next
      y <- ratios[rows, j]
      x <- j + (seq_along(rows) - (length(rows) + 1) / 2) * min(0.5 / length(rows), 0.06)
      high <- y > ylim[2]
      low <- y < ylim[1]
      inside <- !high & !low
      colour <- if (state == "used") colours$accent else plot_colours$muted
      pch <- if (state == "used") 19 else 1
      graphics::points(x[inside], y[inside], pch = pch, col = colour, cex = 0.85, lwd = 1.5)
      #(drawn over the edge of the plot, which would otherwise cut them in half)
      if (any(high)) graphics::points(x[high], rep(ylim[2], sum(high)), pch = 2, col = colour, cex = 0.9, lwd = 1.5, xpd = NA)
      if (any(low)) graphics::points(x[low], rep(ylim[1], sum(low)), pch = 6, col = colour, cex = 0.9, lwd = 1.5, xpd = NA)
    }
  }
  graphics::lines(seq_len(d), cl$factors, col = dft_model_palette[1], lwd = 2.2)
  graphics::points(seq_len(d), cl$factors, pch = 19, col = dft_model_palette[1], cex = 0.9)
  invisible(NULL)
}

# The development factors and the tail curve fitted to them, continued for ten
# periods beyond the triangle.
rsv_tail_chart <- function(cl, dark = FALSE) {
  curve <- cl$tail_curve
  d <- length(cl$factors)
  colours <- rsv_chart_colours(dark)
  plot_colours <- dft_plot_colours(dark)
  periods <- seq_len(d + 10)
  fitted <- curve$fitted(periods)
  used <- seq_len(d) %in% curve$used
  legend <- list(list(label = "Factors the curve is fitted to", colour = colours$accent, type = "marker", pch = 19),
                 list(label = "Other factors", colour = plot_colours$muted, type = "marker", pch = 1),
                 list(label = paste0(if (curve$curve == "exponential") "Exponential" else "Inverse power",
                                     " curve, tail ", rsv_ratio(cl$tail, 4)),
                      colour = dft_model_palette[1], type = "line"))
  ylim <- dft_line_range(c(cl$factors[used], fitted, 1))
  dft_plot_frame(c(0.5, d + 10.5), ylim, "Development factor (period)", "Factor", dark, legend = legend)
  graphics::abline(v = d + 0.5, col = plot_colours$muted, lty = 2, lwd = 1.2)
  graphics::lines(periods, fitted, col = dft_model_palette[1], lwd = 2.2)
  inside <- cl$factors >= ylim[1] & cl$factors <= ylim[2]
  graphics::points(which(used & inside), cl$factors[used & inside], pch = 19, col = colours$accent, cex = 0.95)
  graphics::points(which(!used & inside), cl$factors[!used & inside], pch = 1, col = plot_colours$muted, cex = 0.95, lwd = 1.5)
  invisible(NULL)
}

# The selected ultimate of each origin, as the latest claims with the IBNR above
# them, and the ultimates of the methods as markers.
rsv_ultimate_chart <- function(origins, latest, ibnr, methods, dark = FALSE) {
  colours <- rsv_chart_colours(dark)
  n <- length(origins)
  total <- latest + ibnr
  ylim <- dft_zero_range(c(latest, total, unlist(methods)))
  legend <- c(list(list(label = "Latest", colour = colours$latest, type = "bar"),
                   list(label = "IBNR (selected)", colour = colours$ibnr, type = "bar")),
              unname(Map(function(name, colour) list(label = name, colour = colour, type = "point"),
                         names(methods), rsv_method_colours[names(methods)])))
  dft_plot_frame(c(0.4, n + 0.6), ylim, "Origin", "Claims", dark, legend = legend, x_labels = origins)
  x <- seq_len(n)
  ok <- is.finite(total)
  graphics::rect(x[ok] - 0.38, 0, x[ok] + 0.38, latest[ok], col = colours$latest, border = NA)
  graphics::rect(x[ok] - 0.38, latest[ok], x[ok] + 0.38, total[ok], col = colours$ibnr, border = NA)
  for (name in names(methods)) {
    y <- methods[[name]]
    graphics::lines(x, y, col = rsv_method_colours[[name]], lwd = 1.6)
    graphics::points(x, y, pch = 19, col = rsv_method_colours[[name]], cex = if (n > 20) 0.6 else 0.85)
  }
  invisible(NULL)
}

# The distribution of the simulated total IBNR, with its mean and percentiles.
rsv_bootstrap_chart <- function(totals, mean_ibnr, percentiles, dark = FALSE) {
  colours <- rsv_chart_colours(dark)
  plot_colours <- dft_plot_colours(dark)
  h <- graphics::hist(totals, breaks = min(60, max(10, round(sqrt(length(totals))))), plot = FALSE)
  share <- h$counts / sum(h$counts)
  marks <- c(Mean = mean_ibnr, percentiles)
  marker_colours <- c(dft_model_palette[1], dft_model_palette[c(3, 4, 2, 5, 6)])[seq_along(marks)]
  legend <- c(list(list(label = "Simulated total IBNR", colour = colours$accent_soft, type = "bar")),
              unname(Map(function(name, value, colour) {
                list(label = paste0(name, " ", rsv_amount(value)), colour = colour, type = "line", lty = if (name == "Mean") 1 else 2)
              }, names(marks), marks, marker_colours)))
  dft_plot_frame(range(h$breaks), dft_zero_range(share), "Total IBNR", "Share of simulations", dark,
                 legend = legend, y_percent = TRUE)
  border <- if (graphics::par("pin")[1] / length(share) > 0.06) plot_colours$bar_border else NA
  graphics::rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], share, col = colours$accent_soft, border = border)
  for (k in seq_along(marks)) {
    graphics::abline(v = marks[k], col = marker_colours[k], lwd = 2, lty = if (k == 1) 1 else 2)
  }
  invisible(NULL)
}

# The scaled Pearson residuals of the bootstrap's chain ladder against the origin,
# the development period or the calendar period, with their mean at each.
rsv_residual_chart <- function(residuals, by = c("origin", "development", "calendar"), dark = FALSE) {
  by <- match.arg(by)
  colours <- rsv_chart_colours(dark)
  plot_colours <- dft_plot_colours(dark)
  observed <- which(!is.na(residuals), arr.ind = TRUE)
  values <- residuals[observed]
  position <- switch(by, origin = observed[, 1], development = observed[, 2], calendar = observed[, 1] + observed[, 2] - 1)
  labels <- switch(by,
    origin = rownames(residuals),
    development = colnames(residuals),
    calendar = {
      first <- suppressWarnings(as.numeric(rownames(residuals)[1]))
      steps <- seq_len(max(position))
      if (is.finite(first) && !anyNA(suppressWarnings(as.numeric(rownames(residuals))))) format(first + steps - 1) else format(steps)
    }
  )
  groups <- seq_along(labels)
  means <- vapply(groups, function(g) if (any(position == g)) mean(values[position == g]) else NA_real_, numeric(1))
  limit <- max(abs(values), 1) * 1.08
  legend <- list(list(label = "Residual", colour = colours$accent, type = "marker", pch = 19),
                 list(label = "Mean", colour = dft_model_palette[1], type = "point"))
  title <- switch(by, origin = "Origin", development = "Development period", calendar = "Calendar period")
  dft_plot_frame(c(0.4, length(labels) + 0.6), c(-limit, limit), title, "Scaled Pearson residual", dark,
                 legend = legend, x_labels = labels)
  graphics::abline(h = 0, col = plot_colours$muted, lwd = 1.2)
  graphics::points(position, values, pch = 19, col = colours$accent, cex = 0.8)
  graphics::lines(groups, means, col = dft_model_palette[1], lwd = 2)
  graphics::points(groups, means, pch = 19, col = dft_model_palette[1], cex = 0.75)
  invisible(NULL)
}

# The ultimates of every method for each origin, and the selected one. `premium`
# and `prior` have NA where they are not given; a method that needs a premium
# gives NA for an origin without one, and the Cape Cod needs every origin's.
rsv_method_results <- function(cl, premium, prior, decay, selected) {
  m <- nrow(cl$triangle)
  latest <- cl$by_origin$latest
  developed <- 1 / cl$cdf[cl$latest_period]
  has_premium <- is.finite(premium) & premium > 0
  has_prior <- has_premium & is.finite(prior) & prior >= 0
  expected <- ifelse(has_prior, premium * prior, NA_real_)
  bf <- latest + (1 - developed) * expected
  bk <- latest + (1 - developed) * bf
  cc <- rep(NA_real_, m)
  cc_ratio <- NA_real_
  cc_note <- NULL
  if (all(has_premium)) {
    fit <- tryCatch(cape_cod(cl, premium, decay = decay), error = function(e) e)
    if (inherits(fit, "error")) {
      cc_note <- conditionMessage(fit)
    } else {
      cc <- fit$by_origin$ultimate
      cc_ratio <- fit$total[["loss_ratio"]]
    }
  }
  ultimates <- cbind(cl = cl$by_origin$ultimate, bf = bf, bk = bk, cc = cc)
  method <- ifelse(selected %in% colnames(ultimates), selected, "cl")
  chosen <- ultimates[cbind(seq_len(m), match(method, colnames(ultimates)))]
  results <- data.frame(
    origin = cl$by_origin$origin, latest = latest, developed = unname(developed), premium = ifelse(has_premium, premium, NA_real_),
    prior_loss_ratio = ifelse(has_prior, prior, NA_real_), chain_ladder = ultimates[, "cl"],
    bornhuetter_ferguson = bf, benktander = bk, cape_cod = cc, method = names(rsv_methods)[match(method, rsv_methods)],
    ultimate = chosen, ibnr = chosen - latest, ultimate_loss_ratio = ifelse(has_premium, chosen / premium, NA_real_),
    mack_se = ifelse(method == "cl", cl$by_origin$se, NA_real_),
    stringsAsFactors = FALSE, row.names = NULL
  )
  attr(results, "cape_cod_loss_ratio") <- cc_ratio
  attr(results, "cape_cod_note") <- cc_note
  results
}

# A results table: `rows` and `total` are lists of cells (the first is the row's
# name), the total row set apart at the bottom.
rsv_table <- function(header, rows, total = NULL, note = NULL) {
  row <- function(cells, class = NULL) tags$tr(class = class, lapply(cells, tags$td))
  tagList(
    div(
      class = "dft-table-wrap",
      tags$table(
        class = "dft-table",
        tags$thead(tags$tr(lapply(header, tags$th))),
        tags$tbody(lapply(rows, row), if (!is.null(total)) row(total, "rsv-total"))
      )
    ),
    if (!is.null(note) && length(note) > 0 && any(nzchar(note))) p(class = "dft-table-note", paste(note[nzchar(note)], collapse = " "))
  )
}

# The increments of a cumulative triangle.
rsv_increments <- function(triangle) {
  out <- triangle
  out[, -1] <- triangle[, -1, drop = FALSE] - triangle[, -ncol(triangle), drop = FALSE]
  out
}

# The column a pattern names first among `columns`, or NULL.
rsv_guess_column <- function(columns, pattern, exclude = NULL) {
  hits <- setdiff(columns[grepl(pattern, columns, ignore.case = TRUE)], exclude)
  if (length(hits) > 0) hits[1]
}

# The settings file of the reserving tool (see write_settings_file()): the tool, the
# version, and the inputs it keeps with their kind. "column" inputs refer to columns of
# an uploaded file and are applied once a file with that column is read; the link
# ratios left out and each origin's premium, a priori loss ratio and method are kept by
# origin and applied to a triangle with those origins.
rsv_settings_tool <- "Reserving tool"
rsv_settings_version <- 1L
rsv_settings_inputs <- c(
  data_source = "radio", example = "select", data_includes_header = "switch", sep = "radio", dec = "radio",
  quote = "radio", layout = "radio", origin_col = "column", development_col = "column", value_col = "column",
  incurred_col = "column", premium_col = "column", cumulative = "radio",
  average = "radio", periods = "select", tail_method = "select", tail_from = "select", tail_factor = "numeric",
  sigma = "radio", prior_loss_ratio = "numeric", decay = "slider", method_all = "select",
  boot_n = "numeric", boot_process = "radio", boot_tail = "switch", boot_seed = "numeric",
  clark_curve = "select", clark_method = "radio", clark_truncation = "numeric",
  inflation_past = "numeric", inflation_future = "numeric"
)

# The link ratios left out, as "origin | factor" labels, and back.
rsv_exclusion_labels <- function(excluded) {
  if (is.null(excluded) || !any(excluded)) return(character(0))
  at <- which(excluded, arr.ind = TRUE)
  paste(rownames(excluded)[at[, 1]], colnames(excluded)[at[, 2]], sep = " | ")
}

rsv_exclusion_matrix <- function(labels, ratios) {
  excluded <- matrix(FALSE, nrow(ratios), ncol(ratios), dimnames = dimnames(ratios))
  for (p in strsplit(labels, " | ", fixed = TRUE)) {
    if (length(p) == 2 && p[1] %in% rownames(ratios) && p[2] %in% colnames(ratios) && !is.na(ratios[p[1], p[2]])) {
      excluded[p[1], p[2]] <- TRUE
    }
  }
  excluded
}
