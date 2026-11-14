##############
#The charts of the tools
##############

# Charts of the tools and their reports. They are drawn with base graphics as PNG images
# (shiny::renderPlot()) on a transparent background, so they sit on the card colour of either
# theme (#ffffff in the light theme, #111a2b in the dark one), and are redrawn when the theme
# changes.

# Chart colours for the light or dark app theme. Eight hex digits are a
# translucent colour.
netsimr_plot_colours <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(font = "#cbd5e1", muted = "#94a3b8", grid = "#94a3b82e", axis = "#94a3b873",
         empirical = "#f1f5f9", bar = "#8b5cf6", bar_border = "#111a2b", band_bar = "#14b8a64d")
  } else {
    list(font = "#334155", muted = "#64748b", grid = "#94a3b84d", axis = "#64748b8c",
         empirical = "#0f172a", bar = "#7c3aed", bar_border = "#ffffff", band_bar = "#0d948838")
  }
}

# Colours of the series of a chart (fitted models, methods, lines), which read well on both
# backgrounds.
netsimr_palette <- c("#3b82f6", "#f97316", "#10b981", "#ec4899", "#06b6d4", "#eab308", "#8b5cf6", "#dc2626")

# The colour of a series: a colour as it is, or the name of a theme colour
# ("empirical", "bar", "band_bar"), so that the data of a chart does not depend
# on the theme.
netsimr_series_colour <- function(colour, colours) {
  if (!is.null(colours[[colour]])) colours[[colour]] else colour
}

# Axis labels with thousands separators ("25,000", "0.25"); from 1e12 up, or
# below 1e-6 (but not zero), in scientific notation. Shares as "25%".
netsimr_axis_labels <- function(x, percent = FALSE) {
  if (percent) x <- 100 * x
  scientific <- x != 0 & (abs(x) >= 1e12 | abs(x) < 1e-6)
  out <- formatC(signif(x, 6), format = "fg", digits = 6, big.mark = ",")
  out[scientific] <- formatC(x[scientific], format = "g", digits = 3)
  out <- trimws(out)
  if (percent) paste0(out, "%") else out
}

# Ticks of a logarithmic axis over the range r: the powers of ten, with 2 and 5
# times each power when there are fewer than three of them, or ordinary ticks
# when the range is too short for either.
netsimr_log_ticks <- function(r) {
  powers <- 10^seq(floor(log10(r[1])), ceiling(log10(r[2])))
  ticks <- powers[powers >= r[1] & powers <= r[2]]
  if (length(ticks) < 3) {
    fine <- sort(c(powers, 2 * powers, 5 * powers))
    ticks <- fine[fine >= r[1] & fine <= r[2]]
  }
  if (length(ticks) < 2) {
    ticks <- pretty(r)
    ticks <- ticks[ticks >= r[1] & ticks <= r[2]]
  }
  ticks
}

# A range from zero (or below, for negative values) to a little above the
# values, for bars and averages.
netsimr_zero_range <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(c(0, 1))
  low <- min(0, values)
  high <- max(0, values)
  if (high == low) high <- low + 1
  c(if (low < 0) low - 0.06 * (high - low) else 0, high + 0.08 * (high - low))
}

# A range for lines that need not start at zero: from zero while zero is near
# the values (they cross it, or the smallest is within half of the largest from
# it, as for rates and frequencies), otherwise the values with 8% either side,
# so that a response around 1,000 does not draw as a flat line at the top.
netsimr_line_range <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(c(0, 1))
  r <- range(values)
  near_zero <- (r[1] <= 0 && r[2] >= 0) || (r[1] > 0 && r[1] <= 0.5 * r[2]) || (r[2] < 0 && r[2] >= 0.5 * r[1])
  if (near_zero) return(netsimr_zero_range(values))
  span <- if (diff(r) > 0) diff(r) else 0.1 * abs(r[1])
  c(r[1] - 0.08 * span, r[2] + 0.08 * span)
}

# Maps values on the secondary axis (range y2lim) onto the primary one (ylim).
netsimr_to_y2 <- function(values, ylim, y2lim) {
  ylim[1] + (values - y2lim[1]) / diff(y2lim) * diff(ylim)
}

# Category labels shortened to fit under a chart.
netsimr_short_label <- function(labels, max_chars = 24) {
  labels <- as.character(labels)
  long <- nchar(labels) > max_chars
  labels[long] <- paste0(substr(labels[long], 1, max_chars - 1), intToUtf8(8230))
  labels
}

# Legend entries packed into rows no wider than `width` inches: the row of each
# entry and its offset from the left of the row.
netsimr_legend_layout <- function(labels, width, cex) {
  key <- 0.3
  gap <- 0.07
  spacing <- 0.22
  item <- key + gap + graphics::strwidth(labels, units = "inches", cex = cex) + spacing
  row <- integer(length(labels))
  offset <- numeric(length(labels))
  current <- 1
  x <- 0
  for (i in seq_along(labels)) {
    if (x > 0 && x + item[i] - spacing > width) {
      current <- current + 1
      x <- 0
    }
    row[i] <- current
    offset[i] <- x
    x <- x + item[i]
  }
  list(row = row, offset = offset, rows = current, key = key, gap = gap)
}

# Draws the legend in the top margin, from `left` inches, one row every 1.4
# lines. Entries are lists with a label, a colour and a type: "line", "point"
# (a line with a marker), "marker" (a marker alone, of symbol pch) or "bar"; a
# line can have a line type, lty.
netsimr_draw_legend <- function(entries, layout, left, colours, cex) {
  inch <- graphics::par("csi")
  device_height <- graphics::par("din")[2]
  old <- graphics::par(xpd = NA)
  on.exit(graphics::par(old))
  to_x <- function(x) graphics::grconvertX(x, "inches", "user")
  to_y <- function(y) graphics::grconvertY(y, "inches", "user")
  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    colour <- netsimr_series_colour(entry$colour, colours)
    x0 <- left + layout$offset[i]
    y <- device_height - ((layout$row[i] - 0.5) * 1.4 + 0.15) * inch
    if (identical(entry$type, "bar")) {
      graphics::rect(to_x(x0 + 0.04), to_y(y - 0.065), to_x(x0 + layout$key - 0.04), to_y(y + 0.065),
                     col = colour, border = NA)
    } else if (identical(entry$type, "marker")) {
      graphics::points(to_x(x0 + layout$key / 2), to_y(y), pch = if (is.null(entry$pch)) 19 else entry$pch,
                       cex = 0.9, col = colour, lwd = 1.5)
    } else {
      graphics::segments(to_x(x0), to_y(y), to_x(x0 + layout$key), to_y(y), col = colour, lwd = 2.5,
                         lty = if (is.null(entry$lty)) 1 else entry$lty)
      if (identical(entry$type, "point")) graphics::points(to_x(x0 + layout$key / 2), to_y(y), pch = 19, cex = 0.8, col = colour)
    }
    graphics::text(to_x(x0 + layout$key + layout$gap), to_y(y), entry$label, adj = c(0, 0.5), cex = cex, col = colours$font)
  }
}

# Opens a chart on the current device: theme colours, grid lines, axes with
# thousands separators, axis titles, and the legend above the plot, on as many
# rows as the width needs. The margins follow the widest tick labels, so that
# nothing is cut off at narrow widths. x_labels puts one category label at each
# of 1, 2, ..., slanted and thinned out when they do not fit side by side; y2
# (a list with lim and title) adds a secondary axis on the right, onto which
# values are mapped with netsimr_to_y2(). Returns the theme colours.
netsimr_plot_frame <- function(xlim, ylim, x_title, y_title, dark = FALSE, x_log = FALSE, legend = NULL,
                           x_labels = NULL, y_percent = FALSE, y2 = NULL) {
  colours <- netsimr_plot_colours(dark)
  cex_axis <- 0.8
  cex_title <- 0.85
  # the background is the device's: transparent in the apps (renderPlot(bg = "transparent"))
  graphics::par(fg = colours$axis, col.axis = colours$font, family = "sans",
                las = 1, tcl = -0.25, mgp = c(3, 0.4, 0), cex.axis = cex_axis, xpd = FALSE)
  # the height of a line of text and the size of the device, in inches
  inch <- graphics::par("csi")
  device <- graphics::par("din")
  lines_wide <- function(labels) max(graphics::strwidth(labels, units = "inches", cex = cex_axis), 0) / inch
  within <- function(ticks, r) ticks[ticks >= r[1] - 1e-9 * diff(r) & ticks <= r[2] + 1e-9 * diff(r)]
  y_ticks <- within(pretty(ylim, n = 6), ylim)
  y_labels <- netsimr_axis_labels(y_ticks, y_percent)
  left <- lines_wide(y_labels) + 2.1
  right <- 1
  if (!is.null(y2)) {
    y2_ticks <- within(pretty(y2$lim, n = 5), y2$lim)
    right <- lines_wide(netsimr_axis_labels(y2_ticks)) + 2.1
  }
  # category labels side by side when they fit, otherwise slanted, every few when crowded
  bottom <- 3.1
  slanted <- FALSE
  every <- 1
  if (!is.null(x_labels)) {
    x_labels <- netsimr_short_label(x_labels)
    slot <- (device[1] - (left + right) * inch) / length(x_labels)
    label_width <- graphics::strwidth(x_labels, units = "inches", cex = cex_axis)
    if (max(label_width) + 0.1 > slot) {
      slanted <- TRUE
      every <- max(1, ceiling(1.5 * inch * cex_axis / slot))
      bottom <- min(max(label_width) * sin(pi / 4) / inch + 2.6, 0.4 * device[2] / inch)
    }
  }
  cex_legend <- 0.82
  top <- 0.8
  if (length(legend) > 0) {
    layout <- netsimr_legend_layout(vapply(legend, function(entry) entry$label, character(1)),
                                device[1] - left * inch - 0.1, cex_legend)
    top <- layout$rows * 1.4 + 0.5
  }
  graphics::par(mar = c(bottom, left, top, right))
  graphics::plot.new()
  if (length(legend) > 0) {
    graphics::plot.window(c(0, 1), c(0, 1))
    netsimr_draw_legend(legend, layout, left * inch, colours, cex_legend)
  }
  graphics::plot.window(xlim, ylim, log = if (x_log) "x" else "", xaxs = if (is.null(x_labels)) "r" else "i", yaxs = "i")
  usr <- graphics::par("usr")
  graphics::abline(h = y_ticks, col = colours$grid, lwd = 1)
  if (is.null(x_labels)) {
    x_range <- if (x_log) 10^usr[1:2] else usr[1:2]
    x_ticks <- within(if (x_log) netsimr_log_ticks(x_range) else pretty(x_range, n = 6), x_range)
    graphics::abline(v = x_ticks, col = colours$grid, lwd = 1)
    graphics::axis(1, at = x_ticks, labels = netsimr_axis_labels(x_ticks), lwd = 0, lwd.ticks = 1, col.ticks = colours$axis)
  } else {
    shown <- seq(1, length(x_labels), by = every)
    graphics::axis(1, at = seq_along(x_labels), labels = FALSE, lwd = 0, lwd.ticks = 1, col.ticks = colours$axis)
    if (slanted) {
      y <- graphics::grconvertY(graphics::grconvertY(usr[3], "user", "inches") - 0.4 * inch, "inches", "user")
      graphics::text(shown, y, x_labels[shown], srt = 45, adj = c(1, 1), xpd = NA, cex = cex_axis, col = colours$font)
    } else {
      graphics::axis(1, at = shown, labels = x_labels[shown], lwd = 0, lwd.ticks = 0)
    }
  }
  graphics::axis(2, at = y_ticks, labels = y_labels, lwd = 0, lwd.ticks = 1, col.ticks = colours$axis)
  if (!is.null(y2)) {
    graphics::axis(4, at = netsimr_to_y2(y2_ticks, ylim, y2$lim), labels = netsimr_axis_labels(y2_ticks), lwd = 0,
                   lwd.ticks = 1, col.ticks = colours$axis, col.axis = colours$muted)
    graphics::mtext(y2$title, side = 4, line = right - 1.2, col = colours$muted, cex = cex_title, las = 0)
  }
  graphics::box(bty = if (is.null(y2)) "l" else "u", col = colours$axis)
  graphics::mtext(x_title, side = 1, line = bottom - 1.2, col = colours$muted, cex = cex_title)
  graphics::mtext(y_title, side = 2, line = left - 1.2, col = colours$muted, cex = cex_title, las = 0)
  invisible(colours)
}

# Line chart of series, lists with a name, x, y, a colour and optionally step
# (a step line, as for an empirical cdf) and lwd; vlines are dashed markers
# (slicing points, thresholds). The legend is left out for a single series.
netsimr_line_chart <- function(series, x_title, y_title, dark = FALSE, x_log = FALSE, ylim = NULL, vlines = NULL) {
  usable <- function(x) is.finite(x) & (!x_log | x > 0)
  xs <- c(unlist(lapply(series, function(s) s$x)), vlines)
  xlim <- range(xs[usable(xs)])
  if (is.null(ylim)) ylim <- netsimr_zero_range(unlist(lapply(series, function(s) s$y)))
  # a name given to several series (the two bounds of a band) is in the legend once
  legend <- if (length(series) > 1) lapply(series[!duplicated(vapply(series, function(s) s$name, character(1)))],
                                           function(s) list(label = s$name, colour = s$colour, type = "line"))
  colours <- netsimr_plot_frame(xlim, ylim, x_title, y_title, dark, x_log, legend)
  if (length(vlines) > 0) graphics::abline(v = vlines[usable(vlines)], col = colours$muted, lty = 2, lwd = 1.2)
  for (s in series) {
    ok <- usable(s$x) & is.finite(s$y)
    graphics::lines(s$x[ok], s$y[ok], type = if (isTRUE(s$step)) "s" else "l",
                    col = netsimr_series_colour(s$colour, colours), lwd = if (is.null(s$lwd)) 2 else s$lwd)
  }
  invisible(NULL)
}

# Chart by category: bars (a list with name, values, colour, and optionally a
# border colour and y2, the title of a secondary axis on the right for them)
# and lines with markers (lists with name, values and colour). With the bars on
# the right axis, the left axis follows the lines (netsimr_line_range()).
netsimr_category_chart <- function(labels, bars = NULL, lines = list(), x_title, y_title, dark = FALSE, y_percent = FALSE) {
  n <- length(labels)
  on_y2 <- !is.null(bars) && !is.null(bars$y2)
  line_values <- unlist(lapply(lines, function(l) l$values))
  ylim <- if (on_y2) netsimr_line_range(line_values) else netsimr_zero_range(c(line_values, if (!is.null(bars)) bars$values))
  y2 <- if (on_y2) list(lim = netsimr_zero_range(bars$values), title = bars$y2)
  legend <- c(if (!is.null(bars)) list(list(label = bars$name, colour = bars$colour, type = "bar")),
              lapply(lines, function(l) list(label = l$name, colour = l$colour, type = "point")))
  colours <- netsimr_plot_frame(c(0.4, n + 0.6), ylim, x_title, y_title, dark, legend = legend, x_labels = labels,
                            y_percent = y_percent, y2 = y2)
  if (!is.null(bars)) {
    top <- if (on_y2) netsimr_to_y2(bars$values, ylim, y2$lim) else bars$values
    base <- if (on_y2) netsimr_to_y2(0, ylim, y2$lim) else 0
    # a border separates the bars while they are wide enough
    border <- if (!is.null(bars$border) && graphics::par("pin")[1] / n > 0.06) netsimr_series_colour(bars$border, colours) else NA
    graphics::rect(seq_len(n) - 0.42, base, seq_len(n) + 0.42, top, col = netsimr_series_colour(bars$colour, colours),
                   border = border, lwd = 1)
  }
  point_size <- if (n > 40) 0.55 else if (n > 20) 0.75 else 0.95
  for (l in lines) {
    colour <- netsimr_series_colour(l$colour, colours)
    graphics::lines(seq_len(n), l$values, col = colour, lwd = 2.2)
    graphics::points(seq_len(n), l$values, pch = 19, cex = point_size, col = colour)
  }
  invisible(NULL)
}

# Histogram of counts in the bins between breaks.
netsimr_histogram_chart <- function(breaks, counts, x_title, y_title, dark = FALSE) {
  colours <- netsimr_plot_frame(range(breaks), netsimr_zero_range(counts), x_title, y_title, dark)
  border <- if (graphics::par("pin")[1] / length(counts) > 0.06) colours$bar_border else NA
  graphics::rect(breaks[-length(breaks)], 0, breaks[-1], counts, col = colours$bar, border = border, lwd = 1)
  invisible(NULL)
}
