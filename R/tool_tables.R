##############
#Numbers, tables and tiles of the tools
##############

# How the tools write numbers, and the HTML tables and result tiles they show them in.

# Formats numbers for the result tables and tiles: `digits` significant digits
# with thousands separators, and a dash for missing or infinite values. Values
# from 1e15 up, or below 1e-6 (but not zero), are written as 1.234e+20: in full
# they would be long runs of digits, some of them spurious.
netsimr_dash <- intToUtf8(8212)

netsimr_fmt <- function(x, digits = 4) {
  out <- rep(netsimr_dash, length(x))
  ok <- is.finite(x)
  scientific <- ok & x != 0 & (abs(x) >= 1e15 | abs(x) < 1e-6)
  plain <- ok & !scientific
  out[plain] <- formatC(signif(x[plain], digits), digits = digits, format = "fg", big.mark = ",")
  out[scientific] <- formatC(x[scientific], digits = digits, format = "g")
  trimws(out)
}

# Formats a count or total weight in full with thousands separators (format()
# switches to 1.61e+09 for large doubles); very large values as netsimr_fmt().
netsimr_fmt_count <- function(x) {
  if (is.finite(x) && abs(x) < 1e15) formatC(x, format = "f", digits = 0, big.mark = ",") else netsimr_fmt(x)
}

# Formats amounts in full with thousands separators, ratios with three decimals and
# shares as percentages; a dash for a missing value.
netsimr_amount <- function(x) {
  out <- rep(netsimr_dash, length(x))
  ok <- is.finite(x)
  out[ok] <- formatC(round(x[ok]), format = "f", digits = 0, big.mark = ",")
  out
}

netsimr_percent <- function(x, digits = 1) {
  out <- rep(netsimr_dash, length(x))
  ok <- is.finite(x)
  out[ok] <- paste0(formatC(100 * x[ok], format = "f", digits = digits), "%")
  out
}

# Escapes text for HTML (htmltools::htmlEscape() is not re-exported by shiny).
netsimr_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

# Builds an HTML table for a tool's results. `rows` is a list of lists of
# cells (strings or tags), `header` a character vector, `best` the index of a
# row to mark with a badge. `align` gives "left" for the columns to align left
# (text); the others are aligned right, except the first. `class` is added to
# the scrolling container. With `stack`, a phone shows each row as a card, a line
# per column under the column's name, instead of a table to scroll sideways. A
# line break in a column's name breaks its heading there (a long name over a
# narrow column).
netsimr_html_table <- function(header, rows, best = NULL, best_label = "Best fit", note = NULL,
                           align = NULL, class = NULL, stack = FALSE) {
  if (stack) class <- c(class, "dft-table-stack")
  cell_class <- function(j) if (isTRUE(unname(align[j]) == "left")) "dft-left"
  heading <- function(name) {
    lines <- strsplit(name, "\n", fixed = TRUE)[[1]]
    if (length(lines) < 2) return(name)
    tagList(lines[1], lapply(lines[-1], function(line) tagList(tags$br(), line)))
  }
  header_labels <- gsub("\n", " ", header, fixed = TRUE)
  body <- lapply(seq_along(rows), function(i) {
    cells <- rows[[i]]
    if (!is.null(best) && length(best) == 1 && !is.na(best) && i == best) {
      cells[[1]] <- tagList(cells[[1]], tags$span(class = "dft-badge", best_label))
    }
    tags$tr(lapply(seq_along(cells), function(j) {
      tags$td(class = cell_class(j), `data-label` = if (stack && j <= length(header)) header_labels[[j]], cells[[j]])
    }))
  })
  tagList(
    div(
      class = paste(c("dft-table-wrap", class), collapse = " "),
      tags$table(
        class = "dft-table",
        tags$thead(tags$tr(lapply(seq_along(header), function(j) tags$th(class = cell_class(j), heading(header[[j]]))))),
        tags$tbody(body)
      )
    ),
    if (!is.null(note)) p(class = "dft-table-note", note)
  )
}

# A results table: `rows` and `total` are lists of cells (the first is the row's
# name), the total row set apart at the bottom. With `stack`, a phone shows each
# row as a card, a line per column under the column's name.
netsimr_table <- function(header, rows, total = NULL, note = NULL, stack = FALSE) {
  row <- function(cells, class = NULL) {
    tags$tr(class = class, lapply(seq_along(cells), function(j) {
      tags$td(`data-label` = if (stack && j <= length(header)) header[[j]], cells[[j]])
    }))
  }
  tagList(
    div(
      class = paste(c("dft-table-wrap", if (stack) "dft-table-stack"), collapse = " "),
      tags$table(
        class = "dft-table",
        tags$thead(tags$tr(lapply(header, tags$th))),
        tags$tbody(lapply(rows, row), if (!is.null(total)) row(total, "rsv-total"))
      )
    ),
    if (!is.null(note) && length(note) > 0 && any(nzchar(note))) p(class = "dft-table-note", paste(note[nzchar(note)], collapse = " "))
  )
}

# Summary tile for the results area.
netsimr_stat_tile <- function(label, value, note = NULL, icon_name = NULL, accent = FALSE) {
  div(
    class = paste("dft-stat", if (accent) "dft-stat-accent"),
    div(class = "dft-stat-label", if (!is.null(icon_name)) icon(icon_name), label),
    div(class = "dft-stat-value", value),
    if (!is.null(note)) div(class = "dft-stat-note", note)
  )
}
