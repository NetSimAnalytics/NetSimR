##############
#Report of the distribution fitting tool
##############

# The text of a cell of the tool's tables: a string, or tags such as a parameter's name
# with its value.
dft_cell_text <- function(cell) {
  if (is.null(cell)) return("")
  if (inherits(cell, "shiny.tag")) return(dft_cell_text(cell$children))
  if (is.list(cell)) return(paste(Filter(nzchar, vapply(cell, dft_cell_text, character(1))), collapse = " "))
  paste(as.character(cell), collapse = " ")
}

# A table of a tool as a table of the report. `t` holds the arguments of dft_html_table()
# or rsv_table(): the header, the rows and, optionally, a total row, the best row with its
# label, and a note (one string or several). The best row is named at the start of the note.
dft_report_table <- function(t, title = NULL, text_cols = 1) {
  cells <- lapply(c(t$rows, if (!is.null(t$total)) list(t$total)),
                  function(row) vapply(row, dft_cell_text, character(1), USE.NAMES = FALSE))
  df <- as.data.frame(do.call(rbind, cells), stringsAsFactors = FALSE)
  names(df) <- gsub("\n", " ", t$header, fixed = TRUE)
  note <- t$note[nzchar(t$note)]
  if (!is.null(t$best)) {
    label <- if (is.null(t$best_label)) "Best fit" else t$best_label
    note <- c(paste0(label, ": ", df[t$best, 1], "."), note)
  }
  netsimr_report_table(df, text_cols, title, if (length(note) > 0) paste(note, collapse = " "))
}

# The charts of a section of a report, each a list with a `draw` function of the theme, an
# `alt` text, a `title`, a `note` and, optionally, `wide = TRUE`: the charts marked wide
# across the page, then the rest side by side in pairs; with an odd number of those, the
# first of them across the page too.
dft_report_chart_rows <- function(charts) {
  chart <- function(f, wide = TRUE) {
    netsimr_report_chart(f$draw, f$alt, title = f$title, note = f$note, height = if (wide) 4.2 else 4,
                         width = if (wide) 9 else 6.5)
  }
  charts <- Filter(Negate(is.null), charts)
  wide <- vapply(charts, function(f) isTRUE(f$wide), logical(1))
  odd <- sum(!wide) %% 2 == 1
  first <- lapply(c(charts[wide], if (odd) charts[!wide][1]), chart)
  rest <- charts[!wide]
  if (odd) rest <- rest[-1]
  pairs <- lapply(seq_len(length(rest) / 2), function(i) {
    div(class = "two-col", chart(rest[[2 * i - 1]], wide = FALSE), chart(rest[[2 * i]], wide = FALSE))
  })
  tagList(first, pairs)
}

#' Writes the report of the distribution fitting tool
#'
#' @description An HTML page with the fits of each analysis that has been run: its
#'   figures, its table of fitted distributions and its charts, as on the tool's tabs.
#' @param file The path of the HTML file to write.
#' @param r A list with the \code{source} of the data, its \code{rows} and
#'   \code{columns}, and, for each analysis that has run (\code{frequency},
#'   \code{severity}, \code{sliced} and \code{piecewise}; \code{NULL} otherwise), a list
#'   with its \code{settings} (labels and values), a \code{headline} (a \code{label}, a
#'   \code{value} and a \code{note}) for the key results, its \code{tiles}, its
#'   \code{table} (the arguments of \code{dft_html_table()}, with the columns of text in
#'   \code{text_cols}) and its \code{charts}, each a list with a \code{draw} function of
#'   the theme (\code{TRUE} for dark), an \code{alt} text, a \code{title}, a
#'   \code{note} and, optionally, \code{wide = TRUE} to draw it across the page. The
#'   severity may also hold \code{cte}, the table of conditional tail expectations, and
#'   \code{hill}, the Hill plot.
#' @param generated The time shown on the report.
#' @return \code{file}, invisibly.
#' @noRd
write_distribution_report <- function(file, r, generated = Sys.time()) {
  parts <- c(frequency = "Frequency", severity = "Severity", sliced = "Sliced severity", piecewise = "Piecewise Pareto")
  parts <- parts[vapply(names(parts), function(part) !is.null(r[[part]]), logical(1))]
  count <- function(n) format(n, big.mark = ",", scientific = FALSE)

  headlines <- lapply(unname(r[names(parts)]), function(a) a$headline)
  headlines <- Filter(Negate(is.null), headlines)
  tiles <- c(
    list(Data = list(value = if (is.null(r$rows)) dft_dash else paste(count(r$rows), "rows"), note = r$source)),
    stats::setNames(lapply(headlines, function(h) list(value = h$value, note = h$note)),
                    vapply(headlines, function(h) h$label, character(1)))
  )
  key <- list(id = "key-results", title = "Key results", content = tagList(
    netsimr_report_tiles(tiles),
    if (length(parts) == 0) div(class = "callout callout-warning", tags$ul(tags$li(
      "No analysis has been run. Choose the columns on a tab of the tool and click Run analysis, then save the report again.")))
  ))

  settings <- list(id = "settings", title = "Data and settings", content = netsimr_report_settings(c(
    list(Data = list(Source = r$source, Rows = if (!is.null(r$rows)) count(r$rows),
                     Columns = if (!is.null(r$columns)) count(r$columns))),
    stats::setNames(lapply(names(parts), function(part) r[[part]]$settings), unname(parts))
  )))

  intros <- c(
    frequency = "The distributions fitted to the claim counts by maximum likelihood.",
    severity = "The distributions fitted to the claim sizes by maximum likelihood.",
    sliced = "A LogNormal body with one or two Pareto tails above the slicing points.",
    piecewise = "A Pareto in each layer between the thresholds."
  )
  analyses <- lapply(names(parts), function(part) {
    a <- r[[part]]
    list(id = part, title = parts[[part]], content = tagList(
      tags$p(class = "section-intro", intros[[part]]),
      if (length(a$tiles) > 0) netsimr_report_tiles(a$tiles),
      dft_report_table(a$table, "Fitted distributions", if (is.null(a$text_cols)) 1 else a$text_cols),
      dft_report_chart_rows(a$charts),
      if (!is.null(a$cte)) dft_report_table(a$cte, "Conditional tail expectations"),
      if (!is.null(a$hill)) dft_report_chart_rows(list(c(a$hill, wide = TRUE)))
    ))
  })

  guide <- list(id = "how-to-read", title = "How to read this report", content = div(class = "callout", tags$ul(
    tags$li("Each analysis is the one last run on its tab of the tool, with the columns and options under Data and settings."),
    tags$li("Frequency: the AIC weighs the fit against the number of parameters, and the lowest is the best. ",
            "A zero-inflated model adds periods without a claim to a Poisson or a Negative Binomial."),
    tags$li("Severity: the K-S distance is the largest gap between a fitted and the empirical cumulative distribution; the ",
            "Anderson-Darling statistic weighs the tails more. For both, lower is closer. The Q-Q plot shows how ",
            "the tail fits, the P-P plot the body."),
    tags$li("The conditional tail expectation at a level is the mean claim above that percentile; a Hill plot that is ",
            "level shows a Pareto tail, with the alpha it reads."),
    tags$li("The sliced and piecewise models fit Pareto tails above the points chosen in the tool: move them there ",
            "and save the report again to compare."),
    tags$li("The tool's Export tab sends a fitted frequency and severity to NetSimulator, the claims simulator.")
  )))

  netsimr_write_report(file, "Distribution Fitting Report", "NetDisFit by NetSimR",
                       c(list(key, settings), analyses, list(guide)),
                       accent = c(glow = "rgba(167, 139, 250, 0.35)", dark = "#1e1b4b", middle = "#4c1d95", bright = "#7c3aed",
                                  eyebrow = "#c4b5fd"),
                       generated = generated)
}
