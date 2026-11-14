##############
#Reports of the tools
##############

# The reports of the reserving and portfolio tools are pages in the style of the
# simulation report (R/simulator_report.R), whose styles, theme switch and script they
# share: one HTML file that holds its charts as images, with a light and a dark copy of
# each, and needs neither pandoc nor rmarkdown.

# A chart of the report: `draw(dark)` draws it with a tool's chart functions, once for each
# theme, into PNG images at twice screen resolution embedded in the page; the page shows the
# one that matches its theme.
netsimr_report_chart <- function(draw, alt, title = NULL, note = NULL, height = 4.2, width = 9) {
  backgrounds <- c(light = "#ffffff", dark = "#111a2b")
  images <- lapply(names(backgrounds), function(theme) {
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path), add = TRUE)
    res <- 192
    do.call(grDevices::png, c(list(path, width = width * res, height = height * res, res = res, bg = backgrounds[[theme]]),
                              png_device_args()))
    drawn <- tryCatch({
      draw(theme == "dark")
      TRUE
    }, error = function(e) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, conditionMessage(e), col = "#64748b", cex = 0.9)
      FALSE
    }, finally = grDevices::dev.off())
    tags$img(class = paste0("chart-", theme), src = base64enc::dataURI(file = path, mime = "image/png"), alt = alt)
  })
  div(class = "chart-card",
      if (!is.null(title)) tags$h3(title),
      if (!is.null(note)) tags$p(class = "chart-note", note),
      tagList(images))
}

# Headline figures: a named list of values, or of lists with a value and a note.
netsimr_report_tiles <- function(tiles, tail = character(0)) {
  div(class = "kpi-grid", lapply(names(tiles), function(label) {
    tile <- tiles[[label]]
    value <- if (is.list(tile)) tile$value else tile
    note <- if (is.list(tile)) tile$note
    div(class = if (label %in% tail) "kpi kpi-tail" else "kpi",
        div(class = "kpi-label", label),
        div(class = "kpi-value", value),
        if (!is.null(note)) div(class = "kpi-note", note))
  }))
}

# A table of the report from a data frame of text; the columns after the first are numbers
# (aligned right) unless `text_cols` says otherwise. A last row named "Total" is set apart.
netsimr_report_table <- function(df, text_cols = 1, title = NULL, note = NULL) {
  cell <- function(j, value, tag) tag(class = if (!(j %in% text_cols)) "num", value)
  total <- nrow(df) > 0 && identical(as.character(df[nrow(df), 1]), "Total")
  div(class = "table-card",
      if (!is.null(title)) tags$h3(title),
      tags$table(
        class = "report-table",
        tags$thead(tags$tr(lapply(seq_along(df), function(j) cell(j, names(df)[j], tags$th)))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          tags$tr(style = if (total && i == nrow(df)) "font-weight: 700;",
                  lapply(seq_along(df), function(j) cell(j, df[i, j], tags$td)))
        }))
      ),
      if (!is.null(note)) tags$p(class = "table-note", note))
}

# Settings as cards of labels and values: a named list of named lists.
netsimr_report_settings <- function(cards) {
  div(class = "settings-grid", lapply(names(cards), function(title) {
    rows <- cards[[title]]
    rows <- rows[!vapply(rows, is.null, logical(1))]
    div(class = "setting-card",
        div(class = "setting-title", title),
        tags$dl(lapply(names(rows), function(key) tagList(tags$dt(key), tags$dd(rows[[key]])))))
  }))
}

# Writes a report: `sections` is a list of lists with an id, a title and the content; the
# header shows the eyebrow and the title, in the colours of `accent` (the dark, middle and
# bright colours of the banner and the eyebrow's).
netsimr_write_report <- function(file, title, eyebrow, sections, accent = NULL, generated = Sys.time()) {
  sections <- Filter(Negate(is.null), sections)
  nav <- tags$nav(
    class = "report-nav", `aria-label` = "Contents",
    div(class = "report-nav-title", "Contents"),
    tags$ul(lapply(sections, function(s) tags$li(tags$a(href = paste0("#", s$id), s$title))))
  )
  generated_text <- format(generated, "%d %B %Y, %H:%M")
  body <- tags$body(div(
    class = "report-layout",
    nav,
    tags$main(
      tags$header(
        class = "report-header",
        div(class = "report-header-top", tags$p(class = "report-eyebrow", eyebrow), report_theme_switch()),
        tags$h1(title),
        tags$p(class = "report-date", paste("Generated", generated_text))
      ),
      lapply(sections, function(s) tags$section(id = s$id, tags$h2(s$title), s$content)),
      netsimr_report_footer(generated_text)
    )
  ))
  accent_css <- if (!is.null(accent)) {
    paste0(".report-header { background: radial-gradient(circle at 88% 15%, ", accent[["glow"]], ", transparent 45%), ",
           "linear-gradient(135deg, ", accent[["dark"]], " 0%, ", accent[["middle"]], " 60%, ", accent[["bright"]], " 140%); }\n",
           ".report-eyebrow { color: ", accent[["eyebrow"]], "; }\n")
  } else ""
  html <- enc2utf8(paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", dft_escape(title), "</title>\n",
    "<style>", simulation_report_css, simulation_report_theme_css, accent_css, "</style>\n",
    "<script>", simulation_report_theme_js, "</script>\n",
    "</head>\n", as.character(body), "\n</html>\n"
  ))
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(file)
}
