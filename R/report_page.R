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
        div(class = "report-header-top", tags$p(class = "report-eyebrow", eyebrow), netsimr_report_theme_switch()),
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
    "<title>", netsimr_escape(title), "</title>\n",
    "<style>", netsimr_css("report"), netsimr_report_theme_css, accent_css, "</style>\n",
    "<script>", netsimr_report_theme_js, "</script>\n",
    "</head>\n", as.character(body), "\n</html>\n"
  ))
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(file)
}

#' Light colour variables for the simulation report
#'
#' @noRd
netsimr_report_light_vars <- "
  color-scheme: light;
  --r-page-bg: #f1f5f9;
  --r-card-bg: #ffffff;
  --r-text: #0f172a;
  --r-label: #334155;
  --r-muted: #64748b;
  --r-border: #e2e8f0;
  --r-row-border: #f1f5f9;
  --r-row-hover: #f8fafc;
  --r-blue: #2563eb;
  --r-blue-soft: #eff6ff;
  --r-callout-border: #bfdbfe;
  --r-red: #dc2626;
  --r-amber: #d97706;
  --r-amber-soft: #fffbeb;
  --r-amber-border: #fde68a;
  --r-shadow: 0 1px 2px rgba(15, 23, 42, 0.04), 0 6px 18px rgba(15, 23, 42, 0.05);
"

#' Dark colour variables for the simulation report
#'
#' @noRd
netsimr_report_dark_vars <- "
  color-scheme: dark;
  --r-page-bg: #0b1220;
  --r-card-bg: #111a2b;
  --r-text: #e2e8f0;
  --r-label: #cbd5e1;
  --r-muted: #94a3b8;
  --r-border: #26324a;
  --r-row-border: #1b2538;
  --r-row-hover: #16213a;
  --r-blue: #60a5fa;
  --r-blue-soft: rgba(59, 130, 246, 0.14);
  --r-callout-border: rgba(96, 165, 250, 0.35);
  --r-red: #f87171;
  --r-amber: #fbbf24;
  --r-amber-soft: rgba(251, 191, 36, 0.10);
  --r-amber-border: rgba(251, 191, 36, 0.35);
  --r-shadow: 0 1px 2px rgba(0, 0, 0, 0.3), 0 6px 18px rgba(0, 0, 0, 0.35);
"

#' Theme styles for the simulation report (light, dark and the theme switch)
#'
#' @noRd
netsimr_report_theme_css <- paste0("
:root {", netsimr_report_light_vars, "}

/* an explicit choice from the theme switch (the script always sets data-theme) */
:root[data-theme='dark'] {", netsimr_report_dark_vars, "}

/* if the script cannot run, follow the system setting */
@media (prefers-color-scheme: dark) {
  :root:not([data-theme]) {", netsimr_report_dark_vars, "}
  :root:not([data-theme]) .chart-card img.chart-light { display: none; }
  :root:not([data-theme]) .chart-card img.chart-dark { display: block; }
}

/* every chart is drawn twice; show the one that matches the theme */
.chart-card img.chart-dark { display: none; }
:root[data-theme='dark'] .chart-card img.chart-light { display: none; }
:root[data-theme='dark'] .chart-card img.chart-dark { display: block; }

body {
  transition: background-color 0.2s ease, color 0.2s ease;
}

/* theme switch in the header banner, which is dark in both themes */
.report-header-top {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  gap: 12px;
  flex-wrap: wrap;
}

.theme-switch {
  display: inline-flex;
  align-items: center;
  gap: 2px;
  padding: 3px;
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.08);
  box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.14);
}

/* the switch needs the script, so hide it when the script did not run */
:root:not([data-theme]) .theme-switch {
  display: none;
}

.theme-btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  border: 0;
  background: transparent;
  color: rgba(255, 255, 255, 0.78);
  font: inherit;
  font-size: 0.78rem;
  font-weight: 600;
  padding: 5px 10px;
  border-radius: 999px;
  cursor: pointer;
  transition: background-color 0.15s ease, color 0.15s ease;
}

.theme-btn:hover {
  color: #ffffff;
  background: rgba(255, 255, 255, 0.10);
}

.theme-btn:focus-visible {
  outline: 2px solid #93c5fd;
  outline-offset: 1px;
}

.theme-btn.active {
  background: #ffffff;
  color: #0b1f3d;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
}

@media (max-width: 420px) {
  .theme-btn span { display: none; }
}

/* printing always uses the light theme */
@media print {
  :root, :root[data-theme='dark'] {", netsimr_report_light_vars, "}
  .chart-card img.chart-light { display: block !important; }
  .chart-card img.chart-dark { display: none !important; }
  .theme-switch { display: none !important; }
}
")

#' Script that applies and remembers the report's light / dark / system theme
#'
#' @noRd
netsimr_report_theme_js <- "
(function () {
  var KEY = 'netsimr-report-theme';
  var media = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;

  function storedMode() {
    try { return localStorage.getItem(KEY) || 'system'; } catch (e) { return 'system'; }
  }

  function applyTheme(mode) {
    var dark = mode === 'dark' || (mode === 'system' && media && media.matches);
    document.documentElement.setAttribute('data-theme', dark ? 'dark' : 'light');
    var buttons = document.querySelectorAll('.theme-switch .theme-btn');
    for (var i = 0; i < buttons.length; i++) {
      var on = buttons[i].getAttribute('data-theme-value') === mode;
      buttons[i].classList.toggle('active', on);
      buttons[i].setAttribute('aria-pressed', on ? 'true' : 'false');
    }
  }

  applyTheme(storedMode());

  if (media) {
    var onSystemChange = function () { if (storedMode() === 'system') applyTheme('system'); };
    if (media.addEventListener) media.addEventListener('change', onSystemChange);
    else if (media.addListener) media.addListener(onSystemChange);
  }

  document.addEventListener('DOMContentLoaded', function () { applyTheme(storedMode()); });

  document.addEventListener('click', function (event) {
    var button = event.target.closest && event.target.closest('.theme-switch .theme-btn');
    if (!button) return;
    var mode = button.getAttribute('data-theme-value');
    try { localStorage.setItem(KEY, mode); } catch (e) {}
    applyTheme(mode);
  });
})();
"

#' Theme switch (light / dark / system) for the report header
#'
#' @noRd
netsimr_report_theme_switch <- function() {
  icon <- function(paths) {
    shiny::HTML(paste0(
      '<svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" ',
      'stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">', paths, '</svg>'
    ))
  }
  button <- function(value, label, paths) {
    shiny::tags$button(
      type = "button",
      class = "theme-btn",
      `data-theme-value` = value,
      `aria-pressed` = "false",
      title = paste(label, "theme"),
      icon(paths),
      shiny::tags$span(label)
    )
  }
  shiny::div(
    class = "theme-switch",
    role = "group",
    `aria-label` = "Colour theme",
    button("light", "Light", paste0(
      '<circle cx="12" cy="12" r="4"/>',
      '<path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/>'
    )),
    button("dark", "Dark", '<path d="M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z"/>'),
    button("system", "System", '<circle cx="12" cy="12" r="9"/><path d="M12 3a9 9 0 0 1 0 18z" fill="currentColor"/>')
  )
}

# The text of a cell of the tool's tables: a string, or tags such as a parameter's name
# with its value.
netsimr_cell_text <- function(cell) {
  if (is.null(cell)) return("")
  if (inherits(cell, "shiny.tag")) return(netsimr_cell_text(cell$children))
  if (is.list(cell)) return(paste(Filter(nzchar, vapply(cell, netsimr_cell_text, character(1))), collapse = " "))
  paste(as.character(cell), collapse = " ")
}

# A table of a tool as a table of the report. `t` holds the arguments of netsimr_html_table()
# or netsimr_table(): the header, the rows and, optionally, a total row, the best row with its
# label, and a note (one string or several). The best row is named at the start of the note.
netsimr_report_html_table <- function(t, title = NULL, text_cols = 1) {
  cells <- lapply(c(t$rows, if (!is.null(t$total)) list(t$total)),
                  function(row) vapply(row, netsimr_cell_text, character(1), USE.NAMES = FALSE))
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
netsimr_report_chart_rows <- function(charts) {
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
