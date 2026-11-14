##############
#What the tools' servers share
##############

# The pieces every tool's server builds the same way: the theme its page shows, its chart
# outputs, results that stop with a message, and a data file read with the options of its page.

# Whether the page shows its dark theme, as netsimr_theme_input_js() reports it (input$app_theme):
# the charts are drawn in the colours of that theme, and redrawn when it changes.
netsimr_dark <- function(input) reactive(identical(input$app_theme, "dark"))

# A chart output: `draw()` draws the chart, on a transparent background so that it sits on the
# card of either theme.
netsimr_chart <- function(draw, alt) netsimr_render_plot(draw(), bg = "transparent", res = 96, alt = alt)

# The value of an expression or, when it stops (or is an error), its message shown in place of
# the outputs that use it.
netsimr_checked <- function(expr) {
  result <- tryCatch(expr, error = function(e) e)
  validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result)))
  result
}

# A data file uploaded to a tool (`file`, from fileInput()), read with the file options of its
# page (netsimr_file_options()): `ids` names their inputs, `header`, `sep`, `dec` and, on a page
# that has it, `quote` (otherwise the double quote). Without a file, or when the file cannot be
# read or has no rows (or fewer than `min_columns` columns), the outputs that use it show why.
netsimr_read_upload <- function(input, file, ids, missing, min_columns = 1) {
  validate(need(file, missing))
  quote <- if (is.null(ids$quote)) "\"" else or_default(input[[ids$quote]], "\"")
  df <- tryCatch(
    netsimr_read_data(file$datapath, header = isTRUE(input[[ids$header]]), sep = or_default(input[[ids$sep]], ","),
                      quote = quote, dec = or_default(input[[ids$dec]], ".")),
    error = function(e) e
  )
  if (inherits(df, "error")) {
    validate(paste0("The file could not be read (", conditionMessage(df), "). Check the separator",
                    if (!is.null(ids$quote)) " and quote settings", "."))
  }
  if (min_columns > 1) {
    validate(need(ncol(df) >= min_columns && nrow(df) > 0, "The file needs at least two columns and one row of data."))
  } else {
    validate(need(ncol(df) > 0 && nrow(df) > 0, "The file has no data rows."))
  }
  df
}

# A part of a report: its value or, when the tool cannot work it out (an analysis not run yet,
# or a method the data is too thin for), the error that says why, which the report lists in its
# section of what it leaves out (netsimr_report_omitted()) instead of leaving it out unsaid.
netsimr_report_part <- function(expr) tryCatch(expr, error = function(e) e)
