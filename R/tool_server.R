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
    # the upload limit is generous (shiny_tool_max_request_size): what a tool can read is set by
    # the memory of the computer, or of the browser on the website
    if (grepl("cannot allocate|memory", conditionMessage(df), ignore.case = TRUE)) {
      validate(paste0("The file is too large for the memory available (", conditionMessage(df), "). Use fewer rows or ",
                      "columns, or, on the website, the tool installed in R, which can use all of the computer's memory."))
    }
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

# The values a tool sets in its own inputs (loading a settings file, say), so that an observer of
# such an input can tell the input reporting a value the tool set, which it must not act on as
# if the user had chosen it, from a change the user made. expect(id, value) notes a value the
# tool sets; is_echo(id, value), when the input reports a value, is TRUE for the first value
# expected (which it then forgets) and FALSE for any other, after which nothing more is expected
# of that input: the user changed it; waiting(id) gives the values still expected. Values are
# compared as text, as the page sends them.
netsimr_own_updates <- function() {
  expected <- list()
  list(
    expect = function(id, value) {
      expected[[id]] <<- c(expected[[id]], as.character(value))
      invisible(NULL)
    },
    is_echo = function(id, value) {
      queue <- expected[[id]]
      if (length(queue) > 0 && length(value) == 1 && identical(queue[1], as.character(value))) {
        expected[[id]] <<- queue[-1]
        return(TRUE)
      }
      expected[[id]] <<- NULL
      FALSE
    },
    waiting = function(id) expected[[id]]
  )
}
