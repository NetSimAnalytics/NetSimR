#' Run the generalised linear model fitting application
#'
#' @description Opens NetGLM, a Shiny application that imports data from a CSV
#'   file or a database query, fits generalised linear models to it, compares the AIC
#'   of stored models, suggests terms by stepwise AIC or BIC, charts actual
#'   against predicted values, validates the model on rows held out for testing
#'   (deviance per row, Gini coefficient, lift and double lift charts) and
#'   downloads the model and its predictions.
#'
#' @details The formula typed in the application, or read from one of its
#'   settings files, is restricted to the columns of the imported data, numbers,
#'   the formula operators and a small set of formula functions: \code{I()},
#'   \code{log()}, \code{exp()}, \code{sqrt()}, \code{pmin()}, \code{pmax()},
#'   \code{poly()}, the splines \code{ns()} and \code{bs()}, \code{cut()} with
#'   \code{c()} for its breaks, \code{factor()}, \code{as.factor()},
#'   \code{interaction()} and \code{offset()}. Any other call is refused, so a
#'   settings file shared between colleagues cannot run code.
#' @return A shiny app object. Printing it, as happens when the function is
#'   called at the console, opens the application; it can also be passed to
#'   \code{shiny::runApp()}.
#' @seealso \code{\link{run_shiny_distribution_fitting_tool}} and
#'   \code{\link{run_shiny_simulator}}, the package's other applications.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_glm_fitting_tool()
#' }
run_shiny_glm_fitting_tool <- function(){
  shinyApp(ui = GLMFittingToolUI, server = GLMFittingToolServer, onStart = shiny_tool_on_start)
}

# Inputs of the GLM fitting tool that its settings file keeps, with how each is
# restored. Passwords and uploaded files are never kept; "column" inputs refer
# to columns of the data and are applied once data with that column is imported.
glm_settings_inputs <- data.frame(
  id = c("data_source", "csv_header", "csv_sep", "csv_dec", "csv_quote",
         "db_type", "db_host", "db_name", "db_port", "windows_auth", "db_user", "sql_query",
         "glm_distribution", "link_function", "offset_log", "formula", "number_of_bands_input", "band_method",
         "response_variable", "offset", "weights", "visualize_variable",
         "holdout_share", "holdout_seed", "select_criterion", "tweedie_power"),
  kind = c("radio", "switch", "radio", "radio", "radio",
           "select", "text", "text", "text", "switch", "text", "textarea",
           "select", "select", "switch", "textarea", "slider", "radio",
           "column", "column", "column", "column",
           "radio", "numeric", "radio", "numeric"),
  stringsAsFactors = FALSE
)

# Inputs that say where the data is: the server, port, database (the file path for
# SQLite), user and query. They disclose internal infrastructure when a settings file is
# shared, so they are saved only when asked for. The database type, Windows
# authentication and the data source are kept: they name no server or table.
glm_settings_connection_ids <- c("db_host", "db_port", "db_name", "db_user", "sql_query")

# The tool and version written in the settings file (see write_settings_file()).
glm_settings_tool <- "GLM fitting tool"
glm_settings_version <- 1L

# Allowed values of the inputs with a fixed set of choices.
glm_settings_choices <- list(
  data_source = c("CSV File", "Database", "Example"),
  csv_sep = c(",", ";", "\t"),
  csv_dec = c(".", ","),
  csv_quote = c("\"", "'", ""),
  db_type = c("SQLite", "SQL Server", "PostgreSQL"),
  glm_distribution = c("gaussian", "poisson", "binomial", "Gamma", "inverse.gaussian", "tweedie"),
  link_function = c("identity", "log", "inverse", "sqrt", "logit", "probit", "cloglog", "cauchit", "1/mu^2"),
  band_method = c("quantile", "width"),
  holdout_share = c("0", "0.1", "0.2", "0.3"),
  select_criterion = c("AIC", "BIC", "lasso", "elastic", "ridge")
)

# The usable values of a settings file (the values of read_settings_file()): a named
# list of the known inputs whose values have the right type (and an allowed value),
# or NULL when there are none. Anything unknown, such as a password, is ignored.
glm_settings_values <- function(inputs) {
  if (!is.list(inputs) || is.null(names(inputs))) return(NULL)
  kinds <- stats::setNames(glm_settings_inputs$kind, glm_settings_inputs$id)
  values <- list()
  for (id in intersect(names(inputs), names(kinds))) {
    value <- inputs[[id]]
    if (!is.atomic(value) || length(value) != 1 || is.na(value)) next
    ok <- switch(
      kinds[[id]],
      switch = is.logical(value),
      slider = is.numeric(value) && is.finite(value),
      numeric = is.numeric(value) && is.finite(value),
      is.character(value)
    )
    if (!ok) next
    if (!is.null(glm_settings_choices[[id]]) && !value %in% glm_settings_choices[[id]]) next
    # a hand-edited file may hold any number of bands; the slider only offers its range
    if (id == "number_of_bands_input") value <- glm_band_count(value)
    if (id == "holdout_seed") value <- glm_holdout_seed(value)
    if (id == "tweedie_power" && (value <= 1 || value >= 2)) next
    values[[id]] <- value
  }
  if (length(values) == 0) NULL else values
}

# The range of the number-of-bands slider of the actual vs predicted chart.
glm_band_range <- c(2, 50)

# A number of bands within the slider's range (10 when there is none): a settings
# file or a client may send any number, and seq(0, 1, length.out = bands + 1) with
# a huge one would exhaust the memory.
glm_band_count <- function(bands) {
  if (!is.numeric(bands) || length(bands) != 1 || !is.finite(bands)) return(10)
  min(max(round(bands), glm_band_range[1]), glm_band_range[2])
}

# Words of a column name that suggest the response of a model; counts come first
# for the families of counts and proportions, amounts for the Gamma and inverse
# Gaussian families.
glm_response_words <- list(
  count = c("count", "counts", "claims"),
  amount = c("amount", "amounts", "loss", "losses"),
  other = c("claim", "response", "target", "y")
)

# The response selected after an import: a column whose name suggests a response
# ("claim_count", "ClaimAmount", "y"), else the last numeric column that is not an
# identifier. Only columns the family can model are chosen: numbers or TRUE/FALSE,
# or text with two values for the binomial family.
glm_default_response <- function(df, family = "gaussian") {
  columns <- names(df)
  if (length(columns) == 0) return(NULL)
  usable <- columns[vapply(df, function(x) {
    ok <- is.numeric(x) || is.logical(x) ||
      (identical(family, "binomial") && (is.character(x) || is.factor(x)) && length(unique(x[!is.na(x)])) == 2)
    ok && any(!is.na(x)) && !glm_id_like(x)
  }, logical(1))]
  # the words of each name: "claim_count", "ClaimCount" and "claim.count" are claim and count
  words <- lapply(usable, function(column) {
    tolower(strsplit(gsub("([a-z])([A-Z])", "\\1 \\2", column), "[^A-Za-z]+")[[1]])
  })
  tiers <- if (family %in% c("Gamma", "inverse.gaussian", "tweedie")) {
    glm_response_words[c("amount", "count", "other")]
  } else {
    glm_response_words[c("count", "amount", "other")]
  }
  for (tier in tiers) {
    named <- usable[vapply(words, function(w) any(w %in% tier), logical(1))]
    if (length(named) > 0) return(named[1])
  }
  numbers <- columns[vapply(df, is.numeric, logical(1))]
  if (length(usable) > 0) usable[length(usable)] else if (length(numbers) > 0) numbers[length(numbers)] else columns[1]
}

# The variable of the chart selected after an import: the first column other than
# the response, offset and weights that is not an identifier and can be drawn (text
# with at most 100 values), else the first other column.
glm_default_variable <- function(df, exclude = character(0)) {
  candidates <- setdiff(names(df), exclude)
  drawable <- candidates[vapply(df[candidates], function(x) {
    present <- x[!is.na(x)]
    length(present) > 0 && !glm_id_like(x) && (is.numeric(x) || length(unique(present)) <= 100)
  }, logical(1))]
  if (length(drawable) > 0) drawable[1] else if (length(candidates) > 0) candidates[1] else "None"
}

# The column choices after an import: each choice already made (or loaded from a
# settings file) that the data has, and otherwise the defaults above. wanted is a
# named list of the current choices, by input id.
glm_column_choices <- function(df, family = "gaussian", wanted = list()) {
  columns <- names(df)
  keep <- function(id, default) {
    value <- wanted[[id]]
    if (is.character(value) && length(value) == 1 && value %in% columns) value else default
  }
  response <- keep("response_variable", glm_default_response(df, family))
  offset <- keep("offset", "None")
  weights <- keep("weights", "None")
  variable <- keep("visualize_variable", glm_default_variable(df, setdiff(c(response, offset, weights), "None")))
  list(response_variable = response, offset = offset, weights = weights, visualize_variable = variable)
}

# Database driver packages used by the GLM fitting tool. They are in Suggests,
# so a user who only imports CSV files does not need to install them.
glm_tool_db_packages <- c(
  "SQLite" = "RSQLite",
  "SQL Server" = "RODBC",
  "PostgreSQL" = "RPostgres"
)

# Checks that the packages needed for the selected database type are installed.
# Returns TRUE when they are; otherwise shows a modal explaining what to install
# and returns FALSE.
glm_tool_db_package_available <- function(db_type) {
  pkg <- glm_tool_db_packages[db_type]
  if (is.na(pkg)) {
    showModal(modalDialog(
      title = "Unknown database type",
      paste0("The database type '", db_type, "' is not supported."),
      easyClose = TRUE
    ))
    return(FALSE)
  }
  # the DBI drivers also need DBI itself; RODBC is self-contained
  needed <- if (pkg == "RODBC") pkg else c("DBI", pkg)
  missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    showModal(modalDialog(
      title = "Package required",
      paste0(
        "Install the ", paste(missing_pkgs, collapse = " and "), " package",
        if (length(missing_pkgs) > 1) "s" else "",
        " to connect to ", db_type, " databases, for example with install.packages(",
        paste0("\"", missing_pkgs, "\"", collapse = ", "), ")."
      ),
      easyClose = TRUE
    ))
    return(FALSE)
  }
  TRUE
}

# Runs a query through a DBI driver and always closes the connection.
glm_tool_query_dbi <- function(driver, sql, ...) {
  con <- DBI::dbConnect(driver, ...)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql)
}

# A value for an ODBC connection string, in braces (with any closing brace
# doubled), so that a ; in a host, user or password cannot add attributes.
glm_tool_odbc_value <- function(x) {
  paste0("{", gsub("}", "}}", if (is.null(x)) "" else x, fixed = TRUE), "}")
}

# Runs a query through an ODBC connection string and always closes the connection.
glm_tool_query_odbc <- function(connection_string, sql) {
  con <- RODBC::odbcDriverConnect(connection = connection_string)
  # odbcDriverConnect() returns -1 with a warning instead of an error when it cannot connect
  if (!inherits(con, "RODBC")) stop("could not connect to the database")
  on.exit(RODBC::odbcClose(con), add = TRUE)
  result <- RODBC::sqlQuery(con, sql)
  # RODBC returns a character vector with the error text instead of signalling an error
  if (!is.data.frame(result)) stop(paste(result, collapse = "\n"))
  result
}
