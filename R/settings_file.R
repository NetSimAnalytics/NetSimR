################
#Settings files of the Shiny tools
################

# The tools save their settings as plain text, one "field: value" line per setting (a
# Debian control file, see write.dcf()), not with saveRDS(): a text file cannot carry code,
# whereas readRDS() of a crafted file can run code on R before 4.4.0 (CVE-2024-27322), and
# people can read and compare the file. Each value is written with deparse() and read back
# by settings_literal(), which accepts only literal constants and c() of them, so reading a
# file never evaluates code.

# The value of a parsed setting: numbers, strings, logicals, NA, NULL, Inf and NaN, negative
# numbers, integer ranges (deparse() writes 1L, 2L, 3L as 1:3) and c() of these, with names.
# Anything else is an error.
settings_literal <- function(expr) {
  if (is.null(expr) || (is.atomic(expr) && length(expr) == 1)) return(expr)
  if (is.name(expr)) {
    name <- as.character(expr)
    if (name == "Inf") return(Inf)
    if (name == "NaN") return(NaN)
  }
  if (is.call(expr) && is.name(expr[[1]])) {
    fun <- as.character(expr[[1]])
    args <- as.list(expr)[-1]
    if (fun == "c") {
      parts <- lapply(args, settings_literal)
      if (!all(vapply(parts, function(x) is.null(x) || is.atomic(x), logical(1)))) stop("not a literal value")
      # ranges are held to a million values each; so is what they are joined into, or a file
      # could ask for tens of millions and hold the session while they are built
      if (sum(vapply(parts, length, numeric(1))) > 1e6) stop("not a literal value")
      return(do.call(c, parts))
    }
    if (fun %in% c("-", "+") && length(args) == 1) {
      value <- settings_literal(args[[1]])
      if (is.numeric(value) && length(value) == 1) return(if (fun == "-") -value else value)
    }
    if (fun == ":" && length(args) == 2) {
      from <- settings_literal(args[[1]])
      to <- settings_literal(args[[2]])
      if (is.numeric(from) && is.numeric(to) && length(from) == 1 && length(to) == 1 &&
          is.finite(from) && is.finite(to) && abs(to - from) <= 1e6) {
        return(from:to)
      }
    }
  }
  stop("not a literal value")
}

# The text of one setting. deparse() writes doubles with 15 significant digits, which keeps a
# typed 0.3 as 0.3 but loses the last bit of a value such as a third; a value whose 15 digits
# do not read back as exactly the same double is written with 17 instead, so that a seeded
# run loaded from its own settings file gives the same results. 17 digits name a double
# exactly, though a platform built without long double reads such text back up to a bit out,
# which no decimal text can mend.
settings_text <- function(x) {
  control <- c("keepNA", "keepInteger", "niceNames")
  text <- paste(deparse(x, width.cutoff = 500L, control = control), collapse = " ")
  if (is.double(x)) {
    back <- tryCatch(settings_literal(parse(text = text, keep.source = FALSE)[[1]]), error = function(e) NULL)
    if (!identical(back, x)) text <- paste(deparse(x, width.cutoff = 500L, control = c(control, "digits17")), collapse = " ")
  }
  text
}

# Writes a named list of settings (each a vector of numbers, strings or logicals, or NULL) to
# a text file, after two header fields naming the tool and the settings version.
write_settings_file <- function(values, file, tool, version) {
  ids <- names(values)
  if (length(values) && (is.null(ids) || any(!grepl("^[A-Za-z][A-Za-z0-9._-]*$", ids)))) {
    stop("settings need names made of letters, digits, '.', '_' and '-'")
  }
  ok <- vapply(values, function(x) is.null(x) || (is.atomic(x) && !is.factor(x)), logical(1))
  if (!all(ok)) stop("settings must be vectors of numbers, strings or logicals: ", paste(ids[!ok], collapse = ", "))
  text <- vapply(values, function(x) {
    settings_text(if (is.null(x)) x else stats::setNames(as.vector(x), names(x)))
  }, character(1))
  record <- c(NetSimRSettings = tool, SettingsVersion = as.character(version), text)
  # keep.white stops write.dcf() folding long values over several lines
  write.dcf(as.data.frame(as.list(record), check.names = FALSE, stringsAsFactors = FALSE),
            file = file, keep.white = names(record))
  invisible(file)
}

# Reads a settings file written by write_settings_file(). Returns list(tool, version,
# values), or stops with a message for a file that is not a settings file of this tool
# (such as an .rds file), has been edited into more than one block or a repeated setting,
# or has a value that is not a literal.
read_settings_file <- function(file, tool) {
  fields <- tryCatch(
    suppressWarnings(read.dcf(file, all = TRUE)),
    error = function(e) NULL
  )
  if (!is.data.frame(fields) || nrow(fields) == 0 || !"NetSimRSettings" %in% names(fields)) {
    stop("This is not a NetSimR settings file.")
  }
  # a blank line between settings starts a second record
  if (nrow(fields) > 1) {
    stop("The settings file has more than one block of settings; remove any blank lines between the settings.")
  }
  # a setting given twice comes back as a list of both values, which no setting may hold
  repeated <- vapply(fields, function(x) length(x[[1]]) > 1, logical(1))
  if (any(repeated)) stop("'", names(fields)[repeated][1], "' is given more than once in the settings file.")
  record <- vapply(fields, function(x) as.character(x[[1]]), character(1))
  if (!identical(unname(record[["NetSimRSettings"]]), tool)) {
    stop("This settings file is for the ", record[["NetSimRSettings"]], ", not the ", tool, ".")
  }
  version <- if ("SettingsVersion" %in% names(record)) suppressWarnings(as.numeric(record[["SettingsVersion"]])) else NA_real_
  if (length(version) != 1 || is.na(version)) stop("The settings file has no valid version.")
  ids <- setdiff(names(record), c("NetSimRSettings", "SettingsVersion"))
  values <- lapply(stats::setNames(ids, ids), function(id) {
    tryCatch(
      settings_literal(parse(text = record[[id]], keep.source = FALSE)[[1]]),
      error = function(e) stop("The setting '", id, "' has a value that cannot be read.", call. = FALSE)
    )
  })
  list(tool = unname(record[["NetSimRSettings"]]), version = version, values = values)
}

# The values of a settings file loaded into a tool, or NULL after telling the user why the file
# could not be loaded: it is not a settings file of the tool, or a newer NetSimR saved it.
load_settings_values <- function(file, tool, version) {
  settings <- tryCatch(read_settings_file(file, tool), error = function(e) e)
  problem <- if (inherits(settings, "error")) {
    conditionMessage(settings)
  } else if (settings$version > version) {
    "it was saved by a newer version of NetSimR."
  }
  if (!is.null(problem)) {
    showNotification(paste("The settings could not be loaded:", problem), type = "error", duration = 10)
    return(NULL)
  }
  settings$values
}

# Whether a loaded value can be given to an input of a kind (as a tool's settings inputs list
# them): one value, of the input's type, and one of its choices when it has them.
setting_usable <- function(x, kind, choices = NULL) {
  if (is.null(x) || length(x) != 1) return(FALSE)
  switch(kind,
         radio = , select = is.character(x) && (is.null(choices) || x %in% choices),
         switch = is.logical(x) && !is.na(x),
         numeric = is.numeric(x) || (is.logical(x) && is.na(x)),
         slider = is.numeric(x) && is.finite(x),
         column = is.character(x) && !is.na(x) && nzchar(x),
         is.character(x))
}

# Gives an input of a kind a loaded value. A column is chosen once the data with it is read,
# so it is left to the tool.
update_setting_input <- function(session, id, kind, x) {
  switch(kind,
         radio = updateRadioButtons(session, id, selected = x),
         select = updateSelectInput(session, id, selected = x),
         switch = bslib::update_switch(id, value = x, session = session),
         checkbox = updateCheckboxInput(session, id, value = x),
         numeric = updateNumericInput(session, id, value = if (is.logical(x)) NA else x),
         slider = updateSliderInput(session, id, value = x),
         text = updateTextInput(session, id, value = x),
         column = NULL,
         stop("Unknown input kind: ", kind))
  invisible(NULL)
}

# The values of a tool's settings inputs to save, from `kinds` (the ids and kinds of its
# settings inputs): each input that holds one value. An empty column select (no file read yet)
# is left out; an empty radio value is kept, as it is a choice (the quote option None).
collect_setting_values <- function(input, kinds) {
  values <- list()
  for (id in names(kinds)) {
    x <- input[[id]]
    if (is.null(x) || !is.atomic(x) || length(x) != 1) next
    if (kinds[[id]] == "column" && (is.na(x) || !nzchar(x))) next
    values[[id]] <- x
  }
  values
}

# The choices of the radio buttons and selects of a page (a tag tree), by input id: the values a
# loaded setting is checked against, read from the page so that the two cannot differ. A select
# whose choices the server sets has those of the page when it starts.
page_input_choices <- function(page) {
  found <- list()
  unescape <- function(x) {
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
    x <- gsub("&#39;", "'", x, fixed = TRUE)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    gsub("&amp;", "&", x, fixed = TRUE)
  }
  walk <- function(x) {
    if (inherits(x, "shiny.tag")) {
      attribs <- x$attribs
      if (identical(x$name, "input") && identical(attribs$type, "radio")) {
        found[[attribs$name]] <<- c(found[[attribs$name]], attribs$value)
      } else if (identical(x$name, "select")) {
        # (selectInput() writes its options as one piece of HTML)
        html <- paste(vapply(x$children, as.character, character(1)), collapse = "")
        options <- regmatches(html, gregexpr("<option value=\"[^\"]*\"", html))[[1]]
        found[[attribs$id]] <<- unescape(sub("^<option value=\"(.*)\"$", "\\1", options))
      }
      lapply(x$children, walk)
    } else if (is.list(x)) {
      lapply(x, walk)
    }
    invisible(NULL)
  }
  walk(page)
  found
}

# The choices of a tool's page (page_input_choices()), read once for each tool: `tool` names it
# and `ui` is the function that builds its page.
tool_input_choices <- function(tool, ui) {
  if (is.null(tool_input_choices_read[[tool]])) tool_input_choices_read[[tool]] <- page_input_choices(ui(NULL))
  tool_input_choices_read[[tool]]
}
tool_input_choices_read <- new.env(parent = emptyenv())
