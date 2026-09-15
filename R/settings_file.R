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
    x <- if (is.null(x)) x else stats::setNames(as.vector(x), names(x))
    paste(deparse(x, width.cutoff = 500L, control = c("keepNA", "keepInteger", "niceNames")), collapse = " ")
  }, character(1))
  record <- c(NetSimRSettings = tool, SettingsVersion = as.character(version), text)
  # keep.white stops write.dcf() folding long values over several lines
  write.dcf(as.data.frame(as.list(record), check.names = FALSE, stringsAsFactors = FALSE),
            file = file, keep.white = names(record))
  invisible(file)
}

# Reads a settings file written by write_settings_file(). Returns list(tool, version,
# values), or stops with a message for a file that is not a settings file of this tool
# (such as an .rds file) or has a value that is not a literal.
read_settings_file <- function(file, tool) {
  fields <- tryCatch(
    suppressWarnings(read.dcf(file, all = TRUE)),
    error = function(e) NULL
  )
  if (!is.data.frame(fields) || nrow(fields) != 1 || !"NetSimRSettings" %in% names(fields)) {
    stop("This is not a NetSimR settings file.")
  }
  record <- vapply(fields, function(x) as.character(x)[1], character(1))
  if (!identical(unname(record[["NetSimRSettings"]]), tool)) {
    stop("This settings file is for the ", record[["NetSimRSettings"]], ", not the ", tool, ".")
  }
  version <- suppressWarnings(as.numeric(record[["SettingsVersion"]]))
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
