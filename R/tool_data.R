##############
#Reading the tools' data files
##############

# The data files the tools read (delimited text or Excel), their numeric columns, a preview
# of the first rows, and a guess at which column holds what.

# Converts a data column to numbers. Text is trimmed; thousands separators are
# removed ("1,234.5", or "1.234,5" with a decimal comma) and a decimal comma
# becomes a point. Anything else that is not a number becomes NA.
netsimr_as_numeric <- function(x, dec = ".") {
  if (is.numeric(x) || is.logical(x)) return(as.numeric(x))
  x <- as.character(x)
  # text that is not valid in the session's encoding is not a number, and would
  # stop the text functions below (an error in an observer ends the session)
  x[!validUTF8(x)] <- NA
  x <- trimws(x)
  if (identical(dec, ",")) {
    grouped <- grepl("^[-+]?[0-9]{1,3}([.][0-9]{3})+(,[0-9]*)?$", x)
    x[grouped] <- gsub(".", "", x[grouped], fixed = TRUE)
    x <- sub(",", ".", x, fixed = TRUE)
  } else {
    grouped <- grepl("^[-+]?[0-9]{1,3}(,[0-9]{3})+([.][0-9]*)?$", x)
    x[grouped] <- gsub(",", "", x[grouped], fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

# Names of the columns of df that are mostly numbers (at least 90% of the
# non-blank values convert), in their original order.
netsimr_numeric_columns <- function(df, dec = ".") {
  is_numeric <- vapply(df, function(column) {
    values <- netsimr_as_numeric(column, dec)
    # (bytes, not characters: a value need not be valid text)
    present <- !is.na(column) & !grepl("^[[:space:]]*$", as.character(column), useBytes = TRUE)
    any(present) && mean(!is.na(values[present])) >= 0.9
  }, logical(1))
  names(df)[is_numeric]
}

# The first sheet of an Excel workbook (.xlsx or .xls) as a data frame, with readxl, a
# suggested package: its column names as in the file (blanks get a V name, duplicates are
# made unique, as for a text file), dates as dates and empty cells as NA. Without
# readxl, the message says how to read the file.
netsimr_read_excel <- function(path, header = TRUE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("an Excel file needs the readxl package: install.packages(\"readxl\"), or save the sheet as a CSV file",
         call. = FALSE)
  }
  df <- as.data.frame(readxl::read_excel(path, sheet = 1, col_names = isTRUE(header), .name_repair = "minimal"),
                      stringsAsFactors = FALSE)
  column_names <- names(df)
  blank <- is.na(column_names) | column_names == ""
  column_names[blank] <- paste0("V", which(blank))
  names(df) <- make.unique(column_names, sep = "_")
  # date-times without a time of day are dates
  for (column in names(df)) {
    x <- df[[column]]
    if (inherits(x, "POSIXct") && all(is.na(x) | format(x, "%H:%M:%S", tz = "UTC") == "00:00:00")) {
      df[[column]] <- as.Date(x, tz = "UTC")
    }
  }
  df
}

# Reads an uploaded delimited text file. A UTF-8 byte order mark is skipped, so
# it does not end up in the first column name, and a file that is not UTF-8 is
# read as Latin-1 (Excel on a German or French Windows writes CSV files in the
# Windows-1252 code page): read as UTF-8, an accented character makes a string
# that is not valid text, on which the text functions stop. Column names are
# kept as they are (spaces allowed), with blanks and duplicates made unique.
# The header line is read on its own: given one, read.csv() takes the first
# column as row names when the data rows have one more field than the header
# (a separator at the end of each row), which shifts every column onto the
# next one's values. The columns are sized by the widest line, not by the first
# five as read.table() sizes them without a header: a later row with an extra
# field then keeps it in its own column, where its surplus started a new row
# in the first column, a claim that was never in the file.
netsimr_read_data <- function(path, header = TRUE, sep = ",", quote = "\"", dec = ".") {
  # an Excel workbook (an .xlsx file is a zip file; an .xls file an OLE2 compound file)
  magic <- readBin(path, "raw", 4L)
  if (identical(magic, as.raw(c(0x50, 0x4b, 0x03, 0x04))) || identical(magic, as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))) {
    return(netsimr_read_excel(path, header))
  }
  has_bom <- identical(readBin(path, "raw", 3L), as.raw(c(0xef, 0xbb, 0xbf)))
  lines <- readLines(path, warn = FALSE)
  encoding <- if (has_bom) "UTF-8-BOM" else if (all(validUTF8(lines))) "" else "latin1"
  # the number of fields in the widest line from `skip` lines on (NA for a line
  # that ends inside a quoted field)
  widest <- function(skip) {
    fields <- utils::count.fields(path, sep = sep, quote = quote, skip = skip, comment.char = "")
    max(0L, fields, na.rm = TRUE)
  }
  read <- function(skip = 0L, columns, ...) {
    utils::read.csv(
      path, header = FALSE, sep = sep, quote = quote, dec = dec, skip = skip,
      col.names = paste0("V", seq_len(columns)), fill = TRUE,
      check.names = FALSE, stringsAsFactors = FALSE, strip.white = TRUE,
      fileEncoding = encoding, ...
    )
  }
  if (header) {
    # blank lines before the header are skipped, as read.csv() skips them
    # (bytes, not characters: the file need not be UTF-8)
    not_blank <- grepl("[^[:space:]]", lines, useBytes = TRUE)
    blank_lines <- match(TRUE, not_blank, nomatch = 1L) - 1L
    # a header shorter than the data rows leaves blank names, which get a V
    # name below; a header longer than them gives empty columns
    columns <- widest(blank_lines)
    column_names <- unlist(read(blank_lines, columns, nrows = 1, colClasses = "character", na.strings = character(0)),
                           use.names = FALSE)
    df <- tryCatch(read(blank_lines + 1L, columns), error = function(e) {
      # a file with only the header has no data rows, which the tools report
      if (any(not_blank[-seq_len(blank_lines + 1L)])) stop(e)
      as.data.frame(matrix(logical(0), 0, columns))
    })
  } else {
    df <- read(columns = widest(0L))
    column_names <- names(df)
  }
  blank <- is.na(column_names) | column_names == ""
  column_names[blank] <- paste0("V", which(blank))
  names(df) <- make.unique(column_names, sep = "_")
  df
}

# Preview of a data frame: its first max_rows rows and max_cols columns,
# numbered, in a container that scrolls both ways, so that long and wide files
# stay usable. Numbers are aligned right and missing values shown as a dash.
# The table is the markup of netsimr_html_table(), written as one string: built as
# tags, a file of 1,000 columns took 20 seconds.
netsimr_data_preview <- function(df, max_rows = 100, max_cols = 50) {
  rows <- min(nrow(df), max_rows)
  cols <- min(ncol(df), max_cols)
  shown <- df[seq_len(rows), seq_len(cols), drop = FALSE]
  left <- c(TRUE, !vapply(shown, function(column) is.numeric(column) || is.logical(column), logical(1)))
  opening <- function(tag) ifelse(left, paste0("<", tag, " class=\"dft-left\">"), paste0("<", tag, ">"))
  td <- opening("td")
  missing <- paste0("<span class=\"dft-na\">", netsimr_dash, "</span>")
  cells <- lapply(seq_len(cols), function(j) {
    column <- shown[[j]]
    text <- netsimr_escape(as.character(column))
    text[is.na(column)] <- missing
    paste0(td[j + 1], text, "</td>")
  })
  numbers <- paste0(td[1], formatC(seq_len(rows), format = "d", big.mark = ","), "</td>")
  body <- if (rows > 0) paste0(do.call(paste0, c(list("<tr>", numbers), cells, list("</tr>"))), collapse = "") else ""
  header <- paste0(opening("th"), netsimr_escape(c("#", names(shown))), "</th>", collapse = "")
  count <- function(x) format(x, big.mark = ",", scientific = FALSE)
  columns_note <- if (ncol(df) > max_cols) paste(count(max_cols), "of", count(ncol(df)), "columns")
  note <- if (nrow(df) > max_rows) {
    paste0("Showing the first ", count(max_rows), " of ", count(nrow(df)), " rows",
           if (!is.null(columns_note)) paste(" and", columns_note), ".")
  } else {
    paste0(if (nrow(df) == 1) "1 row" else paste("All", count(nrow(df)), "rows"),
           if (!is.null(columns_note)) paste(" and the first", columns_note), ".")
  }
  HTML(paste0(
    "<div class=\"dft-table-wrap dft-table-preview\"><table class=\"dft-table\"><thead><tr>", header,
    "</tr></thead><tbody>", body, "</tbody></table></div><p class=\"dft-table-note\">", netsimr_escape(note), "</p>"
  ))
}

# The column a pattern names first among `columns`, or NULL.
netsimr_guess_column <- function(columns, pattern, exclude = NULL) {
  hits <- setdiff(columns[grepl(pattern, columns, ignore.case = TRUE)], exclude)
  if (length(hits) > 0) hits[1]
}

# A data frame with its text columns that are mostly numbers (netsimr_numeric_columns(), such as
# "1,200" with a thousands separator) converted to numbers.
netsimr_convert_number_columns <- function(df, dec = ".") {
  text_columns <- names(df)[vapply(df, is.character, logical(1))]
  for (column in netsimr_numeric_columns(df[text_columns], dec)) df[[column]] <- netsimr_as_numeric(df[[column]], dec)
  df
}
