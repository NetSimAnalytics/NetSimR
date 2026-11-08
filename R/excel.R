################
#Excel downloads
################

# Whether the tools can write Excel workbooks: writexl, a suggested package, is installed.
netsimr_can_write_excel <- function() requireNamespace("writexl", quietly = TRUE)

# The button of a tool's Excel download, or nothing without writexl (the CSV downloads
# beside it still work).
netsimr_excel_button <- function(id, label = "All results (Excel)", class = NULL) {
  if (netsimr_can_write_excel()) downloadButton(id, label, class = class)
}

# Writes data frames to an Excel workbook, one sheet each, named by the list's names
# (Excel allows at most 31 characters, and not : \ / ? * [ ]). writexl writes text as text,
# never as a formula, so uploaded values need no escaping, unlike in a CSV file.
netsimr_write_excel <- function(sheets, file) {
  if (!netsimr_can_write_excel()) {
    stop("Excel downloads need the writexl package: install.packages(\"writexl\")", call. = FALSE)
  }
  sheets <- Filter(function(x) is.data.frame(x) && ncol(x) > 0, sheets)
  names(sheets) <- substr(gsub("[]/\\\\?*:[]", " ", names(sheets)), 1, 31)
  # matrices and row names are columns, factors text
  sheets <- lapply(sheets, function(df) {
    df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
    df
  })
  writexl::write_xlsx(sheets, file)
  invisible(file)
}
