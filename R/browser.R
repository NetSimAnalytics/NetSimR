##############
#Running in a web browser
##############

# TRUE when R runs in a web browser, with webR (R compiled to WebAssembly), as the tools on
# the package's website do: there, worker R processes cannot be started and databases
# cannot be reached, so the tools leave out the options that need them.
netsimr_in_browser <- function() {
  identical(R.version$os, "emscripten") || grepl("emscripten", R.version$platform, fixed = TRUE)
}

# The note under a data upload: where the file is read. On the website the tools run in the
# R of the page (webR), on the visitor's computer, and the website is static pages with no
# server to receive a file. From R, the file is read by the R session that runs the tool:
# the user's own computer, or the server of whoever hosts the tool, and nowhere else.
netsimr_private_note <- function() {
  text <- if (netsimr_in_browser()) {
    list(tags$strong("Private."), "The file is read by the R running in this page, on your computer: it is not sent anywhere.")
  } else {
    list("The file is read by the R session that runs this tool, and is not sent anywhere else.")
  }
  div(class = "dft-help netsimr-private", style = "margin: -0.35rem 0 0.9rem 0;", icon("lock"), do.call(div, text))
}
