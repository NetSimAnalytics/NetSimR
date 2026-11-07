##############
#Running in a web browser
##############

# TRUE when R runs in a web browser, with webR (R compiled to WebAssembly), as the tools on
# the package's website do: there, worker R processes cannot be started and databases
# cannot be reached, so the tools leave out the options that need them.
netsimr_in_browser <- function() {
  identical(R.version$os, "emscripten") || grepl("emscripten", R.version$platform, fixed = TRUE)
}
