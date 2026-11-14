#' Run the claims simulator application
#'
#' Opens NetSimulator, a Shiny app for running the frequency-severity model of
#' \code{\link{simulate_function}} from a form, with charts, a report and saved settings,
#' without writing any code.
#'
#' @return A shiny app object (class \code{shiny.appobj}). Printing it, as happens when
#'   \code{run_shiny_simulator()} is called at the console, opens the app; pass it to
#'   \code{shiny::runApp()} to choose options such as the port.
#' @seealso \code{\link{simulate_claims}} and \code{\link{simulate_function}}, which run
#'   the same model from R code.
#' @export
#' @examples
#' if (interactive()) {
#'   run_shiny_simulator()
#' }
run_shiny_simulator <- function() {
  shinyApp(ui = shiny_simulator_ui, server = shiny_simulator_server)
}
