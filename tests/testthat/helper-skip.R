#The Shiny server tests (shiny::testServer()) take most of the suite's time. They run on
#continuous integration and locally, but not on CRAN, whose check time is limited; the
#package's functions are tested there in full, and the browser tests skip themselves on CRAN
#(skip_if_no_app_browser()).
skip_server_on_cran <- function() testthat::skip_on_cran()
