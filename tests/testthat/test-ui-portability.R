#the Shiny tools must not keep paths of the machine the package was built on. The simulator and
#distribution fitting tool built their pages when the package was installed, and the pages kept
#bslib's theme with absolute paths to Bootstrap's SCSS files on that machine; installed from a
#CRAN binary on Windows, the tools stopped with "File to import not found or unreadable:
#D:\RCompile\CRANpkg\lib\4.6\bslib\lib\bs5\scss\_functions.scss", CRAN's build machine

#every character string reachable from an object, without entering functions
portability_strings <- function(x, depth = 0) {
  if (depth > 60 || is.function(x)) return(character(0))
  found <- if (is.character(x)) x else character(0)
  children <- if (is.environment(x)) {
    mget(ls(x, all.names = TRUE), envir = x)
  } else if (is.list(x) || is.pairlist(x)) {
    as.list(x)
  } else {
    list()
  }
  for (child in c(children, attributes(x))) found <- c(found, portability_strings(child, depth + 1))
  found
}

test_that("no object of the package holds a path to bslib's SCSS files", {
  ns <- asNamespace("NetSimR")
  holders <- Filter(function(name) {
    strings <- portability_strings(get(name, envir = ns))
    any(grepl("scss", strings, fixed = TRUE) & grepl("bslib", strings, fixed = TRUE))
  }, ls(ns, all.names = TRUE))
  expect_identical(holders, character(0))
})

test_that("each tool builds its page when it runs, with the bslib of the running machine", {
  #(rendering resolves the theme's dependency, which compiles its SCSS files)
  skip_if_not_installed("htmltools")
  bslib_dir <- normalizePath(system.file(package = "bslib"), winslash = "/")
  for (ui in list(shiny_simulator_ui, distribution_fitting_tool_UI, GLMFittingToolUI, reserving_tool_UI,
                  reinsurance_pricing_tool_UI)) {
    expect_true(is.function(ui))
    page <- ui(NULL)
    #the theme's SCSS paths are those of the bslib this session uses
    imports <- grep("_functions.scss", portability_strings(page), fixed = TRUE, value = TRUE)
    expect_gt(length(imports), 0)
    paths <- normalizePath(sub('^.*"(.*)".*$', "\\1", gsub("\\\\", "/", imports)), winslash = "/", mustWork = FALSE)
    expect_true(all(startsWith(paths, bslib_dir)))
    #and the page renders, which compiles the theme
    expect_no_error(htmltools::renderTags(page))
  }
})
