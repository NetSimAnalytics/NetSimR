#the tools' fonts come with the package, so that a tool asks no other server for them

test_that("the fonts the tools declare are in the package, with their licences", {
  dir <- system.file("fonts", package = "NetSimR")
  expect_true(nzchar(dir))
  css <- netsimr_font_css(code = TRUE)
  files <- regmatches(css, gregexpr("netsimr-fonts/[a-z-]+\\.woff2", css))[[1]]
  expect_length(files, 8)
  for (file in sub("^netsimr-fonts/", "", files)) {
    path <- file.path(dir, file)
    expect_true(file.exists(path), info = file)
    #a WOFF2 file starts with its signature
    expect_identical(rawToChar(readBin(path, "raw", 4)), "wOF2", info = file)
  }
  expect_true(all(file.exists(file.path(dir, c("LICENSE-Inter.txt", "LICENSE-JetBrainsMono.txt")))))
  #every script of each family, and the code font only when asked for
  expect_identical(lengths(regmatches(css, gregexpr("font-family: 'Inter'", css, fixed = TRUE))), 4L)
  expect_identical(lengths(regmatches(css, gregexpr("font-family: 'JetBrains Mono'", css, fixed = TRUE))), 4L)
  expect_no_match(netsimr_font_css(), "JetBrains Mono", fixed = TRUE)
})

test_that("no tool's page asks another server for anything", {
  skip_if_not_installed("htmltools")
  tools <- list(shiny_simulator_ui, distribution_fitting_tool_UI, GLMFittingToolUI, reserving_tool_UI,
                reinsurance_pricing_tool_UI, portfolio_tool_UI)
  for (ui in tools) {
    rendered <- htmltools::renderTags(ui(NULL))
    page <- paste(as.character(rendered$head), as.character(rendered$html))
    #the fonts are the package's own
    expect_match(page, "netsimr-fonts/inter-latin-wght-normal.woff2", fixed = TRUE)
    expect_no_match(page, "fonts.googleapis.com", fixed = TRUE)
    expect_no_match(page, "fonts.gstatic.com", fixed = TRUE)
    #nothing is loaded from an address on the web: scripts, styles, images and fonts are local
    loads <- regmatches(page, gregexpr("(src|url)\\s*[=(]\\s*[\"']?https?://[^\"') ]+", page))[[1]]
    expect_identical(loads, character(0))
    stylesheets <- regmatches(page, gregexpr("<link[^>]+href=\"https?://[^\"]+", page))[[1]]
    expect_identical(stylesheets, character(0))
    #(a theme's own files, which bslib serves from the computer)
    for (dependency in rendered$dependencies) expect_null(dependency$src$href)
  }
})
