#the About card of the tools, the citation and the reports' footer

test_that("the issue forms open with the tool and the version filled in", {
  link <- netsimr_issue_link("bug_report.yml", "Portfolio capital tool")
  expect_match(link, "^https://github.com/NetSimAnalytics/NetSimR/issues/new[?]template=bug_report.yml&")
  expect_match(link, "tool=Portfolio%20capital%20tool", fixed = TRUE)
  expect_match(link, paste0("version=", netsimr_version()), fixed = TRUE)
})

test_that("the citation of the tools is the package's", {
  text <- netsimr_citation_text("0.4.0", "2026")
  expect_identical(text, paste("Parizas, Y. (2026). NetSimR: Actuarial Functions for Non-Life Insurance Modelling.",
                               "R package version 0.4.0. https://netsimanalytics.github.io/NetSimR/"))
  citation_file <- system.file("CITATION", package = "NetSimR")
  skip_if(!nzchar(citation_file), "the CITATION file is not installed")
  entry <- utils::readCitationFile(citation_file, meta = list(Version = "0.4.0", `Date/Publication` = "2026-12-01 10:00:00 UTC"))
  expect_true(nzchar(format(entry, style = "text")))
  expect_identical(attr(unclass(entry)[[1]], "textVersion"), text)
})

test_that("every tool's About card links the website, the tools online and GitHub, and the reports link the website", {
  skip_if_not_installed("htmltools")
  for (ui in list(shiny_simulator_ui, distribution_fitting_tool_UI, GLMFittingToolUI, reserving_tool_UI,
                  reinsurance_pricing_tool_UI, portfolio_tool_UI)) {
    page <- as.character(htmltools::renderTags(ui(NULL))$html)
    expect_match(page, "Cite: ", fixed = TRUE)
    expect_match(page, "https://netsimanalytics.github.io/NetSimR/articles/try-online.html", fixed = TRUE)
    expect_match(page, "https://github.com/NetSimAnalytics/NetSimR\"", fixed = TRUE)
    expect_match(page, "issues/new?template=bug_report.yml", fixed = TRUE)
  }
  footer <- as.character(netsimr_report_footer("1 December 2026, 10:00"))
  expect_match(footer, "href=\"https://netsimanalytics.github.io/NetSimR/\"", fixed = TRUE)
  expect_match(footer, "Generated with NetSimR", fixed = TRUE)
})
