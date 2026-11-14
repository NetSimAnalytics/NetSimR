##############
#About the package, from the tools
##############

# The package's website, its DOI (CRAN's), repository and issue forms (.github/ISSUE_TEMPLATE: a bug report
# and a feature request).
netsimr_website_url <- "https://netsimanalytics.github.io/NetSimR/"
netsimr_doi_url <- "https://doi.org/10.32614/CRAN.package.NetSimR"
netsimr_github_url <- "https://github.com/NetSimAnalytics/NetSimR"
netsimr_issues_url <- "https://github.com/NetSimAnalytics/NetSimR/issues/new"

# The files the tools' uploads accept: text files of values separated by commas, semicolons
# or tabs, and Excel workbooks (read with readxl, see netsimr_read_data()).
netsimr_data_accept <- c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt", ".tsv",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         "application/vnd.ms-excel", ".xlsx", ".xls")

# The version of NetSimR running, "" when it cannot be found.
netsimr_version <- function() {
  tryCatch(as.character(utils::packageVersion("NetSimR")), error = function(e) "")
}

# The year of the version running (when it was published or built), else this year.
netsimr_year <- function() {
  date <- tryCatch(suppressWarnings(utils::packageDate("NetSimR")), error = function(e) NA)
  if (length(date) == 1 && !is.na(date)) format(date, "%Y") else format(Sys.Date(), "%Y")
}

# How to cite the package, as inst/CITATION gives it (citation("NetSimR")): the author, the
# year of the version, the title, the version and the DOI that CRAN gives the package.
netsimr_citation_text <- function(version = netsimr_version(), year = netsimr_year()) {
  paste0("Parizas, Y. (", year, "). NetSimR: Non-Life Actuarial Pricing, Reserving, Reinsurance and Capital. ",
         "R package", if (nzchar(version)) paste0(" version ", version), ". ", netsimr_doi_url)
}

# The link of an issue form, with the tool and the version of NetSimR filled in, so a
# report says where it comes from. Nothing else is sent: the form opens in the browser,
# where the user writes the report and chooses whether to submit it.
netsimr_issue_link <- function(template, tool) {
  query <- c(template = template, tool = tool, version = netsimr_version())
  paste0(netsimr_issues_url, "?",
         paste(names(query), vapply(query, utils::URLencode, character(1), reserved = TRUE), sep = "=", collapse = "&"))
}

# A link of the About card, as a small outlined button that opens in a new tab.
netsimr_link_button <- function(label, href, icon_name) {
  a(class = "btn btn-outline-secondary btn-sm netsimr-feedback-link", href = href, target = "_blank",
    rel = "noopener noreferrer", icon(icon_name), label)
}

# The feedback links of a tool: report a problem, or suggest a feature.
netsimr_feedback_links <- function(tool) {
  tagList(
      netsimr_link_button("Report a problem", netsimr_issue_link("bug_report.yml", tool), "bug"),
      netsimr_link_button("Suggest a feature", netsimr_issue_link("feature_request.yml", tool), "lightbulb"))
}

# The About card of every tool's Welcome tab: who made the package, its website, the tools
# in the browser and its repository, the feedback forms, how to cite it, and `note`, what
# the tool's functions are in R.
netsimr_about_card <- function(tool, note = NULL) {
  version <- netsimr_version()
  bslib::card(
    netsimr_card_header("user", "About", paste0("NetSimR", if (nzchar(version)) paste0(" ", version),
                                             ". Feedback and bug reports are very welcome.")),
    bslib::card_body(
      p(
        "Created by Yiannis Parizas. For more information, visit my ",
        # (no space between a link and the full stop after it)
        a("LinkedIn profile", href = "https://www.linkedin.com/in/yiannisparizas/", target = "_blank", rel = "noopener noreferrer",
          .noWS = "after"),
        ". You can also reach me by email at ",
        a("yiannis.parizas@gmail.com", href = "mailto:yiannis.parizas@gmail.com", .noWS = "after"),
        "."
      ),
      div(class = "netsimr-links d-flex flex-wrap gap-2 mb-2",
          netsimr_link_button("Website", netsimr_website_url, "book-open"),
          netsimr_link_button("All six tools online", paste0(netsimr_website_url, "articles/try-online.html"), "play"),
          netsimr_link_button("GitHub", netsimr_github_url, "github"),
          netsimr_link_button("Custom tools", paste0(netsimr_website_url, "articles/custom-tools.html"), "handshake"),
          netsimr_feedback_links(tool)),
      p(class = "sim-muted mb-2",
        "If the tools help your work, a star on GitHub or a mention to a colleague helps others find them."),
      p(class = "sim-muted mb-2", tags$strong("Cite: "), netsimr_citation_text(version)),
      if (!is.null(note)) p(class = "sim-muted mb-0", note)
    )
  )
}

# The footer of a report: the version of NetSimR that made it, with a link to the website,
# and when.
netsimr_report_footer <- function(generated_text) {
  version <- netsimr_version()
  tags$footer(class = "report-footer",
              paste0("Generated with NetSimR", if (nzchar(version)) paste0(" ", version), " ", intToUtf8(183), " "),
              a(href = netsimr_website_url, target = "_blank", rel = "noopener noreferrer", "netsimanalytics.github.io/NetSimR"),
              paste0(" ", intToUtf8(183), " ", generated_text))
}
