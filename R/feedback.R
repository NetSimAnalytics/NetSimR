##############
#Feedback from the tools
##############

# Where the package's issues are reported: the GitHub repository's issue forms
# (.github/ISSUE_TEMPLATE), a bug report and a feature request.
netsimr_issues_url <- "https://github.com/NetSimAnalytics/NetSimR/issues/new"

# The link of an issue form, with the tool and the version of NetSimR filled in, so a
# report says where it comes from. Nothing else is sent: the form opens in the browser,
# where the user writes the report and chooses whether to submit it.
netsimr_issue_link <- function(template, tool) {
  version <- tryCatch(as.character(utils::packageVersion("NetSimR")), error = function(e) "")
  query <- c(template = template, tool = tool, version = version)
  paste0(netsimr_issues_url, "?",
         paste(names(query), vapply(query, utils::URLencode, character(1), reserved = TRUE), sep = "=", collapse = "&"))
}

# The feedback links of a tool's About card: report a problem, or suggest a feature.
netsimr_feedback_links <- function(tool) {
  link <- function(label, template, icon_name) {
    a(class = "btn btn-outline-secondary btn-sm netsimr-feedback-link", href = netsimr_issue_link(template, tool),
      target = "_blank", rel = "noopener noreferrer", icon(icon_name), label)
  }
  div(class = "netsimr-feedback d-flex flex-wrap gap-2 mb-2",
      link("Report a problem", "bug_report.yml", "bug"),
      link("Suggest a feature", "feature_request.yml", "lightbulb"))
}
