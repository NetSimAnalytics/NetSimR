##############
#Report of the reinsurance pricing tool
##############

#' Writes the report of the reinsurance pricing tool
#'
#' @description An HTML page with the pricing of the layer: its key figures, the
#'   settings, and each part of the tool that can be shown (pricing, experience rating,
#'   exposure rating and, once run, the comparison of programmes) with its figures,
#'   tables and charts, as on the tool's tabs.
#' @param file The path of the HTML file to write.
#' @param r A list with the \code{key} figures (tiles: values, or lists of a value and
#'   a note), the \code{settings} (cards of labels and values) and, for each part
#'   (\code{pricing}, \code{experience}, \code{exposure} and \code{programmes};
#'   \code{NULL} when it cannot be shown), a list with its \code{tiles}, its
#'   \code{tables} (each a list with \code{table}, the arguments of \code{rsv_table()},
#'   and a \code{title}) and its \code{charts} (as \code{dft_report_chart_rows()} takes
#'   them). A part that cannot be shown may give the reason in \code{missing}.
#' @param generated The time shown on the report.
#' @return \code{file}, invisibly.
#' @noRd
write_reinsurance_report <- function(file, r, generated = Sys.time()) {
  parts <- list(
    pricing = list(title = "Pricing", intro = "The experience and exposure rates blended, and the technical premium of the layer."),
    experience = list(title = "Experience rating",
                      intro = "The past large losses, brought to the rating year and put through the layer."),
    exposure = list(title = "Exposure rating", intro = "The layer's share of each band's expected loss, from the risk profile."),
    programmes = list(title = "Programmes",
                      intro = "Reinsurance programmes compared by what they cost and the capital they save, on simulated years.")
  )

  key <- list(id = "key-results", title = "Key results", content = tagList(
    netsimr_report_tiles(r$key),
    if (length(r$warnings) > 0) div(class = "callout callout-warning", tags$ul(lapply(r$warnings, tags$li)))
  ))
  settings <- list(id = "settings", title = "Layer and settings", content = netsimr_report_settings(r$settings))

  sections <- lapply(names(parts), function(part) {
    a <- r[[part]]
    if (is.null(a)) return(NULL)
    list(id = part, title = parts[[part]]$title, content = tagList(
      tags$p(class = "section-intro", parts[[part]]$intro),
      if (length(a$tiles) > 0) netsimr_report_tiles(a$tiles),
      lapply(a$tables, function(t) dft_report_table(t$table, t$title)),
      dft_report_chart_rows(a$charts)
    ))
  })

  guide <- list(id = "how-to-read", title = "How to read this report", content = div(class = "callout", tags$ul(
    tags$li("Rates are shares of the subject premium of the rating year; the expected layer loss is the rate times that premium."),
    tags$li("Burning cost: each year's losses are brought to the rating year by the claims inflation, put through the layer ",
            "and developed, over the year's premium brought to the rating year's rate level."),
    tags$li("Pareto: the losses above the Pareto threshold, in the money of the rating year, give the expected number of ",
            "losses and, from the Pareto's alpha, the share of each that falls in the layer."),
    tags$li("Exposure: each band of the risk profile gives its expected loss to the layer, from an exposure curve (property) ",
            "or the limited expected values of a claim size distribution (casualty)."),
    tags$li("The technical premium is the blended expected loss over one minus the loading, split between the upfront ",
            "premium and the expected reinstatement premiums. The rate on line is the upfront premium over the limit, and ",
            "the payback its inverse, in years."),
    tags$li("Programmes: the capital is the mean net result less its result in the bad year of the level chosen; the ",
            "value of a programme is the cost of the capital it saves less the margin it pays the reinsurers.")
  )))

  netsimr_write_report(file, "Reinsurance Pricing Report", "NetTreaty by NetSimR",
                       c(list(key, settings), Filter(Negate(is.null), sections), list(guide)),
                       accent = c(glow = "rgba(251, 113, 133, 0.32)", dark = "#4c0519", middle = "#881337", bright = "#e11d48",
                                  eyebrow = "#fda4af"),
                       generated = generated)
}
