##############
#Report of the portfolio capital tool
##############

# Writes the portfolio tool's report: the capital of the portfolio, its lines and their
# dependence, each line's figures and capital, and the charts. `portfolio` is the result of
# simulate_portfolio(), `spec` the run's settings (the lines described, the source, the
# seed).
write_portfolio_report <- function(file, portfolio, spec, generated = Sys.time()) {
  p <- portfolio
  s <- p$summary
  total <- s[s$line == "total", ]
  lines <- s[s$line != "total", ]
  d <- p$diversification
  label <- paste0(format(100 * p$level), "%")
  frame <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)

  key <- list(id = "key-results", title = "Key results", content = tagList(
    tags$p(class = "section-intro", paste0(nrow(lines), " lines, ", format(p$n, big.mark = ",", scientific = FALSE), " simulated years, ",
                                           if (p$copula == "t") paste0("t copula with ", format(p$df), " degrees of freedom") else
                                             "Gaussian copula", ".")),
    netsimr_report_tiles(stats::setNames(list(
      list(value = netsimr_amount(total$mean), note = paste("Standard deviation", netsimr_amount(total$sd))),
      list(value = netsimr_amount(total$var), note = "Of the total"),
      list(value = netsimr_amount(total$tvar), note = "The mean beyond the VaR"),
      list(value = netsimr_amount(d[["benefit"]]), note = paste0(netsimr_percent(d[["benefit_share"]]), " of the lines' VaR")),
      list(value = netsimr_amount(d[["stand_alone_var"]]), note = "The sum of the lines' own VaR")
    ), c("Mean total loss", paste("VaR", label), paste("TVaR", label), "Diversification benefit", "Lines' VaR")),
    tail = c(paste("VaR", label), paste("TVaR", label)))
  ))

  described <- spec$described
  line_rows <- if (!is.null(described)) {
    frame(Line = names(described),
          `Claim count` = vapply(described, function(l) paste0(l$frequency, " (", paste(format(l$frequency_params), collapse = ", "), ")"),
                                 character(1)),
          `Claim size` = vapply(described, function(l) paste0(l$severity, " (", paste(format(l$severity_params), collapse = ", "), ")"),
                                character(1)))
  } else {
    frame(Line = lines$line, Source = rep(spec$source, nrow(lines)))
  }
  R <- p$correlation
  corr <- frame(Line = rownames(R), stats::setNames(as.data.frame(formatC(R, format = "f", digits = 2), stringsAsFactors = FALSE),
                                                    colnames(R)))
  model <- list(id = "model", title = "Lines and dependence", content = tagList(
    div(class = "two-col",
        netsimr_report_table(line_rows, text_cols = seq_along(line_rows), title = "Lines",
                             note = if (!is.null(described)) "Each line simulated with simulate_claims(): its claim count and claim size distributions, with their parameters." else
                               paste("Simulated losses uploaded from", spec$source, ".")),
        netsimr_report_table(corr, title = "Correlations of the copula",
                             note = "The correlations of the copula's normal draws, which give the ranks of the lines' losses.")),
    netsimr_report_settings(list(Simulation = list(
      Simulations = format(p$n, big.mark = ",", scientific = FALSE), Level = label,
      Copula = if (p$copula == "t") paste0("t, ", format(p$df), " degrees of freedom") else "Gaussian",
      Seed = if (is.null(spec$seed)) "None" else format(spec$seed))))
  ))

  by_line <- rbind(frame(Line = lines$line, Mean = netsimr_amount(lines$mean), `Standard deviation` = netsimr_amount(lines$sd),
                         VaR = netsimr_amount(lines$var), TVaR = netsimr_amount(lines$tvar), Capital = netsimr_amount(lines$capital),
                         Share = netsimr_percent(lines$capital_share)),
                   frame(Line = "Total", Mean = netsimr_amount(total$mean), `Standard deviation` = netsimr_amount(total$sd),
                         VaR = netsimr_amount(total$var), TVaR = netsimr_amount(total$tvar), Capital = netsimr_amount(total$capital),
                         Share = netsimr_percent(total$capital_share)))
  names(by_line)[4:5] <- paste(c("VaR", "TVaR"), label)
  capital <- list(id = "capital", title = "Capital by line", content = tagList(
    netsimr_report_table(by_line, note = paste(
      "Each line's VaR and TVaR are its own. Its capital is its mean loss in the years when the total is at or beyond",
      "its VaR (co-TVaR, the Euler allocation of the TVaR), so the capitals add up to the total's TVaR.")),
    netsimr_report_chart(function(dark) pfo_capital_chart(s, p$level, dark), "Each line's own VaR and its allocated capital",
                         title = "Capital by line")
  ))

  pair <- utils::head(lines$line, 2)
  charts <- list(id = "charts", title = "Charts", content = tagList(
    netsimr_report_chart(function(dark) pfo_total_chart(p$losses$total, total$var, total$tvar, dark),
                         "Distribution of the portfolio's total losses", title = "Total losses",
                         note = "Dashed lines mark the mean and the VaR, the dotted line the TVaR."),
    netsimr_report_chart(function(dark) pfo_rank_chart(p$losses[[pair[1]]], p$losses[[pair[2]]], pair[1], pair[2], dark),
                         paste("Ranks of", pair[1], "and", pair[2]), title = paste("Dependence of", pair[1], "and", pair[2]),
                         note = "Each point is a simulated year at the ranks of the two lines' losses; the square holds the years in the worst 5% of both.",
                         height = 5, width = 6)
  ))

  guide <- list(id = "how-to-read", title = "How to read this report", content = div(class = "callout", tags$ul(
    tags$li(paste0("The VaR at ", label, " is the loss exceeded in ", format(100 * (1 - p$level)), "% of the years; the TVaR is the mean of those years.")),
    tags$li("The diversification benefit is how much less capital the lines need together than on their own."),
    tags$li("Each line keeps its own distribution: the copula only orders the lines' losses, so that their ranks have its dependence."),
    tags$li("The figures are those of the simulations: with more simulations, the tail figures move less from one run to another.")
  )))

  netsimr_write_report(file, "Portfolio Capital Report", "NetCapital by NetSimR", list(key, model, capital, charts, guide),
                       accent = c(glow = "rgba(74, 222, 128, 0.30)", dark = "#052e16", middle = "#14532d", bright = "#15803d",
                                  eyebrow = "#86efac"),
                       generated = generated)
}
