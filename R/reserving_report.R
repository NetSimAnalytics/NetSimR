##############
#Report of the reserving tool
##############

# Writes the reserving tool's report: an HTML page of the triangle's data and settings, the
# development factors, the ultimates of each method and the selected ones, Mack's and the
# one-year standard errors, the checks and methods of the More methods tab, the Munich chain
# ladder with incurred claims, and the bootstrap when it was run, with their charts.
#
# `r` is a list of what the tool shows: source, settings (a named list of labels and
# values), cl (chain_ladder()), results (rsv_method_results()), and optionally one_year
# (merz_wuthrich()), ave (actual_vs_expected()), clark (clark_growth()), inflation
# (inflation_adjusted_chain_ladder()), munich (munich_chain_ladder()), boot
# (bootstrap_chain_ladder()) and tails (the tail comparison), NULL or an error when missing.
write_reserving_report <- function(file, r, generated = Sys.time()) {
  ok <- function(x) !is.null(x) && !inherits(x, "error")
  cl <- r$cl
  res <- r$results
  complete <- all(is.finite(res$ultimate))
  premium_known <- all(is.finite(res$premium))
  amount <- netsimr_amount
  pct <- netsimr_percent
  frame <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
  with_total <- function(df, total) {
    total <- lapply(total, as.character)
    rbind(df, stats::setNames(as.data.frame(total, stringsAsFactors = FALSE), names(df)))
  }

  # ---------- key results ----------
  boot <- if (ok(r$boot)) r$boot
  key <- list(id = "key-results", title = "Key results", content = tagList(
    tags$p(class = "section-intro", paste0(r$source, ": ", nrow(cl$triangle), " origins, ", ncol(cl$triangle),
                                           " development periods.")),
    netsimr_report_tiles(c(
      list(Latest = amount(sum(res$latest)),
           `Selected ultimate` = list(value = if (complete) amount(sum(res$ultimate)) else netsimr_dash,
                                      note = if (premium_known && complete) paste("Loss ratio", pct(sum(res$ultimate) / sum(res$premium)))),
           `Selected IBNR` = if (complete) amount(sum(res$ibnr)) else netsimr_dash,
           `Chain ladder IBNR` = list(value = amount(cl$total[["ibnr"]]),
                                      note = paste0("Mack standard error ", amount(cl$total[["se"]]), ", CV ", pct(cl$total[["cv"]])))),
      if (ok(r$one_year)) list(`One-year standard error` = list(value = amount(r$one_year$total[["one_year_se"]]),
                                                                note = "Merz-W\u00fcthrich")),
      if (!is.null(boot)) list(`Bootstrap 99.5%` = list(value = amount(boot$total[["p99.5"]]),
                                                       note = paste("Mean", amount(boot$total[["ibnr"]]))))
    ), tail = "Bootstrap 99.5%")
  ))

  # ---------- data and settings ----------
  settings <- list(id = "settings", title = "Data and settings", content = netsimr_report_settings(r$settings))

  # ---------- development ----------
  factors <- frame(Factor = c(names(cl$factors), "Tail"), Selected = rsv_ratio(c(cl$factors, cl$tail), 4),
                   `To ultimate` = rsv_ratio(cl$cdf, 4),
                   `Link ratios used` = c(as.character(colSums(cl$used)), ""))
  tail_table <- if (ok(r$tails)) {
    t <- r$tails
    netsimr_report_table(frame(Tail = t$method, `Tail factor` = rsv_ratio(t$tail, 4), `Chain ladder IBNR` = amount(t$ibnr)),
                         title = "Tail factors compared",
                         note = "The tail each curve gives beyond the last development period, with the chain ladder's IBNR.")
  }
  development <- list(id = "development", title = "Development factors", content = tagList(
    div(class = "two-col",
        netsimr_report_table(factors, title = "Factors",
                             note = paste(c(paste0("Averages: ", tolower(r$settings[["Development"]][["Average"]]), "."), cl$notes),
                                          collapse = " ")),
        tail_table),
    netsimr_report_chart(function(dark) rsv_development_chart(cl, dark), "Cumulative claims by development period",
                         title = "Development", note = "Each line is an origin, from the oldest (lightest) to the latest (darkest)."),
    netsimr_report_chart(function(dark) rsv_link_ratio_chart(cl, dark), "Link ratios with the selected factors",
                         title = "Link ratios"),
    if (!is.null(cl$tail_curve)) netsimr_report_chart(function(dark) rsv_tail_chart(cl, dark), "Tail curve", title = "Tail curve")
  ))

  # ---------- ultimates ----------
  ultimates_df <- with_total(
    frame(Origin = res$origin, Latest = amount(res$latest), Developed = pct(res$developed), `Chain ladder` = amount(res$chain_ladder),
          BF = amount(res$bornhuetter_ferguson), Benktander = amount(res$benktander), `Cape Cod` = amount(res$cape_cod),
          Selected = res$method, Ultimate = amount(res$ultimate), IBNR = amount(res$ibnr), `Loss ratio` = pct(res$ultimate_loss_ratio)),
    list("Total", amount(sum(res$latest)), pct(sum(res$latest) / sum(res$chain_ladder)), amount(sum(res$chain_ladder)),
         if (all(is.finite(res$bornhuetter_ferguson))) amount(sum(res$bornhuetter_ferguson)) else netsimr_dash,
         if (all(is.finite(res$benktander))) amount(sum(res$benktander)) else netsimr_dash,
         if (all(is.finite(res$cape_cod))) amount(sum(res$cape_cod)) else netsimr_dash, "",
         if (complete) amount(sum(res$ultimate)) else netsimr_dash, if (complete) amount(sum(res$ibnr)) else netsimr_dash,
         if (complete && premium_known) pct(sum(res$ultimate) / sum(res$premium)) else netsimr_dash))
  method_ultimates <- list(`Chain ladder` = res$chain_ladder, `Bornhuetter-Ferguson` = res$bornhuetter_ferguson,
                           Benktander = res$benktander, `Cape Cod` = res$cape_cod)
  method_ultimates <- method_ultimates[vapply(method_ultimates, function(x) any(is.finite(x)), logical(1))]
  cc_ratio <- attr(res, "cape_cod_loss_ratio")
  ultimates <- list(id = "ultimates", title = "Ultimates", content = tagList(
    netsimr_report_table(ultimates_df, text_cols = c(1, 8), title = "By origin",
                         note = paste0("BF: Bornhuetter-Ferguson. The selected method of each origin gives its ultimate.",
                                       if (is.finite(cc_ratio)) paste0(" The Cape Cod loss ratio is ", pct(cc_ratio), "."))),
    netsimr_report_chart(function(dark) rsv_ultimate_chart(res$origin, res$latest, res$ibnr, method_ultimates, dark),
                         "Latest claims and IBNR of each origin, with the ultimate of each method", title = "Ultimates by origin")
  ))

  # ---------- uncertainty ----------
  b <- cl$by_origin
  mack_df <- with_total(
    frame(Origin = b$origin, IBNR = amount(b$ibnr), `Standard error` = amount(b$se), CV = pct(b$cv),
          Process = amount(b$process_se), Parameter = amount(b$parameter_se)),
    list("Total", amount(cl$total[["ibnr"]]), amount(cl$total[["se"]]), pct(cl$total[["cv"]]), amount(cl$total[["process_se"]]),
         amount(cl$total[["parameter_se"]])))
  one_year <- if (ok(r$one_year)) {
    mw <- r$one_year
    netsimr_report_table(with_total(
      frame(Origin = mw$by_origin$origin, `One-year standard error` = amount(mw$by_origin$one_year_se),
            `Until ultimate` = amount(mw$by_origin$mack_se)),
      list("Total", amount(mw$total[["one_year_se"]]), amount(mw$total[["mack_se"]]))),
      title = "One-year view", note = "Merz and W\u00fcthrich: how much the ultimate can move over the next year.")
  }
  boot_part <- if (!is.null(boot)) {
    shares <- paste0("p", formatC(100 * boot$probs, format = "fg"))
    o <- boot$by_origin
    boot_df <- frame(Origin = c(o$origin, "Total"), `Mean IBNR` = amount(c(o$ibnr, boot$total[["ibnr"]])),
                     `Standard deviation` = amount(c(o$se, boot$total[["se"]])))
    for (s in shares) boot_df[[paste0(sub("^p", "", s), "%")]] <- amount(c(o[[s]], boot$total[[s]]))
    tagList(
      netsimr_report_table(boot_df, title = "Bootstrap",
                           note = paste0(boot$method, ", ", format(length(boot$total_reserves), big.mark = ",", scientific = FALSE), " simulations.")),
      netsimr_report_chart(function(dark) rsv_bootstrap_chart(boot$total_reserves, boot$total[["ibnr"]],
                                                              c(`75th percentile` = boot$total[["p75"]],
                                                                `99.5th percentile` = boot$total[["p99.5"]]), dark),
                           "Distribution of the simulated total IBNR", title = "Distribution of the total IBNR")
    )
  } else {
    div(class = "callout", "The bootstrap was not run before the report: run it on the Uncertainty tab to add it.")
  }
  uncertainty <- list(id = "uncertainty", title = "Uncertainty", content = tagList(
    div(class = "two-col",
        netsimr_report_table(mack_df, title = "Mack's standard errors",
                             note = "Of the chain ladder: the process and parameter parts of the standard error."),
        one_year),
    boot_part
  ))

  # ---------- more methods ----------
  more <- NULL
  if (ok(r$ave) || ok(r$clark) || ok(r$inflation)) {
    parts <- list()
    if (ok(r$ave)) {
      a <- r$ave
      parts$ave <- netsimr_report_table(with_total(
        frame(Origin = a$by_origin$origin, Expected = amount(a$by_origin$expected), Actual = amount(a$by_origin$actual),
              `Actual / expected` = pct(a$by_origin$actual_to_expected, 0), z = netsimr_fmt(a$by_origin$z, 2)),
        list("Total", amount(a$total[["expected"]]), amount(a$total[["actual"]]), pct(a$total[["actual_to_expected"]], 0),
             netsimr_fmt(a$total[["z"]], 2))),
        title = "Actual vs expected, latest period",
        note = "z beyond about 2 either way is worth a look.")
    }
    if (ok(r$clark) || ok(r$inflation)) {
      rows <- list(c("Chain ladder", amount(cl$total[["ibnr"]]), amount(cl$total[["se"]])))
      if (ok(r$clark)) rows[[length(rows) + 1]] <- c(paste("Clark", r$clark$method), amount(r$clark$total[["ibnr"]]),
                                                     amount(r$clark$total[["se"]]))
      if (ok(r$inflation)) rows[[length(rows) + 1]] <- c("Chain ladder with claims inflation", amount(r$inflation$total[["ibnr"]]),
                                                         netsimr_dash)
      m <- do.call(rbind, rows)
      parts$methods <- netsimr_report_table(frame(Method = m[, 1], IBNR = m[, 2], `Standard error` = m[, 3]),
                                            title = "Other methods")
    }
    more <- list(id = "more", title = "More methods", content = div(class = "two-col", unname(parts)))
  }

  # ---------- paid and incurred ----------
  munich <- if (ok(r$munich)) {
    mb <- r$munich$by_origin
    mt <- r$munich$total
    list(id = "munich", title = "Paid and incurred", content = netsimr_report_table(with_total(
      frame(Origin = mb$origin, `CL paid` = amount(mb$chain_ladder_paid), `CL incurred` = amount(mb$chain_ladder_incurred),
            `Munich paid` = amount(mb$ultimate_paid), `Munich incurred` = amount(mb$ultimate_incurred)),
      list("Total", amount(mt[["chain_ladder_paid"]]), amount(mt[["chain_ladder_incurred"]]), amount(mt[["ultimate_paid"]]),
           amount(mt[["ultimate_incurred"]]))),
      title = "Munich chain ladder", note = "The ultimates of paid and incurred claims projected together."))
  }

  # ---------- IFRS 17 and Solvency II ----------
  ifrs <- if (ok(r$discounted)) {
    d <- r$discounted
    dt <- d$total
    ra <- if (ok(r$risk_adjustment)) r$risk_adjustment
    db <- d$by_origin
    list(id = "ifrs17", title = "IFRS 17 and Solvency II", content = tagList(
      netsimr_report_tiles(c(
        list(`Best estimate` = list(value = amount(dt[["discounted"]]),
                                    note = paste0("Discounted from ", amount(dt[["reserve"]]), "; mean term ",
                                                  netsimr_fmt(dt[["mean_term"]], 3), " years"))),
        if (!is.null(ra)) list(`Risk adjustment` = list(value = amount(ra$risk_adjustment),
                                                         note = paste0(ra$method, if (is.finite(ra$confidence_level))
                                                           paste0("; confidence level ", pct(ra$confidence_level)))),
                               `Liability for incurred claims` = amount(dt[["discounted"]] + ra$risk_adjustment)),
        if (ok(r$sii)) list(`Reserve risk (Solvency II)` = list(value = amount(r$sii$capital),
                                                                 note = paste0("Segment ", r$sii$segment, ", standard deviation ",
                                                                               pct(r$sii$sd))))
      )),
      div(class = "two-col",
          netsimr_report_table(with_total(
            frame(Origin = db$origin, Reserve = amount(db$reserve), Discounted = amount(db$discounted), Discount = amount(db$discount)),
            list("Total", amount(dt[["reserve"]]), amount(dt[["discounted"]]), amount(dt[["discount"]]))),
            title = "Discounted reserves",
            note = paste0("Spot rates ", paste(formatC(100 * d$yield_curve, format = "f", digits = 2), collapse = ", "),
                          "% for 1, 2, 3, ... years; payments ", c("0" = "at the start", "0.5" = "mid-period", "1" = "at the end")[[
                            as.character(d$timing)]], " of each period of ", format(d$period), " years.")),
          netsimr_report_chart(function(dark) {
            cf <- d$cash_flows
            netsimr_category_chart(as.character(cf$period),
                               bars = list(name = "Payments", values = cf$payments, colour = rsv_method_colours[["Chain ladder"]]),
                               lines = list(list(name = "Present value", values = cf$present_value,
                                                 colour = rsv_method_colours[["Cape Cod"]])),
                               x_title = "Future calendar period", y_title = "Amount", dark = dark)
          }, "Future payments and their present value", title = "Cash flows", height = 4.2, width = 6.5))
    ))
  }

  guide <- list(id = "how-to-read", title = "How to read this report", content = div(class = "callout", tags$ul(
    tags$li("The IBNR is the ultimate less the latest claims: for paid claims it includes the case reserves."),
    tags$li("Mack's standard error measures the uncertainty of the chain ladder until the claims are all paid; the one-year standard error, the part that can emerge over the next year."),
    tags$li("The bootstrap simulates the reserves, so its percentiles, such as 99.5%, give the reserve risk directly."),
    tags$li("Figures follow the choices of the tool when the report was made: the factors, the tail and each origin's method.")
  )))

  netsimr_write_report(file, "Reserving Report", "NetReserve by NetSimR",
                       list(key, settings, development, ultimates, uncertainty, ifrs, more, munich, guide),
                       accent = c(glow = "rgba(251, 146, 60, 0.35)", dark = "#431407", middle = "#7c2d12", bright = "#ea580c",
                                  eyebrow = "#fdba74"),
                       generated = generated)
}
