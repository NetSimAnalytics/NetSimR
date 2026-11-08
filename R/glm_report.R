##############
#Rate chart and report of the GLM fitting tool
##############

# The name of a rate table's values: "Relativity", "Odds ratio", or the effect on the link
# scale when the link has no relativity.
glm_rate_label <- function(rates) {
  if (is.na(rates$relativity)) "Effect on the link scale" else paste0(toupper(substring(rates$relativity, 1, 1)),
                                                                     substring(rates$relativity, 2))
}

# The chart of a rating factor (a data frame of glm_rate_table()'s factors): a line for a
# number, bars for a factor, and for an interaction a line per value of its second
# variable, at most eight.
glm_rate_chart <- function(f, rates, dark = FALSE) {
  vars <- setdiff(names(f), c("effect", "relativity", "base", "rows"))
  y <- if (is.na(rates$relativity)) f$effect else f$relativity
  y_title <- glm_rate_label(rates)
  if (length(vars) == 1 && is.numeric(f[[vars]])) {
    dft_line_chart(list(list(x = f[[vars]], y = y, name = y_title, colour = dft_model_palette[1])), vars, y_title, dark)
  } else if (length(vars) == 1) {
    dft_category_chart(as.character(f[[vars]]), bars = list(name = y_title, values = y, colour = dft_model_palette[1]),
                       x_title = vars, y_title = y_title, dark = dark)
  } else {
    second <- unique(f[[vars[2]]])
    second <- second[seq_len(min(length(second), 8))]
    first <- unique(f[[vars[1]]])
    lines <- lapply(seq_along(second), function(k) {
      rows <- f[[vars[2]]] == second[k]
      list(name = paste(vars[2], "=", format(second[k])), values = y[rows][match(first, f[[vars[1]]][rows])],
           colour = dft_model_palette[(k - 1) %% length(dft_model_palette) + 1])
    })
    dft_category_chart(if (is.numeric(first)) dft_fmt(first, 4) else as.character(first), lines = lines,
                       x_title = vars[1], y_title = y_title, dark = dark)
  }
}

# Writes the GLM fitting tool's report, the documentation of a model: the data and the
# choices, the fit's figures, the coefficients, the rate table with a chart per rating
# factor, and the validation (deviance, Gini, lift chart and Lorenz curve) on the rows held
# out, or on the training rows without them.
#
# `r` is a list of what the tool shows: source and rows (of the data), spec (the model's
# choices), model, rates (glm_rate_table(), or an error), training and test (the deviance,
# Gini and rows of each set; test NULL without rows held out), lift (glm_lift_table() of
# the set charted), lorenz (glm_gini_curve()), used (the label of that set), y_title and
# bar_name (the lift chart's titles), and response.
write_glm_report <- function(file, r, generated = Sys.time()) {
  ok <- function(x) !is.null(x) && !inherits(x, "error")
  model <- r$model
  spec <- r$spec
  frame <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
  fmt <- function(x, digits = 4) if (length(x) == 1 && is.finite(x)) dft_fmt(x, digits) else dft_dash
  aic <- stats::AIC(model)
  explained <- if (model$null.deviance > 0) 1 - model$deviance / model$null.deviance else NA_real_
  formula_text <- paste(deparse(stats::formula(model), width.cutoff = 500), collapse = " ")
  test <- r$test
  training <- r$training

  key <- list(id = "key-results", title = "Key results", content = tagList(
    tags$p(class = "section-intro", paste0(formula_text, ", ", glm_family_label(spec), " family with the ", spec$link, " link.")),
    netsimr_report_tiles(c(
      list(Observations = list(value = format(stats::nobs(model), big.mark = ","),
                               note = if (!is.null(test)) paste(format(test$rows, big.mark = ","), "rows held out for testing")),
           Parameters = format(sum(!is.na(stats::coef(model)))),
           AIC = list(value = fmt(aic, 7), note = if (!is.finite(aic)) "None for a quasi-likelihood family"),
           `Deviance explained` = if (is.finite(explained)) paste0(dft_fmt(100 * explained, 3), "%") else dft_dash),
      list(`Gini` = list(value = if (!is.null(test)) fmt(test$gini, 3) else fmt(training$gini, 3),
                         note = if (!is.null(test)) paste("Test rows; training", fmt(training$gini, 3)) else "Training rows"),
           `Deviance per row` = list(value = if (!is.null(test)) fmt(test$deviance) else fmt(training$deviance),
                                     note = if (!is.null(test)) paste("Test rows; training", fmt(training$deviance)) else "Training rows"))
    ))
  ))

  settings <- list(id = "model", title = "Data and model", content = netsimr_report_settings(list(
    Data = list(Source = r$source, Rows = format(r$rows, big.mark = ","),
                `Held out for testing` = if (!is.null(test)) paste0(format(test$rows, big.mark = ","), " rows (seed ", spec$seed, ")") else "None"),
    Model = list(Response = spec$response, Family = glm_family_label(spec), Link = spec$link,
                 Offset = if (spec$offset == "None") "None" else if (spec$offset_log) paste0("log(", spec$offset, ")") else spec$offset,
                 Weights = spec$weights, Terms = if (nzchar(spec$formula)) spec$formula else "1")
  )))

  co <- summary(model)$coefficients
  coefficients <- frame(Term = rownames(co), Estimate = dft_fmt(co[, 1], 5), `Std. error` = dft_fmt(co[, 2], 4),
                        `p-value` = formatC(co[, 4], format = "g", digits = 3))
  if (identical(spec$link, "log")) coefficients$Relativity <- dft_fmt(exp(co[, 1]), 4)
  coefficient_section <- list(id = "coefficients", title = "Coefficients", content = netsimr_report_table(
    coefficients, note = if (identical(spec$link, "log")) "Relativity: exp(estimate)."))

  rates_section <- if (ok(r$rates) && length(r$rates$factors) > 0) {
    rates <- r$rates
    label <- glm_rate_label(rates)
    factor_cards <- lapply(names(rates$factors), function(name) {
      f <- rates$factors[[name]]
      vars <- setdiff(names(f), c("effect", "relativity", "base", "rows"))
      shown <- if (nrow(f) > 30) f[unique(round(seq(1, nrow(f), length.out = 30))), ] else f
      table <- frame(Value = do.call(paste, c(lapply(vars, function(v) as.character(shown[[v]])), sep = ", ")),
                     Rows = ifelse(is.na(shown$rows), dft_dash, format(shown$rows, big.mark = ",")))
      table[[label]] <- dft_fmt(if (is.na(rates$relativity)) shown$effect else shown$relativity, 4)
      div(class = "two-col",
          netsimr_report_table(table, title = name,
                               note = paste0("Terms: ", paste(attr(f, "terms"), collapse = ", "), ". Base: ",
                                             paste(vapply(vars, function(v) format(rates$base[[v]]), character(1)), collapse = ", "),
                                             if (nrow(f) > 30) paste0(". 30 of its ", nrow(f), " values are shown.") else ".")),
          netsimr_report_chart(function(dark) glm_rate_chart(f, rates, dark), paste(label, "of", name), height = 4, width = 6.5))
    })
    list(id = "rates", title = "Rate table", content = tagList(
      tags$p(class = "section-intro", paste0(
        if (is.na(rates$relativity)) "Base prediction " else "Base rate ", dft_fmt(rates$base_rate, 5),
        if (identical(rates$relativity, "relativity")) ": a policy's prediction is the base rate times its relativity in every rating factor (times its exposure)." else ".")),
      factor_cards
    ))
  }

  validation <- if (!is.null(r$lift) || !is.null(r$lorenz)) {
    list(id = "validation", title = "Validation", content = tagList(
      tags$p(class = "section-intro", paste0("On the ", r$used, if (identical(r$used, "training rows") && is.null(test))
        ": no rows were held out, so these figures flatter the model." else ".")),
      if (!is.null(r$lift)) netsimr_report_chart(function(dark) {
        dft_category_chart(as.character(r$lift$band),
                           bars = list(name = r$bar_name, values = r$lift$exposure, colour = "band_bar", y2 = r$bar_name),
                           lines = list(list(name = "Actual", values = r$lift$actual, colour = "empirical"),
                                        list(name = "Predicted", values = r$lift$predicted, colour = dft_model_palette[2])),
                           x_title = "Band of predicted rate, lowest to highest", y_title = r$y_title, dark = dark)
      }, "Lift chart", title = "Lift chart",
      note = "The rows sorted by their predicted rate, in bands of about the same exposure: the actual line should rise steeply and the predicted line follow it."),
      if (!is.null(r$lorenz)) netsimr_report_chart(function(dark) {
        curve <- glm_thin_curve(r$lorenz)
        dft_line_chart(list(list(x = curve$x, y = curve$y, name = paste0("Model (Gini ", dft_fmt(curve$gini, 3), ")"),
                                 colour = dft_model_palette[2]),
                            list(x = c(0, 1), y = c(0, 1), name = "No better than chance", colour = "muted", lwd = 1.4)),
                       x_title = "Share of exposure, lowest predicted rate first", y_title = paste("Share of", r$response),
                       dark = dark, ylim = c(0, 1))
      }, "Lorenz curve", title = "Lorenz curve")
    ))
  }

  guide <- list(id = "how-to-read", title = "How to read this report", content = div(class = "callout", tags$ul(
    tags$li("The deviance per row measures how far the predictions are from the actual values: on rows held out it shows how the model does on data it has not seen."),
    tags$li("The Gini coefficient measures how well the predictions rank the rows, from 0 (no better than chance) up."),
    tags$li("The rate table gives every prediction as a base rate times a relativity for each rating factor, so a rating engine can price with it.")
  )))

  netsimr_write_report(file, "GLM Report", "NetSimR GLM Fitting Tool",
                       list(key, settings, coefficient_section, rates_section, validation, guide),
                       accent = c(glow = "rgba(45, 212, 191, 0.30)", dark = "#042f2e", middle = "#115e59", bright = "#0d9488",
                                  eyebrow = "#99f6e4"),
                       generated = generated)
}
