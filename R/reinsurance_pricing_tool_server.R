#' Server function of the reinsurance pricing tool
#'
#' @description Reads the layer and the rating year, rates the layer on the large
#'   losses (burning cost and Pareto) and on the risk profile (exposure curve or
#'   claim size distribution), blends the rates and prices the layer; paired with
#'   \code{reinsurance_pricing_tool_UI} by
#'   \code{\link{run_shiny_reinsurance_pricing_tool}}.
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @keywords internal
#' @import shiny
reinsurance_pricing_tool_Server <- function(input, output, session) {

  # a single number from an input, NA when it is empty or not a number
  number <- function(x) if (length(x) == 1 && is.numeric(x)) as.numeric(x) else NA_real_

  # the charts are drawn in the colours of the theme the page reports
  dark <- netsimr_dark(input)
  # each chart is drawn for a theme: on the page in the colours of the theme showing, and in
  # the report in both
  charts <- list()
  add_chart <- function(id, draw, alt) {
    charts[[id]] <<- list(draw = draw, alt = alt)
    output[[id]] <- netsimr_chart(function() draw(isTRUE(dark())), alt)
  }

  # the value of an expression with its warnings, or an error; `netsimr_checked()` shows the
  # error in place of the outputs that use it
  attempt <- function(expr) {
    warnings <- character(0)
    result <- tryCatch(
      withCallingHandlers(expr, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
      error = function(e) e
    )
    if (!inherits(result, "error")) attr(result, "warnings") <- warnings
    result
  }

  ######################
  #Layer
  ######################

  layer <- reactive({
    deductible <- number(input$deductible)
    limit <- number(input$limit)
    reinstatements <- number(input$reinstatements)
    validate(need(is.finite(deductible) && deductible >= 0, "Enter the deductible of the layer, at least 0."),
             need(is.finite(limit) && limit > 0, "Enter the limit of the layer, above 0."))
    if (is.na(reinstatements)) reinstatements <- Inf
    validate(need(reinstatements >= 0 && (!is.finite(reinstatements) || reinstatements == round(reinstatements)),
                  "The reinstatements must be a whole number of at least 0, or empty for unlimited."))
    rate <- number(input$reinstatement_rate)
    validate(need(is.finite(rate) && rate >= 0, "Enter the reinstatement rate, at least 0%."))
    aggregate_deductible <- number(input$aggregate_deductible)
    if (is.na(aggregate_deductible)) aggregate_deductible <- 0
    validate(need(aggregate_deductible >= 0, "The annual aggregate deductible must be at least 0."))
    list(deductible = deductible, limit = limit, reinstatements = reinstatements, reinstatement_rate = rate / 100,
         aggregate_deductible = aggregate_deductible,
         aggregate_limit = if (is.finite(reinstatements)) (reinstatements + 1) * limit else Inf)
  })

  rating <- reactive({
    year <- number(input$rating_year)
    premium <- number(input$rating_premium)
    loading <- number(input$loading)
    validate(need(is.finite(year) && year == round(year), "Enter the rating year."),
             need(is.finite(premium) && premium > 0, "Enter the subject premium of the rating year, above 0."),
             need(is.finite(loading) && loading >= 0 && loading < 100, "The loading must be from 0% to below 100%."))
    list(year = year, premium = premium, loading = loading / 100)
  })

  ######################
  #Experience
  ######################

  # what a loaded settings file sets once its data shows: the columns of an uploaded
  # file, and the first and last years
  loaded_columns <- list()
  loaded_years <- NULL
  # each year's premium, rate index, development and inclusion from a loaded settings file
  loaded_by_year <- reactiveVal(NULL)
  # the distributions a loaded file sets (the claim sizes, and those of the gross portfolio),
  # whose defaults must then not overwrite the file's parameters
  own_updates <- netsimr_own_updates()

  read_upload <- function(file, prefix) {
    netsimr_read_upload(input, file, list(header = paste0(prefix, "_header"), sep = paste0(prefix, "_sep"),
                                          dec = paste0(prefix, "_dec")),
                        "Upload a data file, or choose an example.", min_columns = 2)
  }

  # a column named in a loaded settings file, then the one the user chose, then a guess
  keep_column <- function(id, columns, guess) {
    wanted <- loaded_columns[[id]]
    if (!is.null(wanted) && wanted %in% c("none", columns)) return(wanted)
    current <- input[[id]]
    if (!is.null(current) && current %in% columns) current else guess
  }

  loss_upload <- reactive(read_upload(input$loss_file, "loss"))

  observeEvent(loss_upload(), {
    df <- loss_upload()
    columns <- names(df)
    numbers <- netsimr_numeric_columns(df, or_default(input$loss_dec, "."))
    year <- keep_column("loss_year_col", columns, or_default(netsimr_guess_column(numbers, "year|uwy|ay|accident|underwriting|period"),
                                                             numbers[1]))
    amount <- keep_column("loss_amount_col", columns,
                          or_default(netsimr_guess_column(numbers, "amount|loss|incurred|claim|value|cost|gross", year),
                                     setdiff(numbers, year)[1]))
    updateSelectizeInput(session, "loss_year_col", choices = columns, selected = year)
    updateSelectizeInput(session, "loss_amount_col", choices = columns, selected = amount)
  })

  losses <- reactive({
    if (identical(input$loss_source, "upload")) {
      df <- loss_upload()
      netsimr_checked(attempt(xlp_upload_losses(df, input$loss_year_col, input$loss_amount_col, or_default(input$loss_dec, "."))))
    } else {
      NetSimR::xl_large_losses
    }
  })

  # new losses set the first and last years to theirs, unless a loaded settings file
  # gives them
  observeEvent(losses(), {
    years <- range(losses()$year)
    if (!is.null(loaded_years)) {
      years <- c(or_default(loaded_years$first, years[1]), or_default(loaded_years$last, years[2]))
      loaded_years <<- NULL
    }
    updateNumericInput(session, "first_year", value = years[1])
    updateNumericInput(session, "last_year", value = years[2])
  })

  years <- reactive({
    data_years <- range(losses()$year)
    first <- number(input$first_year)
    last <- number(input$last_year)
    if (is.na(first)) first <- data_years[1]
    if (is.na(last)) last <- data_years[2]
    validate(need(first == round(first) && last == round(last), "The first and last years must be whole numbers."),
             need(first <= last, "The first year must not be after the last."),
             need(last - first < 100, "The experience can cover at most 100 years."))
    seq(first, last)
  })

  # the inputs of each year get new ids with each set of years, so values typed for other
  # years are never read for these
  generation <- reactiveVal(0)
  observeEvent(years(), generation(isolate(generation()) + 1))
  year_id <- function(kind, i) paste0("year_", kind, "_", generation(), "_", i)

  # the defaults of each year's inputs: those of a loaded settings file for a year it names,
  # otherwise the example's (or no premium, and factors of 1)
  year_defaults <- function(ys) {
    n <- length(ys)
    defaults <- list(premium = rep(NA_real_, n), index = rep(1, n), development = rep(1, n), include = rep(TRUE, n))
    if (!identical(input$loss_source, "upload")) {
      example <- NetSimR::xl_premium
      at <- match(ys, example$year)
      known <- !is.na(at)
      defaults$premium[known] <- example$premium[at[known]]
      defaults$index[known] <- example$rate_index[at[known]]
      defaults$development[known] <- example$development[at[known]]
    }
    saved <- loaded_by_year()
    if (!is.null(saved)) {
      at <- match(ys, saved$year)
      known <- !is.na(at)
      for (kind in names(defaults)) defaults[[kind]][known] <- saved[[kind]][at[known]]
    }
    defaults
  }

  output$year_inputs <- renderUI({
    ys <- years()
    defaults <- isolate(year_defaults(ys))
    head <- function(x) div(class = "xlp-year-head", x)
    cells <- lapply(seq_along(ys), function(i) {
      y <- ys[i]
      tagList(
        div(class = "xlp-year-name", y),
        netsimr_hidden_label(netsimr_amount_input(year_id("premium", i), paste("Subject premium of", y), value = defaults$premium[i], min = 0)),
        netsimr_hidden_label(numericInput(year_id("index", i), paste("Rate index of", y), value = defaults$index[i], min = 0,
                                      step = 0.01, width = "100%")),
        netsimr_hidden_label(numericInput(year_id("development", i), paste("Development of", y), value = defaults$development[i],
                                      min = 0, step = 0.01, width = "100%")),
        checkboxInput(year_id("include", i), span(class = "visually-hidden", paste("Include", y)), value = defaults$include[i])
      )
    })
    tagList(
      div(class = "xlp-year-wrap",
          div(class = "xlp-year-grid",
              head("Year"), head("Subject premium"), head("Rate index"), head("Development"), head("Include"), cells)),
      netsimr_help("The rate index brings a year's premium to the rating year's rates and exposure (its on-level premium);",
               "the development factor takes its layer losses to ultimate. Clear Include to leave a year out.")
    )
  })

  # each year's inputs, or their defaults before the By year card has been shown
  year_values <- reactive({
    ys <- years()
    defaults <- year_defaults(ys)
    value <- function(kind, i) {
      x <- input[[year_id(kind, i)]]
      if (is.null(x)) defaults[[kind]][i] else x
    }
    list(
      year = ys,
      premium = vapply(seq_along(ys), function(i) number(value("premium", i)), numeric(1)),
      index = vapply(seq_along(ys), function(i) number(value("index", i)), numeric(1)),
      development = vapply(seq_along(ys), function(i) number(value("development", i)), numeric(1)),
      include = vapply(seq_along(ys), function(i) isTRUE(value("include", i)), logical(1))
    )
  })

  # the experience in the form of burning_cost() and pareto_rating()
  experience <- reactive({
    v <- year_values()
    l <- losses()
    validate(need(all(is.finite(v$premium) & v$premium > 0),
                  paste("Enter the subject premium of each year in the By year card:",
                        paste(v$year[!(is.finite(v$premium) & v$premium > 0)], collapse = ", "))),
             need(all(is.finite(v$index) & v$index > 0), "Every year's rate index must be above 0."),
             need(all(is.finite(v$development) & v$development >= 0), "Every year's development factor must be at least 0."),
             need(any(v$include), "Include at least one year."))
    inflation <- number(input$inflation)
    validate(need(is.finite(inflation) && inflation > -100, "Enter the claims inflation, above -100%."))
    inside <- l$year >= min(v$year) & l$year <= max(v$year)
    names_by_year <- function(x) stats::setNames(x, v$year)
    list(losses = l[inside, , drop = FALSE], outside = sum(!inside),
         premium = data.frame(year = v$year, premium = v$premium), index = names_by_year(v$index),
         development = names_by_year(v$development), exclude = v$year[!v$include], inflation = inflation / 100)
  })

  bc <- reactive({
    ex <- experience()
    lay <- layer()
    r <- rating()
    threshold <- number(input$threshold)
    netsimr_checked(attempt(burning_cost(
      ex$losses, ex$premium, lay$deductible, lay$limit, aggregate_deductible = lay$aggregate_deductible,
      aggregate_limit = lay$aggregate_limit, inflation = ex$inflation, rating_year = r$year, development = ex$development,
      premium_index = ex$index, threshold = if (is.finite(threshold) && threshold > 0) threshold,
      exclude = if (length(ex$exclude) > 0) ex$exclude, weights = or_default(input$weights, "premium"),
      rating_premium = r$premium
    )))
  })

  # the Pareto rating, or the error that stops it (it needs a threshold of at most the
  # deductible and at least two losses above it)
  pareto <- reactive({
    ex <- experience()
    lay <- layer()
    r <- rating()
    threshold <- number(input$pareto_threshold)
    alpha <- number(input$pareto_alpha)
    if (!(is.finite(threshold) && threshold > 0)) return(simpleError("Enter the Pareto threshold, above 0."))
    attempt(pareto_rating(
      ex$losses, ex$premium, threshold, lay$deductible, lay$limit, inflation = ex$inflation, rating_year = r$year,
      development = ex$development, premium_index = ex$index, exclude = if (length(ex$exclude) > 0) ex$exclude,
      alpha = if (is.finite(alpha)) alpha, rating_premium = r$premium
    ))
  })

  output$experience_stats <- renderUI({
    b <- bc()
    p <- pareto()
    pareto_ok <- !inherits(p, "error")
    used <- b$by_year$year[b$by_year$included]
    div(
      class = "dft-stats",
      netsimr_stat_tile("Years", length(used), icon_name = "calendar", note = paste(min(used), "to", max(used))),
      netsimr_stat_tile("Losses in the layer", format(b$n_layer_losses, big.mark = ",", scientific = FALSE), icon_name = "bolt"),
      netsimr_stat_tile("Burning cost", netsimr_percent(b$rate, 2), icon_name = "fire", accent = TRUE, note = "Of subject premium"),
      netsimr_stat_tile("Expected layer loss", netsimr_amount(b$expected_loss), icon_name = "coins", note = "Burning cost"),
      netsimr_stat_tile("Pareto", if (pareto_ok) netsimr_percent(p$rate, 2) else netsimr_dash, icon_name = "chart-line",
                    note = if (pareto_ok) paste0("Alpha ", formatC(p$alpha, format = "f", digits = 2)) else "Not available")
    )
  })

  # the tables are built as the arguments of netsimr_table(), for the page and the report
  bc_table <- function() {
    b <- bc()
    y <- b$by_year
    rows <- lapply(seq_len(nrow(y)), function(i) {
      list(as.character(y$year[i]), format(y$losses[i]), format(y$layer_count[i]), netsimr_amount(y$layer_losses[i]),
           netsimr_amount(y$aggregate_losses[i]), netsimr_amount(y$onlevel_premium[i]), netsimr_percent(y$burning_cost[i], 2))
    })
    used <- y[y$included, ]
    total <- list("Total", format(sum(used$losses)), format(sum(used$layer_count)), netsimr_amount(sum(used$layer_losses)),
                  netsimr_amount(sum(used$aggregate_losses)), netsimr_amount(sum(used$onlevel_premium)), netsimr_percent(b$rate, 2))
    ex <- experience()
    notes <- c(
      if (any(!y$included)) paste0("Left out: ", paste(y$year[!y$included], collapse = ", "), "."),
      if (ex$outside > 0) paste0(ex$outside, " loss", if (ex$outside > 1) "es" else "", " outside the years ",
                                 min(y$year), " to ", max(y$year), " left out."),
      "Developed: the layer losses times the year's development factor, after any annual aggregate terms.",
      if (identical(b$weights, "equal")) "The burning cost is the mean of the years'." else
        "The burning cost is the total layer losses over the total on-level premium.",
      if (is.finite(b$aggregate_limit) || b$aggregate_deductible > 0) {
        paste0("Aggregate losses after the annual deductible of ", netsimr_amount(b$aggregate_deductible), " and limit of ",
               if (is.finite(b$aggregate_limit)) netsimr_amount(b$aggregate_limit) else "none", ".")
      },
      attr(b, "warnings")
    )
    list(header = c("Year", "Losses", "In layer", "Layer losses", "Developed", "On-level premium", "Burning cost"),
         rows = rows, total = total, note = notes, excluded = !y$included)
  }

  output$burning_cost_table <- renderUI({
    t <- bc_table()
    row <- function(cells, class = NULL) tags$tr(class = class, lapply(cells, tags$td))
    tagList(
      div(class = "dft-table-wrap",
          tags$table(class = "dft-table",
                     tags$thead(tags$tr(lapply(t$header, tags$th))),
                     tags$tbody(lapply(seq_along(t$rows), function(i) row(t$rows[[i]], if (t$excluded[i]) "xlp-excluded")),
                                row(t$total, "xlp-total")))),
      p(class = "dft-table-note", paste(t$note, collapse = " "))
    )
  })

  add_chart("burning_cost_plot", function(in_dark) {
    b <- bc()
    y <- b$by_year
    netsimr_category_chart(as.character(y$year), bars = list(name = "Burning cost of the year", values = y$burning_cost,
                                                        colour = xlp_chart_colours(in_dark)$accent_soft),
                       lines = list(list(name = paste("Burning cost", netsimr_percent(b$rate, 2)), values = rep(b$rate, nrow(y)),
                                         colour = netsimr_palette[1])),
                       x_title = "Year", y_title = "Layer losses over on-level premium", dark = in_dark, y_percent = TRUE)
  }, alt = "Burning cost of each year, with the burning cost of all the years")

  losses_chart <- function(in_dark) {
    b <- bc()
    lay <- layer()
    validate(need(nrow(b$losses) > 0, "There are no losses in these years."))
    xlp_losses_chart(b$losses, b$by_year$year, lay$deductible, lay$limit, in_dark)
  }
  add_chart("losses_plot", losses_chart, alt = "Indexed large losses of each year against the layer")
  add_chart("layer_losses_plot", losses_chart, alt = "Indexed large losses of each year against the layer")

  add_chart("pareto_plot", function(in_dark) {
    p <- pareto()
    validate(need(!inherits(p, "error"), if (inherits(p, "error")) conditionMessage(p)))
    validate(need(nrow(p$losses) > 0, "No losses above the Pareto threshold."))
    xlp_pareto_chart(p$losses$indexed[p$losses$year %in% p$by_year$year[p$by_year$included]], p$threshold, p$alpha, in_dark)
  }, alt = "Share of the large losses above each size, observed and fitted by the Pareto")

  # what the Pareto rating found, or NULL without one
  pareto_text <- function() {
    p <- pareto()
    if (inherits(p, "error")) return(NULL)
    paste0(p$n, " losses above ", netsimr_amount(p$threshold), " in the money of ", p$rating_year, "; alpha ",
           formatC(p$alpha, format = "f", digits = 3),
           if (!isTRUE(all.equal(p$alpha, p$alpha_estimated)) && is.finite(p$alpha_estimated)) {
             paste0(" (estimated ", formatC(p$alpha_estimated, format = "f", digits = 3), ")")
           },
           ". Expected ", formatC(p$expected_count, format = "f", digits = 2), " losses above the threshold in ", p$rating_year,
           ", each with ", netsimr_amount(p$layer_severity), " in the layer on average.")
  }

  output$pareto_note <- renderUI({
    text <- pareto_text()
    if (!is.null(text)) p(class = "dft-table-note", text)
  })

  output$download_burning_cost <- downloadHandler(
    filename = function() "burning_cost_by_year.csv",
    content = function(file) utils::write.csv(csv_safe(bc()$by_year), file, row.names = FALSE)
  )
  output$download_losses <- downloadHandler(
    filename = function() "indexed_losses.csv",
    content = function(file) utils::write.csv(csv_safe(bc()$losses), file, row.names = FALSE)
  )

  ######################
  #Exposure
  ######################

  profile_upload <- reactive(read_upload(input$profile_file, "profile"))

  observeEvent(profile_upload(), {
    df <- profile_upload()
    columns <- names(df)
    numbers <- netsimr_numeric_columns(df, or_default(input$profile_dec, "."))
    size <- keep_column("size_col", columns, or_default(netsimr_guess_column(numbers, "sum|insured|tsi|mpl|pml|limit|size|value"),
                                                        numbers[1]))
    premium <- keep_column("premium_col", columns, or_default(netsimr_guess_column(numbers, "prem|gnpi|epi", size),
                                                              setdiff(numbers, size)[1]))
    ratio <- keep_column("loss_ratio_col", columns,
                         or_default(netsimr_guess_column(numbers, "loss.?ratio|^lr$|elr", c(size, premium)), "none"))
    band <- keep_column("band_col", columns, or_default(netsimr_guess_column(columns, "band|label|name|range", c(size, premium, ratio)),
                                                        "none"))
    updateSelectizeInput(session, "size_col", choices = columns, selected = size)
    updateSelectizeInput(session, "premium_col", choices = columns, selected = premium)
    updateSelectizeInput(session, "loss_ratio_col", choices = c("None" = "none", columns), selected = ratio)
    updateSelectizeInput(session, "band_col", choices = c("None" = "none", columns), selected = band)
  })

  profile <- reactive({
    if (identical(input$profile_source, "upload")) {
      df <- profile_upload()
      type <- or_default(input$profile_type, "property")
      result <- netsimr_checked(attempt(xlp_upload_profile(df, type, input$size_col, input$premium_col, input$loss_ratio_col,
                                                   input$band_col, or_default(input$profile_dec, "."))))
      list(profile = result, type = type, description = NULL)
    } else {
      xlp_example_profile(or_default(input$profile_example, "property"))
    }
  })

  # the business of the profile, for the curve settings that show
  output$profile_type <- renderText(profile()$type)

  observeEvent(input$severity, {
    s <- xlp_severities[[input$severity]]
    if (is.null(s)) return()
    if (own_updates$is_echo("severity", input$severity)) {
      updateNumericInput(session, "severity_p1", label = s$params[1])
      if (length(s$params) > 1) updateNumericInput(session, "severity_p2", label = s$params[2])
      return()
    }
    updateNumericInput(session, "severity_p1", label = s$params[1], value = s$defaults[1])
    if (length(s$params) > 1) updateNumericInput(session, "severity_p2", label = s$params[2], value = s$defaults[2])
  }, ignoreInit = TRUE)

  curve <- reactive({
    choice <- or_default(input$curve, "Y3")
    if (identical(choice, "c")) {
      c <- number(input$curve_c)
      validate(need(is.finite(c) && c >= 0, "Enter the Swiss Re c, at least 0."))
      c
    } else if (identical(choice, "bg")) {
      b <- number(input$curve_b)
      g <- number(input$curve_g)
      validate(need(is.finite(b) && b >= 0, "Enter b, at least 0."), need(is.finite(g) && g >= 1, "Enter g, at least 1."))
      c(b = b, g = g)
    } else {
      choice
    }
  })

  severity <- reactive({
    id <- or_default(input$severity, "LogNormal")
    s <- xlp_severities[[id]]
    validate(need(!is.null(s), "Choose a claim size distribution."))
    params <- c(number(input$severity_p1), if (length(s$params) > 1) number(input$severity_p2))
    validate(need(all(is.finite(params)), "Enter the parameters of the claim size distribution."))
    list(id = id, params = params)
  })

  exposure <- reactive({
    pr <- profile()
    lay <- layer()
    lr <- number(input$loss_ratio)
    has_ratio <- "loss_ratio" %in% names(pr$profile)
    validate(need(has_ratio || (is.finite(lr) && lr >= 0), "Enter the expected loss ratio, at least 0%."))
    loss_ratio <- if (has_ratio) NULL else lr / 100
    if (identical(pr$type, "casualty")) {
      s <- severity()
      netsimr_checked(attempt(casualty_exposure_rating(pr$profile, lay$deductible, lay$limit, s$id, s$params, loss_ratio = loss_ratio)))
    } else {
      netsimr_checked(attempt(property_exposure_rating(pr$profile, lay$deductible, lay$limit, curve(), loss_ratio = loss_ratio)))
    }
  })

  output$exposure_stats <- renderUI({
    e <- exposure()
    r <- rating()
    basis <- if (identical(e$type, "property")) e$curve$label else e$severity$label
    div(
      class = "dft-stats",
      netsimr_stat_tile("Bands", nrow(e$bands), icon_name = "table-list",
                    note = paste0(if (identical(e$type, "property")) "Property" else "Casualty", "; ", basis)),
      netsimr_stat_tile("Ground-up loss", netsimr_amount(e$ground_up_loss), icon_name = "coins",
                    note = paste("Of a premium of", netsimr_amount(e$subject_premium))),
      netsimr_stat_tile("Exposure rate", netsimr_percent(e$rate, 2), icon_name = "percent", accent = TRUE, note = "Of subject premium"),
      netsimr_stat_tile("Expected layer loss", netsimr_amount(e$rate * r$premium), icon_name = "flag-checkered",
                    note = paste("At the rating year's premium of", netsimr_amount(r$premium)))
    )
  })

  exposure_table <- function() {
    e <- exposure()
    bands <- e$bands
    property <- identical(e$type, "property")
    size <- if (property) bands$sum_insured else bands$policy_limit
    label <- if ("band" %in% names(bands)) as.character(bands$band) else netsimr_amount(size)
    # the loss ratio has a column only when the bands' differ
    one_ratio <- length(unique(bands$loss_ratio)) == 1
    rows <- lapply(seq_len(nrow(bands)), function(i) {
      c(list(label[i], netsimr_amount(size[i]), netsimr_amount(bands$premium[i])),
        if (!one_ratio) list(netsimr_percent(bands$loss_ratio[i])),
        list(netsimr_amount(bands$expected_loss[i])),
        if (property) list(paste(netsimr_percent(bands$deductible_share[i], 0), "to", netsimr_percent(bands$exhaustion_share[i], 0))),
        list(netsimr_percent(bands$layer_share[i], 2), netsimr_amount(bands$layer_loss[i])))
    })
    total <- c(list("Total", "", netsimr_amount(e$subject_premium)),
               if (!one_ratio) list(netsimr_percent(e$ground_up_loss / e$subject_premium)),
               list(netsimr_amount(e$ground_up_loss)), if (property) list(""),
               list(netsimr_percent(e$expected_loss / e$ground_up_loss, 2), netsimr_amount(e$expected_loss)))
    header <- c("Band", if (property) "Sum insured" else "Policy limit", "Premium", if (!one_ratio) "Loss ratio",
                "Expected loss", if (property) "Layer, of sum insured", "Layer share", "Layer loss")
    note <- c(
      if (one_ratio) paste0("Expected loss: the premium times the loss ratio of ", netsimr_percent(bands$loss_ratio[1]), "."),
      if (property) {
        paste0("Layer, of sum insured: the layer's deductible and top as shares of the band's sum insured; the layer",
               " share is the exposure curve's increase between them (", e$curve$label, ").")
      } else {
        paste0("The layer share is the layer's part of the limited expected value of a claim up to the policy limit (",
               e$severity$label, ").")
      },
      profile()$description
    )
    list(header = header, rows = rows, total = total, note = note)
  }

  output$exposure_table <- renderUI(do.call(netsimr_table, exposure_table()))

  add_chart("band_plot", function(in_dark) {
    e <- exposure()
    bands <- e$bands
    size <- if (identical(e$type, "property")) bands$sum_insured else bands$policy_limit
    label <- if ("band" %in% names(bands)) as.character(bands$band) else netsimr_amount(size)
    netsimr_category_chart(label, bars = list(name = "Expected layer loss", values = bands$layer_loss,
                                          colour = xlp_chart_colours(in_dark)$accent_soft),
                       x_title = "Band", y_title = "Expected layer loss", dark = in_dark)
  }, alt = "Expected loss to the layer of each band of the risk profile")

  add_chart("curve_plot", function(in_dark) {
    e <- exposure()
    if (identical(e$type, "property")) {
      xlp_property_curve_chart(e$curve, in_dark)
    } else {
      lay <- layer()
      s <- severity()
      lev <- pricing_lev(s$id, s$params)
      xlp_casualty_curve_chart(lev$fn, lev$label, e$bands$policy_limit, lay$deductible, lay$limit, in_dark)
    }
  }, alt = "The exposure curve or limited expected value of the exposure rating")

  output$download_exposure <- downloadHandler(
    filename = function() "exposure_rating.csv",
    content = function(file) utils::write.csv(csv_safe(exposure()$bands), file, row.names = FALSE)
  )

  ######################
  #Pricing
  ######################

  # the rate of each method, NA (with the reason) when it cannot be found
  method_rates <- reactive({
    rate_of <- function(expr) {
      result <- tryCatch(expr, error = function(e) e)
      if (inherits(result, "error")) list(rate = NA_real_, note = conditionMessage(result)) else result
    }
    burning <- rate_of({
      b <- bc()
      list(rate = b$rate, n = b$n_layer_losses, note = NULL)
    })
    par <- rate_of({
      p <- pareto()
      if (inherits(p, "error")) stop(conditionMessage(p))
      list(rate = p$rate, n = p$n, note = NULL)
    })
    expo <- rate_of(list(rate = exposure()$rate, note = NULL))
    method <- or_default(input$experience_method, "burning_cost")
    experience_rate <- if (identical(method, "pareto")) par else burning
    weight <- or_default(input$experience_weight, 50) / 100
    blended <- if (is.finite(experience_rate$rate) && is.finite(expo$rate)) {
      list(rate = weight * experience_rate$rate + (1 - weight) * expo$rate, note = NULL)
    } else if (is.finite(experience_rate$rate)) {
      list(rate = experience_rate$rate, note = "The exposure rating is not available, so the blend is the experience rate.")
    } else if (is.finite(expo$rate)) {
      list(rate = expo$rate, note = "The experience rating is not available, so the blend is the exposure rate.")
    } else {
      list(rate = NA_real_, note = "Neither rating is available.")
    }
    list(`Burning cost` = burning, Pareto = par, Exposure = expo, Blended = blended,
         experience = if (identical(method, "pareto")) "Pareto" else "Burning cost", weight = weight,
         experience_n = experience_rate$n)
  })

  # the past years' layer losses at the rating year's premium, from the burning cost, or NULL:
  # they give the reinstated cover each method can expect to use
  past_years <- reactive({
    b <- tryCatch(bc(), error = function(e) NULL)
    if (is.null(b) || !(b$expected_loss > 0)) return(NULL)
    list(losses = b$by_year$at_rating_premium[b$by_year$included], expected_loss = b$expected_loss)
  })

  # the expected reinstated cover as a share of the limit, for an expected loss: the past
  # years' layer losses scaled to that expected loss, each capped at the reinstated cover;
  # without them, the expected loss over the limit, at most the reinstatements
  reinstated_share <- function(expected_loss, lay, years) {
    n <- lay$reinstatements
    if (n == 0) return(0)
    if (is.null(years)) return(min(expected_loss / lay$limit, n))
    scaled <- years$losses * expected_loss / years$expected_loss
    mean(pmin(scaled, n * lay$limit)) / lay$limit
  }

  # each method's rate, expected loss and technical premium
  pricing <- reactive({
    m <- method_rates()
    lay <- layer()
    r <- rating()
    years <- past_years()
    methods <- c("Burning cost", "Pareto", "Exposure", "Blended")
    rows <- lapply(methods, function(name) {
      rate <- m[[name]]$rate
      if (!is.finite(rate)) {
        return(data.frame(method = name, rate = NA_real_, expected_loss = NA_real_, total_premium = NA_real_,
                          upfront_premium = NA_real_, reinstatement_premium = NA_real_, rate_on_line = NA_real_,
                          payback = NA_real_, note = or_default(m[[name]]$note, "")))
      }
      el <- rate * r$premium
      tp <- technical_premium(el, lay$limit, loading = r$loading, reinstatements = lay$reinstatements,
                              reinstatement_rate = lay$reinstatement_rate, used = reinstated_share(el, lay, years))
      data.frame(method = name, rate = rate, expected_loss = el, total_premium = tp$total_premium,
                 upfront_premium = tp$upfront_premium, reinstatement_premium = tp$reinstatement_premium,
                 rate_on_line = tp$rate_on_line, payback = tp$payback, note = or_default(m[[name]]$note, ""))
    })
    out <- do.call(rbind, rows)
    attr(out, "experience") <- m$experience
    attr(out, "weight") <- m$weight
    attr(out, "reinstated_from_years") <- !is.null(years)
    out
  })

  observeEvent(input$use_credibility, {
    m <- method_rates()
    full <- number(input$full_credibility)
    if (!is.finite(or_default(m$experience_n, NA_real_)) || !is.finite(full) || full <= 0) {
      showNotification("The credibility weight needs the experience rating and a number of losses for full credibility.",
                       type = "error", duration = 6)
      return()
    }
    z <- credibility_weight(m$experience_n, full)
    updateSliderInput(session, "experience_weight", value = round(100 * z))
    showNotification(paste0("Weight of the experience: ", round(100 * z), "% (", m$experience_n, " losses)."),
                     type = "message", duration = 5)
  })

  output$pricing_stats <- renderUI({
    p <- pricing()
    blended <- p[p$method == "Blended", ]
    lay <- layer()
    ok <- is.finite(blended$rate)
    div(
      class = "dft-stats",
      netsimr_stat_tile("Expected layer loss", if (ok) netsimr_amount(blended$expected_loss) else netsimr_dash, icon_name = "coins",
                    note = paste0(round(100 * attr(p, "weight")), "% ", attr(p, "experience"), ", ",
                                  round(100 * (1 - attr(p, "weight"))), "% exposure")),
      netsimr_stat_tile("Technical premium", if (ok) netsimr_amount(blended$upfront_premium) else netsimr_dash, icon_name = "tags", accent = TRUE,
                    note = if (ok && blended$reinstatement_premium > 0) {
                      paste("Upfront; and", netsimr_amount(blended$reinstatement_premium), "expected from reinstatements")
                    } else "Upfront"),
      netsimr_stat_tile("Rate on line", if (ok) netsimr_percent(blended$rate_on_line, 2) else netsimr_dash, icon_name = "percent",
                    note = paste("Of the limit of", netsimr_amount(lay$limit))),
      netsimr_stat_tile("Payback", if (ok) paste(formatC(blended$payback, format = "f", digits = 1), "years") else netsimr_dash,
                    icon_name = "rotate"),
      netsimr_stat_tile("Rate", if (ok) netsimr_percent(blended$upfront_premium / rating()$premium, 2) else netsimr_dash,
                    icon_name = "scale-balanced", note = "Upfront premium over subject premium")
    )
  })

  pricing_rows <- function(p, full = TRUE) {
    lapply(seq_len(nrow(p)), function(i) {
      cells <- list(p$method[i], netsimr_percent(p$rate[i], 2), netsimr_amount(p$expected_loss[i]))
      if (full) cells <- c(cells, list(netsimr_amount(p$upfront_premium[i]), netsimr_amount(p$reinstatement_premium[i])))
      c(cells, list(netsimr_percent(p$rate_on_line[i], 2)))
    })
  }

  pricing_notes <- function(p) {
    lay <- layer()
    with_note <- nzchar(p$note)
    c(if (any(with_note)) paste0(p$method[with_note], ": ", p$note[with_note]),
      paste0("Blended: ", round(100 * attr(p, "weight")), "% ", attr(p, "experience"), " and ",
             round(100 * (1 - attr(p, "weight"))), "% exposure."),
      if (is.finite(lay$aggregate_limit) || lay$aggregate_deductible > 0) {
        "The aggregate terms apply to the burning cost only; the Pareto and exposure rates are before them."
      })
  }

  pricing_table <- function() {
    p <- pricing()
    r <- rating()
    lay <- layer()
    reinstatement_note <- if (lay$reinstatements > 0) {
      paste0("The reinstatement premium is the expected reinstated cover, at ", netsimr_percent(lay$reinstatement_rate, 0),
             " of the upfront premium; the cover each method expects to use is ",
             if (isTRUE(attr(p, "reinstated_from_years"))) {
               "that of the past years' layer losses at the rating year's premium, scaled to its expected loss."
             } else {
               "its expected loss over the limit, as the burning cost's years are not available."
             })
    }
    list(header = c("Method", "Rate", "Expected loss", "Upfront premium", "Reinstatement premium", "Rate on line"),
         rows = pricing_rows(p), note = c(
           paste0("Rates are shares of the subject premium of ", netsimr_amount(r$premium), ". The upfront and expected",
                  " reinstatement premiums add up to the expected loss over one minus the loading of ",
                  netsimr_percent(r$loading, 0), "."),
           reinstatement_note, pricing_notes(p)))
  }

  output$pricing_table <- renderUI(do.call(netsimr_table, pricing_table()))

  output$glance_table <- renderUI({
    p <- pricing()
    netsimr_table(c("Method", "Rate", "Expected loss", "Rate on line"), pricing_rows(p, full = FALSE), note = pricing_notes(p))
  })

  add_chart("method_plot", function(in_dark) {
    p <- pricing()
    ok <- is.finite(p$expected_loss)
    validate(need(any(ok), "No method can rate the layer yet."))
    netsimr_category_chart(p$method, bars = list(name = "Expected layer loss", values = ifelse(ok, p$expected_loss, 0),
                                             colour = xlp_chart_colours(in_dark)$accent_soft),
                       lines = list(list(name = "Upfront technical premium", values = p$upfront_premium,
                                         colour = netsimr_palette[1])),
                       x_title = "Method", y_title = "Amount", dark = in_dark)
  }, alt = "Expected layer loss and technical premium of each method")

  output$download_pricing <- downloadHandler(
    filename = function() "reinsurance_pricing.csv",
    content = function(file) {
      p <- pricing()
      lay <- layer()
      p$deductible <- lay$deductible
      p$limit <- lay$limit
      p$rating_premium <- rating()$premium
      utils::write.csv(csv_safe(p), file, row.names = FALSE)
    }
  )

  # every table of the tool in one workbook, those that can be found
  output$download_excel <- downloadHandler(
    filename = function() "reinsurance_pricing.xlsx",
    content = function(file) {
      lay <- layer()
      p <- pricing()
      p$deductible <- lay$deductible
      p$limit <- lay$limit
      p$rating_premium <- rating()$premium
      netsimr_write_excel(list(
        Pricing = p,
        `Burning cost by year` = quiet(bc()$by_year),
        `Indexed losses` = quiet(bc()$losses),
        `Exposure rating` = quiet(exposure()$bands)
      ), file)
    }
  )

  output$layer_stats <- renderUI({
    lay <- layer()
    r <- rating()
    div(
      class = "dft-stats",
      netsimr_stat_tile("Layer", xlp_layer_label(lay$deductible, lay$limit), icon_name = "layer-group", accent = TRUE),
      netsimr_stat_tile("Reinstatements", if (is.finite(lay$reinstatements)) format(lay$reinstatements) else "Unlimited",
                    icon_name = "rotate", note = paste("At", netsimr_percent(lay$reinstatement_rate, 0))),
      netsimr_stat_tile("Annual aggregate limit", if (is.finite(lay$aggregate_limit)) netsimr_amount(lay$aggregate_limit) else "None",
                    icon_name = "lock", note = if (lay$aggregate_deductible > 0) paste("Deductible", netsimr_amount(lay$aggregate_deductible))),
      netsimr_stat_tile("Rating year", format(r$year), icon_name = "calendar", note = paste("Premium", netsimr_amount(r$premium)))
    )
  })

  ######################
  #Programmes
  ######################

  # the programmes of the grid: by default a quota share, the Layer tab's layer, a stop loss,
  # a quota share with the layer, and a surplus treaty on the Exposure tab's risk profile
  prog_defaults <- data.frame(
    name = c("Quota share 30%", "The layer", "Stop loss", "Quota share 20% and the layer", "Surplus 4 lines"),
    surplus_retention = c(NA, NA, NA, NA, 1e6), surplus_lines = c(NA, NA, NA, NA, 4),
    qs = c(30, NA, NA, 20, NA), commission = c(25, NA, NA, 25, 30),
    xl_deductible = c(NA, 1e6, NA, 1e6, NA), xl_limit = c(NA, 4e6, NA, 4e6, NA), xl_reinstatements = c(NA, 1, NA, 1, NA),
    xl_premium = NA_real_, sl_attachment = c(NA, NA, 45e6, NA, NA), sl_limit = c(NA, NA, 30e6, NA, NA), sl_premium = NA_real_,
    stringsAsFactors = FALSE
  )
  prog_fields <- names(prog_defaults)
  prog_rows <- seq_len(nrow(prog_defaults))
  prog_id <- function(field, i) paste0("prog_", field, "_", i)
  loaded_programmes <- reactiveVal(NULL)
  prog_value <- function(field, i) {
    x <- input[[prog_id(field, i)]]
    if (!is.null(x)) return(x)
    saved <- loaded_programmes()
    if (!is.null(saved) && !is.null(saved[[field]])) return(saved[[field]][i])
    prog_defaults[[field]][i]
  }

  output$programme_inputs <- renderUI({
    head <- function(x) div(class = "xlp-year-head", x)
    columns <- c("Programme", "Surplus retention", "Lines", "QS %", "Comm. %", "Layer deductible", "Layer limit", "Reinst.",
                 "Layer premium", "Stop loss attachment", "Stop loss limit", "Stop loss premium")
    cells <- isolate(lapply(prog_rows, function(i) {
      amount <- function(field, label) netsimr_hidden_label(netsimr_amount_input(prog_id(field, i), paste(label, "of programme", i),
                                                                         value = prog_value(field, i), min = 0))
      number_in <- function(field, label, step) netsimr_hidden_label(numericInput(prog_id(field, i), paste(label, "of programme", i),
                                                                              value = prog_value(field, i), min = 0, step = step,
                                                                              width = "100%"))
      inputs <- list(
        netsimr_hidden_label(textInput(prog_id("name", i), paste("Name of programme", i), value = prog_value("name", i), width = "100%")),
        amount("surplus_retention", "Surplus retention"), number_in("surplus_lines", "Surplus lines", 1),
        number_in("qs", "Quota share (%)", 5), number_in("commission", "Commission (%)", 1),
        amount("xl_deductible", "Layer deductible"), amount("xl_limit", "Layer limit"),
        number_in("xl_reinstatements", "Reinstatements", 1), amount("xl_premium", "Layer premium"),
        amount("sl_attachment", "Stop loss attachment"), amount("sl_limit", "Stop loss limit"), amount("sl_premium", "Stop loss premium")
      )
      # a row of the grid on a wide screen; a card on a phone, each input under its column's name
      div(class = "xlp-prog-row", Map(function(input, label) div(class = "xlp-prog-cell", `data-label` = label, input),
                                      inputs, columns))
    }))
    tagList(
      div(class = "xlp-year-wrap",
          div(class = "xlp-year-grid xlp-prog-grid",
              style = paste0("grid-template-columns: minmax(14rem, 1.8fr) minmax(7rem, 1fr) repeat(3, minmax(4.5rem, 0.6fr)) ",
                             "repeat(2, minmax(7rem, 1fr)) minmax(4.5rem, 0.6fr) repeat(4, minmax(7rem, 1fr));",
                             " min-width: 1420px;"),
              lapply(columns, head), cells)),
      netsimr_help("Leave a programme's name empty to leave it out, and a part empty when the programme does not have it. Empty",
               "premiums are priced at the loading; empty reinstatements are unlimited."),
      netsimr_help("A surplus treaty cedes of each risk the part of its sum insured above the retention, up to the lines (each a",
               "retention), and that share of its premium and claims. Its risks are the Exposure tab's risk profile: a claim",
               "falls on a band with the chance of the band's share of the premium, whatever the claim's size. The",
               "commission is the quota share's and the surplus treaty's.")
    )
  })

  # the programmes of compare_reinsurance() from the grid
  programme_list <- reactive({
    out <- list()
    for (i in prog_rows) {
      name <- trimws(as.character(or_default(prog_value("name", i), "")))
      if (!nzchar(name)) next
      v <- function(field) number(prog_value(field, i))
      p <- list()
      if (is.finite(v("surplus_retention")) && v("surplus_retention") > 0) {
        validate(need(is.finite(v("surplus_lines")) && v("surplus_lines") > 0,
                      paste0("Enter the lines of the surplus treaty of '", name, "', above 0.")))
        # the risks of the Exposure tab's profile, each band with its share of the premium
        risks <- profile()$profile
        sizes <- or_default(risks$sum_insured, risks$policy_limit)
        p$surplus <- list(retention = v("surplus_retention"), lines = v("surplus_lines"), sum_insured = sizes,
                          weights = if (sum(risks$premium) > 0) risks$premium,
                          commission = if (is.finite(v("commission"))) v("commission") / 100 else 0)
      }
      if (is.finite(v("qs")) && v("qs") > 0) {
        validate(need(v("qs") <= 100, paste0("The quota share of '", name, "' must be at most 100%.")))
        p$quota_share <- v("qs") / 100
        p$commission <- if (is.finite(v("commission"))) v("commission") / 100 else 0
      }
      if (is.finite(v("xl_limit")) && v("xl_limit") > 0) {
        p$xl <- list(deductible = if (is.finite(v("xl_deductible"))) v("xl_deductible") else 0, limit = v("xl_limit"),
                     reinstatements = if (is.finite(v("xl_reinstatements"))) v("xl_reinstatements") else Inf,
                     reinstatement_rate = layer()$reinstatement_rate)
        if (is.finite(v("xl_premium"))) p$xl$premium <- v("xl_premium")
      }
      if (is.finite(v("sl_limit")) && v("sl_limit") > 0) {
        p$stop_loss <- list(attachment = if (is.finite(v("sl_attachment"))) v("sl_attachment") else 0, limit = v("sl_limit"))
        if (is.finite(v("sl_premium"))) p$stop_loss$premium <- v("sl_premium")
      }
      validate(need(length(p) > 0, paste0("Give '", name, "' a surplus retention, a quota share, a layer limit or a stop loss limit.")),
               need(is.null(out[[name]]) && name != "No reinsurance", paste0("Give each programme its own name ('", name, "').")))
      out[[name]] <- p
    }
    validate(need(length(out) > 0, "Name at least one programme."))
    out
  })

  # the gross portfolio: a claim count and a claim size distribution, each with up to three
  # parameters, and the Pareto slices of a fitted model that was loaded
  prog_models <- list(frequency = xlp_prog_frequencies, severity = xlp_prog_severities)
  prog_param_id <- function(what, j) paste0(if (what == "frequency") "prog_f" else "prog_s", j)
  prog_slices <- reactiveVal(NULL)
  # a distribution chosen: its parameters' names, and their defaults unless a file gave them
  for (kind in names(prog_models)) local({
    what <- kind
    observeEvent(input[[paste0("prog_", what)]], {
      d <- prog_models[[what]][[input[[paste0("prog_", what)]]]]
      if (is.null(d)) return()
      keep <- own_updates$is_echo(paste0("prog_", what), input[[paste0("prog_", what)]])
      for (j in seq_along(d$params)) {
        if (keep) {
          updateNumericInput(session, prog_param_id(what, j), label = d$params[j])
        } else {
          updateNumericInput(session, prog_param_id(what, j), label = d$params[j], value = d$defaults[j])
        }
      }
    }, ignoreInit = TRUE)
  })

  # sets a distribution and its parameters, from a file
  set_prog_model <- function(what, name, params) {
    id <- paste0("prog_", what)
    if (!identical(name, input[[id]])) own_updates$expect(id, name)
    updateSelectInput(session, id, selected = name)
    labels <- prog_models[[what]][[name]]$params
    for (j in seq_along(labels)) updateNumericInput(session, prog_param_id(what, j), label = labels[j], value = params[j])
  }

  # the chosen distribution with its parameters, as compare_reinsurance() takes them
  prog_model <- function(what) {
    name <- or_default(input[[paste0("prog_", what)]], names(prog_models[[what]])[1])
    d <- prog_models[[what]][[name]]
    validate(need(!is.null(d), paste("Choose the claim", if (what == "frequency") "count" else "size", "distribution.")))
    list(name = name, labels = d$params,
         params = vapply(seq_along(d$params), function(j) number(input[[prog_param_id(what, j)]]), numeric(1)))
  }

  # a fitted model: a settings file of NetDisFit's Export tab, or of the simulator
  observeEvent(input$prog_fit, {
    fail <- function(message) showNotification(paste("The fitted model could not be loaded:", message), type = "error", duration = 10)
    settings <- tryCatch(read_settings_file(input$prog_fit$datapath, sim_settings_tool), error = function(e) e)
    if (inherits(settings, "error")) return(fail(conditionMessage(settings)))
    if (settings$version > sim_settings_version) return(fail("it was saved by a newer version of NetSimR."))
    fit <- xlp_fit_from_settings(settings$values)
    if (is.null(fit$frequency) && is.null(fit$severity)) {
      return(fail(if (length(fit$notes) > 0) paste0(paste(fit$notes, collapse = "; "), ".") else
        "the file holds no claim count or claim size distribution."))
    }
    if (!is.null(fit$frequency)) set_prog_model("frequency", fit$frequency$name, fit$frequency$params)
    if (!is.null(fit$severity)) {
      set_prog_model("severity", fit$severity$name, fit$severity$params)
      prog_slices(fit$slices)
    }
    loaded <- c(if (!is.null(fit$frequency)) paste("the", fit$frequency$name, "claim count"),
                if (!is.null(fit$severity)) paste0("the ", fit$severity$name, " claim size",
                                                   if (!is.null(fit$slices)) " with its Pareto tail"))
    showNotification(paste0("Loaded ", paste(loaded, collapse = " and "), " of ", input$prog_fit$name, ".",
                            if (length(fit$notes) > 0) paste0(" Left out: ", paste(fit$notes, collapse = "; "), "."),
                            " Click Compare to simulate with ", if (length(loaded) > 1) "them." else "it."),
                     type = if (length(fit$notes) > 0) "warning" else "message", duration = 8)
  })

  observeEvent(input$prog_tail_remove, prog_slices(NULL))

  prog_tail_text <- function() {
    s <- prog_slices()
    if (is.null(s)) return(NULL)
    paste0("Pareto tail: ", paste0("alpha ", netsimr_fmt(s$alpha, 4), " above ", netsimr_amount(s$threshold), collapse = "; "), ".")
  }

  # the Pareto slices of the loaded model, which have no inputs of their own
  output$prog_tail <- renderUI({
    text <- prog_tail_text()
    if (is.null(text)) return(NULL)
    netsimr_help(text, "It replaces the tail of the claim size above, from the fitted model that was loaded.",
             actionLink("prog_tail_remove", "Remove the tail"))
  })

  programmes_run <- eventReactive(input$run_programmes, {
    frequency <- prog_model("frequency")
    severity <- prog_model("severity")
    fp <- frequency$params
    sp <- severity$params
    # (the probability of an extra zero may be 0, and is below 1)
    zero <- grepl("Extra zeros", frequency$labels, fixed = TRUE)
    validate(need(all(is.finite(fp)) && all(fp[!zero] > 0), "Enter the claim count parameters, above 0."),
             need(all(fp[zero] >= 0 & fp[zero] < 1), "The probability of an extra zero must be at least 0 and below 1."),
             need(all(is.finite(sp)), "Enter the claim size parameters."))
    tail <- prog_slices()
    n <- number(input$prog_n)
    validate(need(is.finite(n) && n >= 1000 && n <= 2e5, "The simulated years must be from 1,000 to 200,000."))
    loading <- number(input$prog_loading)
    coc <- number(input$prog_coc)
    seed <- number(input$prog_seed)
    validate(need(is.finite(loading) && loading >= 0 && loading < 100, "The loading must be from 0% to below 100%."),
             need(is.finite(coc) && coc >= 0, "Enter the cost of capital, at least 0%."))
    withProgress(message = "Simulating the programmes", value = 0.3, {
      cmp <- netsimr_checked(attempt(compare_reinsurance(
        frequency$name, fp, severity$name, sp, programme_list(), premium = rating()$premium, n = round(n),
        level = as.numeric(or_default(input$prog_level, "0.995")), loading = loading / 100, coc_rate = coc / 100,
        seed = if (is.finite(seed)) round(seed), pareto_thresholds = tail$threshold, pareto_alphas = tail$alpha
      )))
      # the gross portfolio of the run, for the report
      cmp$portfolio <- list(
        frequency = paste0(frequency$name, " (", paste(netsimr_fmt(fp, 6), collapse = ", "), ")"),
        severity = paste0(severity$name, " (", paste(netsimr_fmt(sp, 6), collapse = ", "), ")"),
        tail = prog_tail_text()
      )
      cmp
    })
  })

  current_programmes <- reactive({
    validate(need(isTruthy(input$run_programmes), "Click Compare to simulate the programmes."))
    programmes_run()
  })

  output$programme_stats <- renderUI({
    cmp <- current_programmes()
    s <- cmp$summary
    best <- s[-1, ][which.max(s$value[-1]), ]
    gross <- s[1, ]
    label <- paste0(format(100 * cmp$level), "%")
    div(class = "dft-stats",
        netsimr_stat_tile("Capital without reinsurance", netsimr_amount(gross$capital), icon_name = "triangle-exclamation",
                      note = paste("Mean result", netsimr_amount(gross$mean_result), "less its", label, "worst")),
        netsimr_stat_tile("Best value", best$programme, icon_name = "trophy", accent = TRUE,
                      note = paste0(if (best$value >= 0) "Saves " else "Costs ", netsimr_amount(abs(best$value)), " a year net of its margin")),
        netsimr_stat_tile("Its capital relief", netsimr_amount(best$capital_relief), icon_name = "shield-halved",
                      note = paste("For a margin of", netsimr_amount(best$margin))),
        netsimr_stat_tile("Subject premium", netsimr_amount(cmp$premium), icon_name = "coins",
                      note = paste(format(cmp$n, big.mark = ",", scientific = FALSE), "simulated years")))
  })

  programme_table <- function() {
    cmp <- current_programmes()
    s <- cmp$summary
    rows <- lapply(seq_len(nrow(s)), function(i) {
      list(s$programme[i], netsimr_amount(s$expected_ceded[i]), netsimr_amount(s$reinsurance_premium[i]), netsimr_amount(s$margin[i]),
           netsimr_amount(s$mean_result[i]), netsimr_percent(s$p_loss[i]), netsimr_amount(s$capital[i]), netsimr_amount(s$capital_relief[i]),
           netsimr_amount(s$value[i]))
    })
    list(header = c("Programme", "Expected ceded", "Reinsurance premium", "Margin", "Mean result", "Loss-making years", "Capital",
                    "Capital relief", "Value"), rows = rows, stack = TRUE,
         note = paste0("Reinsurance premium: the premiums paid less any ceding commission, with the expected reinstatement premiums.",
                            " Margin: that premium less the expected recoveries. Capital: the mean net result less its ",
                            format(100 * (1 - cmp$level)), "% worst. Value: ", netsimr_percent(cmp$coc_rate, 0),
                            " of the capital relief less the margin, positive when the capital saved is worth more than the",
                            " reinsurance costs."))
  }

  output$programme_table <- renderUI(do.call(netsimr_table, programme_table()))

  add_chart("programme_plot", function(in_dark) {
    s <- current_programmes()$summary
    netsimr_category_chart(s$programme, bars = list(name = "Capital", values = s$capital, colour = xlp_chart_colours(in_dark)$accent_soft),
                       lines = list(list(name = "Margin paid to reinsurers", values = s$margin, colour = netsimr_palette[1])),
                       x_title = "Programme", y_title = "Amount", dark = in_dark)
  }, alt = "The capital each programme needs and the margin it pays to the reinsurers")

  add_chart("programme_return_plot", function(in_dark) {
    cmp <- current_programmes()
    p <- exp(seq(log(0.5), log(1 - 10 / cmp$n), length.out = 200))
    periods <- 1 / (1 - p)
    series <- lapply(seq_len(ncol(cmp$results)), function(j) {
      list(name = colnames(cmp$results)[j], x = periods, y = stats::quantile(cmp$results[, j], 1 - p, names = FALSE),
           colour = c(xlp_chart_colours(in_dark)$accent, netsimr_palette)[j])
    })
    netsimr_line_chart(series, "Return period (years)", "Net result in the 1 in N bad year", in_dark, x_log = TRUE)
  }, alt = "The net result of each programme in a bad year of each return period")

  output$download_programmes <- downloadHandler(
    filename = function() "reinsurance_programmes.csv",
    content = function(file) utils::write.csv(csv_safe(current_programmes()$summary), file, row.names = FALSE)
  )

  ######################
  #Report
  ######################

  # what the report shows: the layer and its pricing, and each part of the tool that can be
  # found (a part that cannot, such as a Pareto rating without enough losses, is left out)
  report_data <- function() {
    figure <- function(id, title, note = NULL, wide = FALSE) c(charts[[id]], list(title = title, note = note, wide = wide))
    count <- function(n) format(n, big.mark = ",", scientific = FALSE)
    lay <- layer()
    rt <- rating()
    # each part the report cannot have is listed in its section of what it leaves out, with why
    omitted <- list()
    part <- function(name, expr) {
      x <- netsimr_report_part(expr)
      if (!inherits(x, "error")) return(x)
      omitted[[name]] <<- x
      NULL
    }
    p <- part("Pricing", pricing())
    blended <- if (!is.null(p)) p[p$method == "Blended", ]
    ok <- !is.null(blended) && is.finite(blended$rate)
    b <- part("Burning cost", bc())
    pa <- part("Pareto rating", pareto())
    e <- part("Exposure rating", exposure())
    cmp <- if (isTruthy(input$run_programmes)) part("Reinsurance programmes", current_programmes())
    reinstatements <- if (is.finite(lay$reinstatements)) format(lay$reinstatements) else "Unlimited"

    r <- list()
    r$key <- c(
      list(Layer = list(value = xlp_layer_label(lay$deductible, lay$limit),
                        note = paste(reinstatements, if (identical(reinstatements, "1")) "reinstatement at" else "reinstatements at",
                                     netsimr_percent(lay$reinstatement_rate, 0))),
           `Technical premium` = list(value = if (ok) netsimr_amount(blended$upfront_premium) else netsimr_dash,
                                      note = if (ok && blended$reinstatement_premium > 0) {
                                        paste("Upfront; and", netsimr_amount(blended$reinstatement_premium), "expected from reinstatements")
                                      } else "Upfront"),
           `Rate on line` = list(value = if (ok) netsimr_percent(blended$rate_on_line, 2) else netsimr_dash,
                                 note = if (ok) paste("Payback", formatC(blended$payback, format = "f", digits = 1), "years")),
           `Expected layer loss` = list(value = if (ok) netsimr_amount(blended$expected_loss) else netsimr_dash,
                                        note = if (!is.null(p)) paste0(round(100 * attr(p, "weight")), "% ", attr(p, "experience"),
                                                                       ", ", round(100 * (1 - attr(p, "weight"))), "% exposure"))),
      if (!is.null(cmp)) {
        best <- cmp$summary[-1, ][which.max(cmp$summary$value[-1]), ]
        list(`Best value programme` = list(value = best$programme, note = paste0(if (best$value >= 0) "Saves " else "Costs ",
                                                                                netsimr_amount(abs(best$value)), " a year")))
      }
    )
    r$warnings <- if (!is.null(p)) {
      missing <- !is.finite(p$rate) & nzchar(p$note)
      if (any(missing)) paste0(p$method[missing], ": ", p$note[missing])
    } else "The layer cannot be priced: check the settings of the Experience and Exposure tabs."

    used <- if (!is.null(b)) b$by_year$year[b$by_year$included]
    threshold <- number(input$threshold)
    pareto_alpha <- number(input$pareto_alpha)
    property <- !is.null(e) && identical(e$type, "property")
    profile_source <- if (identical(input$profile_source, "upload")) or_default(input$profile_file$name, "An uploaded file") else
      names(xlp_profile_examples)[match(or_default(input$profile_example, "property"), xlp_profile_examples)]
    r$settings <- list(
      Layer = list(Deductible = netsimr_amount(lay$deductible), Limit = netsimr_amount(lay$limit), Reinstatements = reinstatements,
                   `Reinstatement rate` = netsimr_percent(lay$reinstatement_rate, 0),
                   `Annual aggregate deductible` = if (lay$aggregate_deductible > 0) netsimr_amount(lay$aggregate_deductible),
                   `Annual aggregate limit` = if (is.finite(lay$aggregate_limit)) netsimr_amount(lay$aggregate_limit)),
      `Rating year` = list(`Rating year` = format(rt$year), `Subject premium` = netsimr_amount(rt$premium),
                           Loading = netsimr_percent(rt$loading, 0)),
      Experience = list(
        Losses = if (identical(input$loss_source, "upload")) or_default(input$loss_file$name, "An uploaded file") else "Example large losses",
        Years = if (length(used) > 0) paste(min(used), "to", max(used)),
        `Claims inflation` = if (is.finite(number(input$inflation))) paste0(format(number(input$inflation)), "% a year"),
        `Reporting threshold` = if (is.finite(threshold) && threshold > 0) netsimr_amount(threshold) else "None",
        Weights = if (identical(input$weights, "equal")) "Equal" else "By premium",
        `Pareto threshold` = if (is.finite(number(input$pareto_threshold))) netsimr_amount(number(input$pareto_threshold)),
        `Pareto alpha` = if (is.finite(pareto_alpha)) format(pareto_alpha) else "Estimated"),
      Exposure = list(Profile = profile_source,
                      Business = if (!is.null(e)) if (property) "Property" else "Casualty",
                      `Exposure curve` = if (property) e$curve$label,
                      `Claim sizes` = if (!is.null(e) && !property) e$severity$label,
                      `Expected loss ratio` = if ("loss_ratio" %in% names(quiet(profile()$profile))) "From the profile" else
                        if (is.finite(number(input$loss_ratio))) paste0(format(number(input$loss_ratio)), "%")),
      Blend = if (!is.null(p)) list(`Experience rate` = attr(p, "experience"),
                                    `Weight of the experience` = paste0(round(100 * attr(p, "weight")), "%")),
      Programmes = if (!is.null(cmp)) {
        list(`Claim counts` = cmp$portfolio$frequency, `Claim sizes` = cmp$portfolio$severity,
             `Pareto tail` = if (!is.null(cmp$portfolio$tail)) sub("^Pareto tail: ", "", cmp$portfolio$tail),
             `Simulated years` = count(cmp$n), Level = paste0(format(100 * cmp$level), "%"),
             `Cost of capital` = netsimr_percent(cmp$coc_rate, 0))
      }
    )

    if (!is.null(p)) r$pricing <- part("The pricing section", list(
      tiles = list(`Expected layer loss` = if (ok) netsimr_amount(blended$expected_loss) else netsimr_dash,
                   `Technical premium` = if (ok) netsimr_amount(blended$upfront_premium) else netsimr_dash,
                   `Rate on line` = if (ok) netsimr_percent(blended$rate_on_line, 2) else netsimr_dash,
                   Payback = if (ok) paste(formatC(blended$payback, format = "f", digits = 1), "years") else netsimr_dash,
                   Rate = list(value = if (ok) netsimr_percent(blended$upfront_premium / rt$premium, 2) else netsimr_dash,
                               note = "Upfront premium over subject premium")),
      tables = list(list(table = pricing_table(), title = "Methods")),
      charts = list(figure("method_plot", "Expected loss by method"))
    ))

    if (!is.null(b)) r$experience <- part("The experience section", list(
      tiles = list(Years = list(value = as.character(length(used)), note = paste(min(used), "to", max(used))),
                   `Losses in the layer` = count(b$n_layer_losses),
                   `Burning cost` = list(value = netsimr_percent(b$rate, 2), note = "Of subject premium"),
                   `Expected layer loss` = list(value = netsimr_amount(b$expected_loss), note = "Burning cost"),
                   Pareto = list(value = if (!is.null(pa)) netsimr_percent(pa$rate, 2) else netsimr_dash,
                                 note = if (!is.null(pa)) paste0("Alpha ", formatC(pa$alpha, format = "f", digits = 2)) else "Not available")),
      tables = list(list(table = bc_table(), title = "Burning cost by year")),
      charts = list(figure("burning_cost_plot", "Burning cost by year", wide = TRUE),
                    figure("losses_plot", "Large losses against the layer", wide = TRUE),
                    if (!is.null(pa)) figure("pareto_plot", "Pareto fit", note = pareto_text(), wide = TRUE))
    ))

    if (!is.null(e)) r$exposure <- part("The exposure section", list(
      tiles = list(Bands = list(value = as.character(nrow(e$bands)),
                                note = paste0(if (property) "Property" else "Casualty", "; ",
                                              if (property) e$curve$label else e$severity$label)),
                   `Ground-up loss` = list(value = netsimr_amount(e$ground_up_loss), note = paste("Of a premium of", netsimr_amount(e$subject_premium))),
                   `Exposure rate` = list(value = netsimr_percent(e$rate, 2), note = "Of subject premium"),
                   `Expected layer loss` = list(value = netsimr_amount(e$rate * rt$premium), note = "At the rating year's premium")),
      tables = list(list(table = exposure_table(), title = "Bands of the risk profile")),
      charts = list(figure("band_plot", "Expected layer loss by band"),
                    figure("curve_plot", if (property) "Exposure curve" else "Limited expected value"))
    ))

    if (!is.null(cmp)) r$programmes <- part("The programmes section", {
      s <- cmp$summary
      best <- s[-1, ][which.max(s$value[-1]), ]
      list(
        tiles = list(`Capital without reinsurance` = netsimr_amount(s$capital[1]),
                     `Best value` = list(value = best$programme, note = paste0(if (best$value >= 0) "Saves " else "Costs ",
                                                                             netsimr_amount(abs(best$value)), " a year net of its margin")),
                     `Its capital relief` = list(value = netsimr_amount(best$capital_relief), note = paste("For a margin of", netsimr_amount(best$margin))),
                     `Subject premium` = list(value = netsimr_amount(cmp$premium), note = paste(count(cmp$n), "simulated years"))),
        tables = list(list(table = programme_table(), title = "Programmes")),
        charts = list(figure("programme_plot", "Capital and margin", wide = TRUE),
                      figure("programme_return_plot", "Net result by return period", wide = TRUE))
      )
    })
    r$omitted <- omitted
    r
  }

  output$download_report <- downloadHandler(
    filename = function() "reinsurance_pricing_report.html",
    content = function(file) write_reinsurance_report(file, report_data())
  )

  ######################
  #Settings file
  ######################

  output$save_settings <- downloadHandler(
    filename = function() "reinsurance_pricing_settings.txt",
    content = function(file) {
      values <- collect_setting_values(input, xlp_settings_inputs)
      v <- tryCatch(year_values(), error = function(e) NULL)
      if (!is.null(v)) {
        values$year_labels <- v$year
        values$year_premium <- v$premium
        values$year_index <- v$index
        values$year_development <- v$development
        values$year_include <- v$include
      }
      # the Pareto tail of a fitted model that was loaded
      tail <- prog_slices()
      if (!is.null(tail)) {
        values$prog_tail_thresholds <- tail$threshold
        values$prog_tail_alphas <- tail$alpha
      }
      # the programmes' grid, a vector per column
      for (field in prog_fields) {
        values[[paste0("prog_", field, "s")]] <- if (field == "name") {
          vapply(prog_rows, function(i) as.character(or_default(prog_value("name", i), "")), character(1))
        } else {
          vapply(prog_rows, function(i) number(prog_value(field, i)), numeric(1))
        }
      }
      write_settings_file(values, file, xlp_settings_tool, xlp_settings_version)
    }
  )

  observeEvent(input$load_settings, {
    v <- load_settings_values(input$load_settings$datapath, xlp_settings_tool, xlp_settings_version)
    if (is.null(v)) return()
    # the choices of the radio buttons and selects, as the page offers them
    choices <- tool_input_choices("reinsurance", reinsurance_pricing_tool_UI)
    usable <- function(id) setting_usable(v[[id]], xlp_settings_inputs[[id]], choices[[id]])
    if (usable("severity") && !identical(v$severity, input$severity)) own_updates$expect("severity", v$severity)
    # (the gross portfolio's distributions too; a file without a Pareto tail has none)
    for (what in c("frequency", "severity")) {
      id <- paste0("prog_", what)
      if (usable(id) && !identical(v[[id]], input[[id]])) own_updates$expect(id, v[[id]])
    }
    thresholds <- suppressWarnings(as.numeric(v$prog_tail_thresholds))
    alphas <- suppressWarnings(as.numeric(v$prog_tail_alphas))
    prog_slices(if (length(thresholds) > 0 && length(alphas) == length(thresholds) && !anyNA(c(thresholds, alphas)) &&
                    all(thresholds > 0) && all(alphas > 0) && all(diff(thresholds) > 0)) {
      data.frame(threshold = thresholds, alpha = alphas)
    })
    for (id in names(xlp_settings_inputs)) {
      if (usable(id)) update_setting_input(session, id, xlp_settings_inputs[[id]], v[[id]])
    }
    loaded_columns <<- v[intersect(names(v), names(xlp_settings_inputs)[xlp_settings_inputs == "column"])]
    # the first and last years apply once the file's losses show (new losses reset them)
    new_losses <- (usable("loss_source") && !identical(v$loss_source, input$loss_source)) ||
      (identical(v$loss_source, "upload") && is.null(input$loss_file))
    if (new_losses) {
      loaded_years <<- list(first = if (usable("first_year")) v$first_year, last = if (usable("last_year")) v$last_year)
    }
    labels <- suppressWarnings(as.numeric(v$year_labels))
    if (length(labels) > 0 && !anyNA(labels)) {
      take <- function(x, logical = FALSE) {
        if (length(x) != length(labels)) return(rep(if (logical) TRUE else NA_real_, length(labels)))
        if (logical) as.logical(x) %in% TRUE else suppressWarnings(as.numeric(x))
      }
      saved <- list(year = labels, premium = take(v$year_premium), index = take(v$year_index),
                    development = take(v$year_development), include = take(v$year_include, logical = TRUE))
      loaded_by_year(saved)
      if (!new_losses) {
        # the years showing may be the file's: set their inputs now
        ys <- isolate(years())
        at <- match(ys, saved$year)
        for (i in which(!is.na(at))) {
          updateNumericInput(session, year_id("premium", i), value = saved$premium[at[i]])
          updateNumericInput(session, year_id("index", i), value = saved$index[at[i]])
          updateNumericInput(session, year_id("development", i), value = saved$development[at[i]])
          updateCheckboxInput(session, year_id("include", i), value = saved$include[at[i]])
        }
      }
    }
    # the programmes' grid: a file of fewer programmes, or without a column (one saved before
    # the surplus treaty), leaves the rest empty
    grid <- if (length(v$prog_names) > 0) lapply(stats::setNames(prog_fields, prog_fields), function(field) {
      x <- v[[paste0("prog_", field, "s")]]
      x <- if (field == "name") as.character(x) else suppressWarnings(as.numeric(x))
      c(x, rep(if (field == "name") "" else NA_real_, length(prog_rows)))[prog_rows]
    })
    if (length(grid) > 0) {
      loaded_programmes(grid)
      for (field in names(grid)) for (i in prog_rows) {
        if (field == "name") updateTextInput(session, prog_id(field, i), value = grid[[field]][i]) else
          updateNumericInput(session, prog_id(field, i), value = grid[[field]][i])
      }
    }
    uploads <- c(if (identical(v$loss_source, "upload")) "the losses", if (identical(v$profile_source, "upload")) "the risk profile")
    showNotification(paste0("Loaded the settings of ", input$load_settings$name,
                            if (length(uploads) > 0) paste0(". Upload ", paste(uploads, collapse = " and "),
                                                            " to apply their columns.") else "."),
                     type = "message", duration = 6)
  })

  # the summary tiles are hidden while empty, and shiny does not render hidden outputs,
  # so they must render even when hidden or they would never appear; the profile type
  # drives the curve settings, shown on another tab
  for (id in c("layer_stats", "experience_stats", "exposure_stats", "pricing_stats", "profile_type", "programme_stats")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
