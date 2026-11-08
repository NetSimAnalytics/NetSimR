#' Server function of the reserving tool
#'
#' @description Builds the triangle from the example or the uploaded file, fits
#'   the chain ladder with the user's development settings, computes the
#'   ultimates of each method with the premiums and loss ratios of each origin,
#'   and runs the bootstrap; paired with \code{reserving_tool_UI} by
#'   \code{\link{run_shiny_reserving_tool}}.
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @keywords internal
#' @import shiny
reserving_tool_Server <- function(input, output, session) {

  or_default <- function(x, default) if (is.null(x)) default else x

  # the charts are drawn in the colours of the theme the page reports
  dark <- reactive(identical(input$app_theme, "dark"))
  chart <- function(draw, alt) netsimr_render_plot(draw(), bg = "transparent", res = 96, alt = alt)

  # the value of an expression, or a message in place of the outputs that use it
  checked <- function(expr) {
    result <- tryCatch(suppressWarnings(expr), error = function(e) e)
    validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result)))
    result
  }

  ######################
  #Data
  ######################

  upload_data <- reactive({
    validate(need(input$file1, "Upload a data file, or choose an example."))
    read_error <- NULL
    df <- tryCatch(
      dft_read_data(input$file1$datapath, header = isTRUE(input$data_includes_header), sep = or_default(input$sep, ","),
                    quote = or_default(input$quote, "\""), dec = or_default(input$dec, ".")),
      error = function(e) {
        read_error <<- conditionMessage(e)
        NULL
      }
    )
    validate(need(is.null(read_error), paste0("The file could not be read (", read_error, "). Check the separator and quote settings.")))
    validate(need(ncol(df) > 1 && nrow(df) > 0, "The file needs at least two columns and one row of data."))
    df
  })

  # offer the file's columns, guessing each from the names (a column the user chose is kept
  # while the file has it)
  observeEvent(upload_data(), {
    df <- upload_data()
    columns <- names(df)
    numbers <- dft_numeric_columns(df, or_default(input$dec, "."))
    # a column named in a loaded settings file, then the one the user chose, then a guess
    wanted <- loaded_columns
    keep <- function(id, guess) {
      if (!is.null(wanted[[id]]) && wanted[[id]] %in% c("none", columns)) return(wanted[[id]])
      current <- input[[id]]
      if (!is.null(current) && current %in% columns) current else guess
    }
    origin <- keep("origin_col", or_default(rsv_guess_column(columns, "origin|accident|underwriting|year|period|cohort|uwy|ay"), columns[1]))
    development <- keep("development_col", or_default(rsv_guess_column(numbers, "dev|age|lag|delay|month", origin),
                                                      setdiff(numbers, origin)[1]))
    value <- keep("value_col", or_default(rsv_guess_column(numbers, "paid|incurred|amount|claim|loss|value|cost", c(origin, development)),
                                          setdiff(numbers, c(origin, development))[1]))
    premium_guess <- or_default(rsv_guess_column(numbers, "prem|exposure|epi", c(origin, development, value)), "none")
    premium <- keep("premium_col", premium_guess)
    incurred <- keep("incurred_col", or_default(rsv_guess_column(numbers, "incurred|reported", c(origin, development, value, premium)),
                                                "none"))
    updateSelectizeInput(session, "origin_col", choices = columns, selected = origin)
    updateSelectizeInput(session, "development_col", choices = columns, selected = development)
    updateSelectizeInput(session, "value_col", choices = columns, selected = value)
    updateSelectizeInput(session, "premium_col", choices = c("None" = "none", columns), selected = premium)
    updateSelectizeInput(session, "incurred_col", choices = c("None" = "none", columns), selected = incurred)
  })

  # the triangle and premiums, and the data they come from
  data_input <- reactive({
    if (identical(input$data_source, "upload")) {
      df <- upload_data()
      premium <- input$premium_col
      incurred <- input$incurred_col
      result <- checked(rsv_upload_input(
        df, or_default(input$layout, "long"), input$origin_col, input$development_col, input$value_col,
        premium = if (!identical(premium, "none")) premium, cumulative = !identical(input$cumulative, "incremental"),
        dec = or_default(input$dec, "."), incurred = if (!identical(incurred, "none")) incurred
      ))
      checked(reserving_check_triangle(result$triangle))
      c(result, list(data = df, description = NULL, source = input$file1$name))
    } else {
      example <- or_default(input$example, "raa")
      c(rsv_example_input(example), list(source = names(rsv_examples)[match(example, rsv_examples)]))
    }
  })
  triangle <- reactive(data_input()$triangle)
  data_premium <- reactive({
    p <- data_input()$premium
    if (is.null(p)) rep(NA_real_, nrow(triangle())) else p
  })

  output$data_overview <- renderUI({
    tri <- triangle()
    checked <- reserving_check_triangle(tri)
    latest <- tri[cbind(seq_len(nrow(tri)), checked$latest)]
    premium <- data_premium()
    div(
      class = "dft-stats",
      dft_stat_tile("Source", div(class = "dft-file-name", data_input()$source), icon_name = "file-lines",
                    note = if (!is.null(data_input()$incurred)) "With incurred claims"),
      dft_stat_tile("Origins", format(nrow(tri), big.mark = ","), icon_name = "calendar",
                    note = paste(rownames(tri)[1], "to", rownames(tri)[nrow(tri)])),
      dft_stat_tile("Development periods", format(ncol(tri), big.mark = ","), icon_name = "arrow-right"),
      dft_stat_tile("Latest diagonal", rsv_amount(sum(latest)), icon_name = "layer-group"),
      dft_stat_tile("Premium", if (all(is.na(premium))) "None" else rsv_amount(sum(premium, na.rm = TRUE)), icon_name = "coins",
                    note = if (any(is.na(premium)) && !all(is.na(premium))) "Some origins have none")
    )
  })

  output$triangle_table <- renderUI(rsv_triangle_table(triangle(), note = data_input()$description))
  output$incremental_table <- renderUI(rsv_triangle_table(rsv_increments(triangle()),
                                                          note = "Each period's claims: the change in the cumulative claims."))
  output$data_table <- renderUI(dft_data_preview(data_input()$data, 100))

  ######################
  #Development factors
  ######################

  # what a loaded settings file sets once its triangle shows: the columns of an uploaded file,
  # the periods used, the tail curve's first factor and the link ratios left out
  loaded_columns <- list()
  pending <- NULL
  # each origin's premium, a priori loss ratio and method from a loaded settings file, by label
  loaded_origins <- reactiveVal(NULL)
  # set when a loaded file changes the loss ratio or method for all origins, which must then
  # not overwrite the file's values of each origin
  skip_push <- list(prior = FALSE, method = FALSE)

  # each new triangle starts with every link ratio included (or those of a loaded settings
  # file left out), with the period choices it allows, and with new inputs for its origins;
  # the same triangle read again (another premium column) keeps them
  excluded <- reactiveVal(NULL)
  last_triangle <- NULL
  generation <- reactiveVal(0)
  observeEvent(triangle(), {
    tri <- triangle()
    if (identical(tri, last_triangle)) return()
    last_triangle <<- tri
    generation(isolate(generation()) + 1)
    ratios <- reserving_link_ratios(tri)
    available <- which(!is.na(ratios), arr.ind = TRUE)
    diagonals <- length(unique(available[, 1] + available[, 2]))
    recent <- seq_len(max(diagonals - 1, 0))
    period_choices <- c("All" = "all", stats::setNames(recent, paste("Latest", recent)))
    tail_choices <- stats::setNames(seq_len(ncol(ratios)), colnames(ratios))
    wanted <- pending
    pending <<- NULL
    choose <- function(value, choices, default) if (!is.null(value) && as.character(value) %in% choices) as.character(value) else default
    excluded(rsv_exclusion_matrix(if (is.null(wanted)) character(0) else wanted$exclusions, ratios))
    updateSelectInput(session, "periods", choices = period_choices, selected = choose(wanted$periods, period_choices, "all"))
    updateSelectInput(session, "tail_from", choices = tail_choices, selected = choose(wanted$tail_from, tail_choices, "1"))
  }, priority = 10)

  observeEvent(input$ratio_toggle, {
    position <- input$ratio_toggle
    ex <- excluded()
    i <- as.integer(position$row)
    j <- as.integer(position$col)
    if (is.null(ex) || length(i) != 1 || length(j) != 1 || is.na(i) || is.na(j) || i > nrow(ex) || j > ncol(ex)) return()
    ex[i, j] <- !ex[i, j]
    excluded(ex)
  })
  observeEvent(input$include_all, {
    ex <- excluded()
    if (!is.null(ex)) excluded(ex & FALSE)
  })

  periods_used <- reactive(if (is.null(input$periods) || identical(input$periods, "all")) NULL else as.integer(input$periods))

  tail_setting <- reactive({
    method <- or_default(input$tail_method, "none")
    if (method == "none") return(1)
    if (method == "factor") {
      validate(need(is.numeric(input$tail_factor) && isTRUE(input$tail_factor > 0), "Enter a tail factor above 0."))
      return(input$tail_factor)
    }
    method
  })

  cl <- reactive({
    tri <- triangle()
    ex <- excluded()
    if (!identical(dim(ex), dim(reserving_link_ratios(tri)))) ex <- NULL
    from <- suppressWarnings(as.integer(or_default(input$tail_from, 1)))
    checked(chain_ladder(tri, average = or_default(input$average, "volume"), periods = periods_used(), exclude = ex,
                         tail = tail_setting(), tail_from = if (is.na(from)) 1 else from,
                         sigma = or_default(input$sigma, "log-linear"), tail_periods = tail_periods()))
  })

  # the periods a tail curve is extrapolated over
  tail_periods <- reactive({
    periods <- input$tail_periods
    if (!is.numeric(periods) || length(periods) != 1 || !is.finite(periods)) return(100)
    min(max(round(periods), 1), 1000)
  })

  # the tail factor of each curve, fitted to the factors of the Development tab, and of Clark's
  # growth curves (LDF method, with the truncation of the More methods tab), with the IBNR each
  # gives the chain ladder: so the choice of a tail can be weighed against the others
  tail_options <- reactive({
    fit <- cl()
    from <- suppressWarnings(as.integer(or_default(input$tail_from, 1)))
    if (is.na(from)) from <- 1
    n <- ncol(fit$triangle)
    truncation <- input$clark_truncation
    truncation <- if (is.numeric(truncation) && length(truncation) == 1 && is.finite(truncation)) truncation else Inf
    untailed <- fit$by_origin$ultimate / fit$tail
    option <- function(name, expr) {
      value <- tryCatch(expr, error = function(e) e)
      if (inherits(value, "error")) return(data.frame(method = name, tail = NA_real_, ibnr = NA_real_, note = conditionMessage(value)))
      data.frame(method = name, tail = value, ibnr = sum(untailed * value) - fit$total[["latest"]], note = "")
    }
    rbind(
      option("No tail", 1),
      option(paste0("Exponential curve, from factor ", from), reserving_tail_curve(fit$factors, "exponential", from, tail_periods())$tail),
      option(paste0("Inverse power curve, from factor ", from),
             reserving_tail_curve(fit$factors, "inverse power", from, tail_periods())$tail),
      option(paste0("Clark log-logistic", if (is.finite(truncation)) paste0(", truncated at ", truncation)),
             1 / clark_growth(triangle(), "loglogistic", truncation = truncation)$growth(n)),
      option(paste0("Clark Weibull", if (is.finite(truncation)) paste0(", truncated at ", truncation)),
             1 / clark_growth(triangle(), "weibull", truncation = truncation)$growth(n))
    )
  })

  output$tail_comparison <- renderUI({
    o <- tail_options()
    fit <- cl()
    rows <- lapply(seq_len(nrow(o)), function(i) {
      list(o$method[i], if (is.finite(o$tail[i])) rsv_ratio(o$tail[i], 4) else dft_dash,
           if (is.finite(o$ibnr[i])) rsv_amount(o$ibnr[i]) else dft_dash, o$note[i])
    })
    rsv_table(c("Tail", "Tail factor", "Chain ladder IBNR", "Note"), rows,
              note = paste0("Each curve's tail beyond the last development period, the curves extrapolated over ",
                            tail_periods(), " periods; Clark's is the growth curve's share still to develop at the end ",
                            "of the triangle. The tail used now is ", rsv_ratio(fit$tail, 4), "."))
  })

  output$development_stats <- renderUI({
    fit <- cl()
    left_out <- sum(!is.na(fit$link_ratios) & !fit$used)
    tail_note <- if (!is.null(fit$tail_curve)) {
      paste(if (fit$tail_curve$curve == "exponential") "Exponential" else "Inverse power", "curve")
    } else if (fit$tail == 1) "No tail" else "Entered"
    div(
      class = "dft-stats",
      dft_stat_tile("Latest", rsv_amount(fit$total[["latest"]]), icon_name = "layer-group"),
      dft_stat_tile("Ultimate", rsv_amount(fit$total[["ultimate"]]), icon_name = "flag-checkered"),
      dft_stat_tile("IBNR", rsv_amount(fit$total[["ibnr"]]), icon_name = "hourglass-half", accent = TRUE,
                    note = paste(rsv_percent(fit$total[["developed"]]), "developed")),
      dft_stat_tile("Tail factor", rsv_ratio(fit$tail, 4), icon_name = "arrow-right-long", note = tail_note),
      dft_stat_tile("Ratios left out", format(left_out), icon_name = "filter",
                    note = paste("of", sum(!is.na(fit$link_ratios))))
    )
  })

  output$link_ratio_table <- renderUI({
    fit <- cl()
    ex <- excluded()
    if (!identical(dim(ex), dim(fit$link_ratios))) ex <- matrix(FALSE, nrow(fit$link_ratios), ncol(fit$link_ratios))
    periods <- periods_used()
    outside <- if (is.null(periods)) matrix(FALSE, nrow(ex), ncol(ex)) else
      !reserving_recent_ratios(fit$link_ratios, fit$latest_period, periods)
    rsv_link_ratio_table(fit, ex, outside)
  })

  output$development_notes <- renderUI({
    fit <- cl()
    average <- c(volume = "volume-weighted averages", simple = "simple averages", regression = "regressions through the origin")[[fit$average]]
    periods <- periods_used()
    text <- paste0("Selected factors: ", average, " of the link ratios",
                   if (!is.null(periods)) paste0(" of the latest ", periods, " calendar periods (the others are muted)"),
                   ", leaving out those struck through. Link ratios from claims of zero or less are left out.")
    p(class = "dft-table-note", paste(c(text, fit$notes), collapse = " "))
  })

  output$development_plot <- chart(function() rsv_development_chart(cl(), dark()),
                                   alt = "Cumulative claims of each origin by development period, observed and projected")
  output$link_ratio_plot <- chart(function() rsv_link_ratio_chart(cl(), dark()),
                                  alt = "Link ratios of each development period, with the selected factors")
  output$tail_plot <- chart(function() {
    fit <- cl()
    validate(need(!is.null(fit$tail_curve), "Choose an exponential or inverse power tail curve to see it here."))
    rsv_tail_chart(fit, dark())
  }, alt = "Development factors and the fitted tail curve")

  output$projected_table <- renderUI({
    fit <- cl()
    full <- fit$full_triangle
    projected <- cbind(is.na(fit$triangle), TRUE)
    rsv_triangle_table(full, projected, note = paste("Projected values in italics; the ultimate includes the tail factor of",
                                                     rsv_ratio(fit$tail, 4), "."))
  })

  ######################
  #Ultimates
  ######################

  # the inputs of each origin get new ids with each triangle, so values typed for another
  # triangle are never read for this one
  origin_id <- function(kind, i) paste0(kind, "_", generation(), "_", i)

  # the defaults of each origin's inputs: those of a loaded settings file for an origin it
  # names, otherwise the data's premium and the loss ratio and method for all origins
  origin_defaults <- function(tri, premium, prior, method) {
    defaults <- list(premium = premium, prior = rep(if (is.null(prior)) NA_real_ else prior, nrow(tri)),
                     method = rep(method, nrow(tri)))
    saved <- loaded_origins()
    if (!is.null(saved)) {
      at <- match(rownames(tri), saved$label)
      known <- !is.na(at)
      defaults$premium[known] <- saved$premium[at[known]]
      defaults$prior[known] <- saved$prior[at[known]]
      defaults$method[known] <- ifelse(saved$method[at[known]] %in% rsv_methods, saved$method[at[known]], defaults$method[known])
    }
    defaults
  }

  output$origin_inputs <- renderUI({
    fit <- cl()
    tri <- fit$triangle
    defaults <- isolate(origin_defaults(tri, data_premium(), input$prior_loss_ratio, or_default(input$method_all, "cl")))
    head <- function(x) div(class = "rsv-origin-head", x)
    cells <- lapply(seq_len(nrow(tri)), function(i) {
      o <- rownames(tri)[i]
      tagList(
        div(class = "rsv-origin-name", o),
        div(class = "rsv-origin-latest", rsv_amount(fit$by_origin$latest[i])),
        sim_hidden_label(sim_amount_input(origin_id("premium", i), paste("Premium of", o), value = defaults$premium[i], min = 0)),
        sim_hidden_label(numericInput(origin_id("prior", i), paste("A priori loss ratio of", o, "(%)"), value = defaults$prior[i],
                                      min = 0, step = 0.5, width = "100%")),
        sim_hidden_label(selectInput(origin_id("method", i), paste("Method of", o), rsv_methods, selected = defaults$method[i],
                                     selectize = FALSE, width = "100%"))
      )
    })
    div(class = "rsv-origin-wrap",
        div(class = "rsv-origin-grid",
            head("Origin"), div(class = "rsv-origin-head rsv-origin-latest", "Latest"), head("Premium"), head("A priori LR (%)"), head("Method"), cells))
  })

  observeEvent(input$prior_loss_ratio, {
    if (skip_push$prior) {
      skip_push$prior <<- FALSE
      return()
    }
    loaded_origins(NULL)
    for (i in seq_len(nrow(triangle()))) updateNumericInput(session, origin_id("prior", i), value = input$prior_loss_ratio)
  }, ignoreInit = TRUE)
  observeEvent(input$method_all, {
    if (skip_push$method) {
      skip_push$method <<- FALSE
      return()
    }
    loaded_origins(NULL)
    for (i in seq_len(nrow(triangle()))) updateSelectInput(session, origin_id("method", i), selected = input$method_all)
  }, ignoreInit = TRUE)

  # each origin's inputs, or their defaults before the By origin card has been shown
  origin_values <- reactive({
    tri <- triangle()
    m <- nrow(tri)
    defaults <- origin_defaults(tri, data_premium(), input$prior_loss_ratio, or_default(input$method_all, "cl"))
    value <- function(kind, i) {
      x <- input[[origin_id(kind, i)]]
      if (is.null(x)) defaults[[kind]][i] else x
    }
    list(
      premium = vapply(seq_len(m), function(i) {
        x <- value("premium", i)
        if (length(x) == 1 && is.numeric(x)) x else NA_real_
      }, numeric(1)),
      prior = vapply(seq_len(m), function(i) {
        x <- value("prior", i)
        if (length(x) == 1 && is.numeric(x)) x / 100 else NA_real_
      }, numeric(1)),
      method = vapply(seq_len(m), function(i) as.character(value("method", i)), character(1))
    )
  })

  results <- reactive({
    v <- origin_values()
    decay <- or_default(input$decay, 1)
    rsv_method_results(cl(), v$premium, v$prior, decay, v$method)
  })

  output$ultimate_stats <- renderUI({
    r <- results()
    complete <- all(is.finite(r$ultimate))
    premium_known <- all(is.finite(r$premium))
    counts <- table(factor(r$method, levels = names(rsv_methods)))
    counts <- counts[counts > 0]
    methods_note <- if (length(counts) == 1) paste("for all", nrow(r), "origins") else
      paste(paste(counts, names(counts)), collapse = ", ")
    div(
      class = "dft-stats",
      dft_stat_tile("Latest", rsv_amount(sum(r$latest)), icon_name = "layer-group"),
      dft_stat_tile("Ultimate", if (complete) rsv_amount(sum(r$ultimate)) else dft_dash, icon_name = "flag-checkered",
                    note = if (!complete) "Some origins have no ultimate"),
      dft_stat_tile("IBNR", if (complete) rsv_amount(sum(r$ibnr)) else dft_dash, icon_name = "hourglass-half", accent = TRUE),
      dft_stat_tile("Ultimate loss ratio", if (complete && premium_known) rsv_percent(sum(r$ultimate) / sum(r$premium)) else dft_dash,
                    icon_name = "percent", note = if (!premium_known) "Needs every origin's premium"),
      dft_stat_tile("Method", if (length(counts) == 1) names(counts) else "Mixed", icon_name = "list-check", note = methods_note)
    )
  })

  output$ultimate_table <- renderUI({
    r <- results()
    rows <- lapply(seq_len(nrow(r)), function(i) {
      list(r$origin[i], rsv_amount(r$latest[i]), rsv_percent(r$developed[i]), rsv_amount(r$chain_ladder[i]),
           rsv_amount(r$bornhuetter_ferguson[i]), rsv_amount(r$benktander[i]), rsv_amount(r$cape_cod[i]),
           r$method[i], rsv_amount(r$ultimate[i]), rsv_amount(r$ibnr[i]), rsv_percent(r$ultimate_loss_ratio[i]))
    })
    total_of <- function(x) if (all(is.finite(x))) rsv_amount(sum(x)) else dft_dash
    total <- list("Total", total_of(r$latest), rsv_percent(sum(r$latest) / sum(r$chain_ladder)), total_of(r$chain_ladder),
                  total_of(r$bornhuetter_ferguson), total_of(r$benktander), total_of(r$cape_cod), "",
                  total_of(r$ultimate), total_of(r$ibnr),
                  if (all(is.finite(c(r$ultimate, r$premium)))) rsv_percent(sum(r$ultimate) / sum(r$premium)) else dft_dash)
    missing <- r$origin[!is.finite(r$ultimate)]
    cape_cod_ratio <- attr(r, "cape_cod_loss_ratio")
    notes <- c(
      if (is.finite(cape_cod_ratio)) paste0("The Cape Cod loss ratio is ", rsv_percent(cape_cod_ratio),
                                            if (or_default(input$decay, 1) < 1) " over all origins (each origin has its own)", "."),
      if (!all(is.finite(r$premium))) "Bornhuetter-Ferguson and Benktander need the origin's premium, and Cape Cod every origin's.",
      if (!is.null(attr(r, "cape_cod_note"))) attr(r, "cape_cod_note"),
      if (length(missing) > 0) paste0("No ultimate for ", paste(missing, collapse = ", "),
                                      ": its method needs a premium and an a priori loss ratio.")
    )
    rsv_table(c("Origin", "Latest", "Developed", "Chain ladder", "BF", "Benktander", "Cape Cod",
                "Selected", "Ultimate", "IBNR", "Loss ratio"), rows, total,
              c("BF: Bornhuetter-Ferguson.", notes))
  })

  method_ultimates <- function(r) {
    all_methods <- list(`Chain ladder` = r$chain_ladder, `Bornhuetter-Ferguson` = r$bornhuetter_ferguson,
                        Benktander = r$benktander, `Cape Cod` = r$cape_cod)
    all_methods[vapply(all_methods, function(x) any(is.finite(x)), logical(1))]
  }

  output$ultimate_plot <- chart(function() {
    r <- results()
    rsv_ultimate_chart(r$origin, r$latest, r$ibnr, method_ultimates(r), dark())
  }, alt = "Latest claims and IBNR of each origin, with the ultimate of each method")

  output$loss_ratio_plot <- chart(function() {
    r <- results()
    validate(need(any(is.finite(r$premium)), "Enter premiums, in the By origin card or the data, to see the loss ratios."))
    ratios <- lapply(method_ultimates(r), function(u) u / r$premium)
    ratios[["A priori"]] <- r$prior_loss_ratio
    lines <- unname(Map(function(name, values) list(name = name, values = values, colour = rsv_method_colours[[name]]),
                        names(ratios), ratios))
    dft_category_chart(r$origin, lines = lines, x_title = "Origin", y_title = "Ultimate loss ratio", dark = dark(), y_percent = TRUE)
  }, alt = "Ultimate loss ratio of each origin by method, with the a priori loss ratio")

  output$download_results <- downloadHandler(
    filename = function() "reserving_results.csv",
    content = function(file) {
      r <- results()
      r$cdf <- 1 / r$developed
      utils::write.csv(csv_safe(r), file, row.names = FALSE)
    }
  )
  output$download_triangle <- downloadHandler(
    filename = function() "projected_triangle.csv",
    content = function(file) {
      full <- cl()$full_triangle
      utils::write.csv(csv_safe(data.frame(origin = rownames(full), full, check.names = FALSE, stringsAsFactors = FALSE)),
                       file, row.names = FALSE)
    }
  )

  ######################
  #Report
  ######################

  # the choices behind the figures, for the report
  report_settings <- function() {
    fit <- cl()
    tri <- fit$triangle
    r <- results()
    periods <- periods_used()
    tail_text <- if (!is.null(fit$tail_curve)) {
      paste0(if (fit$tail_curve$curve == "exponential") "Exponential" else "Inverse power", " curve from factor ",
             fit$tail_curve$from, ", ", rsv_ratio(fit$tail, 4))
    } else if (fit$tail == 1) "None" else paste("Entered,", rsv_ratio(fit$tail, 4))
    counts <- table(factor(r$method, levels = names(rsv_methods)))
    counts <- counts[counts > 0]
    list(
      Data = list(Source = data_input()$source, Origins = paste(rownames(tri)[1], "to", rownames(tri)[nrow(tri)]),
                  `Development periods` = format(ncol(tri)),
                  Premiums = if (all(is.na(r$premium))) "None" else rsv_amount(sum(r$premium, na.rm = TRUE)),
                  `Incurred claims` = if (is.null(data_input()$incurred)) "No" else "Yes"),
      Development = list(Average = c(volume = "Volume-weighted", simple = "Simple", regression = "Regression")[[fit$average]],
                         `Calendar periods` = if (is.null(periods)) "All" else paste("Latest", periods),
                         `Link ratios left out` = format(sum(!is.na(fit$link_ratios) & !fit$used)),
                         Tail = tail_text,
                         `Single link ratio sigma` = if (identical(input$sigma, "Mack")) "Mack's rule" else "Log-linear"),
      Ultimates = list(Methods = paste(paste(counts, names(counts)), collapse = ", "),
                       `Cape Cod decay` = format(or_default(input$decay, 1)),
                       `Cape Cod loss ratio` = rsv_percent(attr(r, "cape_cod_loss_ratio")))
    )
  }

  output$download_report <- downloadHandler(
    filename = function() "reserving_report.html",
    content = function(file) {
      quiet <- function(expr) tryCatch(expr, error = function(e) NULL)
      write_reserving_report(file, list(
        source = data_input()$source, settings = report_settings(), cl = cl(), results = results(),
        one_year = quiet(one_year()), ave = quiet(ave()), clark = quiet(clark()), inflation = quiet(inflation()),
        munich = quiet(munich()), boot = quiet(current_boot()), tails = quiet(tail_options())
      ))
    }
  )

  ######################
  #More methods
  ######################

  # the latest calendar period against the chain ladder of the triangle before it, with the
  # averaging and periods of the Development tab
  ave <- reactive(checked(actual_vs_expected(triangle(), average = or_default(input$average, "volume"),
                                             periods = periods_used())))

  clark <- reactive({
    method <- or_default(input$clark_method, "ldf")
    premium <- origin_values()$premium
    has_premium <- all(is.finite(premium)) && all(premium > 0)
    validate(need(method == "ldf" || has_premium,
                  "The Cape Cod method needs every origin's premium: enter them on the Ultimates tab, or use the LDF method."))
    truncation <- input$clark_truncation
    truncation <- if (is.numeric(truncation) && length(truncation) == 1 && is.finite(truncation)) truncation else Inf
    checked(clark_growth(triangle(), curve = or_default(input$clark_curve, "loglogistic"),
                         premium = if (has_premium) premium, method = method, truncation = truncation))
  })

  inflation <- reactive({
    rate <- function(x) if (is.numeric(x) && length(x) == 1 && is.finite(x)) x / 100 else 0
    tail <- cl()$tail
    checked(inflation_adjusted_chain_ladder(triangle(), past = rate(input$inflation_past), future = rate(input$inflation_future),
                                            average = or_default(input$average, "volume"), periods = periods_used(), tail = tail))
  })

  output$more_stats <- renderUI({
    a <- tryCatch(ave(), error = function(e) NULL)
    k <- tryCatch(clark(), error = function(e) NULL)
    f <- tryCatch(inflation(), error = function(e) NULL)
    chain <- cl()$total[["ibnr"]]
    div(
      class = "dft-stats",
      dft_stat_tile("Actual vs expected", if (is.null(a)) dft_dash else rsv_percent(a$total[["actual_to_expected"]], 0),
                    icon_name = "scale-balanced",
                    note = if (!is.null(a) && is.finite(a$total[["z"]])) {
                      paste0(dft_fmt(a$total[["z"]], 2), " standard errors from the expected",
                             if (abs(a$total[["z"]]) > 2) ": worth a look" else "")
                    }),
      dft_stat_tile("Clark IBNR", if (is.null(k)) dft_dash else rsv_amount(k$total[["ibnr"]]), icon_name = "chart-line",
                    accent = TRUE, note = if (!is.null(k)) paste0("Standard error ", rsv_amount(k$total[["se"]]),
                                                                  "; chain ladder ", rsv_amount(chain))),
      dft_stat_tile("IBNR with inflation", if (is.null(f)) dft_dash else rsv_amount(f$total[["ibnr"]]), icon_name = "arrow-trend-up",
                    note = if (!is.null(f)) paste0(rsv_amount(f$total[["future_inflation"]]), " of future inflation"))
    )
  })

  output$ave_table <- renderUI({
    a <- ave()
    b <- a$by_origin
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(b$origin[i], rsv_amount(b$previous[i]), rsv_amount(b$expected[i]), rsv_amount(b$actual[i]),
           rsv_amount(b$difference[i]), rsv_percent(b$actual_to_expected[i], 0), rsv_amount(b$se[i]), dft_fmt(b$z[i], 2))
    })
    t <- a$total
    total <- list("Total", rsv_amount(t[["previous"]]), rsv_amount(t[["expected"]]), rsv_amount(t[["actual"]]),
                  rsv_amount(t[["difference"]]), rsv_percent(t[["actual_to_expected"]], 0), rsv_amount(t[["se"]]),
                  dft_fmt(t[["z"]], 2))
    rsv_table(c("Origin", "Previous", "Expected", "Actual", "Difference", "Actual / expected", "Std. error", "z"),
              rows, total,
              c("The chain ladder of the triangle without its latest diagonal predicts each origin's development in the latest period.",
                "z is the difference over its standard error (Mack's process and parameter error): beyond about 2 either way is worth a look.",
                "The oldest origin has no factor for its last step, and the newest a single value, so neither has an expected development."))
  })

  output$ave_plot <- chart(function() {
    b <- ave()$by_origin
    dft_category_chart(b$origin, lines = list(list(name = "Expected", values = b$expected, colour = rsv_method_colours[["Chain ladder"]]),
                                              list(name = "Actual", values = b$actual, colour = "empirical")),
                       x_title = "Origin", y_title = "Development in the latest period", dark = dark())
  }, alt = "Expected and actual development of each origin in the latest calendar period")

  output$clark_table <- renderUI({
    k <- clark()
    b <- k$by_origin
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(b$origin[i], rsv_amount(b$latest[i]), rsv_percent(b$developed[i]), rsv_amount(b$ultimate[i]), rsv_amount(b$ibnr[i]),
           rsv_amount(b$se[i]), rsv_percent(b$cv[i]))
    })
    t <- k$total
    total <- list("Total", rsv_amount(t[["latest"]]), rsv_percent(t[["developed"]]), rsv_amount(t[["ultimate"]]),
                  rsv_amount(t[["ibnr"]]), rsv_amount(t[["se"]]), rsv_percent(t[["cv"]]))
    p <- k$parameters
    rsv_table(c("Origin", "Latest", "Developed", "Ultimate", "IBNR", "Std. error", "CV"), rows, total,
              c(paste0(k$method, ": omega ", dft_fmt(p[["omega"]], 4), ", theta ", dft_fmt(p[["theta"]], 4),
                       if ("loss_ratio" %in% names(p)) paste0(", loss ratio ", rsv_percent(p[["loss_ratio"]])),
                       ", scale sigma^2 ", dft_fmt(p[["sigma2"]], 4), "."),
                "The standard error has a process part (over-dispersed Poisson) and a parameter part (the curve and the ultimates)."))
  })

  output$clark_plot <- chart(function() {
    k <- clark()
    chain <- cl()
    n <- ncol(chain$triangle)
    ages <- seq(0.5, n + 5, by = 0.25)
    dft_line_chart(
      list(list(x = ages, y = k$growth(ages), name = "Clark growth curve", colour = rsv_method_colours[["Cape Cod"]]),
           list(x = seq_len(n), y = unname(1 / chain$cdf), name = "Chain ladder (1 / CDF)", colour = rsv_method_colours[["Chain ladder"]])),
      x_title = "Development period", y_title = "Share developed", dark = dark(), ylim = c(0, 1.02), vlines = n
    )
  }, alt = "Share of the ultimate developed by age: Clark's growth curve and the chain ladder")

  output$inflation_table <- renderUI({
    f <- inflation()
    chain <- cl()$by_origin
    b <- f$by_origin
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(b$origin[i], rsv_amount(b$latest[i]), rsv_amount(chain$ibnr[i]), rsv_amount(b$ibnr_today[i]),
           rsv_amount(b$future_inflation[i]), rsv_amount(b$ibnr[i]), rsv_amount(b$ultimate[i]))
    })
    t <- f$total
    total <- list("Total", rsv_amount(t[["latest"]]), rsv_amount(sum(chain$ibnr)), rsv_amount(t[["ibnr_today"]]),
                  rsv_amount(t[["future_inflation"]]), rsv_amount(t[["ibnr"]]), rsv_amount(t[["ultimate"]]))
    rsv_table(c("Origin", "Latest", "Chain ladder IBNR", "IBNR in today's money", "Future inflation", "IBNR", "Ultimate"),
              rows, total,
              c("The chain ladder of nominal claims carries the past's average inflation into the future;",
                "here the past is restated in today's money and the future inflated at the rate you expect.",
                "With the same rate for both, the result is close to the chain ladder's."))
  })

  ######################
  #Uncertainty
  ######################

  output$mack_table <- renderUI({
    fit <- cl()
    b <- fit$by_origin
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(b$origin[i], rsv_amount(b$latest[i]), rsv_amount(b$ultimate[i]), rsv_amount(b$ibnr[i]), rsv_amount(b$se[i]),
           rsv_percent(b$cv[i]), rsv_amount(b$process_se[i]), rsv_amount(b$parameter_se[i]))
    })
    t <- fit$total
    total <- list("Total", rsv_amount(t[["latest"]]), rsv_amount(t[["ultimate"]]), rsv_amount(t[["ibnr"]]), rsv_amount(t[["se"]]),
                  rsv_percent(t[["cv"]]), rsv_amount(t[["process_se"]]), rsv_amount(t[["parameter_se"]]))
    rsv_table(c("Origin", "Latest", "Ultimate", "IBNR", "Standard error", "CV", "Process", "Parameter"), rows, total,
              "The process error is the randomness of the future claims, the parameter error that of the estimated factors; the total's parameter error counts the origins' shared factors.")
  })

  boot <- eventReactive(input$run_bootstrap, {
    n <- input$boot_n
    validate(need(is.numeric(n) && isTRUE(n >= 100 && n <= 1e5 && n == round(n)),
                  "The number of simulations must be a whole number from 100 to 100,000."))
    seed <- input$boot_seed
    seed <- if (is.numeric(seed) && length(seed) == 1 && is.finite(seed)) round(seed) else NULL
    tri <- triangle()
    tail <- if (isTRUE(input$boot_tail)) cl()$tail else 1
    result <- checked(bootstrap_chain_ladder(tri, n = n, process = or_default(input$boot_process, "gamma"), tail = tail, seed = seed))
    result
  })

  # the bootstrap of the triangle showing, run
  current_boot <- reactive({
    validate(need(isTruthy(input$run_bootstrap), "Click Run bootstrap to simulate the reserves."))
    b <- boot()
    validate(need(identical(unname(b$triangle), unname(triangle())), "The triangle has changed: run the bootstrap again."))
    b
  })

  output$uncertainty_stats <- renderUI({
    fit <- cl()
    b <- tryCatch(current_boot(), error = function(e) NULL)
    percentile <- function(name) if (!is.null(b)) rsv_amount(b$total[[name]]) else dft_dash
    div(
      class = "dft-stats",
      dft_stat_tile("Chain ladder IBNR", rsv_amount(fit$total[["ibnr"]]), icon_name = "hourglass-half"),
      dft_stat_tile("Mack standard error", rsv_amount(fit$total[["se"]]), icon_name = "arrows-left-right",
                    note = paste("CV", rsv_percent(fit$total[["cv"]]))),
      dft_stat_tile("One-year standard error", tryCatch(rsv_amount(one_year()$total[["one_year_se"]]), error = function(e) dft_dash),
                    icon_name = "calendar-check", note = "Merz-W\u00fcthrich"),
      dft_stat_tile("Bootstrap mean", percentile("ibnr"), icon_name = "bullseye",
                    note = if (!is.null(b)) paste("Standard deviation", rsv_amount(b$total[["se"]])) else "Not run yet"),
      dft_stat_tile("75th percentile", percentile("p75"), icon_name = "chart-simple"),
      dft_stat_tile("99.5th percentile", percentile("p99.5"), icon_name = "triangle-exclamation", accent = TRUE,
                    note = "1 in 200")
    )
  })

  output$bootstrap_table <- renderUI({
    b <- current_boot()
    o <- b$by_origin
    shares <- paste0("p", formatC(100 * b$probs, format = "fg"))
    rows <- lapply(seq_len(nrow(o)), function(i) {
      c(list(o$origin[i], rsv_amount(o$ibnr[i]), rsv_amount(o$se[i]), rsv_percent(o$cv[i])),
        as.list(rsv_amount(unlist(o[i, shares]))))
    })
    t <- b$total
    total <- c(list("Total", rsv_amount(t[["ibnr"]]), rsv_amount(t[["se"]]), rsv_percent(t[["cv"]])), as.list(rsv_amount(t[shares])))
    rsv_table(c("Origin", "Mean IBNR", "Standard deviation", "CV", paste0(formatC(100 * b$probs, format = "fg"), "%")), rows, total,
              paste0(b$method, ", with a scale parameter of ", dft_fmt(b$scale, 4),
                     if (b$tail != 1) paste0(" and the tail factor of ", rsv_ratio(b$tail, 4)), "."))
  })

  output$bootstrap_download <- renderUI({
    current_boot()
    downloadButton("download_bootstrap", "Simulated IBNR (CSV)", class = "btn-outline-rsv")
  })
  output$download_bootstrap <- downloadHandler(
    filename = function() "bootstrap_ibnr.csv",
    content = function(file) {
      b <- current_boot()
      out <- data.frame(simulation = seq_along(b$total_reserves), b$reserves, total = b$total_reserves, check.names = FALSE)
      utils::write.csv(csv_safe(out), file, row.names = FALSE)
    }
  )

  output$bootstrap_plot <- chart(function() {
    b <- current_boot()
    rsv_bootstrap_chart(b$total_reserves, b$total[["ibnr"]],
                        c(`75th percentile` = b$total[["p75"]], `99.5th percentile` = b$total[["p99.5"]]), dark())
  }, alt = "Distribution of the simulated total IBNR, with its mean and percentiles")

  ######################
  #One-year view
  ######################

  one_year <- reactive(checked(merz_wuthrich(triangle(), sigma = or_default(input$sigma, "log-linear"))))

  output$one_year_table <- renderUI({
    mw <- one_year()
    b <- mw$by_origin
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(b$origin[i], rsv_amount(b$ibnr[i]), rsv_amount(b$one_year_se[i]), rsv_amount(b$mack_se[i]),
           rsv_percent(if (b$mack_se[i] > 0) b$one_year_se[i] / b$mack_se[i] else NA_real_))
    })
    t <- mw$total
    total <- list("Total", rsv_amount(t[["ibnr"]]), rsv_amount(t[["one_year_se"]]), rsv_amount(t[["mack_se"]]),
                  rsv_percent(t[["one_year_se"]] / t[["mack_se"]]))
    rsv_table(c("Origin", "IBNR", "One-year standard error", "Until ultimate", "One-year share"), rows, total,
              paste("How much the ultimate can move over the next year, as its claims emerge and the factors are",
                    "estimated again: the reserve risk over one year that Solvency II measures, against Mack's standard",
                    "error until the claims are all paid. Merz and W\u00fcthrich's formulas use the volume-weighted chain",
                    "ladder of every link ratio without a tail, whatever the Development tab's choices."))
  })

  ######################
  #Paid and incurred
  ######################

  munich <- reactive({
    incurred <- data_input()$incurred
    validate(need(!is.null(incurred), paste(
      "The Munich chain ladder needs incurred claims: choose an incurred column on the Data tab (with a row per cell),",
      "or load the simulated motor or the Quarg and Mack example.")))
    checked(munich_chain_ladder(triangle(), incurred, sigma = or_default(input$sigma, "log-linear")))
  })

  output$munich_stats <- renderUI({
    mcl <- munich()
    t <- mcl$total
    gap <- function(paid, incurred) paste0(if (paid >= incurred) "+" else "", rsv_amount(paid - incurred))
    div(
      class = "dft-stats",
      dft_stat_tile("Paid ultimate", rsv_amount(t[["ultimate_paid"]]), icon_name = "money-bill",
                    note = paste("Chain ladder", rsv_amount(t[["chain_ladder_paid"]]))),
      dft_stat_tile("Incurred ultimate", rsv_amount(t[["ultimate_incurred"]]), icon_name = "file-invoice",
                    note = paste("Chain ladder", rsv_amount(t[["chain_ladder_incurred"]]))),
      dft_stat_tile("Paid less incurred", gap(t[["ultimate_paid"]], t[["ultimate_incurred"]]), icon_name = "scale-balanced",
                    accent = TRUE, note = paste("Chain ladders", gap(t[["chain_ladder_paid"]], t[["chain_ladder_incurred"]]))),
      dft_stat_tile("Lambda", paste(rsv_ratio(mcl$lambda[["paid"]], 2), "/", rsv_ratio(mcl$lambda[["incurred"]], 2)),
                    icon_name = "arrows-left-right", note = "Paid / incurred correction")
    )
  })

  output$munich_table <- renderUI({
    mcl <- munich()
    b <- mcl$by_origin
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(b$origin[i], rsv_amount(b$latest_paid[i]), rsv_amount(b$latest_incurred[i]), rsv_percent(b$latest_ratio[i]),
           rsv_amount(b$chain_ladder_paid[i]), rsv_amount(b$chain_ladder_incurred[i]), rsv_amount(b$ultimate_paid[i]),
           rsv_amount(b$ultimate_incurred[i]), rsv_percent(b$ultimate_ratio[i]))
    })
    t <- mcl$total
    total <- list("Total", rsv_amount(t[["latest_paid"]]), rsv_amount(t[["latest_incurred"]]), rsv_percent(t[["latest_ratio"]]),
                  rsv_amount(t[["chain_ladder_paid"]]), rsv_amount(t[["chain_ladder_incurred"]]), rsv_amount(t[["ultimate_paid"]]),
                  rsv_amount(t[["ultimate_incurred"]]), rsv_percent(t[["ultimate_ratio"]]))
    rsv_table(c("Origin", "Paid", "Incurred", "Paid / incurred", "CL paid", "CL incurred", "Munich paid", "Munich incurred",
                "Munich paid / incurred"), rows, total,
              "CL: the chain ladder of each triangle on its own. The Munich ultimates of paid and incurred claims are close where the chain ladders are not.")
  })

  munich_colours <- c(`Chain ladder, paid` = "#3b82f6", `Chain ladder, incurred` = "#8b5cf6",
                      `Munich, paid` = "#10b981", `Munich, incurred` = "#ec4899")
  output$munich_plot <- chart(function() {
    b <- munich()$by_origin
    values <- list(`Chain ladder, paid` = b$chain_ladder_paid, `Chain ladder, incurred` = b$chain_ladder_incurred,
                   `Munich, paid` = b$ultimate_paid, `Munich, incurred` = b$ultimate_incurred)
    lines <- unname(Map(function(name, y) list(name = name, values = y, colour = munich_colours[[name]]), names(values), values))
    dft_category_chart(b$origin, lines = lines, x_title = "Origin", y_title = "Ultimate claims", dark = dark())
  }, alt = "Paid and incurred ultimates of each origin by the chain ladders and the Munich chain ladder")
  output$munich_ratio_plot <- chart(function() {
    b <- munich()$by_origin
    lines <- list(list(name = "Latest", values = b$latest_ratio, colour = "empirical"),
                  list(name = "Munich ultimate", values = b$ultimate_ratio, colour = munich_colours[["Munich, paid"]]),
                  list(name = "Chain ladder ultimates", values = b$chain_ladder_paid / b$chain_ladder_incurred,
                       colour = munich_colours[["Chain ladder, paid"]]))
    dft_category_chart(b$origin, lines = lines, x_title = "Origin", y_title = "Paid / incurred", dark = dark(), y_percent = TRUE)
  }, alt = "Paid to incurred ratio of each origin: latest, and at ultimate by the Munich chain ladder and the chain ladders")
  output$download_munich <- downloadHandler(
    filename = function() "paid_incurred.csv",
    content = function(file) utils::write.csv(csv_safe(munich()$by_origin), file, row.names = FALSE)
  )

  ######################
  #Settings file
  ######################

  output$save_settings <- downloadHandler(
    filename = function() "reserving_settings.txt",
    content = function(file) {
      values <- list()
      for (id in names(rsv_settings_inputs)) {
        x <- input[[id]]
        #(a column select is empty until a file is uploaded)
        if (!is.null(x) && is.atomic(x) && length(x) == 1 && !identical(x, "")) values[[id]] <- x
      }
      tri <- triangle()
      ex <- excluded()
      if (identical(dim(ex), dim(reserving_link_ratios(tri)))) {
        dimnames(ex) <- dimnames(reserving_link_ratios(tri))
        labels <- rsv_exclusion_labels(ex)
        if (length(labels) > 0) values$excluded_ratios <- labels
      }
      ov <- origin_values()
      values$origin_labels <- rownames(tri)
      values$origin_premium <- ov$premium
      values$origin_prior <- 100 * ov$prior
      values$origin_method <- ov$method
      write_settings_file(values, file, rsv_settings_tool, rsv_settings_version)
    }
  )

  observeEvent(input$load_settings, {
    fail <- function(message) showNotification(paste("The settings could not be loaded:", message), type = "error", duration = 10)
    settings <- tryCatch(read_settings_file(input$load_settings$datapath, rsv_settings_tool), error = function(e) e)
    if (inherits(settings, "error")) return(fail(conditionMessage(settings)))
    if (settings$version > rsv_settings_version) {
      return(fail("it was saved by a newer version of NetSimR."))
    }
    v <- settings$values
    choices <- list(data_source = c("example", "upload"), example = unname(rsv_examples), sep = c(",", ";", "\t"),
                    dec = c(".", ","), quote = c("\"", "'", ""), layout = c("long", "wide"),
                    cumulative = c("cumulative", "incremental"), average = c("volume", "simple", "regression"),
                    tail_method = c("none", "exponential", "inverse power", "factor"), sigma = c("log-linear", "Mack"),
                    method_all = unname(rsv_methods), boot_process = c("gamma", "odp"),
                    clark_curve = c("loglogistic", "weibull"), clark_method = c("ldf", "cape cod"))
    usable <- function(id) {
      x <- v[[id]]
      if (is.null(x) || length(x) != 1) return(FALSE)
      kind <- rsv_settings_inputs[[id]]
      if (kind %in% c("radio", "select") && !is.null(choices[[id]])) return(is.character(x) && x %in% choices[[id]])
      if (kind %in% c("numeric", "slider")) return(is.numeric(x) || (is.logical(x) && is.na(x)))
      if (kind == "switch") return(is.logical(x) && !is.na(x))
      is.character(x)
    }
    # the data shows another triangle once these are set, which then takes the periods, tail and
    # link ratios of the file; otherwise they are set on the triangle showing
    data_ids <- c("data_source", if (identical(v$data_source, "upload")) {
      c("data_includes_header", "sep", "dec", "quote", "layout", "origin_col", "development_col", "value_col",
        "incurred_col", "premium_col", "cumulative")
    } else "example")
    changes <- vapply(data_ids, function(id) usable(id) && !identical(as.character(v[[id]]), as.character(input[[id]])), logical(1))
    new_triangle <- any(changes) || (identical(v$data_source, "upload") && is.null(input$file1))

    skip_push$prior <<- usable("prior_loss_ratio") && !identical(as.numeric(v$prior_loss_ratio), as.numeric(input$prior_loss_ratio))
    skip_push$method <<- usable("method_all") && !identical(v$method_all, input$method_all)
    for (id in names(rsv_settings_inputs)) {
      if (!usable(id) || id %in% c("periods", "tail_from")) next
      x <- v[[id]]
      switch(rsv_settings_inputs[[id]],
        radio = updateRadioButtons(session, id, selected = x),
        select = updateSelectInput(session, id, selected = x),
        switch = bslib::update_switch(id, value = x, session = session),
        numeric = updateNumericInput(session, id, value = if (is.logical(x)) NA else x),
        slider = updateSliderInput(session, id, value = x),
        column = NULL
      )
    }
    loaded_columns <<- v[intersect(names(v), names(rsv_settings_inputs)[rsv_settings_inputs == "column"])]
    labels <- v$origin_labels
    if (is.character(labels) && length(labels) > 0) {
      take <- function(x, numeric = TRUE) {
        if (length(x) != length(labels)) return(rep(if (numeric) NA_real_ else NA_character_, length(labels)))
        if (numeric) suppressWarnings(as.numeric(x)) else as.character(x)
      }
      loaded_origins(list(label = labels, premium = take(v$origin_premium), prior = take(v$origin_prior),
                          method = take(v$origin_method, numeric = FALSE)))
    }
    wanted <- list(periods = v$periods, tail_from = v$tail_from,
                   exclusions = if (is.character(v$excluded_ratios)) v$excluded_ratios else character(0))
    if (new_triangle) {
      pending <<- wanted
    } else {
      # the triangle showing is the file's: set its periods, tail, link ratios and origins now
      tri <- triangle()
      ratios <- reserving_link_ratios(tri)
      excluded(rsv_exclusion_matrix(wanted$exclusions, ratios))
      if (!is.null(wanted$periods)) updateSelectInput(session, "periods", selected = as.character(wanted$periods))
      if (!is.null(wanted$tail_from)) updateSelectInput(session, "tail_from", selected = as.character(wanted$tail_from))
      defaults <- origin_defaults(tri, data_premium(), v$prior_loss_ratio, or_default(v$method_all, "cl"))
      for (i in seq_len(nrow(tri))) {
        updateNumericInput(session, origin_id("premium", i), value = defaults$premium[i])
        updateNumericInput(session, origin_id("prior", i), value = defaults$prior[i])
        updateSelectInput(session, origin_id("method", i), selected = defaults$method[i])
      }
    }
    showNotification(paste0("Loaded the settings of ", input$load_settings$name,
                            if (identical(v$data_source, "upload")) ". Upload the data file to apply its columns." else "."),
                     type = "message", duration = 6)
  })

  # the summary tiles are hidden while empty, and shiny does not render hidden outputs,
  # so they must render even when hidden or they would never appear
  for (id in c("data_overview", "development_stats", "ultimate_stats", "uncertainty_stats", "munich_stats", "more_stats")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  residual_chart <- function(by) chart(function() rsv_residual_chart(current_boot()$residuals, by, dark()),
                                       alt = paste("Scaled Pearson residuals of the chain ladder by", by, "period"))
  output$residual_origin_plot <- residual_chart("origin")
  output$residual_development_plot <- residual_chart("development")
  output$residual_calendar_plot <- residual_chart("calendar")
}
