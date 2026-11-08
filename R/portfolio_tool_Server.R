#' Server function of the portfolio capital tool
#'
#' @description Reads the lines (a frequency and a severity for each, or uploaded
#'   simulated losses) and their dependence, runs \code{\link{simulate_portfolio}}
#'   and shows the capital of the portfolio and of each line; paired with
#'   \code{portfolio_tool_UI} by \code{\link{run_shiny_portfolio_tool}}.
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @keywords internal
#' @import shiny
portfolio_tool_Server <- function(input, output, session) {

  or_default <- function(x, default) if (is.null(x) || length(x) == 0) default else x
  # a single number from an input, NA when it is empty or not a number
  number <- function(x) if (length(x) == 1 && is.numeric(x)) as.numeric(x) else NA_real_

  # the charts are drawn in the colours of the theme the page reports
  dark <- reactive(identical(input$app_theme, "dark"))
  chart <- function(draw, alt) netsimr_render_plot(draw(), bg = "transparent", res = 96, alt = alt)
  checked <- function(expr) {
    result <- tryCatch(expr, error = function(e) e)
    validate(need(!inherits(result, "error"), if (inherits(result, "error")) conditionMessage(result)))
    result
  }

  ######################
  #Lines
  ######################

  n_lines <- reactive({
    k <- number(input$n_lines)
    if (!is.finite(k)) 3L else as.integer(min(max(round(k), 2), pfo_max_lines))
  })

  line_id <- function(field, i) paste0("line_", field, "_", i)
  # each line's values from a loaded settings file, a data frame like pfo_default_lines
  loaded_lines <- reactiveVal(NULL)
  line_default <- function(i, field) {
    saved <- loaded_lines()
    if (!is.null(saved) && i <= nrow(saved) && !is.na(saved[[field]][i])) return(saved[[field]][i])
    pfo_default_lines[[field]][i]
  }
  line_value <- function(i, field) {
    x <- input[[line_id(field, i)]]
    if (is.null(x)) line_default(i, field) else x
  }

  output$line_inputs <- renderUI({
    k <- n_lines()
    rows <- isolate(lapply(seq_len(k), function(i) {
      frequency <- line_value(i, "frequency")
      severity <- line_value(i, "severity")
      f <- or_default(pfo_frequencies[[frequency]], pfo_frequencies$Poisson)
      s <- or_default(pfo_severities[[severity]], pfo_severities$LogNormal)
      # a second parameter shows only for a distribution that has one; its cell stays, so
      # the grid keeps its columns
      second <- function(id, label, value, shown) {
        div(conditionalPanel(condition = shown, numericInput(id, label, value = value, step = 0.1, width = "100%")))
      }
      tagList(
        textInput(line_id("name", i), paste("Line", i), value = line_value(i, "name"), width = "100%"),
        selectInput(line_id("frequency", i), "Claim count", names(pfo_frequencies), selected = frequency, selectize = FALSE,
                    width = "100%"),
        numericInput(line_id("f1", i), f$params[1], value = line_value(i, "f1"), min = 0, step = 0.5, width = "100%"),
        second(line_id("f2", i), or_default(f$params[2], "beta (scale)"), line_value(i, "f2"),
               paste0("input.", line_id("frequency", i), " == 'Negative Binomial'")),
        selectInput(line_id("severity", i), "Claim size", names(pfo_severities), selected = severity, selectize = FALSE,
                    width = "100%"),
        numericInput(line_id("s1", i), s$params[1], value = line_value(i, "s1"), step = 0.1, width = "100%"),
        second(line_id("s2", i), or_default(s$params[2], "Second parameter"), line_value(i, "s2"),
               paste0("input.", line_id("severity", i), " != 'Exponential'"))
      )
    }))
    tagList(
      div(class = "pfo-grid-wrap", div(class = "pfo-line-grid", rows)),
      dft_help("The Negative Binomial count is a Poisson whose mean is Gamma with shape r and scale beta: its mean is",
               "r times beta. The Pareto's x_m is its smallest claim.")
    )
  })

  # a new distribution gives its parameters their labels and defaults (the distribution a
  # line had is remembered, so a line shown again, or set by a settings file, keeps its own)
  shown_distribution <- new.env()
  for (i in seq_len(pfo_max_lines)) local({
    line <- i
    watch <- function(field, choices, first, second) {
      observeEvent(input[[line_id(field, line)]], {
        value <- input[[line_id(field, line)]]
        key <- paste(field, line)
        previous <- shown_distribution[[key]]
        assign(key, value, envir = shown_distribution)
        d <- choices[[value]]
        if (is.null(d) || is.null(previous) || identical(previous, value)) return()
        updateNumericInput(session, line_id(first, line), label = d$params[1], value = d$defaults[1])
        if (length(d$params) > 1) updateNumericInput(session, line_id(second, line), label = d$params[2], value = d$defaults[2])
      })
    }
    watch("frequency", pfo_frequencies, "f1", "f2")
    watch("severity", pfo_severities, "s1", "s2")
  })

  # the lines of simulate_portfolio() from the By line card
  model_lines <- reactive({
    k <- n_lines()
    lines <- lapply(seq_len(k), function(i) {
      name <- trimws(as.character(line_value(i, "name")))
      frequency <- as.character(line_value(i, "frequency"))
      severity <- as.character(line_value(i, "severity"))
      f <- pfo_frequencies[[frequency]]
      s <- pfo_severities[[severity]]
      validate(need(nzchar(name), paste("Give line", i, "a name.")),
               need(!is.null(f) && !is.null(s), paste("Choose the distributions of", name, ".")))
      fp <- c(number(line_value(i, "f1")), if (length(f$params) > 1) number(line_value(i, "f2")))
      sp <- c(number(line_value(i, "s1")), if (length(s$params) > 1) number(line_value(i, "s2")))
      validate(need(all(is.finite(fp)) && all(fp > 0), paste0("Enter the claim count parameters of ", name, ", above 0.")),
               need(all(is.finite(sp)), paste0("Enter the claim size parameters of ", name, ".")))
      list(name = name, frequency = frequency, frequency_params = fp, severity = severity, severity_params = sp)
    })
    names <- vapply(lines, function(l) l$name, character(1))
    validate(need(!anyDuplicated(names), "Give each line its own name."))
    stats::setNames(lines, names)
  })

  # the expected claims, claim size and losses of a line
  line_means <- function(l) {
    count <- if (l$frequency == "Poisson") l$frequency_params[1] else prod(l$frequency_params)
    p <- l$severity_params
    size <- switch(l$severity,
                   LogNormal = exp(p[1] + p[2]^2 / 2),
                   Gamma = p[1] * p[2],
                   Pareto = if (p[1] > 1) p[1] * p[2] / (p[1] - 1) else Inf,
                   Exponential = 1 / p[1])
    c(count = count, size = size, loss = count * size)
  }

  # an uploaded file of simulated losses, a column per line
  upload_data <- reactive({
    file <- input$upload_file
    validate(need(file, "Upload a file of simulated losses, a column per line."))
    df <- tryCatch(dft_read_data(file$datapath, header = isTRUE(input$upload_header), sep = or_default(input$upload_sep, ","),
                                 quote = "\"", dec = or_default(input$upload_dec, ".")),
                   error = function(e) e)
    validate(need(!inherits(df, "error"), if (inherits(df, "error")) paste0("The file could not be read (", conditionMessage(df),
                                                                            "). Check the separator.")))
    validate(need(ncol(df) >= 2 && nrow(df) > 0, "The file needs at least two columns and one row."))
    df
  })
  loaded_columns <- NULL
  observeEvent(upload_data(), {
    df <- upload_data()
    numbers <- dft_numeric_columns(df, or_default(input$upload_dec, "."))
    wanted <- if (!is.null(loaded_columns)) intersect(loaded_columns, names(df)) else intersect(input$upload_columns, names(df))
    loaded_columns <<- NULL
    # without a choice, every numeric column but a simulation number, at most the most lines
    guess <- setdiff(numbers, numbers[grepl("^(sim|simulation|year|id|index|row)", numbers, ignore.case = TRUE)])
    selected <- if (length(wanted) > 0) wanted else utils::head(guess, pfo_max_lines)
    updateSelectizeInput(session, "upload_columns", choices = names(df), selected = selected)
  })
  upload_lines <- reactive({
    df <- upload_data()
    validate(need(length(input$upload_columns) <= pfo_max_lines, paste("Choose at most", pfo_max_lines, "columns.")))
    checked(pfo_upload_lines(df, input$upload_columns, or_default(input$upload_dec, ".")))
  })

  uploading <- reactive(identical(input$line_source, "upload"))
  # the lines' names, and their number
  line_names <- reactive(if (uploading()) names(upload_lines()) else names(model_lines()))
  # (slowed, so the correlation grid is not drawn again at each letter of a name)
  line_names_slow <- debounce(line_names, 700)

  output$lines_stats <- renderUI({
    if (uploading()) {
      l <- upload_lines()
      means <- vapply(l, mean, numeric(1))
      return(div(class = "dft-stats",
                 dft_stat_tile("Lines", length(l), icon_name = "layer-group", note = "Uploaded simulated losses"),
                 dft_stat_tile("Simulations", format(length(l[[1]]), big.mark = ","), icon_name = "hashtag",
                               note = input$upload_file$name),
                 dft_stat_tile("Mean total loss", rsv_amount(sum(means)), icon_name = "coins", accent = TRUE)))
    }
    l <- model_lines()
    means <- vapply(l, function(x) line_means(x)[["loss"]], numeric(1))
    div(class = "dft-stats",
        dft_stat_tile("Lines", length(l), icon_name = "layer-group", note = "Frequency and severity"),
        dft_stat_tile("Expected claims", format(round(sum(vapply(l, function(x) line_means(x)[["count"]], numeric(1)))), big.mark = ","),
                      icon_name = "hashtag", note = "A year"),
        dft_stat_tile("Expected total loss", if (all(is.finite(means))) rsv_amount(sum(means)) else "Infinite", icon_name = "coins",
                      accent = TRUE, note = if (!all(is.finite(means))) "A Pareto with alpha of 1 or less has no mean"))
  })

  output$lines_table <- renderUI({
    if (uploading()) {
      l <- upload_lines()
      rows <- lapply(names(l), function(name) {
        x <- l[[name]]
        list(name, rsv_amount(mean(x)), rsv_amount(stats::sd(x)), rsv_amount(stats::quantile(x, 0.995, names = FALSE)))
      })
      return(rsv_table(c("Line", "Mean", "Standard deviation", "99.5th percentile"), rows,
                       note = "Each column's simulated losses, as uploaded: the copula only reorders them."))
    }
    l <- model_lines()
    m <- lapply(l, line_means)
    total <- sum(vapply(m, function(x) x[["loss"]], numeric(1)))
    rows <- lapply(seq_along(l), function(i) {
      x <- l[[i]]
      list(x$name, paste0(x$frequency, ", mean ", dft_fmt(m[[i]][["count"]], 4)),
           paste0(x$severity, ", mean ", if (is.finite(m[[i]][["size"]])) rsv_amount(m[[i]][["size"]]) else "infinite"),
           if (is.finite(m[[i]][["loss"]])) rsv_amount(m[[i]][["loss"]]) else "Infinite",
           if (is.finite(total)) rsv_percent(m[[i]][["loss"]] / total) else dft_dash)
    })
    rsv_table(c("Line", "Claim count", "Claim size", "Expected loss", "Share"), rows,
              total = list("Total", "", "", if (is.finite(total)) rsv_amount(total) else "Infinite", if (is.finite(total)) "100.0%" else dft_dash),
              note = "The expected loss of a line is its expected number of claims times its expected claim size.")
  })

  ######################
  #Dependence
  ######################

  corr_id <- function(i, j) paste0("corr_", i, "_", j)
  loaded_corr <- reactiveVal(NULL)
  corr_value <- function(i, j) {
    x <- input[[corr_id(i, j)]]
    if (!is.null(x)) return(x)
    saved <- loaded_corr()
    if (!is.null(saved) && i <= nrow(saved) && j <= ncol(saved)) return(saved[i, j])
    or_default(number(input$corr_all), 0.25)
  }

  output$corr_inputs <- renderUI({
    names <- line_names_slow()
    k <- length(names)
    cells <- isolate(unlist(lapply(seq_len(k), function(i) {
      c(list(div(class = "pfo-corr-name", names[i])),
        lapply(seq_len(k), function(j) {
          if (j < i) div() else if (j == i) div(class = "pfo-corr-one", "1") else {
            sim_hidden_label(numericInput(corr_id(i, j), paste("Correlation of", names[i], "and", names[j]),
                                          value = corr_value(i, j), min = -0.99, max = 0.99, step = 0.05, width = "100%"))
          }
        }))
    }), recursive = FALSE))
    tagList(
      div(class = "pfo-grid-wrap",
          div(class = "pfo-corr-grid", style = paste0("grid-template-columns: minmax(6rem, auto) repeat(", k, ", minmax(5rem, 1fr));",
                                                      " min-width: ", 130 + 90 * k, "px;"),
              div(), lapply(names, function(n) div(class = "pfo-head", n)), cells)),
      dft_help("The matrix must be positive definite: the correlations must be consistent with one another (A and B",
               "close, B and C close, but A and C far apart is not possible).")
    )
  })

  correlation <- reactive({
    names <- line_names()
    k <- length(names)
    if (!identical(input$corr_mode, "matrix")) {
      r <- number(input$corr_all)
      validate(need(is.finite(r) && r > -1 && r < 1, "Enter a correlation between -1 and 1."))
      return(r)
    }
    upper <- unlist(lapply(seq_len(k - 1), function(i) vapply((i + 1):k, function(j) number(corr_value(i, j)), numeric(1))))
    validate(need(all(is.finite(upper)) && all(abs(upper) < 1), "Every correlation must be between -1 and 1."))
    R <- pfo_correlation_matrix(k, upper)
    dimnames(R) <- list(names, names)
    R
  })

  copula_settings <- reactive({
    copula <- or_default(input$copula, "gaussian")
    df <- number(input$copula_df)
    validate(need(copula != "t" || (is.finite(df) && df > 0), "Enter the degrees of freedom of the t copula, above 0."))
    list(copula = copula, df = if (copula == "t") df else 4)
  })

  output$dependence_stats <- renderUI({
    names <- line_names()
    R <- correlation()
    k <- length(names)
    pairs <- if (length(R) == 1) rep(R, k * (k - 1) / 2) else R[upper.tri(R)]
    valid <- tryCatch({
      portfolio_correlation(R, names)
      TRUE
    }, error = function(e) FALSE)
    cs <- copula_settings()
    div(class = "dft-stats",
        dft_stat_tile("Copula", if (cs$copula == "t") paste0("t, ", format(cs$df), " df") else "Gaussian", icon_name = "link"),
        dft_stat_tile("Mean correlation", dft_fmt(mean(pairs), 3), icon_name = "arrows-left-right",
                      note = paste("Rank correlation about", dft_fmt(mean(6 / pi * asin(pairs / 2)), 3))),
        dft_stat_tile("Matrix", if (valid) "Valid" else "Not valid", icon_name = if (valid) "circle-check" else "triangle-exclamation",
                      accent = !valid, note = if (valid) paste(k, "lines") else "Not positive definite: change a correlation"))
  })

  ######################
  #Capital
  ######################

  # what a run needs, from the three tabs
  run_spec <- reactive({
    names <- line_names()
    level <- suppressWarnings(as.numeric(or_default(input$level, "0.995")))
    seed <- number(input$seed)
    lines <- if (uploading()) upload_lines() else lapply(model_lines(), function(l) l[c("frequency", "frequency_params", "severity",
                                                                                         "severity_params")])
    n <- if (uploading()) length(lines[[1]]) else number(input$n_sims)
    validate(need(is.finite(n) && n >= 1000 && n <= 1e6 && n == round(n), "The number of simulations must be a whole number from 1,000 to 1,000,000."))
    cs <- copula_settings()
    list(lines = lines, correlation = correlation(), copula = cs$copula, df = cs$df, n = n, level = level,
         seed = if (is.finite(seed)) round(seed) else NULL, source = if (uploading()) input$upload_file$name else "Frequency and severity",
         described = if (uploading()) NULL else model_lines())
  })

  result <- eventReactive(input$run, {
    spec <- run_spec()
    out <- withProgress(message = "Simulating the portfolio", value = 0.2, {
      checked(simulate_portfolio(spec$lines, correlation = spec$correlation, copula = spec$copula, df = spec$df, n = spec$n,
                                 level = spec$level, seed = spec$seed))
    })
    list(portfolio = out, spec = spec)
  })

  current <- reactive({
    validate(need(isTruthy(input$run), "Click Run to simulate the portfolio."))
    result()
  })

  # whether the settings changed since the run shown
  stale <- reactive({
    r <- current()
    spec <- tryCatch(run_spec(), error = function(e) NULL)
    !identical(spec[c("lines", "correlation", "copula", "df", "n", "level", "seed")],
               r$spec[c("lines", "correlation", "copula", "df", "n", "level", "seed")])
  })

  observeEvent(current(), {
    names <- names(current()$portfolio$losses)
    names <- names[names != "total"]
    updateSelectInput(session, "rank_x", choices = names, selected = names[1])
    updateSelectInput(session, "rank_y", choices = names, selected = names[min(2, length(names))])
  })

  output$capital_stats <- renderUI({
    r <- current()
    p <- r$portfolio
    s <- p$summary
    total <- s[s$line == "total", ]
    d <- p$diversification
    label <- paste0(format(100 * p$level), "%")
    tagList(
      if (stale()) dft_help("The settings have changed since this run: click Run to simulate them."),
      div(class = "dft-stats",
          dft_stat_tile(paste("VaR", label), rsv_amount(total$var), icon_name = "triangle-exclamation", accent = TRUE,
                        note = paste("Of the total; mean", rsv_amount(total$mean))),
          dft_stat_tile(paste("TVaR", label), rsv_amount(total$tvar), icon_name = "chart-area", note = "The mean beyond the VaR"),
          dft_stat_tile("Diversification", rsv_amount(d[["benefit"]]), icon_name = "scale-balanced",
                        note = paste0(rsv_percent(d[["benefit_share"]]), " of the lines' VaR of ", rsv_amount(d[["stand_alone_var"]]))),
          dft_stat_tile("Simulations", format(p$n, big.mark = ","), icon_name = "hashtag",
                        note = if (p$copula == "t") paste0("t copula, ", format(p$df), " df") else "Gaussian copula"))
    )
  })

  output$capital_table <- renderUI({
    p <- current()$portfolio
    s <- p$summary
    lines <- s[s$line != "total", ]
    total <- s[s$line == "total", ]
    row <- function(x, name = x$line) {
      list(name, rsv_amount(x$mean), rsv_amount(x$sd), rsv_amount(x$var), rsv_amount(x$tvar), rsv_amount(x$capital),
           rsv_percent(x$capital_share))
    }
    label <- paste0(format(100 * p$level), "%")
    rsv_table(c("Line", "Mean", "Standard deviation", paste("VaR", label), paste("TVaR", label), "Capital", "Share"),
              lapply(seq_len(nrow(lines)), function(i) row(lines[i, ])), row(total, "Total"),
              note = paste0("Each line's VaR and TVaR are its own; its capital is its mean loss in the years when the total is at ",
                            "or beyond its VaR (co-TVaR), so the lines' capitals add up to the total's TVaR. The ",
                            "diversification benefit is the sum of the lines' VaR less the total's."))
  })

  output$capital_plot <- chart(function() {
    p <- current()$portfolio
    pfo_capital_chart(p$summary, p$level, dark())
  }, alt = "Each line's value at risk on its own and its allocated capital")

  output$total_plot <- chart(function() {
    p <- current()$portfolio
    total <- p$summary[p$summary$line == "total", ]
    pfo_total_chart(p$losses$total, total$var, total$tvar, dark())
  }, alt = "Distribution of the portfolio's total losses, with its mean, VaR and TVaR")

  output$rank_plot <- chart(function() {
    p <- current()$portfolio
    x <- input$rank_x
    y <- input$rank_y
    validate(need(!is.null(x) && x %in% names(p$losses) && !is.null(y) && y %in% names(p$losses), "Choose two lines."))
    pfo_rank_chart(p$losses[[x]], p$losses[[y]], x, y, dark())
  }, alt = "The ranks of two lines' simulated losses, one point per simulated year")

  output$download_summary <- downloadHandler(
    filename = function() "portfolio_capital.csv",
    content = function(file) {
      p <- current()$portfolio
      out <- p$summary
      out$level <- p$level
      utils::write.csv(csv_safe(out), file, row.names = FALSE)
    }
  )
  output$download_losses <- downloadHandler(
    filename = function() "portfolio_losses.csv",
    content = function(file) {
      p <- current()$portfolio
      utils::write.csv(csv_safe(data.frame(simulation = seq_len(nrow(p$losses)), p$losses, check.names = FALSE)), file,
                       row.names = FALSE)
    }
  )
  output$download_report <- downloadHandler(
    filename = function() "portfolio_report.html",
    content = function(file) {
      r <- current()
      write_portfolio_report(file, r$portfolio, r$spec)
    }
  )

  ######################
  #Settings file
  ######################

  output$save_settings <- downloadHandler(
    filename = function() "portfolio_settings.txt",
    content = function(file) {
      values <- list()
      for (id in names(pfo_settings_inputs)) {
        x <- input[[id]]
        if (!is.null(x) && is.atomic(x) && length(x) == 1 && !identical(x, "")) values[[id]] <- x
      }
      k <- n_lines()
      for (field in names(pfo_default_lines)) {
        values[[paste0("line_", field)]] <- if (field %in% c("name", "frequency", "severity")) {
          vapply(seq_len(k), function(i) as.character(line_value(i, field)), character(1))
        } else {
          vapply(seq_len(k), function(i) number(line_value(i, field)), numeric(1))
        }
      }
      names <- tryCatch(line_names(), error = function(e) NULL)
      if (length(names) >= 2) {
        k <- length(names)
        values$corr_matrix <- unlist(lapply(seq_len(k - 1), function(i) vapply((i + 1):k, function(j) number(corr_value(i, j)), numeric(1))))
      }
      if (length(input$upload_columns) > 0) values$upload_columns <- input$upload_columns
      write_settings_file(values, file, pfo_settings_tool, pfo_settings_version)
    }
  )

  observeEvent(input$load_settings, {
    fail <- function(message) showNotification(paste("The settings could not be loaded:", message), type = "error", duration = 10)
    settings <- tryCatch(read_settings_file(input$load_settings$datapath, pfo_settings_tool), error = function(e) e)
    if (inherits(settings, "error")) return(fail(conditionMessage(settings)))
    if (settings$version > pfo_settings_version) return(fail("it was saved by a newer version of NetSimR."))
    v <- settings$values
    choices <- list(line_source = c("model", "upload"), upload_sep = c(",", ";", "\t"), upload_dec = c(".", ","),
                    copula = c("gaussian", "t"), corr_mode = c("all", "matrix"), level = unname(pfo_levels))
    usable <- function(id) {
      x <- v[[id]]
      if (is.null(x) || length(x) != 1) return(FALSE)
      kind <- pfo_settings_inputs[[id]]
      if (kind %in% c("radio", "select") && !is.null(choices[[id]])) return(is.character(x) && x %in% choices[[id]])
      if (kind == "numeric") return(is.numeric(x) || (is.logical(x) && is.na(x)))
      if (kind == "switch") return(is.logical(x) && !is.na(x))
      is.character(x)
    }
    # the lines: the distributions first, so their parameters are not reset to the defaults
    labels <- v$line_name
    if (is.character(labels) && length(labels) >= 2 && length(labels) <= pfo_max_lines) {
      k <- length(labels)
      take <- function(x, numeric = TRUE) {
        if (length(x) != k) return(rep(NA, k))
        if (numeric) suppressWarnings(as.numeric(x)) else as.character(x)
      }
      lines <- data.frame(name = labels, frequency = take(v$line_frequency, FALSE), f1 = take(v$line_f1), f2 = take(v$line_f2),
                          severity = take(v$line_severity, FALSE), s1 = take(v$line_s1), s2 = take(v$line_s2),
                          stringsAsFactors = FALSE)
      lines$frequency[!lines$frequency %in% names(pfo_frequencies)] <- NA
      lines$severity[!lines$severity %in% names(pfo_severities)] <- NA
      loaded_lines(lines)
      for (i in seq_len(k)) {
        for (field in c("frequency", "severity")) {
          if (!is.na(lines[[field]][i])) {
            assign(paste(field, i), lines[[field]][i], envir = shown_distribution)
            updateSelectInput(session, line_id(field, i), selected = lines[[field]][i])
          }
        }
        updateTextInput(session, line_id("name", i), value = lines$name[i])
        for (field in c("f1", "f2", "s1", "s2")) {
          if (!is.na(lines[[field]][i])) updateNumericInput(session, line_id(field, i), value = lines[[field]][i])
        }
      }
      updateNumericInput(session, "n_lines", value = k)
    }
    for (id in names(pfo_settings_inputs)) {
      if (!usable(id) || id == "n_lines") next
      x <- v[[id]]
      switch(pfo_settings_inputs[[id]],
        radio = updateRadioButtons(session, id, selected = x),
        select = updateSelectInput(session, id, selected = x),
        switch = bslib::update_switch(id, value = x, session = session),
        numeric = updateNumericInput(session, id, value = if (is.logical(x)) NA else x)
      )
    }
    upper <- suppressWarnings(as.numeric(v$corr_matrix))
    k <- (1 + sqrt(1 + 8 * length(upper))) / 2
    if (length(upper) > 0 && !anyNA(upper) && k == round(k)) {
      R <- pfo_correlation_matrix(k, upper)
      loaded_corr(R)
      for (i in seq_len(k - 1)) for (j in (i + 1):k) updateNumericInput(session, corr_id(i, j), value = R[i, j])
    }
    if (is.character(v$upload_columns)) {
      loaded_columns <<- v$upload_columns
      if (!is.null(input$upload_file)) {
        df <- tryCatch(upload_data(), error = function(e) NULL)
        if (!is.null(df)) {
          updateSelectizeInput(session, "upload_columns", selected = intersect(v$upload_columns, names(df)))
          loaded_columns <<- NULL
        }
      }
    }
    showNotification(paste0("Loaded the settings of ", input$load_settings$name,
                            if (identical(v$line_source, "upload")) ". Upload the losses to apply their columns." else "."),
                     type = "message", duration = 6)
  })

  # the summary tiles are hidden while empty, and shiny does not render hidden outputs,
  # so they must render even when hidden or they would never appear
  for (id in c("lines_stats", "dependence_stats", "capital_stats")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
