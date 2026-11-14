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
      netsimr_help("The Negative Binomial count is a Poisson whose mean is Gamma with shape r and scale beta: its mean is",
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
    df <- tryCatch(netsimr_read_data(file$datapath, header = isTRUE(input$upload_header), sep = or_default(input$upload_sep, ","),
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
    numbers <- netsimr_numeric_columns(df, or_default(input$upload_dec, "."))
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
                 netsimr_stat_tile("Lines", length(l), icon_name = "layer-group", note = "Uploaded simulated losses"),
                 netsimr_stat_tile("Simulations", format(length(l[[1]]), big.mark = ",", scientific = FALSE), icon_name = "hashtag",
                               note = input$upload_file$name),
                 netsimr_stat_tile("Mean total loss", netsimr_amount(sum(means)), icon_name = "coins", accent = TRUE)))
    }
    l <- model_lines()
    means <- vapply(l, function(x) line_means(x)[["loss"]], numeric(1))
    div(class = "dft-stats",
        netsimr_stat_tile("Lines", length(l), icon_name = "layer-group", note = "Frequency and severity"),
        netsimr_stat_tile("Expected claims", format(round(sum(vapply(l, function(x) line_means(x)[["count"]], numeric(1)))), big.mark = ",", scientific = FALSE),
                      icon_name = "hashtag", note = "A year"),
        netsimr_stat_tile("Expected total loss", if (all(is.finite(means))) netsimr_amount(sum(means)) else "Infinite", icon_name = "coins",
                      accent = TRUE, note = if (!all(is.finite(means))) "A Pareto with alpha of 1 or less has no mean"))
  })

  output$lines_table <- renderUI({
    if (uploading()) {
      l <- upload_lines()
      rows <- lapply(names(l), function(name) {
        x <- l[[name]]
        list(name, netsimr_amount(mean(x)), netsimr_amount(stats::sd(x)), netsimr_amount(stats::quantile(x, 0.995, names = FALSE)))
      })
      return(netsimr_table(c("Line", "Mean", "Standard deviation", "99.5th percentile"), rows,
                       note = "Each column's simulated losses, as uploaded: the copula only reorders them."))
    }
    l <- model_lines()
    m <- lapply(l, line_means)
    total <- sum(vapply(m, function(x) x[["loss"]], numeric(1)))
    rows <- lapply(seq_along(l), function(i) {
      x <- l[[i]]
      list(x$name, paste0(x$frequency, ", mean ", netsimr_fmt(m[[i]][["count"]], 4)),
           paste0(x$severity, ", mean ", if (is.finite(m[[i]][["size"]])) netsimr_amount(m[[i]][["size"]]) else "infinite"),
           if (is.finite(m[[i]][["loss"]])) netsimr_amount(m[[i]][["loss"]]) else "Infinite",
           if (is.finite(total)) netsimr_percent(m[[i]][["loss"]] / total) else netsimr_dash)
    })
    netsimr_table(c("Line", "Claim count", "Claim size", "Expected loss", "Share"), rows,
              total = list("Total", "", "", if (is.finite(total)) netsimr_amount(total) else "Infinite", if (is.finite(total)) "100.0%" else netsimr_dash),
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
            netsimr_hidden_label(numericInput(corr_id(i, j), paste("Correlation of", names[i], "and", names[j]),
                                          value = corr_value(i, j), min = -0.99, max = 0.99, step = 0.05, width = "100%"))
          }
        }))
    }), recursive = FALSE))
    tagList(
      div(class = "pfo-grid-wrap",
          div(class = "pfo-corr-grid", style = paste0("grid-template-columns: minmax(6rem, auto) repeat(", k, ", minmax(5rem, 1fr));",
                                                      " min-width: ", 130 + 90 * k, "px;"),
              div(), lapply(names, function(n) div(class = "pfo-head", n)), cells)),
      netsimr_help("The matrix must be positive definite: the correlations must be consistent with one another (A and B",
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
        netsimr_stat_tile("Copula", if (cs$copula == "t") paste0("t, ", format(cs$df), " df") else "Gaussian", icon_name = "link"),
        netsimr_stat_tile("Mean correlation", netsimr_fmt(mean(pairs), 3), icon_name = "arrows-left-right",
                      note = paste("Rank correlation about", netsimr_fmt(mean(6 / pi * asin(pairs / 2)), 3))),
        netsimr_stat_tile("Matrix", if (valid) "Valid" else "Not valid", icon_name = if (valid) "circle-check" else "triangle-exclamation",
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
      if (stale()) netsimr_help("The settings have changed since this run: click Run to simulate them."),
      div(class = "dft-stats",
          netsimr_stat_tile(paste("VaR", label), netsimr_amount(total$var), icon_name = "triangle-exclamation", accent = TRUE,
                        note = paste("Of the total; mean", netsimr_amount(total$mean))),
          netsimr_stat_tile(paste("TVaR", label), netsimr_amount(total$tvar), icon_name = "chart-area", note = "The mean beyond the VaR"),
          netsimr_stat_tile("Diversification", netsimr_amount(d[["benefit"]]), icon_name = "scale-balanced",
                        note = paste0(netsimr_percent(d[["benefit_share"]]), " of the lines' VaR of ", netsimr_amount(d[["stand_alone_var"]]))),
          netsimr_stat_tile("Simulations", format(p$n, big.mark = ",", scientific = FALSE), icon_name = "hashtag",
                        note = if (p$copula == "t") paste0("t copula, ", format(p$df), " df") else "Gaussian copula"))
    )
  })

  output$capital_table <- renderUI({
    p <- current()$portfolio
    s <- p$summary
    lines <- s[s$line != "total", ]
    total <- s[s$line == "total", ]
    row <- function(x, name = x$line) {
      list(name, netsimr_amount(x$mean), netsimr_amount(x$sd), netsimr_amount(x$var), netsimr_amount(x$tvar), netsimr_amount(x$capital),
           netsimr_percent(x$capital_share))
    }
    label <- paste0(format(100 * p$level), "%")
    netsimr_table(c("Line", "Mean", "Standard deviation", paste("VaR", label), paste("TVaR", label), "Capital", "Share"),
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
  output$download_excel <- downloadHandler(
    filename = function() "portfolio_capital.xlsx",
    content = function(file) {
      p <- current()$portfolio
      summary <- p$summary
      summary$level <- p$level
      R <- p$correlation
      netsimr_write_excel(list(
        Summary = summary,
        Diversification = data.frame(measure = names(p$diversification), value = unname(p$diversification)),
        Correlation = data.frame(line = rownames(R), R, check.names = FALSE),
        `Simulated losses` = data.frame(simulation = seq_len(nrow(p$losses)), p$losses, check.names = FALSE)
      ), file)
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
  #Standard formula
  ######################

  # the inputs of each segment; by default the example's: a motor and property insurer
  sf_defaults <- data.frame(segment = 1:12, premium = c(40e6, 25e6, 0, 30e6, 10e6, rep(0, 7)),
                            reserve = c(60e6, 10e6, 0, 20e6, 15e6, rep(0, 7)))
  sf_id <- function(kind, s) paste0("sf_", kind, "_", s)
  loaded_sf <- reactiveVal(NULL)
  sf_value <- function(kind, s) {
    x <- input[[sf_id(kind, s)]]
    if (!is.null(x)) return(x)
    saved <- loaded_sf()
    if (!is.null(saved) && !is.null(saved[[kind]])) return(saved[[kind]][s])
    switch(kind, premium = sf_defaults$premium[s], reserve = sf_defaults$reserve[s], np = FALSE, div = 1)
  }

  output$sf_inputs <- renderUI({
    head <- function(x) div(class = "pfo-head", x)
    cells <- isolate(lapply(1:12, function(s) {
      name <- paste0(s, ". ", sii_nonlife_segments$name[s])
      tagList(
        div(class = "pfo-corr-name", name),
        netsimr_hidden_label(netsimr_amount_input(sf_id("premium", s), paste("Premium volume of", name), value = sf_value("premium", s), min = 0)),
        netsimr_hidden_label(netsimr_amount_input(sf_id("reserve", s), paste("Reserve volume of", name), value = sf_value("reserve", s), min = 0)),
        netsimr_hidden_label(numericInput(sf_id("div", s), paste("Geographical diversification of", name), value = sf_value("div", s),
                                      min = 0, max = 1, step = 0.05, width = "100%")),
        if (s %in% c(1, 4, 5)) {
          checkboxInput(sf_id("np", s), span(class = "visually-hidden", paste("Non-proportional reinsurance adjustment for", name)),
                        value = isTRUE(sf_value("np", s)))
        } else div()
      )
    }))
    tagList(
      div(class = "pfo-grid-wrap",
          div(class = "pfo-corr-grid", style = "grid-template-columns: minmax(14rem, 2fr) repeat(2, minmax(8rem, 1fr)) minmax(6rem, 0.7fr) minmax(4rem, auto); min-width: 760px;",
              head("Segment"), head("Premium volume"), head("Reserve volume"), head("Diversification"), head("NP 80%"), cells)),
      netsimr_help("Leave a segment's volumes at 0 if it is not written.")
    )
  })

  sf_result <- reactive({
    rows <- lapply(1:12, function(s) {
      p <- number(sf_value("premium", s))
      r <- number(sf_value("reserve", s))
      d <- number(sf_value("div", s))
      data.frame(segment = s, premium = if (is.finite(p)) p else 0, reserve = if (is.finite(r)) r else 0,
                 diversification = if (is.finite(d)) d else 1, np = isTRUE(sf_value("np", s)))
    })
    v <- do.call(rbind, rows)
    validate(need(all(v$premium >= 0 & v$reserve >= 0), "The volumes must be at least 0."),
             need(all(v$diversification >= 0 & v$diversification <= 1), "The geographical diversification must be from 0 to 1."))
    used <- v[v$premium + v$reserve > 0, ]
    validate(need(nrow(used) > 0, "Enter the premium or reserve volume of at least one segment."))
    np <- used$segment[used$np]
    checked(sii_premium_reserve_risk(used[c("segment", "premium", "reserve", "diversification")],
                                     np_adjustment = if (length(np)) np))
  })

  output$sf_stats <- renderUI({
    s <- sf_result()
    t <- s$total
    simulated <- tryCatch({
      p <- current()$portfolio
      total <- p$summary[p$summary$line == "total", ]
      list(value = total$var - total$mean, level = p$level)
    }, error = function(e) NULL)
    div(class = "dft-stats",
        netsimr_stat_tile("Capital requirement", netsimr_amount(t[["capital"]]), icon_name = "building-columns", accent = TRUE,
                      note = paste0("3 ", intToUtf8(215), " ", netsimr_percent(t[["sd"]], 2), " ", intToUtf8(215), " ", netsimr_amount(t[["volume"]]))),
        netsimr_stat_tile("Premium risk alone", netsimr_amount(s$premium_risk), icon_name = "file-invoice-dollar"),
        netsimr_stat_tile("Reserve risk alone", netsimr_amount(s$reserve_risk), icon_name = "hourglass-half"),
        netsimr_stat_tile("Diversification", netsimr_amount(t[["diversification"]]), icon_name = "scale-balanced",
                      note = paste("Of", netsimr_amount(t[["standalone"]]), "for the segments on their own")),
        netsimr_stat_tile("Simulated capital", if (is.null(simulated)) netsimr_dash else netsimr_amount(simulated$value), icon_name = "cubes",
                      note = if (is.null(simulated)) "Run the Capital tab to compare" else
                        paste0("VaR ", format(100 * simulated$level), "% less the mean, from the Capital tab")))
  })

  output$sf_table <- renderUI({
    s <- sf_result()
    b <- s$by_segment
    rows <- lapply(seq_len(nrow(b)), function(i) {
      list(paste0(b$segment[i], ". ", b$name[i]), netsimr_amount(b$premium[i]), netsimr_amount(b$reserve[i]), netsimr_percent(b$premium_sd[i], 1),
           netsimr_percent(b$reserve_sd[i], 1), netsimr_percent(b$sd[i], 2), netsimr_amount(b$capital[i]))
    })
    t <- s$total
    netsimr_table(c("Segment", "Premium volume", "Reserve volume", "Premium SD", "Reserve SD", "Combined SD", "Capital alone"), rows,
              list("Total", netsimr_amount(sum(b$premium)), netsimr_amount(sum(b$reserve)), "", "", netsimr_percent(t[["sd"]], 2),
                   netsimr_amount(t[["capital"]])),
              note = paste("Capital alone: three times the segment's combined standard deviation times its volume. The total",
                           "joins the segments with the correlations of Annex IV."))
  })

  output$sf_plot <- chart(function() {
    s <- sf_result()
    b <- s$by_segment
    labels <- c(paste0(b$segment, ". ", substr(b$name, 1, 18)), "Total")
    netsimr_category_chart(labels, bars = list(name = "Capital", values = c(b$capital, s$total[["capital"]]),
                                           colour = pfo_chart_colours(dark())$accent_soft),
                       lines = list(list(name = "Sum of the segments", values = c(rep(NA, nrow(b)), s$total[["standalone"]]),
                                         colour = netsimr_palette[1])),
                       x_title = "Segment", y_title = "Capital", dark = dark())
  }, alt = "The standard formula's capital of each segment on its own and of the total")

  output$download_sf <- downloadHandler(
    filename = function() "standard_formula.csv",
    content = function(file) {
      s <- sf_result()
      b <- s$by_segment
      t <- s$total
      total <- data.frame(segment = NA, name = "Total", premium = sum(b$premium), reserve = sum(b$reserve), premium_sd = NA,
                          reserve_sd = NA, sd = t[["sd"]], volume = t[["volume"]], capital = t[["capital"]])
      utils::write.csv(csv_safe(rbind(b, total)), file, row.names = FALSE)
    }
  )

  ######################
  #Settings file
  ######################

  output$save_settings <- downloadHandler(
    filename = function() "portfolio_settings.txt",
    content = function(file) {
      values <- collect_setting_values(input, pfo_settings_inputs)
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
      # the standard formula's volumes of the 12 segments
      values$sf_premium <- vapply(1:12, function(s) number(sf_value("premium", s)), numeric(1))
      values$sf_reserve <- vapply(1:12, function(s) number(sf_value("reserve", s)), numeric(1))
      values$sf_div <- vapply(1:12, function(s) number(sf_value("div", s)), numeric(1))
      values$sf_np <- vapply(1:12, function(s) isTRUE(sf_value("np", s)), logical(1))
      write_settings_file(values, file, pfo_settings_tool, pfo_settings_version)
    }
  )

  observeEvent(input$load_settings, {
    v <- load_settings_values(input$load_settings$datapath, pfo_settings_tool, pfo_settings_version)
    if (is.null(v)) return()
    # the choices of the radio buttons and selects, as the page offers them
    choices <- tool_input_choices("portfolio", portfolio_tool_UI)
    usable <- function(id) setting_usable(v[[id]], pfo_settings_inputs[[id]], choices[[id]])
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
      if (usable(id) && id != "n_lines") update_setting_input(session, id, pfo_settings_inputs[[id]], v[[id]])
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
    take12 <- function(x, logical = FALSE) {
      if (length(x) != 12) return(NULL)
      if (logical) as.logical(x) %in% TRUE else suppressWarnings(as.numeric(x))
    }
    sf <- list(premium = take12(v$sf_premium), reserve = take12(v$sf_reserve), div = take12(v$sf_div),
               np = take12(v$sf_np, logical = TRUE))
    sf <- sf[!vapply(sf, is.null, logical(1))]
    if (length(sf) > 0) {
      loaded_sf(sf)
      for (s in 1:12) {
        for (kind in intersect(c("premium", "reserve", "div"), names(sf))) updateNumericInput(session, sf_id(kind, s), value = sf[[kind]][s])
        if (!is.null(sf$np) && s %in% c(1, 4, 5)) updateCheckboxInput(session, sf_id("np", s), value = sf$np[s])
      }
    }
    showNotification(paste0("Loaded the settings of ", input$load_settings$name,
                            if (identical(v$line_source, "upload")) ". Upload the losses to apply their columns." else "."),
                     type = "message", duration = 6)
  })

  # the summary tiles are hidden while empty, and shiny does not render hidden outputs,
  # so they must render even when hidden or they would never appear
  for (id in c("lines_stats", "dependence_stats", "capital_stats", "sf_stats")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
