#' Server function for the Distribution Fitting tool application
#'
#' @description Reads the uploaded file, fits the frequency, severity, sliced
#'   and piecewise Pareto models the user asks for and renders their tables and
#'   charts; paired with \code{distribution_fitting_tool_UI} by
#'   \code{\link{run_shiny_distribution_fitting_tool}}.
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @keywords internal
#' @import shiny

distribution_fitting_tool_Server <- function(input, output, session) {

  ######################
  #help functions
  ######################


  # the charts are drawn in the colours of the theme the page reports
  dark <- reactive(identical(input$app_theme, "dark"))

  # reports a failed fit to the user (instead of the console) and returns NULL,
  # so that the dependent outputs show a dash rather than an error
  failed_fit <- function(what, e) {
    showNotification(paste0(what, " failed: ", conditionMessage(e)), type = "error", duration = 10)
    NULL
  }

  # message shown in place of an output until its analysis has run
  need_run <- function(button, what) {
    validate(
      need(has_data(), "Upload a data file on the Data tab first, or choose an example."),
      need(isTruthy(input[[button]]), paste0("Choose the ", what, " column and click Run analysis."))
    )
  }

  # the claim sizes of a column: positive numbers only, with the number of rows left out
  claim_sizes <- function(column) {
    df <- data()
    validate(need(!is.null(column) && column %in% names(df), "Choose the claim size column."))
    values <- netsimr_as_numeric(df[[column]], or_default(input$dec, "."))
    keep <- is.finite(values) & values > 0
    claims <- values[keep]
    validate(
      need(length(claims) >= 2, "The column needs at least two positive claim sizes."),
      need(length(unique(claims)) >= 2, "All the claim sizes are the same, so no distribution can be fitted.")
    )
    list(claims = claims, dropped = sum(!keep), rows = keep, column = column)
  }

  rows_left_out_note <- function(dropped) {
    if (dropped > 0) paste(format(dropped, big.mark = ",", scientific = FALSE), if (dropped == 1) "row left out" else "rows left out")
  }

  # lognormal maximum likelihood estimates, as fitdistrplus::fitdist(x, "lnorm") gives them
  fit_lnorm_mle <- function(x) {
    meanlog <- mean(log(x))
    list(estimate = c(meanlog = meanlog, sdlog = sqrt(mean((log(x) - meanlog)^2))))
  }

  # least squares fit of the Pareto alpha of the layer between the slicing points to the empirical cdf
  fit_slice_pareto <- function (sev_data, slic_pont_lft, slic_pont_rght) {
    z <- subset(sev_data, sev_data > slic_pont_lft)
    w <- subset(z, z <= slic_pont_rght)
    # with one or two claims the sum of squares is nearly flat: the optimiser stops at
    # whatever alpha it reaches, an arbitrary number that would be shown as the fit
    if (length(w) < 3) stop("at least three claims must lie between the slicing points")
    dat <- data.frame(severity = w, empirical = rank(w) / (length(z) + 1))
    SumOfSquares <- function(data, par) {
      sum((1 - (slic_pont_lft / data$severity)^par[1] - data$empirical)^2)
    }
    result <- optim(par = 1, SumOfSquares, data = dat, lower = 0.0001, method = "L-BFGS-B")
    if (result$convergence != 0) stop("the optimisation did not converge")
    return(result$par)
  }

  # a chart, drawn as a PNG image on a transparent background; it is redrawn when resized,
  # so that its legend is laid out for the new width
  chart <- function(draw, alt) netsimr_render_plot(draw(), bg = "transparent", res = 96, alt = alt)

  # the charts of the analyses, each drawn for a theme: on the page once its analysis has
  # run, in the colours of the theme showing (it is redrawn when the theme changes), and in
  # the report in both
  charts <- list()
  add_chart <- function(id, button, what, draw, alt) {
    charts[[id]] <<- list(draw = draw, alt = alt)
    output[[id]] <- chart(function() {
      need_run(button, what)
      draw(isTRUE(dark()))
    }, alt)
  }

  # the fitted cdfs of claim size models that have one, named, with the colour of each model
  model_cdfs <- function(models) {
    has_cdf <- !vapply(models, function(m) is.null(m$cdf), logical(1))
    list(cdfs = stats::setNames(lapply(models[has_cdf], function(m) m$cdf), vapply(models[has_cdf], function(m) m$name, "")),
         colours = netsimr_palette[which(has_cdf)])
  }

  ######################
  #Data input
  ######################

  # an example data set is chosen, or a file uploaded
  using_example <- reactive(identical(input$data_source, "example"))
  has_data <- function() using_example() || isTruthy(input$file1)
  data_name <- reactive({
    if (using_example()) names(dft_examples)[match(or_default(input$example, "motor"), dft_examples)] else input$file1$name
  })

  data <- reactive({
    if (using_example()) {
      return(switch(or_default(input$example, "motor"), liability = NetSimR::liability_claims, NetSimR::motor_policies))
    }
    validate(need(input$file1, "Upload a data file to see a preview here, or choose an example."))
    read_error <- NULL
    df <- tryCatch(
      netsimr_read_data(
        input$file1$datapath,
        header = isTRUE(input$data_includes_header),
        sep = or_default(input$sep, ","),
        quote = or_default(input$quote, "\""),
        dec = or_default(input$dec, ".")
      ),
      error = function(e) {
        read_error <<- conditionMessage(e)
        NULL
      }
    )
    validate(need(is.null(read_error), paste0("The file could not be read (", read_error, "). Check the separator and quote settings.")))
    validate(need(ncol(df) > 0 && nrow(df) > 0, "The file has no data rows."))
    df
  })

  numeric_columns <- reactive(netsimr_numeric_columns(data(), or_default(input$dec, ".")))

  # the columns of the last read (names and which are numeric), the guess made for each
  # select and the column each select was given
  column_choices <- reactiveVal(list(layout = NULL, guesses = list(), selected = list()))

  # what a loaded settings file holds for later: its columns, until data that has them is
  # read, and its slicing points and thresholds, until their analysis runs (the tool sets
  # the columns when data is read, and the points and thresholds when an analysis runs)
  pending <- new.env()
  pending$columns <- list()
  pending$slicing <- NULL
  pending$thresholds <- NULL

  # offer the columns of the file in every analysis tab, guessing from the column names, then
  # from the values. A column the user chose is kept while the file still has it; a guess is
  # made again when the columns or their types change (a file first read with the wrong
  # separator or decimal mark gives other columns, or other numeric columns, once corrected)
  observeEvent(data(), {
    df <- data()
    columns <- names(df)
    numbers <- numeric_columns()
    by_name <- function(pattern) {
      matched <- numbers[grepl(pattern, numbers, ignore.case = TRUE)]
      if (length(matched) > 0) matched[1] else NULL
    }
    means <- vapply(numbers, function(column) mean(netsimr_as_numeric(df[[column]], or_default(input$dec, ".")), na.rm = TRUE), numeric(1))
    first <- if (length(numbers) > 0) numbers[1] else columns[1]
    counts_default <- or_default(by_name("count|freq|number|claims?_n|n_claims"), first)
    severity_default <- or_default(by_name("amount|sever|size|loss|cost|paid|incurred|value"),
                                   if (length(numbers) > 0) numbers[which.max(means)] else first)
    weights_default <- or_default(by_name("weight|exposure"), setdiff(c(numbers, first), counts_default)[1])
    defaults <- list(counts_var = counts_default, counts_weights_var = weights_default, severity_var = severity_default,
                     sliced_sev_var = severity_default, piecewise_pareto_var = severity_default)
    previous <- column_choices()
    layout <- list(columns = columns, numbers = numbers)
    changed <- !identical(layout, previous$layout)
    guesses <- previous$guesses
    selected <- list()
    for (id in names(defaults)) {
      current <- input[[id]]
      loaded <- pending$columns[[id]]
      # still the guess (the user has not chosen another column), made from other columns
      stale_guess <- changed && identical(current, guesses[[id]])
      if (!is.null(loaded) && loaded %in% columns) {
        # the column of a loaded settings file, now that the data has it
        selected[[id]] <- loaded
        pending$columns[[id]] <- NULL
      } else if (is.null(current) || !(current %in% columns) || stale_guess) {
        selected[[id]] <- defaults[[id]]
        guesses[[id]] <- defaults[[id]]
      } else {
        selected[[id]] <- current
      }
      updateSelectizeInput(session, id, choices = columns, selected = selected[[id]])
    }
    column_choices(list(layout = layout, guesses = guesses, selected = selected))
    # the policy limit column: the one chosen while the file has it, else one named like a limit
    limit <- input$sev_limit_col
    loaded <- pending$columns$sev_limit_col
    if (!is.null(loaded) && loaded %in% columns) {
      limit <- loaded
      pending$columns$sev_limit_col <- NULL
    } else if (is.null(limit) || !(limit %in% columns)) {
      limit <- or_default(by_name("limit|cap|maximum|max_|sum_insured"), first)
    }
    updateSelectizeInput(session, "sev_limit_col", choices = columns, selected = limit)
  })

  # the preview shows the first rows; the analyses use every row
  preview_rows <- 100

  output$data_overview <- renderUI({
    req(has_data())
    df <- data()
    truncated <- nrow(df) > preview_rows
    # a file without a header read with one: its first row became the column names
    numeric_names <- isTRUE(input$data_includes_header) && !anyNA(netsimr_as_numeric(names(df), or_default(input$dec, ".")))
    columns_note <- if (numeric_names) {
      "The column names look like numbers: is the first row a header?"
    } else if (ncol(df) == 1) {
      "Only one column: check the separator"
    } else {
      paste(length(numeric_columns()), "numeric")
    }
    div(
      class = "dft-stats",
      netsimr_stat_tile(if (using_example()) "Example" else "File", div(class = "dft-file-name", data_name()), icon_name = "file-lines"),
      netsimr_stat_tile("Rows", format(nrow(df), big.mark = ",", scientific = FALSE), icon_name = "bars",
                    note = if (truncated) paste("The preview shows the first", format(preview_rows, big.mark = ",", scientific = FALSE))),
      netsimr_stat_tile("Columns", format(ncol(df), big.mark = ",", scientific = FALSE), icon_name = "table-columns", note = columns_note)
    )
  })

  output$data_table <- renderUI(netsimr_data_preview(data(), preview_rows))

  # a file uploaded is the data, also after an example
  observeEvent(input$file1, updateRadioButtons(session, "data_source", selected = "upload"))

  # the example: the liability claims, fitted with their policy limits on the Severity tab
  observeEvent(input$try_example, {
    updateRadioButtons(session, "data_source", selected = "example")
    updateSelectInput(session, "example", selected = "liability")
    updateRadioButtons(session, "sev_limit_source", selected = "column")
    bslib::nav_select("dft_navbar", "severity", session = session)
    showNotification(paste("The example liability claims are loaded, with their policy limits: click Run analysis to fit them.",
                           "The motor policies example, on the Data tab, has claim counts for the Frequency tab."),
                     type = "message", duration = 10)
  })

  ######################
  #Frequency analysis
  ######################

  # counts and weights are cleaned together, so that each count keeps the weight of its own row
  freq_input <- eventReactive(input$execute_freq_analysis, {
    df <- data()
    validate(need(!is.null(input$counts_var) && input$counts_var %in% names(df), "Choose the claim counts column."))
    dec <- or_default(input$dec, ".")
    counts <- netsimr_as_numeric(df[[input$counts_var]], dec)
    weighted <- isTRUE(input$counts_weighted_var)
    if (weighted) {
      validate(need(!is.null(input$counts_weights_var) && input$counts_weights_var %in% names(df), "Choose the weights column."))
      weights <- netsimr_as_numeric(df[[input$counts_weights_var]], dec)
    } else {
      weights <- rep(1, length(counts))
    }
    keep <- is.finite(counts) & counts >= 0 & is.finite(weights) & weights > 0
    counts <- counts[keep]
    weights <- weights[keep]
    validate(
      need(length(counts) >= 2, "The column needs at least two valid claim counts."),
      # the Poisson mean would be 0 and there is no Negative Binomial to fit
      need(any(counts > 0), "All the claim counts are zero, so no distribution can be fitted."),
      need(all(counts == round(counts)),
           "Claim counts must be whole numbers. For claim amounts, use the Severity tab."),
      need(!weighted || all(weights == round(weights)),
           "Weights must be whole numbers: each row counts as that many observations.")
    )
    list(counts = counts, weights = if (weighted) weights else NULL, weighted = weighted, dropped = sum(!keep),
         column = input$counts_var, weights_column = if (weighted) input$counts_weights_var)
  })

  counts_data <- reactive(freq_input()$counts)
  weights_data <- reactive(freq_input()$weights)

  fit_counts <- function(fit, label) {
    x <- freq_input()
    tryCatch(fit(x$counts, x$weights), error = function(e) failed_fit(label, e))
  }
  freq_po_fit <- reactive(fit_counts(fit_poisson_mle, "Poisson fit"))
  freq_nb_fit <- reactive(fit_counts(fit_nbinom_mle, "Negative Binomial fit"))
  freq_zip_fit <- reactive(fit_counts(fit_zip_mle, "Zero-inflated Poisson fit"))
  freq_zinb_fit <- reactive(fit_counts(fit_zinb_mle, "Zero-inflated Negative Binomial fit"))
  # the four fits, by the name of their model; NULL for one that failed
  freq_fits <- reactive(stats::setNames(list(freq_po_fit(), freq_nb_fit(), freq_zip_fit(), freq_zinb_fit()),
                                        unname(dft_count_model_names)))

  # the fitted cdfs for the charts, named, with the colour of each model; a model that failed is left
  # out, and so is a zero-inflated one without extra zeros, which would draw over its base distribution
  count_cdfs <- reactive({
    fits <- freq_fits()
    shown <- !vapply(fits, is.null, logical(1)) & !vapply(fits, function(f) identical(f$inflated, FALSE), logical(1))
    list(cdfs = lapply(fits[shown], dft_count_fit_cdf), colours = netsimr_palette[which(shown)])
  })

  # weighted moments; without weights they are the usual mean and sample variance
  freq_moments <- reactive({
    x <- freq_input()
    w <- if (is.null(x$weights)) rep(1, length(x$counts)) else x$weights
    total <- sum(w)
    mean <- sum(w * x$counts) / total
    list(n = total, mean = mean, variance = sum(w * (x$counts - mean)^2) / (total - 1))
  })

  fit_value <- function(fit, name) if (is.null(fit)) NA_real_ else unname(fit$estimate[[name]])
  fit_stat <- function(fit, name) if (is.null(fit)) NA_real_ else fit[[name]]

  # the suggested model follows the fits: the lowest AIC of the models that are models of their own
  # (a Negative Binomial with a size, a zero-inflated model with extra zeros), the Poisson otherwise
  freq_suggested <- function() {
    m <- freq_moments()
    fits <- freq_fits()
    aic <- vapply(fits, function(f) if (dft_count_fit_distinct(f)) f$aic else NA_real_, numeric(1))
    aic[1] <- fit_stat(fits[[1]], "aic")
    suggested <- if (any(is.finite(aic))) names(fits)[which.min(aic)] else "Poisson"
    note <- if (startsWith(suggested, "Zero-inflated")) {
      "Lowest AIC: more periods without a claim than the other models give"
    } else if (suggested == "Negative Binomial") {
      "Lowest AIC: the counts are overdispersed"
    } else if (isTRUE(m$variance > m$mean)) {
      "Too little overdispersion for the other models"
    } else {
      "The variance is not above the mean"
    }
    list(model = suggested, note = note)
  }

  output$freq_stats <- renderUI({
    m <- freq_moments()
    x <- freq_input()
    suggested <- freq_suggested()
    div(
      class = "dft-stats",
      netsimr_stat_tile(if (x$weighted) "Weighted observations" else "Observations", netsimr_fmt_count(m$n),
                    note = rows_left_out_note(x$dropped), icon_name = "hashtag"),
      netsimr_stat_tile("Mean", netsimr_fmt(m$mean), icon_name = "bullseye"),
      netsimr_stat_tile("Variance", netsimr_fmt(m$variance), icon_name = "arrows-left-right",
                    note = if (m$mean > 0) paste("Variance / mean:", netsimr_fmt(m$variance / m$mean, 3))),
      netsimr_stat_tile("Suggested model", suggested$model, note = suggested$note,
                    icon_name = "lightbulb", accent = TRUE)
    )
  })

  # the tables of the fits are built as the arguments of netsimr_html_table(), for the page and the report
  freq_table <- function() {
    po <- freq_po_fit()
    nb <- freq_nb_fit()
    zip <- freq_zip_fit()
    zinb <- freq_zinb_fit()
    size <- fit_value(nb, "size")
    zinb_size <- fit_value(zinb, "size")
    blank <- dft_param_cell("", NA)
    stats <- function(fit) list(netsimr_fmt(fit_stat(fit, "loglik"), 6), netsimr_fmt(fit_stat(fit, "aic"), 6))
    rows <- list(
      c(list("Poisson", dft_param_cell("Mean", fit_value(po, "lambda")), blank, blank), stats(po)),
      # Beta = mu / size, as the simulator's Negative Binomial: mean r * Beta, variance r * Beta * (1 + Beta)
      c(list("Negative Binomial", dft_param_cell("r", size), dft_param_cell("Beta", fit_value(nb, "mu") / size), blank),
        stats(nb)),
      c(list("Zero-inflated Poisson", dft_param_cell("Mean", fit_value(zip, "lambda")),
             dft_param_cell("Extra zeros", fit_value(zip, "zero")), blank), stats(zip)),
      c(list("Zero-inflated Negative Binomial", dft_param_cell("r", zinb_size),
             dft_param_cell("Beta", fit_value(zinb, "mu") / zinb_size),
             dft_param_cell("Extra zeros", fit_value(zinb, "zero"))), stats(zinb))
    )
    aic <- vapply(list(po, nb, zip, zinb), fit_stat, numeric(1), name = "aic")
    no_extra_zeros <- c(if (identical(zip$inflated, FALSE)) "Poisson", if (identical(zinb$inflated, FALSE)) "Negative Binomial")
    list(
      header = c("Distribution", "Parameter 1", "Parameter 2", "Parameter 3", "Log-likelihood", "AIC"), rows = rows,
      best = if (any(is.finite(aic))) which.min(aic), best_label = "Lowest AIC", stack = TRUE,
      note = paste0(
        "Negative Binomial with mean r x Beta and variance r x Beta x (1 + Beta), as in the NetSimR simulator. ",
        "A zero-inflated count is zero with the probability of its extra zeros, and otherwise a count of its Poisson ",
        "or Negative Binomial.",
        if (isTRUE(nb$capped)) paste0(" The variance is not above the mean, so the Negative Binomial tends to the Poisson: ",
                                      "r is set to ", netsimr_fmt_count(dft_nbinom_max_size), "."),
        if (length(no_extra_zeros)) paste0(" The data has no more zeros than the ", paste(no_extra_zeros, collapse = " or the "),
                                           " gives, so its zero-inflated fit has no extra zeros.")
      )
    )
  }

  output$selected_freq_params <- renderUI({
    need_run("execute_freq_analysis", "claim counts")
    do.call(netsimr_html_table, freq_table())
  })

  output$freq_summary <- renderPrint({
    need_run("execute_freq_analysis", "claim counts")
    x <- freq_input()
    print(summary(x$counts))
    if (x$weighted) cat("\nTotal weight:", netsimr_fmt_count(sum(x$weights)), "\n")
  })

  output$selected_distribution_summary <- renderPrint({
    need_run("execute_freq_analysis", "claim counts")
    fit <- switch(or_default(input$FreqDistri, "Poisson"), NegativeBinomial = freq_nb_fit(), ZIPoisson = freq_zip_fit(),
                  ZINegativeBinomial = freq_zinb_fit(), freq_po_fit())
    validate(need(!is.null(fit), "This distribution could not be fitted to the data."))
    dft_print_count_fit(fit)
  })

  # observed share of observations in each bin of counts, against the probability of each fitted model
  add_chart("count_hist", "execute_freq_analysis", "claim counts", function(in_dark) {
    x <- freq_input()
    models <- count_cdfs()
    d <- dft_count_hist_data(x$counts, x$weights, or_default(input$count_hist_bins, 20), models$cdfs)
    netsimr_category_chart(
      d$labels,
      bars = list(name = "Observed", values = d$observed, colour = "bar", border = "bar_border"),
      lines = unname(Map(function(name, y, colour) list(name = name, values = y, colour = colour), names(d$fitted), d$fitted, models$colours)),
      x_title = "Number of claims", y_title = "Share of observations", dark = in_dark, y_percent = TRUE
    )
  }, alt = "Histogram of the claim counts, with the probabilities of the fitted distributions")

  add_chart("freq_fit_plot", "execute_freq_analysis", "claim counts", function(in_dark) {
    x <- freq_input()
    models <- count_cdfs()
    d <- dft_count_cdf_data(x$counts, x$weights, models$cdfs)
    netsimr_line_chart(dft_cdf_series(d, models$colours, step = TRUE), "Number of claims", "Cumulative probability",
                   in_dark, ylim = c(0, 1.02))
  }, alt = "Empirical and fitted cumulative distributions of the claim counts")

  ######################
  #Severity analysis
  ######################

  # the policy limit of each claim of the rows kept: one for all, a column (a row without a
  # positive limit has none), or none (Inf)
  severity_limits <- function(rows) {
    source <- or_default(input$sev_limit_source, "none")
    if (identical(source, "value")) {
      limit <- input$sev_limit
      validate(need(is.numeric(limit) && length(limit) == 1 && is.finite(limit) && limit > 0,
                    "Enter the policy limit, a positive amount."))
      return(rep(limit, sum(rows)))
    }
    if (identical(source, "column")) {
      df <- data()
      column <- input$sev_limit_col
      validate(need(!is.null(column) && column %in% names(df), "Choose the policy limit column."),
               need(!identical(column, input$severity_var), "Choose a policy limit column other than the claim sizes."))
      limits <- netsimr_as_numeric(df[[column]], or_default(input$dec, "."))[rows]
      return(ifelse(is.finite(limits) & limits > 0, limits, Inf))
    }
    rep(Inf, sum(rows))
  }

  # the claim sizes, the reporting threshold (0 for none): with one, only the claims above it;
  # and which claims are censored, at or above their policy limit (the loss was at least that)
  sev_input <- eventReactive(input$execute_sev_analysis, {
    sizes <- claim_sizes(input$severity_var)
    censored <- sizes$claims >= severity_limits(sizes$rows)
    d <- input$sev_threshold
    if (is.null(d) || length(d) != 1 || is.na(d) || d == 0) {
      d <- 0
    } else {
      validate(need(is.finite(d) && d > 0, "The reporting threshold must be a positive amount, or empty."))
    }
    above <- sizes$claims > d
    validate(need(length(unique(sizes$claims[above & !censored])) >= 2,
                  if (d > 0) "At least two different claim sizes below their limits must lie above the reporting threshold." else
                    "At least two different claim sizes must be below their policy limits."))
    limits <- switch(or_default(input$sev_limit_source, "none"),
                     value = paste(netsimr_fmt(input$sev_limit), "for every claim"),
                     column = paste("The column", input$sev_limit_col), "None")
    list(claims = sizes$claims[above], censored = censored[above], dropped = sizes$dropped, threshold = d, below = sum(!above),
         column = sizes$column, limits = limits)
  })
  severity_data <- reactive(sev_input()$claims)
  sev_threshold <- reactive(sev_input()$threshold)
  sev_censored <- reactive(sev_input()$censored)
  sev_has_censoring <- reactive(any(sev_censored()))
  # the empirical cdf of the claims: with censoring, the Kaplan-Meier estimate
  sev_km <- reactive(dft_kaplan_meier(severity_data(), sev_censored()))

  # maximum likelihood fits
  sev_norm_fit <- reactive({
    x <- severity_data()
    list(estimate = c(mean = mean(x), sd = sqrt(mean((x - mean(x))^2))))
  })
  sev_lnorm_fit <- reactive(fit_lnorm_mle(severity_data()))
  sev_gamma_fit <- reactive({
    tryCatch(fit_gamma_mle(severity_data()), error = function(e) failed_fit("Gamma fit", e))
  })
  sev_weibull_fit <- reactive({
    tryCatch(dft_fit_weibull(severity_data()), error = function(e) failed_fit("Weibull fit", e))
  })
  sev_pareto_xm <- reactive(min(severity_data()))
  sev_pareto_alpha <- reactive(length(severity_data()) / sum(log(severity_data() / sev_pareto_xm())))

  # the fits to claims above a reporting threshold (a fit that fails is explained under the table)
  sev_truncated_models <- reactive(dft_truncated_severity_models(severity_data(), sev_threshold(), sev_censored()))

  # the fitted models, each with its parameters, mean, cdf and log-likelihood; above a reporting
  # threshold, the mean and cdf are of the claims above it
  sev_models <- reactive({
    if (sev_threshold() > 0 || sev_has_censoring()) return(sev_truncated_models())
    x <- severity_data()
    norm <- unname(sev_norm_fit()$estimate)
    lnorm <- unname(sev_lnorm_fit()$estimate)
    gamma <- if (is.null(sev_gamma_fit())) c(NA_real_, NA_real_) else unname(sev_gamma_fit()$estimate)
    weibull <- if (is.null(sev_weibull_fit())) c(shape = NA_real_, scale = NA_real_) else sev_weibull_fit()$estimate
    xm <- sev_pareto_xm()
    alpha <- sev_pareto_alpha()
    rate <- 1 / mean(x)
    pareto_cdf <- function(q) ifelse(q < xm, 0, 1 - (xm / q)^alpha)
    # the Burr, and the generalised Pareto from zero (a fit that fails is explained under the table)
    heavy <- dft_heavy_tail_models(x)
    models <- list(
      list(name = "Normal", params = c(Mean = norm[1], `Std. dev.` = norm[2]), mean = norm[1],
           cdf = function(q) pnorm(q, norm[1], norm[2]), loglik = sum(dnorm(x, norm[1], norm[2], log = TRUE)), k = 2),
      list(name = "LogNormal", params = c(mu = lnorm[1], sigma = lnorm[2]), mean = exp(lnorm[1] + lnorm[2]^2 / 2),
           cdf = function(q) plnorm(q, lnorm[1], lnorm[2]), loglik = sum(dlnorm(x, lnorm[1], lnorm[2], log = TRUE)), k = 2),
      list(name = "Exponential", params = c(Mean = mean(x)), mean = mean(x),
           cdf = function(q) pexp(q, rate), loglik = sum(dexp(x, rate, log = TRUE)), k = 1),
      list(name = "Gamma", params = c(Scale = gamma[1], Shape = gamma[2]), mean = gamma[1] * gamma[2],
           cdf = if (anyNA(gamma)) NULL else function(q) pgamma(q, scale = gamma[1], shape = gamma[2]),
           loglik = sum(dgamma(x, scale = gamma[1], shape = gamma[2], log = TRUE)), k = 2),
      list(name = "Pareto", params = c(xm = xm, alpha = alpha), mean = if (alpha > 1) alpha * xm / (alpha - 1) else Inf,
           cdf = pareto_cdf, loglik = length(x) * (log(alpha) + alpha * log(xm)) - (alpha + 1) * sum(log(x)), k = 2),
      list(name = "Weibull", params = c(Scale = weibull[["scale"]], Shape = weibull[["shape"]]),
           mean = weibull[["scale"]] * gamma(1 + 1 / weibull[["shape"]]),
           cdf = if (anyNA(weibull)) NULL else function(q) pweibull(q, shape = weibull[["shape"]], scale = weibull[["scale"]]),
           loglik = sum(dweibull(x, shape = weibull[["shape"]], scale = weibull[["scale"]], log = TRUE)), k = 2)
    )
    structure(c(models, heavy), failed = attr(heavy, "failed"))
  })

  output$sev_stats <- renderUI({
    x <- severity_data()
    div(
      class = "dft-stats",
      netsimr_stat_tile("Claims", format(length(x), big.mark = ",", scientific = FALSE), icon_name = "hashtag",
                    note = paste(c(rows_left_out_note(sev_input()$dropped),
                                   if (sev_input()$below > 0) paste(format(sev_input()$below, big.mark = ",", scientific = FALSE), "at or below",
                                                                    netsimr_fmt(sev_threshold()), "left out"),
                                   if (sev_has_censoring()) paste(format(sum(sev_censored()), big.mark = ",", scientific = FALSE),
                                                                  "at their policy limits")),
                                 collapse = "; ")),
      netsimr_stat_tile("Mean", netsimr_fmt(mean(x)), icon_name = "bullseye"),
      netsimr_stat_tile("Median", netsimr_fmt(median(x)), icon_name = "arrows-left-right"),
      netsimr_stat_tile("Coefficient of variation", netsimr_fmt(sd(x) / mean(x), 3), icon_name = "wave-square"),
      netsimr_stat_tile("Largest claim", netsimr_fmt(max(x)), icon_name = "arrow-up-right-dots")
    )
  })

  sev_table <- function() {
    x <- severity_data()
    models <- sev_models()
    sorted <- sort(x)
    censoring <- sev_has_censoring()
    # with censoring, the distances are to the Kaplan-Meier estimate, and there is no A-D statistic
    ks <- vapply(models, function(m) {
      if (is.null(m$cdf)) NA_real_ else if (censoring) dft_km_distance(sev_km(), m$cdf) else ks_distance(sorted, m$cdf, sorted = TRUE)
    }, numeric(1))
    ad <- vapply(models, function(m) if (is.null(m$cdf) || censoring) NA_real_ else ad_statistic(sorted, m$cdf, sorted = TRUE),
                 numeric(1))
    rows <- lapply(seq_along(models), function(i) {
      m <- models[[i]]
      params <- names(m$params)
      list(
        m$name,
        dft_param_cell(params[1], m$params[[1]]),
        if (length(params) > 1) dft_param_cell(params[2], m$params[[2]]) else dft_param_cell("", NA),
        if (length(params) > 2) dft_param_cell(params[3], m$params[[3]]) else dft_param_cell("", NA),
        if (is.infinite(m$mean)) "Infinite" else netsimr_fmt(m$mean),
        netsimr_fmt(ks[i], 3),
        netsimr_fmt(ad[i], 4),
        netsimr_fmt(2 * m$k - 2 * m$loglik, 6)
      )
    })
    d <- sev_threshold()
    list(
      header = c("Distribution", "Parameter 1", "Parameter 2", "Parameter 3", if (d > 0) "Mean above\nthreshold" else "Mean",
                 "K-S distance", "A-D statistic", "AIC"), rows = rows,
      best = if (any(is.finite(ks))) which.min(ks), best_label = "Closest", stack = TRUE,
      note = if (censoring) {
        k <- sum(sev_censored())
        paste0(format(k, big.mark = ",", scientific = FALSE), if (k == 1) " claim is" else " claims are", " at or above the policy limit, so the ",
               "loss was at least the claim: each counts as the probability of exceeding it (censored), not as a claim of ",
               "that size. ", if (d > 0) paste0("Fitted to the claims above the reporting threshold of ", netsimr_fmt(d),
                                                 " as claims from zero; the means are of the claims above the threshold. ") else "",
               "The parameters are of all claims, before the limits. The K-S distance is to the Kaplan-Meier estimate of the ",
               "claim sizes, which allows for the limits; the A-D statistic needs every claim's size, so it is not given. ",
               if (d > 0) "The Pareto and the generalised Pareto start at the threshold." else
                 "The Pareto starts at the smallest claim, the generalised Pareto at zero.",
               dft_failed_fits_note(attr(models, "failed")))
      } else if (d > 0) {
        paste0("Fitted to the claims above the reporting threshold of ", netsimr_fmt(d), " as claims from zero: the parameters ",
               "are of all claims, the means and the K-S distances of the claims above the threshold. Empirical mean ",
               netsimr_fmt(mean(x)), ". The Pareto and the generalised Pareto start at the threshold: they are of the claims ",
               "above it only.", dft_failed_fits_note(attr(models, "failed")))
      } else {
        paste0("Empirical mean ", netsimr_fmt(mean(x)), ". The Pareto starts at the smallest claim, the generalised Pareto ",
               "at zero. The Anderson-Darling statistic weighs the tails more than the K-S distance; for both, lower is closer.",
               dft_failed_fits_note(attr(models, "failed")))
      }
    )
  }

  output$sev_param_summary <- renderUI({
    need_run("execute_sev_analysis", "claim size")
    do.call(netsimr_html_table, sev_table())
  })

  add_chart("sev_hist", "execute_sev_analysis", "claim size", function(in_dark) {
    d <- dft_severity_hist_data(severity_data(), or_default(input$severity_hist_bins, 20))
    netsimr_histogram_chart(d$breaks, d$counts, "Claim size", "Number of claims", in_dark)
  }, alt = "Histogram of the claim sizes")

  output$sev_summary <- renderPrint({
    need_run("execute_sev_analysis", "claim size")
    x <- severity_data()
    print(summary(x))
    cat("\nPercentiles\n")
    print(quantile(x, c(0.75, 0.9, 0.95, 0.99, 0.995)))
  })

  # the quantiles of each fitted distribution against those of the claims
  add_chart("sev_qq_plot", "execute_sev_analysis", "claim size", function(in_dark) {
    log_scale <- isTRUE(input$sev_fit_log_scale)
    models <- model_cdfs(sev_models())
    q <- dft_qq_data(severity_data(), models$cdfs)
    if (sev_has_censoring()) {
      # the quantiles of the Kaplan-Meier estimate, as far as it reaches below the limits
      km <- sev_km()
      q$empirical <- dft_km_quantiles(km, q$p)
      known <- is.finite(q$empirical)
      validate(need(sum(known) >= 2, "Too few claims below their limits for a Q-Q plot."))
      q$p <- q$p[known]
      q$empirical <- q$empirical[known]
      q$fitted <- lapply(q$fitted, function(f) f[known])
    }
    axis <- if (log_scale) function(v) log10(pmax(v, .Machine$double.xmin)) else identity
    keep <- if (log_scale) q$empirical > 0 else rep(TRUE, length(q$empirical))
    x <- axis(q$empirical[keep])
    series <- c(
      list(list(x = range(x), y = range(x), name = "Perfect fit", colour = "muted", lwd = 1.4)),
      lapply(seq_along(q$fitted), function(i) {
        list(x = x, y = axis(q$fitted[[i]][keep]), name = names(q$fitted)[i], colour = models$colours[i])
      })
    )
    # the axes follow the claims: a model whose quantiles run far beyond them would hide the rest
    span <- diff(range(x))
    netsimr_line_chart(series, if (log_scale) "Claim size quantile (log10)" else "Claim size quantile",
                   if (log_scale) "Fitted quantile (log10)" else "Fitted quantile", in_dark,
                   ylim = c(min(x) - 0.1 * span, max(x) + 0.25 * span))
  }, alt = "Q-Q plot: the quantiles of each fitted distribution against those of the claim sizes")

  # the probabilities of each fitted distribution against those of the claims: where the Q-Q
  # plot shows the tail, the P-P plot shows the body of the distribution
  add_chart("sev_pp_plot", "execute_sev_analysis", "claim size", function(in_dark) {
    models <- model_cdfs(sev_models())
    pp <- dft_pp_data(severity_data(), models$cdfs, if (sev_has_censoring()) sev_km())
    series <- c(
      list(list(x = c(0, 1), y = c(0, 1), name = "Perfect fit", colour = "muted", lwd = 1.4)),
      lapply(seq_along(pp$fitted), function(i) {
        list(x = pp$empirical, y = pp$fitted[[i]], name = names(pp$fitted)[i], colour = models$colours[i])
      })
    )
    netsimr_line_chart(series, if (sev_has_censoring()) "Kaplan-Meier probability" else "Empirical probability",
                   "Fitted probability", in_dark, ylim = c(0, 1.02))
  }, alt = "P-P plot: the cumulative probabilities of each fitted distribution against those of the claim sizes")

  # the conditional tail expectations: the mean claim above each percentile, of the claims and
  # of each fit
  sev_cte <- function() {
    x <- severity_data()
    levels <- dft_cte_levels
    censoring <- sev_has_censoring()
    cells <- function(v) lapply(v, function(value) if (is.infinite(value)) "Infinite" else netsimr_fmt(value))
    empirical <- if (censoring) rep(NA_real_, length(levels)) else dft_empirical_cte(x, levels)
    rows <- c(list(c(list("Claims"), cells(empirical))),
              lapply(sev_models(), function(m) c(list(m$name), cells(dft_model_cte(m, levels, x)))))
    largest <- round(length(x) * (1 - max(levels)), 9)
    d <- sev_threshold()
    percent <- function(v) paste0(formatC(100 * v, format = "fg"), "%")
    shares <- percent(1 - levels)
    list(
      header = c("Distribution", paste("CTE", percent(levels))), rows = rows,
      note = paste0(
        "The conditional tail expectation (CTE, also TVaR or expected shortfall) at a level is the mean claim above ",
        "that percentile", if (d > 0) paste0(", of the claims above the reporting threshold of ", netsimr_fmt(d)), ". ",
        if (censoring) paste0("The claims at their policy limits hide the sizes of the largest losses, so the claims give ",
                              "no CTE: the fits estimate it, from the limits as well as the claims.") else
          paste0("For the claims it is the mean of the largest ", paste(shares[-length(shares)], collapse = ", "), " and ",
                 shares[length(shares)], " of them; at ", percent(max(levels)), " it rests on ",
                 if (largest < 1) "less than one claim, so it is the largest claim" else
                   paste(netsimr_fmt(largest, 3), if (largest == 1) "claim" else "claims"),
                 if (largest < 20) ", too few for a reliable estimate, where the fits help." else ".")
      )
    )
  }

  output$sev_cte_table <- renderUI({
    need_run("execute_sev_analysis", "claim size")
    do.call(netsimr_html_table, sev_cte())
  })

  # the Hill plot: the tail index of the largest claims, by how many of them are used
  add_chart("sev_hill_plot", "execute_sev_analysis", "claim size", function(in_dark) {
    validate(need(!sev_has_censoring(),
                  "The Hill plot needs the sizes of the largest claims, which the policy limits hide."))
    h <- tryCatch(dft_hill_data(severity_data()), error = function(e) NULL)
    validate(need(!is.null(h), "Too few claims of different sizes for a Hill plot."))
    series <- list(
      list(x = h$k, y = h$lower, name = "95% band", colour = "muted", lwd = 1.2),
      list(x = h$k, y = h$upper, name = "95% band", colour = "muted", lwd = 1.2),
      list(x = h$k, y = h$alpha, name = "Tail index (alpha)", colour = netsimr_palette[1])
    )
    # (the band of the first few claims is far wider than the rest of the chart)
    settled <- h$k >= min(max(h$k), 15)
    netsimr_line_chart(series, "Number of largest claims used", "Tail index (alpha)", in_dark,
                   ylim = c(0, 1.05 * max(h$upper[settled])))
  }, alt = "Hill plot: the tail index of the largest claims against the number of claims used")

  # what the Hill plot shows, with its estimates from three shares of the claims; NULL without a plot
  sev_hill_text <- function() {
    if (sev_has_censoring()) return(NULL)
    h <- tryCatch(dft_hill_data(severity_data()), error = function(e) NULL)
    if (is.null(h)) return(NULL)
    at <- function(share) {
      i <- which.min(abs(h$k - share * length(severity_data())))
      paste0(netsimr_fmt(h$alpha[i], 3), " from the largest ", format(h$k[i], big.mark = ",", scientific = FALSE),
             " claims (above ", netsimr_fmt(h$threshold[i]), ")")
    }
    paste0("The Hill estimate of the tail index is the alpha of a Pareto fitted to the largest claims: ", at(0.05), ", ",
           at(0.1), " and ", at(0.25), ". Where the line is level, the claims above that point follow a Pareto with that ",
           "alpha, and the point is a threshold for the Sliced severity and Piecewise Pareto tabs; a line that keeps ",
           "rising to the right is a tail lighter than a Pareto. An alpha of 1 or less is a tail without a mean.")
  }

  output$sev_hill_note <- renderUI({
    req(isTruthy(input$execute_sev_analysis), has_data())
    p(class = "dft-table-note", req(sev_hill_text()))
  })

  add_chart("sev_fit_plot", "execute_sev_analysis", "claim size", function(in_dark) {
    log_scale <- isTRUE(input$sev_fit_log_scale)
    models <- model_cdfs(sev_models())
    d <- dft_severity_cdf_data(severity_data(), models$cdfs, log_scale)
    if (sev_has_censoring()) {
      # the Kaplan-Meier estimate in place of the empirical cdf, which would count each claim at
      # its limit as a claim of that size
      km <- sev_km()
      d$x <- km$x
      d$empirical <- km$cdf
    }
    netsimr_line_chart(dft_cdf_series(d, models$colours), "Claim size", "Cumulative probability", in_dark,
                   x_log = log_scale, ylim = c(0, 1.02))
  }, alt = "Empirical and fitted cumulative distributions of the claim sizes")

  ######################
  #Sliced Sev analysis
  ######################

  sliced_input <- eventReactive(input$execute_sliced_sev_analysis, claim_sizes(input$sliced_sev_var))
  sliced_sev_data <- reactive(sliced_input()$claims)

  # the range of the sliders: from the smallest claim to the 99.5th percentile (dft_slider_top()),
  # widened to hold a typed point beyond it
  slicing_range <- reactiveVal(NULL)

  # the sliders cover the claims up to the 99.5th percentile, not the largest claim, which on a
  # heavy tail left the body of the distribution in less than a pixel of the bar; they start at
  # the 75th and 95th percentiles
  observeEvent(sliced_input(), {
    x <- sliced_sev_data()
    lowest <- dft_nice_bound(min(x))
    highest <- dft_slider_top(x)
    # (nearly every claim the smallest: the percentile is the smallest claim too)
    if (highest <= lowest) highest <- dft_nice_bound(max(x), up = TRUE)
    left <- signif(quantile(x, 0.75, names = FALSE), 3)
    right <- signif(quantile(x, 0.95, names = FALSE), 3)
    if (right <= left) right <- signif((left + highest) / 2, 3)
    # the points of a loaded settings file, when they lie among these claims
    loaded <- pending$slicing
    pending$slicing <- NULL
    if (!is.null(loaded) && loaded[1] > min(x) && loaded[2] < max(x)) {
      left <- loaded[1]
      right <- loaded[2]
      lowest <- min(lowest, dft_nice_bound(left))
      highest <- max(highest, dft_nice_bound(right, up = TRUE))
    }
    # a fine step, so that claims below 1 can still be sliced precisely
    step <- signif((highest - lowest) / 1000, 1)
    slicing_range(c(lowest, highest))
    updateSliderInput(session, "slicing_point_left", min = lowest, max = highest, value = left, step = step)
    updateSliderInput(session, "slicing_point_right", min = lowest, max = highest, value = right, step = step)
  })

  # a slicing point can also be typed, in the box beside its slider: the slider follows the
  # typed value, and the range of both sliders grows to hold a value beyond it, so that the
  # point is fitted wherever it lies (a slider clamps a value beyond its range); the box
  # follows the slider. Neither update comes back: the browser sends a value only when it changes
  for (id in c("slicing_point_left", "slicing_point_right")) local({
    slider <- id
    typed <- paste0(id, "_typed")
    observeEvent(input[[slider]], updateNumericInput(session, typed, value = input[[slider]]))
    observeEvent(input[[typed]], {
      value <- input[[typed]]
      if (!isTRUE(is.finite(value)) || isTRUE(value == input[[slider]])) return()
      range <- slicing_range()
      if (!is.null(range) && (value < range[1] || value > range[2])) {
        range <- c(min(range[1], dft_nice_bound(value)), max(range[2], dft_nice_bound(value, up = TRUE)))
        slicing_range(range)
        for (other in setdiff(c("slicing_point_left", "slicing_point_right"), slider)) {
          updateSliderInput(session, other, min = range[1], max = range[2])
        }
      }
      updateSliderInput(session, slider, value = value, min = range[1], max = range[2])
    })
  })

  # moving the first point past the second moves the second above it
  observeEvent(input$slicing_point_left, {
    range <- req(slicing_range())
    if (isTRUE(input$slicing_point_right <= input$slicing_point_left)) {
      updateSliderInput(session, "slicing_point_right", value = signif((input$slicing_point_left + range[2]) / 2, 3))
    }
  }, ignoreInit = TRUE)

  slicing_points <- reactive({
    x <- sliced_sev_data()
    x1 <- req(input$slicing_point_left)
    x2 <- req(input$slicing_point_right)
    below <- x[x <= x1]
    validate(
      need(x2 > x1, "The second slicing point must be above the first."),
      need(length(unique(below)) >= 2, "Move the first slicing point up: at least two different claims must lie below it."),
      need(sum(x > x1 & x <= x2) >= 3, "Move the slicing points apart: at least three claims must lie between them."),
      need(any(x > x2), "Move the second slicing point down: some claims must lie above it.")
    )
    c(x1, x2)
  })

  slc_sev_lnorm_fit <- reactive(fit_lnorm_mle(sliced_sev_data()))

  # least squares fit of the lognormal body to the empirical cdf below the first slicing point
  slc_sev_censored_lnorm_fit <- reactive({
    x1 <- slicing_points()[1]
    tryCatch({
      w <- sliced_sev_data()[sliced_sev_data() <= x1]
      dat <- data.frame(severity = w, empirical = rank(w) / (length(sliced_sev_data()) + 1))
      SumOfSquares <- function(data, par) {
        sum((plnorm(data$severity, meanlog = par[1], sdlog = par[2]) - data$empirical)^2)
      }
      # meanlog can be negative (claims below 1); only sdlog must be positive
      result <- optim(par = c(mean(log(w)), sd(log(w))), SumOfSquares, data = dat,
                      lower = c(-Inf, 0.0001), method = "L-BFGS-B")
      if (result$convergence != 0) stop("the optimisation did not converge")
      result
    }, error = function(e) {
      failed_fit("Sliced LogNormal fit", e)
      list(par = rep(NA_real_, 2))
    })
  })

  zGreaterThanXm1 <- reactive(sliced_sev_data()[sliced_sev_data() > slicing_points()[1]])
  zGreaterThanXm2 <- reactive(sliced_sev_data()[sliced_sev_data() > slicing_points()[2]])
  paretoX1Alpha <- reactive(length(zGreaterThanXm1()) / sum(log(zGreaterThanXm1() / slicing_points()[1])))
  paretoX2AlphaMod <- reactive({
    # (the points are checked first: a message about them is not a failed fit)
    points <- slicing_points()
    tryCatch(
      fit_slice_pareto(sliced_sev_data(), points[1], points[2]),
      error = function(e) {
        failed_fit("Sliced Pareto fit", e)
        NA_real_
      }
    )
  })
  paretoX2Alpha <- reactive(length(zGreaterThanXm2()) / sum(log(zGreaterThanXm2() / slicing_points()[2])))

  # cdfs of the LogNormal - Pareto and LogNormal - Pareto - Pareto models
  sliced_cdfs <- reactive({
    x1 <- slicing_points()[1]
    x2 <- slicing_points()[2]
    body <- slc_sev_censored_lnorm_fit()$par
    a1 <- paretoX1Alpha()
    a2_lower <- paretoX2AlphaMod()
    a2_upper <- paretoX2Alpha()
    p1 <- plnorm(x1, meanlog = body[1], sdlog = body[2])
    p2 <- (1 - (x1 / x2)^a2_lower) * (1 - p1) + p1
    list(
      one = function(q) {
        ifelse(q <= x1, plnorm(q, meanlog = body[1], sdlog = body[2]), (1 - (x1 / q)^a1) * (1 - p1) + p1)
      },
      two = function(q) {
        ifelse(q <= x2,
               ifelse(q <= x1, plnorm(q, meanlog = body[1], sdlog = body[2]), (1 - (x1 / q)^a2_lower) * (1 - p1) + p1),
               (1 - (x2 / q)^a2_upper) * (1 - p2) + p2)
      }
    )
  })

  sliced_table <- function() {
    x <- sliced_sev_data()
    points <- slicing_points()
    whole <- unname(slc_sev_lnorm_fit()$estimate)
    body <- slc_sev_censored_lnorm_fit()$par
    cdfs <- sliced_cdfs()
    ks <- c(ks_distance(x, function(q) plnorm(q, whole[1], whole[2])), ks_distance(x, cdfs$one), ks_distance(x, cdfs$two))
    blank <- ""
    rows <- list(
      list("LogNormal", "Whole distribution", dft_param_cell("mu", whole[1]), dft_param_cell("sigma", whole[2]), netsimr_fmt(ks[1], 3)),
      list("LogNormal - Pareto", "Body below the first point", dft_param_cell("mu", body[1]), dft_param_cell("sigma", body[2]), netsimr_fmt(ks[2], 3)),
      list(blank, "Tail above the first point", dft_param_cell("xm", points[1]), dft_param_cell("alpha", paretoX1Alpha()), blank),
      list("LogNormal - Pareto - Pareto", "Body below the first point", dft_param_cell("mu", body[1]), dft_param_cell("sigma", body[2]), netsimr_fmt(ks[3], 3)),
      list(blank, "Layer between the points", dft_param_cell("xm", points[1]), dft_param_cell("alpha", paretoX2AlphaMod()), blank),
      list(blank, "Tail above the second point", dft_param_cell("xm", points[2]), dft_param_cell("alpha", paretoX2Alpha()), blank)
    )
    list(
      header = c("Model", "Part", "Parameter 1", "Parameter 2", "K-S distance"), rows = rows,
      best = if (any(is.finite(ks))) c(1, 2, 4)[which.min(ks)], best_label = "Closest",
      note = paste("The body and the alpha of the layer between the points are fitted to the empirical cdf by least squares;",
                   "the alphas of the tails above each point are maximum likelihood estimates.")
    )
  }

  output$slc_sev_fitted_param_summary <- renderUI({
    need_run("execute_sliced_sev_analysis", "claim size")
    do.call(netsimr_html_table, sliced_table())
  })

  add_chart("sliced_sev_cdf_plot", "execute_sliced_sev_analysis", "claim size", function(in_dark) {
    points <- slicing_points()
    log_scale <- isTRUE(input$sev_cens_fit_log_scale)
    whole <- unname(slc_sev_lnorm_fit()$estimate)
    cdfs <- sliced_cdfs()
    models <- list(LogNormal = function(q) plnorm(q, whole[1], whole[2]),
                   "LogNormal - Pareto" = cdfs$one, "LogNormal - Pareto - Pareto" = cdfs$two)
    d <- dft_severity_cdf_data(sliced_sev_data(), models, log_scale, extra_points = points)
    netsimr_line_chart(dft_cdf_series(d), "Claim size", "Cumulative probability", in_dark,
                   x_log = log_scale, ylim = c(0, 1.02), vlines = points)
  }, alt = "Empirical and fitted cumulative distributions of the claim sizes, with the slicing points")

  add_chart("mean_excess_func_plot", "execute_sliced_sev_analysis", "claim size", function(in_dark) {
    d <- dft_mean_excess_data(sliced_sev_data())
    # the largest claim has no mean excess, so two different claims leave a single point
    validate(need(length(d$x) >= 2, "The mean excess chart needs at least three different claim sizes."))
    points_ok <- tryCatch(slicing_points(), error = function(e) NULL)
    netsimr_line_chart(list(list(name = "Mean excess", x = d$x, y = d$y, colour = "bar")), "Threshold",
                   "Mean excess over the threshold", in_dark, x_log = isTRUE(input$sev_cens_fit_log_scale), vlines = points_ok)
  }, alt = "Empirical mean excess function of the claim sizes, with the slicing points")

  ######################
  #piecewise pareto
  ######################

  piecewise_input <- eventReactive(input$execute_piecewise_sev_analysis, claim_sizes(input$piecewise_pareto_var))
  piecewise_sev_data <- reactive(sort(piecewise_input()$claims))

  # one slider per threshold, from the second smallest distinct claim to the 99.5th percentile
  # (dft_slider_top(); at least the third distinct claim, and at most the second largest); a
  # threshold already set is kept when it is still in range
  dynamic_sliders <- eventReactive(list(input$num_pareto_slices, input$execute_piecewise_sev_analysis), {
    sorted_claims <- piecewise_sev_data()
    distinct <- unique(sorted_claims)
    validate(need(length(distinct) >= 4, "The thresholds need at least four different claim sizes."))
    min_val <- distinct[2]
    max_val <- min(distinct[length(distinct) - 1], max(dft_slider_top(sorted_claims), distinct[3]))
    num_sliders <- or_default(input$num_pareto_slices, 1)
    step <- signif((max_val - min_val) / 1000, 1)
    # the thresholds of a loaded settings file, until an analysis run after the load has shown them
    loaded <- pending$thresholds
    if (!is.null(loaded) && !identical(input$execute_piecewise_sev_analysis, loaded$run)) pending$thresholds <- NULL
    tagList(lapply(seq_len(num_sliders), function(i) {
      id <- paste0("slider_", i)
      current <- isolate(input[[id]])
      if (!is.null(loaded) && i <= length(loaded$values)) current <- loaded$values[i]
      # the 50th, 75th, 87.5th, ... percentiles: thresholds get closer together towards the tail
      default <- quantile(sorted_claims, 1 - 0.5^i, names = FALSE)
      value <- if (!is.null(current) && current >= min_val && current <= max_val) current else min(max(signif(default, 4), min_val), max_val)
      # without tick labels: at the width of the settings card, the labels of a range from a
      # small claim to a large one overlap
      sliderInput(id, label = paste("Threshold", i), min = min_val, max = max_val, value = value,
                  step = if (step > 0) step else NULL, ticks = FALSE)
    }))
  })

  output$pareto_slider_inputs <- renderUI({
    dynamic_sliders()
  })

  piecwise_pareto_mu <- reactive({
    # nothing to do until the analysis has been executed
    lowest_claim <- min(piecewise_sev_data())
    num_sliders <- req(input$num_pareto_slices)
    # sliders that are not rendered yet are NULL and are dropped by unlist()
    slider_values <- unlist(lapply(seq_len(num_sliders), function(i) input[[paste0("slider_", i)]]))
    sort(c(lowest_claim, slider_values))
  })

  piecwise_pareto_alpha <- reactive({
    piecewise_pareto_alpha(piecewise_sev_data(), piecwise_pareto_mu())
  })

  # the thresholds must be usable before the fit is shown
  piecewise_ready <- reactive({
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    validate(
      need(length(mu) == or_default(input$num_pareto_slices, 1) + 1, "Setting up the thresholds..."),
      need(!anyDuplicated(mu), "The thresholds must all be different and above the smallest claim."),
      need(max(mu) < max(x), "The largest threshold must be below the largest claim.")
    )
    TRUE
  })

  empirical_piecwise_cdf <- reactive(seq_along(piecewise_sev_data()) / length(piecewise_sev_data()))
  predicted_piecwise_cdf <- reactive(piecewise_pareto_cdf(piecewise_sev_data(), piecwise_pareto_mu(), piecwise_pareto_alpha()))

  piecewise_table <- function() {
    piecewise_ready()
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    upper <- c(mu[-1], Inf)
    rows <- lapply(seq_along(mu), function(i) {
      list(paste("Layer", i), netsimr_fmt(mu[i]), if (is.finite(upper[i])) netsimr_fmt(upper[i]) else "and above",
           format(sum(x >= mu[i] & x < upper[i]), big.mark = ",", scientific = FALSE), netsimr_fmt(alpha[i]))
    })
    list(header = c("Layer", "From", "To", "Claims", "Alpha"), rows = rows,
         note = paste("Each layer holds the claims from its threshold up to the next one; layer 1 starts at,",
                      "and includes, the smallest claim. Each alpha is the maximum likelihood estimate for the claims in its layer."))
  }

  output$fitted_sliced_pareto <- renderUI({
    need_run("execute_piecewise_sev_analysis", "claim size")
    do.call(netsimr_html_table, piecewise_table())
  })

  output$piecewise_pareto_ks_test <- renderUI({
    need_run("execute_piecewise_sev_analysis", "claim size")
    piecewise_ready()
    x <- piecewise_sev_data()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    div(
      class = "dft-stats",
      netsimr_stat_tile("Claims", format(length(x), big.mark = ",", scientific = FALSE), note = rows_left_out_note(piecewise_input()$dropped), icon_name = "hashtag"),
      netsimr_stat_tile("Layers", length(mu), icon_name = "layer-group"),
      netsimr_stat_tile("K-S distance", netsimr_fmt(ks_distance(x, function(q) piecewise_pareto_cdf(q, mu, alpha)), 3),
                    note = "Largest gap between the empirical and fitted cdf", icon_name = "ruler", accent = TRUE)
    )
  })

  # the summary tiles are hidden while empty, and shiny does not render hidden outputs,
  # so they must render even when hidden or they would never appear
  for (id in c("data_overview", "freq_stats", "sev_stats", "piecewise_pareto_ks_test")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  add_chart("piecewise_pareto_cdf_plot", "execute_piecewise_sev_analysis", "claim size", function(in_dark) {
    piecewise_ready()
    mu <- piecwise_pareto_mu()
    alpha <- piecwise_pareto_alpha()
    log_scale <- isTRUE(input$piecewise_pareto_fit_log_scale)
    d <- dft_severity_cdf_data(piecewise_sev_data(), list("Piecewise Pareto" = function(q) piecewise_pareto_cdf(q, mu, alpha)),
                               log_scale, extra_points = mu)
    netsimr_line_chart(dft_cdf_series(d), "Claim size", "Cumulative probability", in_dark,
                   x_log = log_scale, ylim = c(0, 1.02), vlines = mu)
  }, alt = "Empirical and fitted piecewise Pareto cumulative distributions of the claim sizes, with the thresholds")

  ######################
  #to the simulator
  ######################

  # the run of a model's tab, with a message naming the tab when it has not been run
  need_tab_run <- function(button, tab, model) {
    validate(
      need(has_data(), "Upload a data file on the Data tab first, or choose an example."),
      need(isTruthy(input[[button]]), paste0("Run the analysis on the ", tab, " tab to send its ", model, " fit."))
    )
  }

  # the frequency chosen on the tab, as dft_simulator_settings() takes it; NULL to leave the simulator's
  export_frequency <- reactive({
    model <- or_default(input$export_frequency, "none")
    if (identical(model, "none")) return(NULL)
    need_tab_run("execute_freq_analysis", "Frequency", model)
    fit <- freq_fits()[[model]]
    validate(need(!is.null(fit), paste("The", model, "fit failed on the Frequency tab.")))
    poisson <- model %in% c("Poisson", "Zero-inflated Poisson")
    if (startsWith(model, "Zero-inflated")) {
      validate(need(!identical(fit$inflated, FALSE),
                    paste("The counts have no extra zeros: send the", if (poisson) "Poisson" else "Negative Binomial", "instead.")))
    }
    if (poisson) return(c(list(model = model, lambda = fit_value(fit, "lambda")),
                          if (model != "Poisson") list(zero = fit_value(fit, "zero"))))
    validate(
      # (its size is at the cap, a Poisson in all but name)
      need(!isTRUE(fit$capped), paste("The counts are not overdispersed, so the Negative Binomial has no finite size:",
                                      "send the", if (model == "Negative Binomial") "Poisson" else "zero-inflated Poisson", "instead."))
    )
    c(list(model = model, size = fit_value(fit, "size"), mu = fit_value(fit, "mu")),
      if (model != "Negative Binomial") list(zero = fit_value(fit, "zero")))
  })

  # the severity chosen on the tab, with its slices; NULL to leave the simulator's
  export_severity <- reactive({
    model <- or_default(input$export_severity, "none")
    if (identical(model, "none")) return(NULL)
    tab <- dft_export_severity_models[[model]]
    if (tab == "severity") {
      need_tab_run("execute_sev_analysis", "Severity", model)
      fit <- sev_models()[[match(model, c("Normal", "LogNormal", "Exponential", "Gamma", "Pareto", "Weibull", "Burr",
                                          "Generalised Pareto"))]]
      p <- fit$params
      validate(need(!anyNA(p), paste("The", model, "fit failed on the Severity tab.")))
      # above a reporting threshold, the share of all claims that exceed it
      threshold <- list(threshold = sev_threshold(),
                        share = if (sev_threshold() > 0) fit$survival_at_threshold else 1)
      return(c(threshold, switch(model,
        "Normal" = list(model = model, mean = p[["Mean"]], sd = p[["Std. dev."]]),
        "LogNormal" = list(model = model, mu = p[["mu"]], sigma = p[["sigma"]]),
        "Exponential" = list(model = model, mean = p[["Mean"]]),
        "Gamma" = list(model = model, shape = p[["Shape"]], scale = p[["Scale"]]),
        "Pareto" = list(model = model, xm = p[["xm"]], alpha = p[["alpha"]]),
        "Weibull" = list(model = model, shape = p[["Shape"]], scale = p[["Scale"]]),
        "Burr" = list(model = model, alpha = p[["alpha"]], gamma = p[["gamma"]], scale = p[["Scale"]]),
        "Generalised Pareto" = list(model = model, location = p[["Location"]], scale = p[["Scale"]], shape = p[["Shape"]])
      )))
    }
    if (tab == "sliced") {
      need_tab_run("execute_sliced_sev_analysis", "Sliced severity", model)
      points <- slicing_points()
      body <- slc_sev_censored_lnorm_fit()$par
      validate(need(!anyNA(body), "The LogNormal body failed to fit on the Sliced severity tab."))
      slices <- if (model == "LogNormal - Pareto") {
        data.frame(x_m = points[1], alpha = paretoX1Alpha())
      } else {
        data.frame(x_m = points, alpha = c(paretoX2AlphaMod(), paretoX2Alpha()))
      }
      return(list(model = "LogNormal", mu = body[1], sigma = body[2], slices = slices))
    }
    need_tab_run("execute_piecewise_sev_analysis", "Piecewise Pareto", model)
    piecewise_ready()
    dft_piecewise_severity(piecwise_pareto_mu(), piecwise_pareto_alpha())
  })

  # the share of all claims that the counts count: below 1 for a severity fitted above a reporting
  # threshold, with counts of the claims above it (the simulator draws claims from zero)
  export_share <- reactive({
    severity <- export_severity()
    if (is.null(severity) || is.null(severity$share) || severity$share >= 1) return(1)
    if (isFALSE(input$export_counts_above_threshold)) 1 else severity$share
  })

  # the settings the file holds
  export_values <- reactive({
    frequency <- export_frequency()
    severity <- export_severity()
    validate(need(!is.null(frequency) || !is.null(severity), "Choose a frequency or a severity to send."))
    values <- tryCatch(dft_simulator_settings(frequency, severity, frequency_share = if (is.null(frequency)) 1 else export_share()),
                       error = function(e) conditionMessage(e))
    validate(need(is.list(values), values))
    values
  })

  # with a severity fitted above a reporting threshold: whether the counts are of those claims
  output$export_threshold_option <- renderUI({
    severity <- tryCatch(export_severity(), error = function(e) NULL)
    if (is.null(severity) || is.null(severity$share) || severity$share >= 1) return(NULL)
    current <- isolate(input$export_counts_above_threshold)
    tagList(
      bslib::input_switch("export_counts_above_threshold", "The claim counts are of the claims above the threshold",
                          value = !isFALSE(current)),
      netsimr_help(paste0("The severity is fitted as claims from zero, of which ", netsimr_fmt(100 * severity$share, 3),
                      "% exceed the reporting threshold of ", netsimr_fmt(severity$threshold), ". The simulator draws claims ",
                      "from zero, so counts of the claims above the threshold are scaled up to all claims."))
    )
  })

  output$export_summary <- renderUI({
    values <- export_values()
    severity <- export_severity()
    share <- export_share()
    scaled <- !is.null(export_frequency()) && share < 1
    unscaled_note <- !is.null(severity) && !is.null(severity$share) && severity$share < 1 && !scaled
    netsimr_html_table(c("Setting", "Value"), dft_simulator_settings_rows(values), align = c("left", "right"),
                   note = paste(c(
                     "Parameters are sent in full; the table rounds them to six significant figures.",
                     if (scaled) paste0("The frequency is of all claims from zero: the fitted counts of the claims above ",
                                        "the threshold divided by ", netsimr_fmt(share, 4), ", the share of claims above it."),
                     if (unscaled_note) paste0("The severity is of claims from zero, of which ", netsimr_fmt(100 * severity$share, 3),
                                               "% exceed the threshold: the simulator's frequency must count all of them.")
                   ), collapse = " "))
  })

  # the save button, shown once there is something to save
  output$export_download <- renderUI({
    ready <- tryCatch(is.list(export_values()), error = function(e) FALSE)
    if (!ready) return(NULL)
    downloadButton("export_settings", "Save simulator settings", icon = icon("download"), class = "btn-run")
  })

  output$export_settings <- downloadHandler(
    filename = function() "fitted_claims_simulator_settings.txt",
    content = function(file) {
      write_settings_file(export_values(), file, tool = sim_settings_tool, version = sim_settings_version)
    }
  )

  ######################
  #settings file
  ######################

  # the choices of the session: the data options, each tab's columns and options, the slicing
  # points and thresholds, and the models chosen on the Export tab (not the data file itself)
  output$save_settings <- downloadHandler(
    filename = function() "distribution_fitting_settings.txt",
    content = function(file) {
      values <- collect_setting_values(input, dft_settings_inputs)
      points <- c(input$slicing_point_left, input$slicing_point_right)
      if (!is.null(slicing_range()) && length(points) == 2) values$slicing_points <- points
      n <- or_default(input$num_pareto_slices, 0)
      thresholds <- unlist(lapply(seq_len(n), function(i) input[[paste0("slider_", i)]]))
      if (n > 0 && length(thresholds) == n) values$piecewise_thresholds <- thresholds
      write_settings_file(values, file, dft_settings_tool, dft_settings_version)
    }
  )

  observeEvent(input$load_settings, {
    v <- load_settings_values(input$load_settings$datapath, dft_settings_tool, dft_settings_version)
    if (is.null(v)) return()
    # the choices of the radio buttons and selects, as the page offers them
    choices <- tool_input_choices("distribution", distribution_fitting_tool_UI)
    ok <- Filter(function(id) setting_usable(v[[id]], dft_settings_inputs[[id]], choices[[id]]), names(dft_settings_inputs))
    for (id in ok) update_setting_input(session, id, dft_settings_inputs[[id]], v[[id]])

    # the data the settings read: a change of example, or of the options a file is read with,
    # reads the data again, and its columns are chosen then
    now <- function(id) if (id %in% ok) v[[id]] else input[[id]]
    source <- or_default(now("data_source"), "upload")
    data_changes <- if (identical(source, "example")) {
      !identical(source, input$data_source) || !identical(now("example"), input$example)
    } else {
      !identical(source, input$data_source) || is.null(input$file1) ||
        !all(vapply(c("data_includes_header", "sep", "dec", "quote"), function(id) identical(now(id), input[[id]]), logical(1)))
    }
    columns <- v[intersect(ok, names(dft_settings_inputs)[dft_settings_inputs == "column"])]
    present <- if (!data_changes) tryCatch(names(data()), error = function(e) NULL)
    if (!data_changes && !is.null(present)) {
      for (id in names(columns)) if (columns[[id]] %in% present) updateSelectizeInput(session, id, selected = columns[[id]])
      pending$columns <- list()
    } else {
      pending$columns <- columns
    }

    # the slicing points and thresholds are set when their analysis runs next
    points <- v$slicing_points
    pending$slicing <- if (is.numeric(points) && length(points) == 2 && all(is.finite(points)) && points[1] < points[2]) points
    thresholds <- v$piecewise_thresholds
    pending$thresholds <- if (is.numeric(thresholds) && length(thresholds) > 0 && all(is.finite(thresholds))) {
      list(values = thresholds, run = isolate(input$execute_piecewise_sev_analysis))
    }

    showNotification(paste0("Loaded the settings of ", input$load_settings$name, ".",
                            if (identical(source, "upload") && is.null(input$file1)) " Upload the data file to apply its columns.",
                            " Click Run analysis on each tab to fit with them."),
                     type = "message", duration = 8)
  })

  ######################
  #report
  ######################

  analyses <- c(frequency = "execute_freq_analysis", severity = "execute_sev_analysis",
                sliced = "execute_sliced_sev_analysis", piecewise = "execute_piecewise_sev_analysis")
  ran <- function(analysis) has_data() && isTruthy(input[[analyses[[analysis]]]])

  output$report_download <- renderUI({
    if (!any(vapply(names(analyses), ran, logical(1)))) {
      return(netsimr_help("Run an analysis on its tab, and its fits can be saved here as a report."))
    }
    div(class = "dft-downloads", downloadButton("download_report", "Report (HTML)", class = "btn-outline-dft"))
  })

  # what the report shows: each analysis run, as on its tab; an analysis that cannot be shown
  # (its fit failed, or its points are not usable) is left out
  report_data <- function() {
    figure <- function(id, title, note = NULL) c(charts[[id]], list(title = title, note = note))
    count <- function(n) format(n, big.mark = ",", scientific = FALSE)
    left_out <- function(n) if (n > 0) count(n)
    df <- quiet(data())
    r <- list(source = quiet(data_name()), rows = if (!is.null(df)) nrow(df), columns = if (!is.null(df)) ncol(df))

    if (ran("frequency")) r$frequency <- quiet({
      x <- freq_input()
      m <- freq_moments()
      suggested <- freq_suggested()
      list(
        settings = list(`Claim counts` = x$column, Weights = if (x$weighted) x$weights_column else "None",
                        `Rows left out` = left_out(x$dropped)),
        headline = list(label = "Suggested frequency", value = suggested$model, note = suggested$note),
        tiles = c(stats::setNames(list(netsimr_fmt_count(m$n)), if (x$weighted) "Weighted observations" else "Observations"),
                  list(Mean = netsimr_fmt(m$mean), Variance = netsimr_fmt(m$variance),
                       `Variance over mean` = if (m$mean > 0) netsimr_fmt(m$variance / m$mean, 3) else netsimr_dash)),
        table = freq_table(),
        charts = list(figure("freq_fit_plot", "Cumulative distribution"), figure("count_hist", "Histogram of the counts"))
      )
    })

    if (ran("severity")) r$severity <- quiet({
      s <- sev_input()
      x <- s$claims
      table <- sev_table()
      hill <- quiet(sev_hill_text())
      list(
        settings = list(`Claim sizes` = s$column, `Reporting threshold` = if (s$threshold > 0) netsimr_fmt(s$threshold) else "None",
                        `Policy limits` = s$limits, `Claims at their limits` = if (any(s$censored)) count(sum(s$censored)),
                        `Rows left out` = left_out(s$dropped)),
        headline = if (!is.null(table$best)) list(label = "Closest severity", value = table$rows[[table$best]][[1]],
                                                  note = "Smallest K-S distance"),
        tiles = list(Claims = count(length(x)), Mean = netsimr_fmt(mean(x)), Median = netsimr_fmt(median(x)),
                     `Coefficient of variation` = netsimr_fmt(sd(x) / mean(x), 3), `Largest claim` = netsimr_fmt(max(x))),
        table = table,
        # (the cumulative distributions across the page: their legend names eight models)
        charts = list(c(figure("sev_fit_plot", "Cumulative distribution"), wide = TRUE),
                      figure("sev_hist", "Histogram of the claim sizes"), figure("sev_qq_plot", "Q-Q plot"),
                      figure("sev_pp_plot", "P-P plot")),
        cte = sev_cte(),
        hill = if (!is.null(hill)) figure("sev_hill_plot", "Hill plot", hill)
      )
    })

    if (ran("sliced")) r$sliced <- quiet({
      s <- sliced_input()
      x <- s$claims
      points <- slicing_points()
      table <- sliced_table()
      list(
        settings = list(`Claim sizes` = s$column, `First slicing point` = netsimr_fmt(points[1]),
                        `Second slicing point` = netsimr_fmt(points[2]), `Rows left out` = left_out(s$dropped)),
        headline = if (!is.null(table$best)) list(label = "Closest sliced model", value = table$rows[[table$best]][[1]],
                                                  note = "Smallest K-S distance"),
        tiles = list(Claims = count(length(x)), `Above the first point` = count(sum(x > points[1])),
                     `Above the second point` = count(sum(x > points[2]))),
        table = table, text_cols = 1:2,
        charts = list(figure("sliced_sev_cdf_plot", "Cumulative distribution"), figure("mean_excess_func_plot", "Mean excess function"))
      )
    })

    if (ran("piecewise")) r$piecewise <- quiet({
      piecewise_ready()
      x <- piecewise_sev_data()
      mu <- piecwise_pareto_mu()
      ks <- netsimr_fmt(ks_distance(x, function(q) piecewise_pareto_cdf(q, mu, piecwise_pareto_alpha())), 3)
      list(
        settings = list(`Claim sizes` = piecewise_input()$column, Thresholds = paste(netsimr_fmt(mu[-1]), collapse = "; "),
                        `Rows left out` = left_out(piecewise_input()$dropped)),
        headline = list(label = "Piecewise Pareto", value = ks, note = "K-S distance"),
        tiles = list(Claims = count(length(x)), Layers = as.character(length(mu)), `K-S distance` = ks),
        table = piecewise_table(),
        charts = list(figure("piecewise_pareto_cdf_plot", "Cumulative distribution"))
      )
    })
    r
  }

  output$download_report <- downloadHandler(
    filename = function() "distribution_fitting_report.html",
    content = function(file) write_distribution_report(file, report_data())
  )

}
