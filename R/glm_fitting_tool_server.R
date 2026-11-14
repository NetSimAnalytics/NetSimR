#' Server function for the GLM Fitting tool application
#'
#' @description Runs a session of the GLM fitting tool: imports the data, fits
#'   the models, compares the stored ones, draws the actual against predicted
#'   chart and saves and loads the settings.
#'
#' @param input Input for the server function.
#' @param output Output for the server function.
#' @param session Session for the server function.
#' @return Called by shiny for its side effects, the outputs and observers of a
#'   session; the value is not used.
#' @keywords internal
#' @import shiny
GLMFittingToolServer <- function(input, output, session) {

  # the chart is drawn in the colours of the theme the page reports
  dark <- netsimr_dark(input)

  # the tabs, each a function of its own (below), in the order of what they use of each other
  tool <- list(dark = dark)
  tool <- c(tool, glm_server_data(input, output, session, tool))
  tool <- c(tool, glm_server_columns(input, output, session, tool))
  tool <- c(tool, glm_server_model(input, output, session, tool))
  tool <- c(tool, glm_server_stored(input, output, session, tool))
  tool <- c(tool, glm_server_downloads(input, output, session, tool))
  tool <- c(tool, glm_server_charts(input, output, session, tool))
  tool <- c(tool, glm_server_validation(input, output, session, tool))
  tool <- c(tool, glm_server_rates(input, output, session, tool))
  tool <- c(tool, glm_server_selection(input, output, session, tool))
  tool <- c(tool, glm_server_settings(input, output, session, tool))
  # every reactive of the tabs under its own name here too, where tests of the server find it
  list2env(tool, environment())
  invisible(NULL)
}

# The import data tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_data <- function(input, output, session, tool) {
  # the data last imported, and where it came from; an import that fails keeps it, so that the
  # data and a model fitted to it stay together. The imports are numbered, so that a model can
  # tell whether it was fitted to the data now imported
  imported <- reactiveVal(NULL)
  import_failed <- reactiveVal(FALSE)
  imports <- 0L
  next_import <- function() {
    imports <<- imports + 1L
    imports
  }

  import_error <- function(message) {
    kept <- !is.null(isolate(imported()))
    showModal(modalDialog(
      title = "The data could not be imported",
      message,
      if (kept) tags$p(class = "mt-2", "The data imported before is still used."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    NULL
  }

  db_port <- function(default) {
    port <- trimws(or_default(input$db_port, ""))
    if (port == "") return(default)
    value <- suppressWarnings(as.numeric(port))
    if (is.na(value) || value != round(value) || value <= 0) stop("the port must be a whole number, such as ", default)
    value
  }

  # the data of the file or query chosen on the Data tab, or NULL (with a message) when it cannot be read
  read_import <- function() {
    tryCatch({
      if (identical(input$data_source, "Example")) {
        df_input <- NetSimR::motor_policies
      } else if (identical(input$data_source, "Database")) {
        # The database driver packages are optional (Suggests): check the one
        # needed for the selected database type before trying to connect
        if (!glm_tool_db_package_available(input$db_type)) {
          return(NULL)
        }
        df_input <- switch(
          input$db_type,
          "SQLite" = {
            # connecting to a file that does not exist would create an empty database
            if (!file.exists(input$db_name)) stop("the SQLite file '", input$db_name, "' does not exist")
            glm_tool_query_dbi(RSQLite::SQLite(), input$sql_query, dbname = input$db_name)
          },
          "SQL Server" = glm_tool_query_odbc(
            # values are braced, so that a ; in them cannot add attributes to the connection string
            if (isTRUE(input$windows_auth)) {
              # Use Windows authentication
              paste0("Driver={SQL Server};Server=", glm_tool_odbc_value(input$db_host),
                     ";Database=", glm_tool_odbc_value(input$db_name), ";Trusted_Connection=yes")
            } else {
              # Use username and password
              paste0("Driver={SQL Server};Server=", glm_tool_odbc_value(input$db_host),
                     ";Database=", glm_tool_odbc_value(input$db_name),
                     ";Uid=", glm_tool_odbc_value(input$db_user), ";Pwd=", glm_tool_odbc_value(input$db_password))
            },
            input$sql_query
          ),
          # RPostgres, the current DBI driver for PostgreSQL (RPostgreSQL is its older
          # predecessor); 64-bit integer columns come back as numbers, which the models
          # take, instead of bit64's integer64
          "PostgreSQL" = glm_tool_query_dbi(
            RPostgres::Postgres(), input$sql_query,
            host = input$db_host, port = db_port(5432), dbname = input$db_name,
            user = input$db_user, password = input$db_password, bigint = "numeric"
          )
        )
      } else {
        file <- input$csv_file
        if (is.null(file)) {
          return(import_error("Choose a CSV file first."))
        }
        df_input <- netsimr_read_data(
          file$datapath,
          header = !isFALSE(input$csv_header),
          sep = or_default(input$csv_sep, ","),
          quote = or_default(input$csv_quote, "\""),
          dec = or_default(input$csv_dec, ".")
        )
        # numbers with thousands separators ("1,200") are read as text: text columns that are
        # mostly numbers become numbers, as in the distribution fitting tool
        dec <- or_default(input$csv_dec, ".")
        df_input <- netsimr_convert_number_columns(df_input, dec)
      }
      if (!is.data.frame(df_input) || ncol(df_input) == 0 || nrow(df_input) == 0) {
        return(import_error("The file or query returned no rows."))
      }
      df_input
    }, error = function(e) {
      import_error(paste0(
        "Check the settings and try again. The error was: ", conditionMessage(e)
      ))
    })
  }

  observeEvent(input$submit, {
    df_input <- read_import()
    import_failed(is.null(df_input) && !is.null(imported()))
    if (!is.null(df_input)) {
      source <- switch(or_default(input$data_source, "CSV File"), Database = input$db_type,
                       Example = "Example: motor policies", input$csv_file$name)
      imported(list(data = df_input, source = or_default(source, ""), id = next_import()))
    }
  })

  selected_data <- reactive(imported()$data)

  data_columns <- reactive(names(selected_data()))
  numeric_columns <- reactive(names(selected_data())[vapply(selected_data(), is.numeric, logical(1))])

  # the preview shows the first rows; the model uses every row
  preview_rows <- 100

  output$data_overview <- renderUI({
    df <- req(selected_data())
    div(
      class = "dft-stats",
      netsimr_stat_tile("Source", div(class = "dft-file-name", imported()$source), icon_name = "database",
                    note = if (import_failed()) "The last import failed: this is the data imported before"),
      netsimr_stat_tile("Rows", format(nrow(df), big.mark = ",", scientific = FALSE), icon_name = "bars",
                    note = if (nrow(df) > preview_rows) paste("The preview shows the first", format(preview_rows, big.mark = ",", scientific = FALSE))),
      netsimr_stat_tile("Columns", format(ncol(df), big.mark = ",", scientific = FALSE), icon_name = "table-columns",
                    note = if (ncol(df) == 1) "Only one column: check the separator" else paste(length(numeric_columns()), "numeric"))
    )
  })

  # Display the first rows of the query result or uploaded data in a table
  output$selected_input_data_table <- renderUI({
    validate(need(input$submit, "Import data to see a preview here."))
    df <- selected_data()
    validate(need(!is.null(df), "No data has been imported."))
    netsimr_data_preview(df, preview_rows)
  })

  list(imported = imported, import_failed = import_failed, imports = imports, next_import = next_import,
       import_error = import_error, db_port = db_port, read_import = read_import,
       selected_data = selected_data, data_columns = data_columns, numeric_columns = numeric_columns,
       preview_rows = preview_rows)
}

# The column choices tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_columns <- function(input, output, session, tool) {
  data_columns <- tool$data_columns
  selected_data <- tool$selected_data

  # column choices loaded from a settings file before the data they refer to was imported
  pending_columns <- reactiveVal(list())

  # the choices already made are kept when the data has their columns; otherwise a response whose
  # name suggests one and a chart variable that is not an ID (see glm_column_choices())
  observeEvent(selected_data(), {
    columns <- data_columns()
    pending <- pending_columns()
    ids <- c("response_variable", "offset", "weights", "visualize_variable")
    wanted <- stats::setNames(lapply(ids, function(id) or_default(pending[[id]], input[[id]])), ids)
    choices <- glm_column_choices(selected_data(), or_default(input$glm_distribution, "gaussian"), wanted)
    for (id in c("response_variable", "offset", "weights")) hold_spec_input(id, choices[[id]])
    updateSelectInput(session, "response_variable", choices = columns, selected = choices$response_variable)
    updateSelectInput(session, "offset", choices = c("None", columns), selected = choices$offset)
    updateSelectInput(session, "weights", choices = c("None", columns), selected = choices$weights)
    updateSelectInput(session, "visualize_variable", choices = c("None", columns), selected = choices$visualize_variable)
    pending_columns(list())
  })

  # a link to select once the family it belongs to is selected (a stored model or a settings file)
  pending_link <- reactiveVal(NULL)

  # the link the model is fitted with. The family observer chooses it and the browser echoes the
  # choice later, so a fit clicked in between would otherwise use the old family's link (the
  # identity link is valid for the Poisson family, say) while the interface shows the new one
  # (these observers run before anything that reads the fit, priority 10, so a value the browser
  # sends with the click of Fit model is the value fitted)
  chosen_link <- reactiveVal(NULL)
  observeEvent(input$link_function, chosen_link(input$link_function), priority = 10)

  # the family the model is fitted with, for the same reason: loading a stored model or a
  # settings file selects the family on the server, and a fit clicked before the browser echoes
  # it would otherwise use the family shown before the load
  chosen_family <- reactiveVal(NULL)
  observeEvent(input$glm_distribution, chosen_family(input$glm_distribution), priority = 10)

  # The rest of what the model is fitted with, likewise: a load sets the response, offset,
  # weights and formula on the server too, and a fit clicked before the browser reported them
  # combined the loaded family with the response and formula shown before (a binomial family
  # with the previous count response, which fails). A value the server sets is held here until
  # the browser reports it; a value the server sets to what is already shown is not held, as
  # the browser reports nothing for it.
  spec_inputs <- c("response_variable", "offset", "offset_log", "weights", "formula", "holdout_share", "holdout_seed",
                   "tweedie_power")
  unreported <- reactiveValues()
  for (id in spec_inputs) local({
    input_id <- id
    observeEvent(input[[input_id]], unreported[[input_id]] <- NULL, priority = 10)
  })
  hold_spec_input <- function(id, value) {
    if (id %in% spec_inputs && !identical(value, input[[id]])) unreported[[id]] <- value
  }
  spec_value <- function(id) {
    held <- unreported[[id]]
    if (is.null(held)) input[[id]] else held
  }

  # only the links that make sense for the family are offered, with the family's default selected
  observeEvent(input$glm_distribution, {
    links <- glm_family_links[[input$glm_distribution]]
    req(links)
    wanted <- pending_link()
    pending_link(NULL)
    link <- if (!is.null(wanted) && wanted %in% links) wanted else links[1]
    chosen_link(link)
    updateSelectInput(session, "link_function", choices = links, selected = link)
  }, ignoreInit = TRUE, priority = 10)

  # the columns, as buttons that add them to the formula
  output$formula_columns <- renderUI({
    columns <- setdiff(req(data_columns()), c(input$response_variable, input$offset, input$weights))
    lapply(columns, function(column) {
      tags$button(type = "button", class = "glm-chip", `data-term` = glm_term(column), column)
    })
  })

  output$formula_lhs <- renderUI({
    req(input$response_variable, input$response_variable != "")
    div(class = "glm-formula-lhs",
        paste(c(paste(glm_term(input$response_variable), "~"), glm_offset_term(input$offset, input$offset_log)), collapse = " "),
        if (!is.null(glm_offset_term(input$offset, input$offset_log))) " +", " ...")
  })

  list(pending_columns = pending_columns, pending_link = pending_link, chosen_link = chosen_link,
       chosen_family = chosen_family, spec_inputs = spec_inputs, unreported = unreported,
       hold_spec_input = hold_spec_input, spec_value = spec_value)
}

# The modelling tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_model <- function(input, output, session, tool) {
  chosen_family <- tool$chosen_family
  chosen_link <- tool$chosen_link
  imported <- tool$imported
  selected_data <- tool$selected_data
  spec_value <- tool$spec_value

  current_spec <- function() {
    list(
      response = spec_value("response_variable"),
      family = or_default(chosen_family(), or_default(input$glm_distribution, "gaussian")),
      link = or_default(chosen_link(), input$link_function),
      offset = or_default(spec_value("offset"), "None"),
      offset_log = isTRUE(spec_value("offset_log")),
      weights = or_default(spec_value("weights"), "None"),
      formula = trimws(or_default(spec_value("formula"), "")),
      # the share of the rows held out for testing ("0" for none) and the seed that picks them
      holdout = or_default(as.character(spec_value("holdout_share")), "0"),
      seed = glm_holdout_seed(spec_value("holdout_seed")),
      # the variance power of a Tweedie model (kept for the others, which ignore it)
      power = glm_tweedie_power(spec_value("tweedie_power"))
    )
  }

  fit_result <- eventReactive(input$fit_model, {
    spec <- current_spec()
    import <- imported()
    result <- tryCatch(
      glm_fit_model(spec, import$data),
      error = function(e) {
        showNotification(paste("Model fitting failed:", conditionMessage(e)), type = "error", duration = 10)
        list(model = NULL, spec = spec, error = conditionMessage(e))
      }
    )
    # the import the model was fitted to, so that the Model tab can say when the data has changed since
    result$import <- import[c("id", "source")]
    if (length(result$left_out) > 0) {
      showNotification(paste0("Left out of '.': ", paste0(names(result$left_out), " (", result$left_out, ")", collapse = ", "),
                              ". Name a column in the formula to use it anyway."), type = "message", duration = 12)
    }
    if (length(result$warnings) > 0) {
      showNotification(paste("The model was fitted with warnings:", paste(result$warnings, collapse = "; ")),
                       type = "warning", duration = 12)
    }
    result
  })

  # whether the model was fitted to part of the rows, the others held out for testing
  has_holdout <- function(result) isTRUE(any(result$test))

  # predictions for every row of the data (NA where a variable is missing); with rows held out,
  # for those too, and NA for a row with a value of a factor the training rows did not have
  model_predictions <- function(result) {
    if (has_holdout(result)) return(glm_predict_all(result$model, result$data)$predicted)
    unname(stats::predict(result$model, type = "response"))
  }

  # the response as glm() codes it (a text binomial response is 0 for its first level and 1 for
  # every other), for every row of the data (NA where the fit left the row out)
  model_actuals <- function(result) {
    if (has_holdout(result)) return(glm_response_coded(result$data[[result$spec$response]]))
    unname(stats::naresid(result$model$na.action, result$model$y))
  }

  # the exposure of every row: the offset column when the offset is its log, exp(offset) for a
  # raw offset with the log link; a raw offset with another link is not an exposure, so the rows
  # count as 1 each. NULL as the name when there is no exposure
  model_exposure <- function(result) {
    spec <- result$spec
    df <- result$data
    uses_exposure <- spec$offset != "None" && (spec$offset_log || spec$link == "log")
    if (!uses_exposure) return(list(values = rep(1, nrow(df)), name = NULL))
    if (spec$offset_log) return(list(values = df[[spec$offset]], name = spec$offset))
    list(values = exp(df[[spec$offset]]), name = paste0("exp(", spec$offset, ")"))
  }

  model_weights <- function(result) {
    if (result$spec$weights == "None") rep(1, nrow(result$data)) else result$data[[result$spec$weights]]
  }

  # the predictions of the shown model, computed once for the chart, the validation and the download
  shown_predictions <- reactive({
    result <- fit_result()
    req(result$model)
    model_predictions(result)
  })

  fitted_model <- reactive({
    if (!isTruthy(input$fit_model)) return(NULL)
    fit_result()$model
  })

  # whether the data was imported again since the shown model was fitted: its figures, chart and
  # downloads are then of the data imported before, which is said wherever they are shown
  model_outdated <- reactive({
    result <- fit_result()
    !is.null(result$import) && !identical(result$import$id, imported()$id)
  })

  outdated_note <- function(then) {
    result <- fit_result()
    netsimr_help(paste0("This model was fitted to '", result$import$source, "' (", format(nrow(result$data), big.mark = ",", scientific = FALSE),
                    " rows), imported before the data now on the Data tab ('", imported()$source, "', ",
                    format(nrow(selected_data()), big.mark = ",", scientific = FALSE), " rows). ", then))
  }

  #print the model summary
  output$model_summary <- renderPrint({
    validate(need(input$fit_model, "Choose the model and click Fit model."))
    result <- fit_result()
    if (is.null(result$model)) {
      cat("Model fitting failed:", result$error, "\nPlease check your formula and dataset.\n")
    } else {
      summary(result$model)
    }
  })

  # figures of a fitted model shared by the tiles and the stored models
  model_facts <- function(model) {
    list(
      aic = AIC(model),
      deviance_explained = if (model$null.deviance > 0) 1 - model$deviance / model$null.deviance else NA_real_,
      n = stats::nobs(model),
      dropped = length(model$na.action),
      parameters = sum(!is.na(stats::coef(model)))
    )
  }

  output$model_stats <- renderUI({
    model <- req(fitted_model())
    facts <- model_facts(model)
    dispersion <- summary(model)$dispersion
    # the dispersion is fixed at 1 for these families; the Pearson statistic shows whether the data agrees
    fixed <- model$family$family %in% c("poisson", "binomial")
    pearson <- if (fixed && model$df.residual > 0) sum(stats::residuals(model, type = "pearson")^2, na.rm = TRUE) / model$df.residual else NA_real_
    tagList(
      if (model_outdated()) outdated_note("Click Fit model to fit it to the new data."),
      div(
        class = "dft-stats",
        netsimr_stat_tile("Observations", format(facts$n, big.mark = ",", scientific = FALSE), icon_name = "hashtag",
                      note = paste(c(
                        if (has_holdout(fit_result())) paste(format(sum(fit_result()$test), big.mark = ",", scientific = FALSE), "rows held out for testing"),
                        if (facts$dropped > 0) paste(format(facts$dropped, big.mark = ",", scientific = FALSE), "rows with missing values left out")
                      ), collapse = "; ")),
        netsimr_stat_tile("Parameters", facts$parameters, icon_name = "list-ol"),
        netsimr_stat_tile("AIC", netsimr_fmt(facts$aic, 7), icon_name = "scale-balanced", accent = TRUE,
                      note = if (!is.finite(facts$aic) && identical(model$family$family, "Tweedie")) {
                        "None for a Tweedie model: compare the test deviance on the Validation tab"
                      }),
        netsimr_stat_tile("Deviance explained", if (is.finite(facts$deviance_explained)) paste0(netsimr_fmt(100 * facts$deviance_explained, 3), "%") else netsimr_dash,
                      icon_name = "chart-pie"),
        netsimr_stat_tile(if (fixed) "Pearson dispersion" else "Dispersion", netsimr_fmt(if (fixed) pearson else dispersion, 4),
                      icon_name = "wave-square",
                      note = if (fixed) {
                        if (is.finite(pearson) && pearson > 1.5) "Well above 1: the data is overdispersed" else "Pearson chi-squared / residual df"
                      })
      )
    )
  })

  output$coefficients_table <- renderUI({
    validate(need(input$fit_model, "Choose the model and click Fit model."))
    model <- fitted_model()
    validate(need(!is.null(model), paste("The model could not be fitted:", fit_result()$error)))
    estimates <- stats::coef(model)
    table <- summary(model)$coefficients
    link <- model$family$link
    relativity_label <- switch(link, log = "Relativity", logit = "Odds ratio", NULL)
    stars <- function(p) {
      if (!is.finite(p)) return(NULL)
      mark <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""
      if (mark != "") tags$span(class = "glm-signif", mark)
    }
    rows <- lapply(names(estimates), function(name) {
      present <- name %in% rownames(table)
      value <- function(column) if (present) table[name, column] else NA_real_
      cells <- list(
        name,
        netsimr_fmt(estimates[[name]], 5),
        netsimr_fmt(value(2), 4),
        netsimr_fmt(value(3), 4),
        tagList(if (present) format.pval(value(4), digits = 3, eps = 1e-4) else netsimr_dash, stars(value(4)))
      )
      if (!is.null(relativity_label)) cells <- c(cells, list(if (name == "(Intercept)") netsimr_dash else netsimr_fmt(exp(estimates[[name]]), 4)))
      cells
    })
    statistic <- colnames(table)[3]
    netsimr_html_table(
      c("Term", "Estimate", "Std. error", statistic, "p-value", relativity_label), rows,
      note = paste0(
        "Significance: *** 0.001, ** 0.01, * 0.05, . 0.1.",
        if (!is.null(relativity_label)) paste0(" ", relativity_label, " = exp(estimate)."),
        if (any(is.na(estimates))) " A dash marks a term that is aliased with others and was not estimated." else ""
      )
    )
  })

  list(current_spec = current_spec, fit_result = fit_result, has_holdout = has_holdout,
       model_predictions = model_predictions, model_actuals = model_actuals, model_exposure = model_exposure,
       model_weights = model_weights, shown_predictions = shown_predictions, fitted_model = fitted_model,
       model_outdated = model_outdated, outdated_note = outdated_note, model_facts = model_facts)
}

# The stored models tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_stored <- function(input, output, session, tool) {
  chosen_family <- tool$chosen_family
  chosen_link <- tool$chosen_link
  data_columns <- tool$data_columns
  fit_result <- tool$fit_result
  hold_spec_input <- tool$hold_spec_input
  model_facts <- tool$model_facts
  pending_link <- tool$pending_link

  stored_models <- list(reactiveVal(NULL), reactiveVal(NULL))

  store_model <- function(i) {
    result <- if (isTruthy(input$fit_model)) fit_result() else NULL
    if (is.null(result) || is.null(result$model)) {
      showNotification("Fit a model first, then store it.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    # the import and the rows left out say which observations the model was fitted to
    stored_models[[i]](c(list(spec = result$spec, import = result$import, left_out_rows = as.integer(result$model$na.action)),
                         model_facts(result$model)))
  }

  load_model <- function(i) {
    stored <- stored_models[[i]]()
    if (is.null(stored)) {
      showNotification(paste("Model", i, "is empty."), type = "warning", duration = 6)
      return(invisible(NULL))
    }
    apply_model_spec(stored$spec)
    showNotification(paste0("Model ", i, " loaded. Click Fit model to fit it again."), type = "message", duration = 5)
  }

  apply_model_spec <- function(spec) {
    # (cleared when the browser already shows the family: it sends no report to consume it)
    pending_link(if (!identical(spec$family, input$glm_distribution)) spec$link)
    updateSelectInput(session, "glm_distribution", selected = spec$family)
    chosen_family(spec$family)
    chosen_link(spec$link)
    updateSelectInput(session, "link_function", choices = glm_family_links[[spec$family]], selected = spec$link)
    for (id in c("response_variable", "offset", "weights")) {
      value <- spec[[switch(id, response_variable = "response", id)]]
      if (value %in% c("None", data_columns())) {
        hold_spec_input(id, value)
        updateSelectInput(session, id, selected = value)
      }
    }
    hold_spec_input("offset_log", isTRUE(spec$offset_log))
    bslib::update_switch("offset_log", value = isTRUE(spec$offset_log), session = session)
    hold_spec_input("formula", spec$formula)
    updateTextAreaInput(session, "formula", value = spec$formula)
    holdout <- or_default(spec$holdout, "0")
    hold_spec_input("holdout_share", holdout)
    updateRadioButtons(session, "holdout_share", selected = holdout)
    seed <- or_default(spec$seed, 1L)
    hold_spec_input("holdout_seed", seed)
    updateNumericInput(session, "holdout_seed", value = seed)
    if (identical(spec$family, "tweedie")) {
      power <- glm_tweedie_power(spec$power)
      hold_spec_input("tweedie_power", power)
      updateNumericInput(session, "tweedie_power", value = power)
    }
  }

  observeEvent(input$save_formula_1, store_model(1))
  observeEvent(input$save_formula_2, store_model(2))
  observeEvent(input$load_formula_1, load_model(1))
  observeEvent(input$load_formula_2, load_model(2))

  describe_stored <- function(i) {
    stored <- stored_models[[i]]()
    if (is.null(stored)) return(div(class = "glm-slot-empty", "Empty. Fit a model and store it here."))
    other <- stored_models[[3 - i]]()
    spec <- stored$spec
    # AIC only compares models fitted to the same observations (the same import, with the same rows
    # left out) of the same response, with the same weights: the weights scale each row's
    # log-likelihood, so a weighted and an unweighted AIC differ
    comparable <- !is.null(other) && identical(other$import$id, stored$import$id) &&
      identical(other$left_out_rows, stored$left_out_rows) && identical(other$spec$response, spec$response) &&
      other$n == stored$n && identical(other$spec$weights, spec$weights) &&
      identical(or_default(other$spec$holdout, "0"), or_default(spec$holdout, "0")) &&
      identical(or_default(other$spec$seed, 1L), or_default(spec$seed, 1L))
    lhs <- paste(c(paste(glm_term(spec$response), "~"), glm_offset_term(spec$offset, spec$offset_log)), collapse = " ")
    tagList(
      div(class = "glm-slot-formula", paste(lhs, if (!is.null(glm_offset_term(spec$offset, spec$offset_log))) "+", if (spec$formula == "") "1" else spec$formula)),
      div(class = "sim-muted", paste0(glm_family_label(spec), " (", spec$link, " link)",
                                      if (spec$weights != "None") paste0(", weights ", spec$weights), ", ",
                                      format(stored$n, big.mark = ",", scientific = FALSE), " observations",
                                      if (!identical(or_default(spec$holdout, "0"), "0")) {
                                        paste0(", ", names(glm_holdout_choices)[glm_holdout_choices == spec$holdout], " held out (seed ", spec$seed, ")")
                                      })),
      div(class = "glm-slot-aic", paste("AIC", netsimr_fmt(stored$aic, 7)),
          if (comparable && isTRUE(stored$aic < other$aic)) tags$span(class = "glm-better", " lower")),
      if (!is.null(other) && !comparable) div(class = "sim-muted", "Not comparable with the other model: different data, response, observations, weights or rows held out.")
    )
  }

  output$aic_output_1 <- renderUI(describe_stored(1))
  output$aic_output_2 <- renderUI(describe_stored(2))

  list(stored_models = stored_models, store_model = store_model, load_model = load_model,
       apply_model_spec = apply_model_spec, describe_stored = describe_stored)
}

# The downloads tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_downloads <- function(input, output, session, tool) {
  fit_result <- tool$fit_result
  fitted_model <- tool$fitted_model
  has_holdout <- tool$has_holdout
  model_outdated <- tool$model_outdated
  outdated_note <- tool$outdated_note
  shown_predictions <- tool$shown_predictions

  output$model_downloads <- renderUI({
    if (is.null(fitted_model())) {
      return(div(class = "sim-downloads-hint", icon("circle-info"), "Fit a model to enable the downloads."))
    }
    # the downloads would be of the data imported before: they wait for the model to be fitted again
    if (model_outdated()) return(outdated_note("Fit it again to enable the downloads."))
    div(
      class = "glm-downloads",
      downloadButton("download_model", "Model (RDS)", class = "btn-outline-glm"),
      downloadButton("download_summary", "Model summary (text)", class = "btn-outline-glm"),
      downloadButton("download_data_with_predictions", "Data with predictions (CSV)", class = "btn-outline-glm"),
      downloadButton("download_relativities", "Rating factors (CSV)", class = "btn-outline-glm"),
      downloadButton("download_glm_report", "Report (HTML)", class = "btn-outline-glm"),
      netsimr_help("The model refers to its data as model_data: to update() it in R, assign the data to ",
               "model_data first, or pass it to update() as data =. The predictions are added to the data ",
               "as the column prediction (model_prediction when the data has a column of that name). ",
               "The rating factors list the intercept, every level of each factor (its base level with a ",
               "relativity of 1) and each numeric variable per unit; with a log link the relativities are ",
               "exp(coefficient), with 95% intervals, and with another link the coefficients are given alone.")
    )
  })

  # the model the downloads are of. The buttons are only shown for an up-to-date model, but a
  # download can be asked for anyway (a page left open across an import, a kept link): with no
  # model, or one fitted to the data imported before, the request is refused as the buttons are,
  # with an error response and no file, and the session carries on
  downloadable_model <- function() {
    model <- fitted_model()
    validate(need(!is.null(model), "Fit a model to enable the downloads."))
    validate(need(!model_outdated(), "This model was fitted to data imported before. Fit it again to enable the downloads."))
    model
  }

  #download the model summary
  output$download_summary <- downloadHandler(
    filename = function() "glm_summary.txt",
    content = function(file) {
      writeLines(utils::capture.output(print(summary(downloadable_model()))), file)
    }
  )

  # Download the GLM model object as an RDS file
  output$download_model <- downloadHandler(
    filename = function() "glm_model.rds",
    content = function(file) {
      saveRDS(downloadable_model(), file)
    }
  )

  # the data with the model's prediction for every row (NA where a variable is missing)
  output$download_data_with_predictions <- downloadHandler(
    filename = function() "predicted_data.csv",
    content = function(file) {
      downloadable_model()
      result <- fit_result()
      data <- result$data
      # a prediction column of the data keeps its name; the model's gets one of its own
      column <- if ("prediction" %in% names(data)) make.unique(c(names(data), "model_prediction"))[ncol(data) + 1] else "prediction"
      data[[column]] <- shown_predictions()
      # with rows held out, which rows the model was fitted to
      if (has_holdout(result)) data[[make.unique(c(names(data), "sample"))[ncol(data) + 1]]] <- ifelse(result$test, "test", "training")
      #text from the uploaded data that a spreadsheet would read as a formula is written as text
      utils::write.csv(csv_safe(data), file, row.names = FALSE)
    }
  )

  # the rating factors: the relativity of every level of each factor (see glm_relativity_table())
  output$download_relativities <- downloadHandler(
    filename = function() "glm_rating_factors.csv",
    content = function(file) {
      #the levels are values of the uploaded data, so they are written as text where a
      #spreadsheet would read them as a formula
      utils::write.csv(csv_safe(glm_relativity_table(downloadable_model())), file, row.names = FALSE)
    }
  )

  list(downloadable_model = downloadable_model)
}

# The visualisation tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_charts <- function(input, output, session, tool) {
  dark <- tool$dark
  fit_result <- tool$fit_result
  fitted_model <- tool$fitted_model
  has_holdout <- tool$has_holdout
  model_actuals <- tool$model_actuals
  model_exposure <- tool$model_exposure
  model_outdated <- tool$model_outdated
  model_weights <- tool$model_weights
  outdated_note <- tool$outdated_note
  shown_predictions <- tool$shown_predictions

  # actual and predicted per band of the chosen variable; with an offset the chart compares
  # rates per unit of exposure, and weights (e.g. binomial trials) weigh each row
  # redrawn when the chart is asked for and when the model is refitted
  fitness_data <- eventReactive(list(input$execute_visualization, input$fit_model, input$avp_rows), {
    model <- fitted_model()
    validate(need(!is.null(model), "Fit a model on the Model tab first."))
    result <- fit_result()
    spec <- result$spec
    # the data and columns the model was fitted with, not the current choices
    df <- result$data
    variable <- input$visualize_variable
    validate(need(!is.null(variable) && variable %in% names(df), "Choose the explanatory variable."))
    # the response as the model codes it, on the same scale as the prediction
    actual <- model_actuals(result)
    predicted <- shown_predictions()
    exposure_of <- model_exposure(result)
    exposure <- exposure_of$values
    weight <- model_weights(result)
    # with rows held out, the chart can show the training or the test rows alone
    rows <- if (has_holdout(result)) or_default(input$avp_rows, "all") else "all"
    in_rows <- switch(rows, training = !result$test, test = result$test, rep(TRUE, nrow(df)))
    grouping <- df[[variable]]
    # within the slider's range, whatever a client sends
    bands <- glm_band_count(input$number_of_bands_input)
    values <- sort(unique(grouping[!is.na(grouping)]))
    if (is.numeric(grouping) && length(values) > bands) {
      breaks <- glm_band_breaks(grouping, bands, or_default(input$band_method, "quantile"))
      # readable labels ("18-24"), or cut()'s own when they would not be distinct
      grouping <- cut(grouping, breaks = breaks, labels = glm_band_labels(breaks, all(values == round(values))),
                      include.lowest = TRUE, dig.lab = 6)
    } else {
      grouping <- factor(grouping, levels = values, labels = if (is.numeric(values)) glm_value_labels(values) else values)
    }
    keep <- in_rows & !is.na(actual) & !is.na(predicted) & !is.na(grouping) & is.finite(exposure) & is.finite(weight)
    validate(need(any(keep), "No row has the response, the prediction and the variable all present."))
    band <- droplevels(grouping[keep])
    # text is not grouped: an ID would draw hundreds of unreadable bars
    validate(need(nlevels(band) <= 100, paste0("'", variable, "' has ", format(nlevels(band), big.mark = ",", scientific = FALSE),
                                               " different values: choose a numeric variable, or one with at most 100 values.")))
    w <- weight[keep]
    e <- exposure[keep]
    total <- function(x) as.numeric(tapply(x, band, sum))
    denominator <- total(w * e)
    plot_data <- data.frame(
      band = levels(band),
      actual = total(w * actual[keep]) / denominator,
      predicted = total(w * predicted[keep]) / denominator,
      exposure = denominator,
      stringsAsFactors = FALSE
    )
    attr(plot_data, "dropped") <- sum(in_rows & !keep)
    attr(plot_data, "rows") <- rows
    attr(plot_data, "exposure") <- exposure_of$name
    attr(plot_data, "variable") <- variable
    plot_data
  })

  # the titles of the chart follow what it shows: an exposure only when the offset was used as one
  fitness_labels <- reactive({
    plot_data <- fitness_data()
    spec <- isolate(fit_result()$spec)
    exposure <- attr(plot_data, "exposure")
    list(
      x = attr(plot_data, "variable"),
      y = if (!is.null(exposure)) paste(spec$response, "per unit of exposure") else paste("Average", spec$response),
      bars = if (!is.null(exposure)) paste0("Exposure (", exposure, ")") else if (spec$weights != "None") paste0("Weight (", spec$weights, ")") else "Rows"
    )
  })

  output$fitness_note <- renderUI({
    # the chart shows the message when there is no data; the note stays empty
    plot_data <- tryCatch(fitness_data(), error = function(e) NULL)
    dropped <- attr(req(plot_data), "dropped")
    tagList(
      if (model_outdated()) outdated_note("The chart is of that data; click Fit model to fit it to the new data."),
      if (isTRUE(dropped > 0)) {
        netsimr_help(format(dropped, big.mark = ",", scientific = FALSE), if (dropped == 1) " row is" else " rows are",
                 " left out: the response, the prediction or the variable is missing.")
      }
    )
  })

  # actual and predicted on the left axis, and the exposure (or weight, or rows) of each band
  # as bars on a secondary axis on the right; redrawn in the other theme's colours when it changes,
  # and when the chart is resized, so that the legend is laid out for the new width
  output$fitness_plot <- netsimr_render_plot({
    validate(need(input$execute_visualization, "Choose a variable and click Draw chart."))
    plot_data <- fitness_data()
    labels <- fitness_labels()
    netsimr_category_chart(
      plot_data$band,
      bars = list(name = labels$bars, values = plot_data$exposure, colour = "band_bar", y2 = labels$bars),
      lines = list(list(name = "Actual", values = plot_data$actual, colour = "empirical"),
                   list(name = "Predicted", values = plot_data$predicted, colour = netsimr_palette[2])),
      x_title = labels$x, y_title = labels$y, dark = dark()
    )
  }, bg = "transparent", res = 96, alt = function() {
    labels <- tryCatch(fitness_labels(), error = function(e) NULL)
    if (is.null(labels)) return("Actual and predicted chart")
    paste0("Actual and predicted by band of ", labels$x, ". Left axis: ", labels$y, ". Bars on the right axis: ", labels$bars, ".")
  })

  # the choice of rows, shown for a model fitted with rows held out
  output$avp_rows_ui <- renderUI({
    result <- if (isTruthy(input$fit_model)) fit_result() else NULL
    if (!has_holdout(result)) return(NULL)
    tagList(
      div(class = "sim-section-label mt-2", "Rows"),
      div(class = "pill-radio",
          radioButtons("avp_rows", NULL, c("All" = "all", "Training" = "training", "Test" = "test"),
                       or_default(isolate(input$avp_rows), "all"), inline = TRUE))
    )
  })

  list(fitness_data = fitness_data, fitness_labels = fitness_labels)
}

# The validation tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_validation <- function(input, output, session, tool) {
  dark <- tool$dark
  fit_result <- tool$fit_result
  has_holdout <- tool$has_holdout
  imported <- tool$imported
  model_actuals <- tool$model_actuals
  model_exposure <- tool$model_exposure
  model_outdated <- tool$model_outdated
  model_predictions <- tool$model_predictions
  model_weights <- tool$model_weights
  outdated_note <- tool$outdated_note
  shown_predictions <- tool$shown_predictions
  stored_models <- tool$stored_models

  # every row's actual, prediction, exposure and weight, and which rows were held out
  validation_rows <- reactive({
    validate(need(input$fit_model, "Fit a model on the Model tab first."))
    result <- fit_result()
    validate(need(!is.null(result$model), "Fit a model on the Model tab first."))
    exposure <- model_exposure(result)
    list(result = result, actual = model_actuals(result), predicted = shown_predictions(),
         exposure = exposure$values, exposure_name = exposure$name, weight = model_weights(result),
         test = if (has_holdout(result)) result$test else rep(FALSE, nrow(result$data)))
  })

  # the rows the charts are drawn for: the test rows when there are any (the training rows
  # otherwise, said in a note), the training rows or all of them
  validation_set <- reactive({
    v <- validation_rows()
    wanted <- or_default(input$validation_rows, "test")
    used <- if (identical(wanted, "test") && !any(v$test)) "training" else wanted
    rows <- switch(used, test = v$test, training = !v$test, rep(TRUE, length(v$test)))
    list(rows = rows, used = used, wanted = wanted)
  })

  validation_label <- function(used) switch(used, test = "test rows", training = "training rows", "all rows")

  # the deviance per row and the Gini coefficient of a set of rows
  validation_measures <- function(v, rows) {
    gini <- glm_gini_curve(v$actual[rows], v$predicted[rows], v$exposure[rows], v$weight[rows])
    list(deviance = glm_unit_deviance(v$result$model$family, v$actual[rows], v$predicted[rows], v$weight[rows]),
         gini = if (is.null(gini)) NA_real_ else gini$gini, rows = sum(rows))
  }

  output$validation_stats <- renderUI({
    v <- validation_rows()
    training <- validation_measures(v, !v$test)
    test <- if (any(v$test)) validation_measures(v, v$test)
    both <- function(f) if (is.null(test)) f(training) else paste(f(training), "/", f(test))
    fmt <- function(x, digits) if (is.finite(x)) netsimr_fmt(x, digits) else netsimr_dash
    tagList(
      if (model_outdated()) outdated_note("Click Fit model to fit it to the new data."),
      if (is.null(test)) netsimr_help("No rows were held out: the figures are of the rows the model was fitted to. ",
                                  "Hold out rows on the Model tab to see how it does on rows it has not seen."),
      div(
        class = "dft-stats",
        netsimr_stat_tile(if (is.null(test)) "Training rows" else "Training / test rows",
                      both(function(m) format(m$rows, big.mark = ",", scientific = FALSE)), icon_name = "table-list"),
        netsimr_stat_tile(if (is.null(test)) "Deviance per row" else "Deviance per row, training / test",
                      both(function(m) fmt(m$deviance, 4)), icon_name = "scale-balanced", accent = TRUE,
                      note = if (!is.null(test) && is.finite(test$deviance) && is.finite(training$deviance) &&
                                 test$deviance > 1.2 * training$deviance) "Much higher on the test rows: the model may be overfitted"),
        netsimr_stat_tile(if (is.null(test)) "Gini" else "Gini, training / test", both(function(m) fmt(m$gini, 3)),
                      icon_name = "chart-area", note = "How well the predictions rank the rows: 0 is no better than chance")
      )
    )
  })

  validation_bands <- reactive({
    bands <- suppressWarnings(as.integer(or_default(input$validation_bands, "10")))
    if (length(bands) != 1 || is.na(bands) || !bands %in% c(5L, 10L, 20L)) 10L else bands
  })

  validation_y_title <- function(v) {
    response <- v$result$spec$response
    if (!is.null(v$exposure_name)) paste(response, "per unit of exposure") else paste("Average", response)
  }

  validation_bar_name <- function(v) {
    if (!is.null(v$exposure_name)) paste0("Exposure (", v$exposure_name, ")") else if (v$result$spec$weights != "None") paste0("Weight (", v$result$spec$weights, ")") else "Rows"
  }

  set_note <- function(set) {
    if (identical(set$wanted, "test") && set$used != "test") netsimr_help("No rows were held out, so the chart is of the training rows.")
  }

  lift_table <- reactive({
    v <- validation_rows()
    set <- validation_set()
    r <- set$rows
    table <- glm_lift_table(v$actual[r], v$predicted[r], v$predicted[r] / v$exposure[r], v$exposure[r], v$weight[r],
                            bands = validation_bands())
    validate(need(!is.null(table), "No row has both the response and a prediction."))
    table
  })

  output$lift_plot <- netsimr_render_plot({
    v <- validation_rows()
    table <- lift_table()
    netsimr_category_chart(
      as.character(table$band),
      bars = list(name = validation_bar_name(v), values = table$exposure, colour = "band_bar", y2 = validation_bar_name(v)),
      lines = list(list(name = "Actual", values = table$actual, colour = "empirical"),
                   list(name = "Predicted", values = table$predicted, colour = netsimr_palette[2])),
      x_title = paste0("Band of predicted rate, lowest to highest (", validation_label(validation_set()$used), ")"),
      y_title = validation_y_title(v), dark = dark()
    )
  }, bg = "transparent", res = 96, alt = "Lift chart: actual and predicted per band of the predicted rate")

  output$lift_note <- renderUI({
    tagList(
      set_note(validation_set()),
      netsimr_help("The rows are sorted by their predicted rate and cut into bands of about the same exposure. ",
               "A good model's actual line rises steeply from the first band to the last, and its predicted ",
               "line follows it.")
    )
  })

  output$lorenz_plot <- netsimr_render_plot({
    v <- validation_rows()
    r <- validation_set()$rows
    curve <- glm_gini_curve(v$actual[r], v$predicted[r], v$exposure[r], v$weight[r])
    validate(need(!is.null(curve), "No row has both the response and a prediction."))
    curve <- glm_thin_curve(curve)
    netsimr_line_chart(
      list(list(x = curve$x, y = curve$y, name = paste0("Model (Gini ", netsimr_fmt(curve$gini, 3), ")"), colour = netsimr_palette[2]),
           list(x = c(0, 1), y = c(0, 1), name = "No better than chance", colour = "muted", lwd = 1.4)),
      x_title = paste0("Share of exposure, lowest predicted rate first (", validation_label(validation_set()$used), ")"),
      y_title = paste("Share of", v$result$spec$response), dark = dark(), ylim = c(0, 1)
    )
  }, bg = "transparent", res = 96, alt = "Lorenz curve of the model's predictions, with its Gini coefficient")

  # the stored model to compare with, fitted again to the same rows
  double_lift <- reactive({
    v <- validation_rows()
    choice <- or_default(input$validation_compare, "none")
    validate(need(choice %in% c("1", "2"), "Choose a stored model to compare this model with."))
    stored <- stored_models[[as.integer(choice)]]()
    validate(need(!is.null(stored), paste0("Model ", choice, " is empty: store a model in it on the Model tab.")))
    result <- v$result
    validate(need(!model_outdated(), "The data was imported again since this model was fitted: fit it again first."))
    same_rows <- identical(stored$import$id, result$import$id) &&
      identical(or_default(stored$spec$holdout, "0"), result$spec$holdout) && identical(or_default(stored$spec$seed, 1L), result$spec$seed)
    validate(need(same_rows, paste0("Model ", choice, " was fitted to other data or other rows held out, so the two cannot be compared row by row.")))
    other <- tryCatch(glm_fit_model(stored$spec, isolate(imported()$data)), error = function(e) NULL)
    validate(need(!is.null(other), paste0("Model ", choice, " could not be fitted again.")))
    other_predicted <- model_predictions(other)
    r <- validation_set()$rows
    table <- glm_lift_table(v$actual[r], v$predicted[r], v$predicted[r] / other_predicted[r], v$exposure[r], v$weight[r],
                            bands = validation_bands(), others = list(other = other_predicted[r]))
    validate(need(!is.null(table), "No row has the response and both predictions."))
    list(table = table, name = paste("Model", choice))
  })

  output$double_lift_plot <- netsimr_render_plot({
    v <- validation_rows()
    d <- double_lift()
    table <- d$table
    netsimr_category_chart(
      as.character(table$band),
      bars = list(name = validation_bar_name(v), values = table$exposure, colour = "band_bar", y2 = validation_bar_name(v)),
      lines = list(list(name = "Actual", values = table$actual, colour = "empirical"),
                   list(name = "This model", values = table$predicted, colour = netsimr_palette[2]),
                   list(name = d$name, values = table$other, colour = netsimr_palette[1])),
      x_title = paste0("Band of the ratio to ", d$name, " (", validation_label(validation_set()$used), ")"),
      y_title = validation_y_title(v), dark = dark()
    )
  }, bg = "transparent", res = 96, alt = "Double lift chart: actual and both models' predictions per band of their ratio")

  output$double_lift_note <- renderUI({
    tagList(
      set_note(validation_set()),
      netsimr_help("The rows are sorted by the ratio of this model's prediction to the stored model's, and cut into bands. ",
               "Where the two disagree most, at either end, the model whose line is nearer the actual line is the better one.")
    )
  })

  list(validation_rows = validation_rows, validation_set = validation_set,
       validation_label = validation_label, validation_measures = validation_measures,
       validation_bands = validation_bands, validation_y_title = validation_y_title,
       validation_bar_name = validation_bar_name, set_note = set_note, lift_table = lift_table,
       double_lift = double_lift)
}

# The rates and scoring tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_rates <- function(input, output, session, tool) {
  dark <- tool$dark
  downloadable_model <- tool$downloadable_model
  fit_result <- tool$fit_result
  fitted_model <- tool$fitted_model
  model_exposure <- tool$model_exposure
  model_outdated <- tool$model_outdated
  validation_bar_name <- tool$validation_bar_name
  validation_measures <- tool$validation_measures
  validation_rows <- tool$validation_rows
  validation_y_title <- tool$validation_y_title

  # the rating table of the shown model (see glm_rate_table()), from the rows it was fitted to
  rate_table <- reactive({
    model <- fitted_model()
    validate(need(!is.null(model), "Fit a model on the Model tab to see its rating table."),
             need(!model_outdated(), "This model was fitted to data imported before: fit it again to see its rating table."))
    result <- fit_result()
    values <- suppressWarnings(as.numeric(or_default(input$rate_values, 100)))
    values <- if (length(values) == 1 && is.finite(values)) min(max(round(values), 5), 500) else 100
    rates <- tryCatch(glm_rate_table(model, result$data[!result$test, , drop = FALSE], values), error = function(e) e)
    validate(need(!inherits(rates, "error"), if (inherits(rates, "error")) paste("No rating table:", conditionMessage(rates))))
    rates
  })

  observeEvent(rate_table(), {
    choices <- names(rate_table()$factors)
    current <- isolate(input$rate_factor)
    updateSelectInput(session, "rate_factor", choices = choices,
                      selected = if (!is.null(current) && current %in% choices) current else choices[1])
  })

  # the rating factor chosen, NULL for a model with none (an intercept only)
  chosen_rate_factor <- reactive({
    rates <- rate_table()
    validate(need(length(rates$factors) > 0, "The model has no rating factors: its prediction is the base rate."))
    name <- input$rate_factor
    if (is.null(name) || !name %in% names(rates$factors)) name <- names(rates$factors)[1]
    list(name = name, table = rates$factors[[name]], rates = rates)
  })

  rate_label <- function(rates) {
    if (is.na(rates$relativity)) "Effect on the link scale" else tools_title(rates$relativity)
  }
  tools_title <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))

  output$rate_stats <- renderUI({
    rates <- rate_table()
    result <- fit_result()
    score <- scored()
    exposure <- model_exposure(result)$name
    div(
      class = "dft-stats",
      netsimr_stat_tile(if (is.na(rates$relativity)) "Base prediction" else "Base rate", netsimr_fmt(rates$base_rate, 5),
                    icon_name = "tag", accent = TRUE,
                    note = paste0(if (!is.null(exposure) && rates$link == "log") paste0("Per unit of ", exposure, "; ") else "",
                                  "at the base value of every factor")),
      netsimr_stat_tile("Rating factors", length(rates$factors), icon_name = "table-list",
                    note = if (rates$exact) "Every prediction is the base rate times the relativities" else
                      "Numbers with many values are shown at quantiles"),
      netsimr_stat_tile("Scored", if (is.null(score)) netsimr_dash else format(score$predicted, big.mark = ",", scientific = FALSE), icon_name = "file-import",
                    note = if (is.null(score)) "Upload a file of policies to score it" else
                      paste("of", format(score$total, big.mark = ",", scientific = FALSE), "rows of", score$name))
    )
  })

  output$rate_table_ui <- renderUI({
    chosen <- chosen_rate_factor()
    f <- chosen$table
    rates <- chosen$rates
    vars <- setdiff(names(f), c("effect", "relativity", "base", "rows"))
    has_relativity <- !is.na(rates$relativity)
    rows <- lapply(seq_len(nrow(f)), function(i) {
      c(lapply(vars, function(v) if (is.numeric(f[[v]])) netsimr_fmt(f[[v]][i], 6) else as.character(f[[v]][i])),
        list(if (is.na(f$rows[i])) netsimr_dash else format(f$rows[i], big.mark = ",", scientific = FALSE), netsimr_fmt(f$effect[i], 4)),
        if (has_relativity) list(netsimr_fmt(f$relativity[i], 4)))
    })
    base <- which(f$base)
    note <- c(
      paste0("Terms: ", paste(attr(f, "terms"), collapse = ", "), ". The base is ",
             paste(vapply(vars, function(v) paste0(v, " = ", format(rates$base[[v]])), character(1)), collapse = ", "), "."),
      if (has_relativity && rates$relativity == "relativity") {
        "A policy's prediction is the base rate times its relativity in every rating factor (times its exposure)."
      } else if (has_relativity) {
        "The odds of a policy are the base odds times its odds ratio in every rating factor."
      } else {
        "With this link the effects add up: the prediction is the inverse link of the base linear predictor plus every effect."
      },
      if (!isTRUE(attr(f, "exact"))) "The numbers are shown at quantiles: a value between two rows has a relativity between theirs."
    )
    netsimr_html_table(c(vars, "Rows", "Effect", if (has_relativity) rate_label(rates)), rows,
                   best = if (length(base)) base[1], best_label = "Base", note = paste(note, collapse = " "),
                   class = "dft-table-scroll")
  })

  output$rate_plot <- netsimr_render_plot({
    chosen <- chosen_rate_factor()
    glm_rate_chart(chosen$table, chosen$rates, dark())
  }, bg = "transparent", res = 96, alt = "The relativity of each value of the chosen rating factor")

  output$download_rate_table <- downloadHandler(
    filename = function() "glm_rate_table.csv",
    content = function(file) utils::write.csv(csv_safe(glm_rate_table_long(rate_table())), file, row.names = FALSE)
  )

  # the rate table in a workbook: the base rate and every factor on one sheet, a sheet per
  # rating factor, and the coefficients
  output$download_rates_excel <- downloadHandler(
    filename = function() "glm_rate_table.xlsx",
    content = function(file) {
      rates <- rate_table()
      factors <- lapply(rates$factors, function(f) {
        out <- as.data.frame(f)
        if (is.na(rates$relativity)) out$relativity <- NULL
        out
      })
      netsimr_write_excel(c(list(`Rate table` = glm_rate_table_long(rates)), factors,
                            list(Coefficients = glm_relativity_table(fitted_model()))), file)
    }
  )

  # a file of policies scored with the shown model, read with the Data tab's file options
  scored <- reactiveVal(NULL)
  observeEvent(fit_result(), scored(NULL))
  observeEvent(input$score_file, {
    file <- input$score_file
    model <- tryCatch(downloadable_model(), error = function(e) NULL)
    fail <- function(message) {
      scored(NULL)
      showNotification(paste("The file could not be scored:", message), type = "error", duration = 10)
    }
    if (is.null(model)) return(fail("fit a model on the Model tab first."))
    result <- tryCatch({
      dec <- or_default(input$csv_dec, ".")
      df <- netsimr_read_data(file$datapath, header = !isFALSE(input$csv_header), sep = or_default(input$csv_sep, ","),
                          quote = or_default(input$csv_quote, "\""), dec = dec)
      df <- netsimr_convert_number_columns(df, dec)
      if (nrow(df) == 0) stop("it has no rows")
      glm_score(model, df, response = fit_result()$spec$response)
    }, error = function(e) e)
    if (inherits(result, "error")) return(fail(conditionMessage(result)))
    result$name <- file$name
    scored(result)
    left <- result$total - result$predicted
    showNotification(paste0("Scored ", format(result$predicted, big.mark = ",", scientific = FALSE), " of ", format(result$total, big.mark = ",", scientific = FALSE),
                            " rows", if (left > 0) paste0("; ", format(left, big.mark = ",", scientific = FALSE), " have a missing value or a level ",
                                                          "the model has not seen, and no prediction") else "", "."),
                     type = if (left > 0) "warning" else "message", duration = 8)
  })

  output$score_result <- renderUI({
    s <- scored()
    if (is.null(s)) return(netsimr_help("The file needs the columns of the model's terms (and its offset); its other columns are kept."))
    tagList(
      downloadButton("download_scored", "Scored data (CSV)", class = "btn-outline-glm"),
      netsimr_help(paste0(format(s$predicted, big.mark = ",", scientific = FALSE), " of ", format(s$total, big.mark = ",", scientific = FALSE), " rows of ", s$name,
                      " scored, in the column ", s$column, ".",
                      if (s$unseen > 0) paste0(" ", format(s$unseen, big.mark = ",", scientific = FALSE), " with a level the model has not seen.") else "",
                      if (s$incomplete > 0) paste0(" ", format(s$incomplete, big.mark = ",", scientific = FALSE), " with a missing value.") else ""))
    )
  })

  output$download_scored <- downloadHandler(
    filename = function() "scored_data.csv",
    content = function(file) {
      s <- scored()
      validate(need(!is.null(s), "Upload a file to score first."))
      utils::write.csv(csv_safe(s$data), file, row.names = FALSE)
    }
  )

  # the report of the model: its data, choices, coefficients, rate table and validation
  output$download_glm_report <- downloadHandler(
    filename = function() "glm_report.html",
    content = function(file) {
      model <- downloadable_model()
      result <- fit_result()
      v <- validation_rows()
      training <- validation_measures(v, !v$test)
      test <- if (any(v$test)) validation_measures(v, v$test)
      rows <- if (any(v$test)) v$test else !v$test
      lift <- glm_lift_table(v$actual[rows], v$predicted[rows], v$predicted[rows] / v$exposure[rows], v$exposure[rows],
                             v$weight[rows], bands = 10L)
      write_glm_report(file, list(
        source = result$import$source, rows = nrow(result$data), spec = result$spec, model = model,
        rates = tryCatch(rate_table(), error = function(e) e), training = training, test = test, lift = lift,
        lorenz = glm_gini_curve(v$actual[rows], v$predicted[rows], v$exposure[rows], v$weight[rows]),
        used = if (any(v$test)) "test rows" else "training rows", y_title = validation_y_title(v),
        bar_name = validation_bar_name(v), response = result$spec$response
      ))
    }
  )

  list(rate_table = rate_table, chosen_rate_factor = chosen_rate_factor, rate_label = rate_label,
       tools_title = tools_title, scored = scored)
}

# The variable selection tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_selection <- function(input, output, session, tool) {
  dark <- tool$dark
  fit_result <- tool$fit_result
  has_holdout <- tool$has_holdout
  hold_spec_input <- tool$hold_spec_input
  model_outdated <- tool$model_outdated
  selected_data <- tool$selected_data

  # the columns a selection may add, offered once data is imported
  observeEvent(selected_data(), {
    updateSelectizeInput(session, "select_candidates", choices = glm_candidate_columns(selected_data()), selected = character(0))
  })

  selection <- eventReactive(input$run_selection, {
    result <- if (isTruthy(input$fit_model)) fit_result() else NULL
    if (is.null(result) || is.null(result$model)) return(list(error = "Fit a model first: the selection starts from its terms."))
    if (model_outdated()) return(list(error = "The data was imported again since the model was fitted: fit it again first."))
    spec <- result$spec
    pool <- glm_candidate_columns(result$data, c(spec$response, spec$offset, spec$weights))
    chosen <- input$select_candidates
    candidates <- if (length(chosen) > 0) intersect(chosen, pool) else pool
    test <- if (has_holdout(result)) result$data[result$test, , drop = FALSE]
    method <- or_default(input$select_criterion, "AIC")
    if (!method %in% c("AIC", "BIC", names(glm_penalty_alpha))) method <- "AIC"
    training <- result$data[!result$test, , drop = FALSE]
    tryCatch(
      shiny::withProgress(message = "Selecting terms", value = 0.3, {
        if (method %in% c("AIC", "BIC")) {
          c(glm_stepwise(result$model, training, candidates, method, test), kind = "stepwise")
        } else {
          c(glm_penalised(result$model, training, candidates, method, test, seed = spec$seed), kind = "penalised")
        }
      }),
      error = function(e) list(error = paste("The selection failed:", conditionMessage(e)))
    )
  })

  # the cross-validated deviance of a penalised fit along its penalties
  output$selection_plot <- netsimr_render_plot({
    s <- selection()
    req(identical(s$kind, "penalised"))
    path <- s$path
    netsimr_line_chart(
      list(list(x = path$log_lambda, y = path$cv_deviance, name = "Cross-validated deviance", colour = netsimr_palette[2]),
           list(x = path$log_lambda, y = path$upper, name = "One standard error", colour = "muted", lwd = 1),
           list(x = path$log_lambda, y = path$lower, name = "One standard error", colour = "muted", lwd = 1)),
      x_title = "log(lambda), the penalty: weaker to the left", y_title = "Deviance per row", dark = dark(),
      ylim = range(c(path$lower, path$upper), finite = TRUE), vlines = log(unname(s$lambda))
    )
  }, bg = "transparent", res = 96, alt = "Cross-validated deviance of the penalised fit by the size of its penalty")

  output$selection_result <- renderUI({
    s <- selection()
    if (!is.null(s$error)) return(netsimr_help(s$error))
    if (identical(s$kind, "penalised")) return(penalised_result(s))
    with_test <- any(is.finite(s$path$test_deviance))
    best_test <- if (with_test) which.min(s$path$test_deviance)
    rows <- lapply(seq_len(nrow(s$path)), function(i) {
      cells <- list(as.character(i - 1), s$path$change[i], netsimr_fmt(s$path$criterion[i], 7))
      if (with_test) cells <- c(cells, list(netsimr_fmt(s$path$test_deviance[i], 5)))
      cells
    })
    tagList(
      netsimr_html_table(c("Step", "Change", s$criterion, if (with_test) "Test deviance per row"), rows,
                     best = best_test, best_label = "Lowest", align = c("left", "left", "right", "right")),
      div(class = "sim-section-label mt-2", "Suggested terms"),
      div(class = "glm-slot-formula", s$terms),
      actionButton("use_selection", "Use these terms", icon = icon("arrow-up"), class = "btn-outline-glm mt-1"),
      netsimr_help(
        "Each step adds a column or drops a term, whichever lowers the ", s$criterion, " most, until none does. ",
        if (s$left_out > 0) paste0(format(s$left_out, big.mark = ",", scientific = FALSE), " rows with a candidate column missing were left out of the selection. "),
        if (with_test) "The test deviance is measured on the rows held out: if it rises at a step, that step fits noise. ",
        "Fit the suggested terms with Fit model, and check them on the Validation tab."
      ),
      if (length(s$warnings) > 0) netsimr_help(paste("The fits warned:", paste(s$warnings, collapse = "; ")))
    )
  })

  penalised_result <- function(s) {
    with_test <- any(is.finite(s$summary$test_deviance))
    rows <- lapply(seq_len(nrow(s$summary)), function(i) {
      cells <- list(s$summary$choice[i], netsimr_fmt(s$summary$lambda[i], 4), as.character(s$summary$terms[i]),
                    netsimr_fmt(s$summary$cv_deviance[i], 5))
      if (with_test) cells <- c(cells, list(netsimr_fmt(s$summary$test_deviance[i], 5)))
      cells
    })
    name <- c(lasso = "lasso", elastic = "elastic net", ridge = "ridge regression")[[s$penalty]]
    tagList(
      netsimr_html_table(c("Penalty", "lambda", "Terms", "CV deviance per row", if (with_test) "Test deviance per row"), rows,
                     align = c("left", "right", "right", "right", "right")),
      netsimr_plot_output("selection_plot", height = "320px"),
      div(class = "sim-section-label mt-2", "Suggested terms (within one standard error)"),
      div(class = "glm-slot-formula", s$terms),
      if (length(s$dropped) > 0) div(class = "sim-muted", paste("Dropped:", paste(s$dropped, collapse = ", "))),
      actionButton("use_selection", "Use these terms", icon = icon("arrow-up"), class = "btn-outline-glm mt-1"),
      netsimr_help(paste0(
        "The ", name, " shrinks the coefficients towards zero by a penalty chosen by 5-fold cross-validation on the training rows",
        if (s$penalty != "ridge") ", and drops the terms whose coefficients reach zero" else "; ridge shrinks every term but drops none",
        ". The larger penalty within one standard error of the lowest cross-validated deviance keeps fewer terms. ",
        if (s$left_out > 0) paste0(format(s$left_out, big.mark = ",", scientific = FALSE), " rows with a column missing were left out. "),
        "Use the terms to fit an ordinary GLM, whose relativities and standard errors the Coefficients table gives."
      )),
      if (length(s$warnings) > 0) netsimr_help(paste("The fit warned:", paste(unique(s$warnings), collapse = "; ")))
    )
  }

  observeEvent(input$use_selection, {
    s <- selection()
    req(is.null(s$error))
    hold_spec_input("formula", s$terms)
    updateTextAreaInput(session, "formula", value = s$terms)
    showNotification("The suggested terms are in the formula. Click Fit model to fit them.", type = "message", duration = 6)
  })

  list(selection = selection, penalised_result = penalised_result)
}

# The save and load settings tab of the GLM fitting tool: its outputs and observers. `tool` holds what the
# earlier tabs define; the tab returns what it defines, for the tabs after it.
glm_server_settings <- function(input, output, session, tool) {
  chosen_family <- tool$chosen_family
  chosen_link <- tool$chosen_link
  data_columns <- tool$data_columns
  hold_spec_input <- tool$hold_spec_input
  import_failed <- tool$import_failed
  imported <- tool$imported
  next_import <- tool$next_import
  pending_columns <- tool$pending_columns
  pending_link <- tool$pending_link

  #save configurations as a text file: the import options and model choices, never the password
  #or files; the database connection details only when the box is ticked
  output$DownloadDataHandlerConf <- downloadHandler(
    filename = function() "glm_fitting_tool_settings.txt",
    contentType = "text/plain",
    content = function(file) {
      ids <- glm_settings_inputs$id
      if (!isTRUE(isolate(input$settings_include_db))) ids <- setdiff(ids, glm_settings_connection_ids)
      values <- isolate(stats::setNames(lapply(ids, function(id) input[[id]]), ids))
      write_settings_file(Filter(Negate(is.null), values), file, glm_settings_tool, glm_settings_version)
    }
  )

  #load configurations: the fields the file has are applied (it never has a password)
  observeEvent(input$load_config, {
    file_info <- input$load_config
    req(file_info$datapath)
    settings <- tryCatch(read_settings_file(file_info$datapath, glm_settings_tool), error = function(e) e)
    if (inherits(settings, "error")) {
      showNotification(paste0("'", file_info$name, "' was not loaded. ", conditionMessage(settings)), type = "error", duration = 8)
      return(invisible(NULL))
    }
    values <- glm_settings_values(settings$values)
    if (is.null(values)) {
      showNotification(paste0("'", file_info$name, "' has no settings this version of the GLM fitting tool can use."), type = "error", duration = 8)
      return(invisible(NULL))
    }
    # a formula that calls anything but the formula functions would run its author's code at the
    # next click of Fit model, so the file is refused here
    problem <- if (!is.null(values$formula)) glm_formula_problem(values$formula)
    if (!is.null(problem)) {
      showNotification(paste0("'", file_info$name, "' was not loaded. ", toupper(substr(problem, 1, 1)), substring(problem, 2)),
                       type = "error", duration = 10)
      return(invisible(NULL))
    }
    applied <- apply_settings(values)
    showNotification(
      paste0("Settings loaded from '", file_info$name, "'.", applied$link_note,
             if (applied$pending > 0) " The column choices are applied when the data is imported." else ""),
      type = "message", duration = 6
    )
  })

  # applies the usable values of a settings file (glm_settings_values()); column choices
  # that the data does not have yet wait for the next import. Returns a note on the link
  # and the number of choices waiting
  apply_settings <- function(values) {
    kinds <- stats::setNames(glm_settings_inputs$kind, glm_settings_inputs$id)
    # before the first import there are no columns yet (data_columns() stops silently)
    columns <- tryCatch(data_columns(), error = function(e) NULL)
    pending <- list()
    # The family and link are set together, so that the model fitted and the selects agree
    # however the browser's replies arrive: a file without a link keeps the link shown unless
    # it changes the family (whose default link is then used, as when the family is changed by
    # hand), and a link the family does not offer, which would leave the link select blank
    # while the fit used the default, is replaced by the default and the user told so
    link_note <- NULL
    if (!is.null(values$glm_distribution) || !is.null(values$link_function)) {
      shown_family <- or_default(chosen_family(), or_default(input$glm_distribution, "gaussian"))
      family <- or_default(values$glm_distribution, shown_family)
      links <- glm_family_links[[family]]
      link <- values$link_function
      if (is.null(link)) link <- if (identical(family, shown_family)) chosen_link() else links[1]
      if (!is.null(values$link_function) && !values$link_function %in% links) {
        link_note <- paste0(" The ", family, " family has no '", values$link_function, "' link, so its default, '",
                            links[1], "', is used.")
      }
      if (is.null(link) || !link %in% links) link <- links[1]
      chosen_family(family)
      chosen_link(link)
      # the family observer selects this link when the browser reports the family; a family the
      # browser already shows sends no report, and a link left pending from an earlier load would
      # be selected at the next change of family
      pending_link(if (!identical(family, input$glm_distribution)) link)
      updateSelectInput(session, "glm_distribution", selected = family)
      updateSelectInput(session, "link_function", choices = links, selected = link)
    }
    for (id in setdiff(names(values), c("glm_distribution", "link_function"))) {
      value <- values[[id]]
      switch(
        kinds[[id]],
        radio = {
          hold_spec_input(id, value)
          updateRadioButtons(session, id, selected = value)
        },
        switch = {
          hold_spec_input(id, isTRUE(value))
          bslib::update_switch(id, value = isTRUE(value), session = session)
        },
        text = updateTextInput(session, id, value = value),
        textarea = {
          hold_spec_input(id, value)
          updateTextAreaInput(session, id, value = value)
        },
        slider = updateSliderInput(session, id, value = value),
        numeric = {
          hold_spec_input(id, value)
          updateNumericInput(session, id, value = value)
        },
        select = updateSelectInput(session, id, selected = value),
        column = if (!is.null(columns) && value %in% c("None", columns)) {
          hold_spec_input(id, value)
          updateSelectInput(session, id, selected = value)
        } else {
          pending[[id]] <- value
        }
      )
    }
    pending_columns(pending)
    list(link_note = link_note, pending = length(pending))
  }

  # the example: the motor policies imported, with a Poisson model of the claim counts set up,
  # its exposure the offset, ready to fit
  observeEvent(input$try_example, {
    apply_settings(list(data_source = "Example", glm_distribution = "poisson", link_function = "log",
                        response_variable = "claim_count", offset = "exposure", offset_log = TRUE, weights = "None",
                        formula = "ns(age, df = 4) + region + vehicle + log(vehicle_value)", visualize_variable = "age"))
    import_failed(FALSE)
    imported(list(data = NetSimR::motor_policies, source = "Example: motor policies", id = next_import()))
    bslib::nav_select("main_navbar", "model", session = session)
    showNotification("The example motor policies are imported, with a Poisson model of their claim counts: click Fit model.",
                     type = "message", duration = 8)
  })

  # the tiles and the formula helpers are hidden while empty, and shiny does not render
  # hidden outputs, so they must render even when hidden or they would never appear
  for (id in c("data_overview", "model_stats", "formula_columns", "formula_lhs", "validation_stats", "rate_stats")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  list(apply_settings = apply_settings)
}
