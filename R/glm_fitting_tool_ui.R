#' Script for the formula column chips: clicking a column adds it to the formula
#'
#' @noRd
glm_formula_chips_js <- "
(function () {
  document.addEventListener('click', function (event) {
    var chip = event.target.closest && event.target.closest('.glm-chip');
    if (!chip) return;
    var area = document.getElementById('formula');
    if (!area) return;
    var term = chip.getAttribute('data-term');
    var current = area.value.replace(/\\s+$/, '');
    var endsWithOperator = /[+*:~(\\-]$/.test(current);
    area.value = current === '' ? term : current + (endsWithOperator ? ' ' : ' + ') + term;
    if (window.jQuery) window.jQuery(area).trigger('change');
    area.focus();
  });
})();
"

#' User interface of the Shiny GLM fitting tool
#'
#' @description Builds the pages of the GLM fitting tool: Welcome, Data, Model,
#'   Actual vs predicted, Validation and Save & load, with the theme switch.
#'
#' @details A function of the request, so that it is built when the app starts,
#'   after every helper of the package is defined.
#' @param request The request, supplied by shiny.
#' @return The user interface of the application, a bslib navbar page.
#' @keywords internal
GLMFittingToolUI <- function(request) {
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", icon("chart-line")),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "NetGLM"),
        tags$span(class = "sim-brand-subtitle", "GLM fitting by NetSimR")
      )
    ),
    window_title = "NetGLM \u2013 GLM Fitting Tool",
    id = "main_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#042f2e",
      theme = "dark",
      underline = FALSE
    ),

    theme = netsimr_bs_theme("#0d9488", code = TRUE),

    header = tagList(
      netsimr_tool_head("netsimr-glm-fitting-theme", netsimr_css(c("tools", "analysis", "glm")), code = TRUE, amount_inputs = FALSE),
      #tells the server which theme is showing, so the chart is drawn in matching colours
      tags$script(HTML(netsimr_theme_input_js("app_theme"))),
      tags$script(HTML(glm_formula_chips_js)),
      netsimr_busy_indicators("#2dd4bf", "#0d9488")
    ),

    # -------------------------------------------------------------- Welcome
    bslib::nav_panel(
      title = "Welcome",
      value = "welcome",
      icon = icon("house"),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),

        div(
          class = "sim-hero",
          div(class = "sim-hero-eyebrow", "NetGLM"),
          div(class = "sim-hero-title", "GLM Fitting Tool"),
          p(
            class = "sim-hero-text",
            "Import data from a file or a database, fit generalised linear models, ",
            "compare formulas side by side and check actual against predicted ",
            "values across any explanatory variable."
          ),
          div(class = "d-flex flex-wrap gap-2",
              tags$button(
                type = "button",
                class = "btn btn-light",
                onclick = "document.querySelector('.navbar a[data-value=\"data\"]').click();",
                icon("upload"), " Import data"
              ),
              actionButton("try_example", "Try an example", icon = icon("wand-magic-sparkles"), class = "btn btn-outline-light"))
        ),

        bslib::card(
          netsimr_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Import"),
                          div(class = "sim-step-text", "A CSV file, or a query on SQLite, SQL Server or PostgreSQL."))),
              tags$li(div(div(class = "sim-step-title", "Fit"),
                          div(class = "sim-step-text", "Choose the response, family, link, offset, weights and formula."))),
              tags$li(div(div(class = "sim-step-title", "Compare"),
                          div(class = "sim-step-text", "Store two models and compare their AIC, or let stepwise selection suggest the terms."))),
              tags$li(div(div(class = "sim-step-title", "Validate"),
                          div(class = "sim-step-text", "Hold out rows for testing: deviance, Gini, lift and double lift charts."))),
              tags$li(div(div(class = "sim-step-title", "Check and export"),
                          div(class = "sim-step-text", "Actual against predicted, the rate table, and scores for a new file.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("database"), "Data sources"),
          div(class = "sim-stat-value", paste(c("CSV", "SQLite", "SQL Server", "PostgreSQL"),
                                              collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("sitemap"), "Model families"),
          div(class = "sim-stat-value", paste(c("Gaussian", "Poisson", "Binomial", "Gamma", "Inverse Gaussian", "Tweedie"),
                                              collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("file-export"), "Exports"),
          div(class = "sim-stat-value", paste(c("Model (RDS)", "Rate table (CSV)", "Scored data (CSV)"),
                                              collapse = paste0(" ", intToUtf8(183), " ")))
        )
      ),

      netsimr_about_card("GLM fitting tool", tagList(
        "The Tweedie family of the tool, ", tags$code("tweedie_family()"), ", can also be used with ", tags$code("glm()"), " in R."))
    ),

    # ----------------------------------------------------------------- Data
    bslib::nav_panel(
      title = "Data",
      value = "data",
      icon = icon("table"),

      netsimr_page_intro("Data", "Import a CSV or Excel file, an example, or the result of a database query."),

      netsimr_layout(
        netsimr_settings_card(
          "file-import", "Import", "Choose where the data comes from.",
          div(class = "sim-section-label", "Source"),
          div(class = "pill-radio",
              #(a web browser cannot reach a database)
              radioButtons("data_source", NULL,
                           c("CSV or Excel file" = "CSV File", if (!netsimr_in_browser()) c("Database" = "Database"),
                             "Example" = "Example"),
                           "CSV File", inline = TRUE)),
          conditionalPanel(
            condition = "input.data_source == 'Example'",
            netsimr_help("10,000 simulated motor policies (motor_policies): age, region, vehicle, its value, exposure, ",
                     "and the year's claim count and cost.")
          ),

          conditionalPanel(
            condition = "input.data_source == 'CSV File'",
            fileInput("csv_file", "File", accept = netsimr_data_accept, placeholder = "No file selected"),
            netsimr_private_note(),
            netsimr_file_options("csv_header", "csv_sep", "csv_dec", "csv_quote")
          ),

          conditionalPanel(
            condition = "input.data_source == 'Database'",
            selectInput("db_type", "Database type", choices = c("SQLite", "SQL Server", "PostgreSQL")),
            conditionalPanel(
              condition = "input.db_type != 'SQLite'",
              textInput("db_host", "Host", "localhost")
            ),
            textInput("db_name", "Database name (the file path for SQLite)", "public"),
            conditionalPanel(
              condition = "input.db_type == 'PostgreSQL'",
              textInput("db_port", "Port (optional)", "", placeholder = "Default port")
            ),
            conditionalPanel(
              condition = "input.db_type == 'SQL Server'",
              bslib::input_switch("windows_auth", "Windows authentication", value = FALSE)
            ),
            conditionalPanel(
              condition = "input.db_type != 'SQLite' && (input.db_type != 'SQL Server' || !input.windows_auth)",
              textInput("db_user", "User", ""),
              passwordInput("db_password", "Password", "")
            ),
            textAreaInput("sql_query", "SQL query", "SELECT * FROM your_table", rows = 5, width = "100%"),
            netsimr_help("The password is used for this connection only; it is never saved with the settings.")
          ),

          actionButton("submit", "Import data", icon = icon("file-import"), class = "btn-run mt-2")
        ),
        uiOutput("data_overview"),
        bslib::card(
          netsimr_card_header("table-list", "Preview", "The first rows of the imported data."),
          bslib::card_body(uiOutput("selected_input_data_table"))
        )
      )
    ),

    # ---------------------------------------------------------------- Model
    bslib::nav_panel(
      title = "Model",
      value = "model",
      icon = icon("sliders"),

      netsimr_page_intro("Model", "Fit a generalised linear model to the imported data."),

      netsimr_layout(
        tagList(
          netsimr_settings_card(
            "sliders", "Model", "Response, distribution and formula.",
            netsimr_column_select("response_variable", "Response"),
            div(
              class = "param-grid-2",
              selectInput("glm_distribution", "Family",
                          c("Gaussian" = "gaussian", "Poisson" = "poisson", "Binomial" = "binomial",
                            "Gamma" = "Gamma", "Inverse Gaussian" = "inverse.gaussian", "Tweedie" = "tweedie"),
                          selected = "gaussian"),
              selectInput("link_function", "Link", glm_family_links$gaussian, selected = "identity")
            ),
            conditionalPanel(
              condition = "input.glm_distribution == 'tweedie'",
              numericInput("tweedie_power", "Variance power", value = 1.5, min = 1.05, max = 1.95, step = 0.05),
              netsimr_help("Between 1 and 2: a compound Poisson-Gamma model of the loss cost, zero for most rows. ",
                       "Near 1 it behaves like a model of the claim count, near 2 like a model of the amount. ",
                       "Tweedie models have no AIC: compare them by their test deviance on the Validation tab.")
            ),
            div(
              class = "param-grid-2",
              selectInput("offset", "Offset", choices = c("None")),
              selectInput("weights", "Weights", choices = c("None"))
            ),
            conditionalPanel(
              condition = "input.offset != 'None'",
              bslib::input_switch("offset_log", "Offset is the log of this column", value = TRUE),
              netsimr_help("With a log link, the offset of an exposure column is its log. ",
                       "Turn this off if the column already holds the offset.")
            ),
            textAreaInput("formula", "Explanatory terms", rows = 4, width = "100%",
                          placeholder = "e.g. age + region + log(sum_insured)"),
            uiOutput("formula_lhs"),
            uiOutput("formula_columns", class = "glm-chips"),
            netsimr_help("Click a column to add it. Use + between terms, * for interactions (age * region), ",
                     "and 1 for an intercept-only model. A smooth curve: ns(age, df = 4); bands with breaks you choose: ",
                     "cut(age, c(17, 25, 35, 50, 65, 100)), which predicts new data in the same bands. ",
                     "The terms may only use the data's columns, numbers, ",
                     "+ - * : / ^ and ", glm_formula_functions_text(), "; nothing else is run."),
            div(class = "sim-section-label mt-2", "Hold out for testing"),
            div(class = "pill-radio", radioButtons("holdout_share", NULL, glm_holdout_choices, "0", inline = TRUE)),
            conditionalPanel(
              condition = "input.holdout_share != '0'",
              numericInput("holdout_seed", "Seed", value = 1, min = 1, step = 1),
              netsimr_help("The model is fitted to the other rows; the Validation tab measures it on the rows held out. ",
                       "The same seed holds out the same rows.")
            ),
            actionButton("fit_model", "Fit model", icon = icon("play"), class = "btn-run mt-3")
          ),
          bslib::card(
            class = "mt-3",
            netsimr_card_header("download", "Downloads", "For the fitted model."),
            bslib::card_body(uiOutput("model_downloads"))
          )
        ),
        uiOutput("model_stats"),
        netsimr_result_tabs(
          id = "model_results_tabs",
          bslib::nav_panel("Coefficients", uiOutput("coefficients_table")),
          bslib::nav_panel("Summary", verbatimTextOutput("model_summary"))
        ),
        bslib::card(
          netsimr_card_header("layer-group", "Stored models", "Store the fitted model in a slot to compare it with another."),
          bslib::card_body(
            div(
              class = "glm-slots",
              lapply(1:2, function(i) {
                div(
                  class = "glm-slot",
                  div(class = "glm-slot-title", paste("Model", i)),
                  div(class = "glm-slot-body", uiOutput(paste0("aic_output_", i))),
                  div(
                    class = "glm-slot-buttons",
                    actionButton(paste0("save_formula_", i), "Store fitted", icon = icon("floppy-disk"), class = "btn-outline-glm"),
                    actionButton(paste0("load_formula_", i), "Load", icon = icon("arrow-rotate-left"), class = "btn-outline-glm")
                  )
                )
              })
            )
          )
        ),
        bslib::card(
          netsimr_card_header("wand-magic-sparkles", "Variable selection",
                          "Stepwise by AIC or BIC, or penalised: the lasso, the elastic net or ridge."),
          bslib::card_body(
            div(class = "sim-section-label", "Method"),
            div(class = "pill-radio",
                radioButtons("select_criterion", NULL,
                             c("Stepwise AIC" = "AIC", "Stepwise BIC" = "BIC", "Lasso" = "lasso",
                               "Elastic net" = "elastic", "Ridge" = "ridge"), "AIC", inline = TRUE)),
            selectizeInput("select_candidates", "Columns it may add", choices = NULL, multiple = TRUE,
                           options = list(placeholder = "Every column that can be a predictor")),
            actionButton("run_selection", "Suggest terms", icon = icon("wand-magic-sparkles"), class = "btn-outline-glm"),
            div(class = "mt-3", uiOutput("selection_result"))
          )
        )
      )
    ),

    # ---------------------------------------------------- Actual vs predicted
    bslib::nav_panel(
      title = "Actual vs predicted",
      value = "visualisation",
      icon = icon("chart-column"),

      netsimr_page_intro("Actual vs predicted",
                     "Compare the actual and predicted response across the bands of an explanatory variable."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "Choose the variable to band by.",
          selectInput("visualize_variable", "Explanatory variable", choices = c("None")),
          sliderInput("number_of_bands_input", "Number of bands for numeric variables",
                      min = glm_band_range[1], max = glm_band_range[2], value = 10),
          div(class = "sim-section-label", "Bands"),
          div(class = "pill-radio",
              radioButtons("band_method", NULL, c("Equal counts" = "quantile", "Equal width" = "width"),
                           "quantile", inline = TRUE)),
          uiOutput("avp_rows_ui"),
          actionButton("execute_visualization", "Draw chart", icon = icon("chart-column"), class = "btn-run mt-2"),
          netsimr_help("Numeric variables with more distinct values than bands are grouped into bands. ",
                   "With an offset, the chart compares rates per unit of exposure; with weights, weighted averages. ",
                   "The bars show the exposure, the weights, or the number of rows.")
        ),
        bslib::card(
          netsimr_card_header("chart-column", "Actual vs predicted", "Per band of the explanatory variable, for the fitted model."),
          bslib::card_body(netsimr_plot_output("fitness_plot", height = "520px"), uiOutput("fitness_note"))
        )
      )
    ),

    # ----------------------------------------------------------- Validation
    bslib::nav_panel(
      title = "Validation",
      value = "validation",
      icon = icon("bullseye"),

      netsimr_page_intro("Validation",
                     "How well the model ranks and predicts the rows, above all the rows it was not fitted to."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "Choose the rows and the bands.",
          div(class = "sim-section-label", "Rows"),
          div(class = "pill-radio",
              radioButtons("validation_rows", NULL, c("Test" = "test", "Training" = "training", "All" = "all"),
                           "test", inline = TRUE)),
          selectInput("validation_bands", "Bands", c("5" = "5", "10" = "10", "20" = "20"), selected = "10"),
          selectInput("validation_compare", "Double lift against", c("None" = "none", "Model 1" = "1", "Model 2" = "2")),
          netsimr_help("Hold out rows on the Model tab to test the model on rows it has not seen. ",
                   "The double lift compares this model with a stored one, fitted again to the same rows.")
        ),
        uiOutput("validation_stats"),
        netsimr_result_tabs(
          id = "validation_tabs",
          bslib::nav_panel("Lift", netsimr_plot_output("lift_plot", height = "460px"), uiOutput("lift_note")),
          bslib::nav_panel("Lorenz curve", netsimr_plot_output("lorenz_plot", height = "460px")),
          bslib::nav_panel("Double lift", netsimr_plot_output("double_lift_plot", height = "460px"), uiOutput("double_lift_note"))
        )
      )
    ),

    # ---------------------------------------------------------------- Rates
    bslib::nav_panel(
      title = "Rates",
      value = "rates",
      icon = icon("tags"),

      netsimr_page_intro("Rates",
                     "The fitted model as a base rate and a table of relativities, and its predictions for a new file."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Rating table", "Of the model on the Model tab.",
          selectInput("rate_factor", "Rating factor", choices = NULL),
          numericInput("rate_values", "Values shown for a number", value = 100, min = 5, max = 500, step = 5),
          div(class = "d-flex flex-wrap gap-2 mt-1",
              downloadButton("download_rate_table", "Rate table (CSV)", class = "btn-outline-glm"),
              netsimr_excel_button("download_rates_excel", "Rate table (Excel)", class = "btn-outline-glm")),
          netsimr_help("Each rating factor is a column of the data, or two for an interaction, with every term that uses it: ",
                   "age and ns(age, 4), say. Its base value, at relativity 1, is the most common level, or the value ",
                   "nearest the median. The CSV has the base rate and every rating factor, ready for a rating engine."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Score new data"),
          fileInput("score_file", "File of policies", accept = netsimr_data_accept, placeholder = "No file selected"),
          netsimr_private_note(),
          uiOutput("score_result"),
          netsimr_help("Read with the file options of the Data tab.")
        ),
        uiOutput("rate_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Relativities", "The chosen rating factor, at each of its values."),
          bslib::card_body(uiOutput("rate_table_ui"))
        ),
        bslib::card(
          netsimr_card_header("chart-column", "Chart", "The relativity of each value of the rating factor."),
          bslib::card_body(netsimr_plot_output("rate_plot", height = "420px"))
        )
      )
    ),

    # --------------------------------------------------------- Save and load
    bslib::nav_panel(
      title = "Save & load",
      value = "settings",
      icon = icon("floppy-disk"),

      netsimr_page_intro("Save & load", "Keep the settings of a session and restore them later."),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        bslib::card(
          netsimr_card_header("floppy-disk", "Settings file", "Import options, model choices and the formula."),
          bslib::card_body(
            downloadButton("DownloadDataHandlerConf", "Save settings", class = "btn-run"),
            checkboxInput("settings_include_db",
                          "Include database connection details (server, user, database and query; never the password)",
                          value = FALSE),
            netsimr_help("Leave this unticked to share the file: it then names no server, database or table."),
            tags$hr(class = "sim-divider"),
            fileInput("load_config", "Load settings", accept = ".txt", placeholder = "No file selected"),
            netsimr_help("The settings are a text file. Database passwords and uploaded files are not saved.")
          )
        ),
        bslib::card(
          netsimr_card_header("circle-question", "Restoring a session"),
          bslib::card_body(
            tags$ol(
              class = "glm-steps-inline",
              tags$li("Load the settings file: the import options are restored."),
              tags$li("Import the same data again, from the file or the database."),
              tags$li("The response, offset, weights and variable choices from the settings file are selected once the columns are available.")
            )
          )
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(netsimr_theme_switch())
  )
}
