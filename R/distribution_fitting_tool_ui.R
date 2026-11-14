#' Run button for an analysis tab
#'
#' @noRd
dft_run_button <- function(id) {
  actionButton(id, "Run analysis", icon = icon("play"), class = "btn-run")
}

#' Chart options switch for a logarithmic claim size axis
#'
#' On by default: claim sizes are heavy tailed, and on a linear axis the fitted
#' curves are squeezed against the left edge.
#' @noRd
dft_log_switch <- function(id) {
  bslib::input_switch(id, "Log scale for claim size", value = TRUE)
}

#' Slider for a slicing point, with a box beside it where the value can be typed
#'
#' The slider is without tick labels: once it covers the claims, from a small
#' claim to a large one, the labels near the smallest overlap. The box lets a
#' point be set exactly, or beyond the range of the slider, which stops at the
#' 99.5th percentile of the claims; its label is for screen readers only. The
#' server keeps the two in step.
#' @noRd
dft_slicing_point <- function(id, label, max, value) {
  div(
    class = "dft-slice-point",
    sliderInput(id, label, min = 0, max = max, value = value, ticks = FALSE),
    div(class = "dft-slice-typed",
        netsimr_hidden_label(numericInput(paste0(id, "_typed"), paste(label, "as a number"), value = NA, width = "100%")))
  )
}

#' User interface of the Shiny distribution fitting tool
#'
#' @description The page of the distribution fitting application: a welcome
#'   tab, the data upload and one tab for each analysis, with the settings
#'   beside the results; \code{\link{run_shiny_distribution_fitting_tool}} pairs
#'   it with \code{distribution_fitting_tool_Server}. The page is built each time
#'   the application starts, not when the package is installed: its theme
#'   compiles Bootstrap's SCSS files from where bslib is installed on the machine
#'   running it, and a page built at installation kept the paths of the machine
#'   that built the package (for a CRAN binary, CRAN's build machine).
#' @param request The request of the browser, passed by shiny; not used.
#' @return The user interface of the application, a bslib navbar page.
#' @keywords internal
distribution_fitting_tool_UI <- function(request) {
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", icon("chart-area")),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "NetDisFit"),
        tags$span(class = "sim-brand-subtitle", "Distribution fitting by NetSimR")
      )
    ),
    window_title = "NetDisFit \u2013 Distribution Fitting Tool",
    id = "dft_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#1e1b4b",
      theme = "dark",
      underline = FALSE
    ),

    theme = netsimr_bs_theme("#7c3aed"),

    header = tagList(
      netsimr_tool_head("netsimr-distribution-fitting-theme", netsimr_css(c("tools", "analysis"))),
      #tells the server which theme is showing, so the charts are drawn in matching colours
      tags$script(HTML(netsimr_theme_input_js("app_theme"))),
      netsimr_busy_indicators("#a78bfa", "#7c3aed")
    ),

    # ---------------------------------------------------------------- Welcome
    bslib::nav_panel(
      title = "Welcome",
      value = "welcome",
      icon = icon("house"),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),

        div(
          class = "sim-hero",
          div(class = "sim-hero-eyebrow", "NetDisFit"),
          div(class = "sim-hero-title", "Distribution Fitting Tool"),
          p(
            class = "sim-hero-text",
            "Upload claims data, fit frequency and severity distributions, ",
            "compare them against the data and splice Pareto tails onto the ",
            "severity fit, ready to use in the NetSimR simulator."
          ),
          div(class = "d-flex flex-wrap gap-2",
              tags$button(
                type = "button",
                class = "btn btn-light",
                onclick = "document.querySelector('.navbar a[data-value=\"data\"]').click();",
                icon("upload"), " Upload data"
              ),
              actionButton("try_example", "Try an example", icon = icon("wand-magic-sparkles"), class = "btn btn-outline-light"))
        ),

        bslib::card(
          netsimr_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Upload"),
                          div(class = "sim-step-text", "A CSV or text file with one row per claim or period."))),
              tags$li(div(div(class = "sim-step-title", "Choose a column"),
                          div(class = "sim-step-text", "Pick the claim counts or claim sizes to fit."))),
              tags$li(div(div(class = "sim-step-title", "Run the analysis"),
                          div(class = "sim-step-text", "Fitted parameters and goodness of fit appear at once."))),
              tags$li(div(div(class = "sim-step-title", "Compare"),
                          div(class = "sim-step-text", "Check each fit against the empirical distribution.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("chart-column"), "Frequency models"),
          div(class = "sim-stat-value", paste("Poisson", "Negative Binomial", "Zero-inflated", sep = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("chart-line"), "Severity models"),
          div(class = "sim-stat-value",
              paste(c("Normal", "LogNormal", "Exponential", "Gamma", "Pareto", "Weibull", "Burr", "Generalised Pareto"),
                    collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("scissors"), "Tail models"),
          div(class = "sim-stat-value",
              paste(c("Sliced LogNormal-Pareto", "Piecewise Pareto", "Hill plot"), collapse = paste0(" ", intToUtf8(183), " ")))
        )
      ),

      netsimr_about_card("Distribution fitting tool", tagList(
        "Its fits go to the claims simulator from the Export tab, or to ", tags$code("simulate_claims()"), " in R."))
    ),

    # ------------------------------------------------------------------- Data
    bslib::nav_panel(
      title = "Data",
      value = "data",
      icon = icon("table"),

      netsimr_page_intro("Data", "Upload a CSV, text or Excel file, or choose an example. Every analysis tab reads its columns from it."),

      netsimr_layout(
        div(class = "dft-settings-stack", netsimr_settings_card(
          "file-arrow-up", "Data file", "CSV or text (comma, semicolon or tab separated), or Excel.",
          div(class = "sim-section-label", "Source"),
          div(class = "pill-radio",
              netsimr_hidden_label(radioButtons("data_source", "Source", c("Upload a file" = "upload", "Example" = "example"),
                                            "upload", inline = TRUE))),
          conditionalPanel(
            condition = "input.data_source == 'example'",
            selectInput("example", "Example", choices = dft_examples, selectize = FALSE),
            netsimr_help("Simulated data sets of the package: motor_policies and liability_claims.")
          ),
          conditionalPanel(
            condition = "input.data_source != 'example'",
            fileInput("file1", "File", accept = netsimr_data_accept, placeholder = "No file selected"),
            netsimr_private_note(),
            netsimr_file_options("data_includes_header", "sep", "dec", "quote")
          )
        ),
        netsimr_settings_card(
          "floppy-disk", "Settings file", "Keep this session's choices and restore them later.",
          div(class = "dft-downloads", downloadButton("save_settings", "Save settings", class = "btn-outline-dft")),
          fileInput("load_settings", "Load settings", accept = ".txt", placeholder = "No file selected"),
          netsimr_help("The file keeps the data options, each tab's columns and options, the slicing points and thresholds,",
                   "and the models chosen on the Export tab. An uploaded data file is not kept: upload it again, and its",
                   "columns are chosen as they were. Run each analysis again to fit with the loaded settings.")
        )),
        uiOutput("data_overview"),
        bslib::card(
          netsimr_card_header("table-list", "Preview", "The first rows of the uploaded data, as the analysis tabs read them."),
          bslib::card_body(
            uiOutput("data_table")
          )
        )
      )
    ),

    # -------------------------------------------------------------- Frequency
    bslib::nav_panel(
      title = "Frequency",
      value = "frequency",
      icon = icon("chart-column"),

      netsimr_page_intro("Frequency analysis", "Fit Poisson and Negative Binomial distributions, plain and zero-inflated, to claim counts."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "Choose the claim counts and run the fit.",
          netsimr_column_select("counts_var", "Claim counts column"),
          bslib::input_switch("counts_weighted_var", "Weighted fit", value = FALSE),
          conditionalPanel(
            condition = "input.counts_weighted_var",
            netsimr_column_select("counts_weights_var", "Weights column")
          ),
          dft_run_button("execute_freq_analysis"),
          netsimr_help("Rows with a missing, negative or non-numeric count (or weight) are left out."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Chart options"),
          sliderInput("count_hist_bins", "Histogram bins", min = 1, max = 100, value = 20)
        ),
        uiOutput("freq_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Fitted distributions",
                          "Maximum likelihood fits; the lower the AIC, the better the fit."),
          bslib::card_body(uiOutput("selected_freq_params"))
        ),
        netsimr_result_tabs(
          id = "freq_results_tabs",
          bslib::nav_panel("CDF fit", netsimr_plot_output("freq_fit_plot")),
          bslib::nav_panel("Histogram", netsimr_plot_output("count_hist")),
          bslib::nav_panel(
            "Fit details",
            div(class = "pill-radio mb-3",
                radioButtons("FreqDistri", NULL,
                             c("Poisson" = "Poisson", "Negative Binomial" = "NegativeBinomial",
                               "Zero-inflated Poisson" = "ZIPoisson", "Zero-inflated Negative Binomial" = "ZINegativeBinomial"),
                             inline = TRUE)),
            verbatimTextOutput("selected_distribution_summary")
          ),
          bslib::nav_panel("Data summary", verbatimTextOutput("freq_summary"))
        )
      )
    ),

    # --------------------------------------------------------------- Severity
    bslib::nav_panel(
      title = "Severity",
      value = "severity",
      icon = icon("chart-line"),

      netsimr_page_intro("Severity analysis", paste("Fit Normal, LogNormal, Exponential, Gamma, Pareto, Weibull, Burr and generalised",
                                                "Pareto distributions to claim sizes.")),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "Choose the claim sizes and run the fit.",
          netsimr_column_select("severity_var", "Claim size column"),
          netsimr_amount_input("sev_threshold", "Reporting threshold (optional)", min = 0),
          div(class = "sim-section-label mt-2", "Policy limits"),
          div(class = "pill-radio",
              netsimr_hidden_label(radioButtons("sev_limit_source", "Policy limits",
                                            c("None" = "none", "One limit" = "value", "A column" = "column"),
                                            "none", inline = TRUE))),
          conditionalPanel(
            condition = "input.sev_limit_source == 'value'",
            netsimr_amount_input("sev_limit", "Policy limit", min = 0)
          ),
          conditionalPanel(
            condition = "input.sev_limit_source == 'column'",
            netsimr_column_select("sev_limit_col", "Policy limit column")
          ),
          dft_run_button("execute_sev_analysis"),
          netsimr_help("Missing, negative, zero and non-numeric values are left out."),
          netsimr_help("For claims that are only reported above a threshold, enter it: the claims at or below it are left out,",
                   "and each distribution is fitted to the claims above it as the claims from zero, of which the data",
                   "holds only those above the threshold."),
          netsimr_help("A claim paid at its policy limit hides how large the loss was: with the limits, such claims count as",
                   "losses of at least the limit (censored), so the fitted tail is not cut short at the limits."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Chart options"),
          dft_log_switch("sev_fit_log_scale"),
          sliderInput("severity_hist_bins", "Histogram bins", min = 1, max = 200, value = 20)
        ),
        uiOutput("sev_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Fitted distributions",
                          "The lower the Kolmogorov-Smirnov distance or the Anderson-Darling statistic, the closer the fit."),
          bslib::card_body(uiOutput("sev_param_summary"))
        ),
        netsimr_result_tabs(
          id = "sev_results_tabs",
          bslib::nav_panel("CDF fit", netsimr_plot_output("sev_fit_plot")),
          bslib::nav_panel("Q-Q plot", netsimr_plot_output("sev_qq_plot")),
          bslib::nav_panel("P-P plot", netsimr_plot_output("sev_pp_plot")),
          bslib::nav_panel("Tail (CTE)", uiOutput("sev_cte_table")),
          bslib::nav_panel("Hill plot", netsimr_plot_output("sev_hill_plot"), uiOutput("sev_hill_note")),
          bslib::nav_panel("Histogram", netsimr_plot_output("sev_hist")),
          bslib::nav_panel("Data summary", verbatimTextOutput("sev_summary"))
        )
      )
    ),

    # -------------------------------------------------------- Sliced severity
    bslib::nav_panel(
      title = "Sliced severity",
      value = "sliced",
      icon = icon("scissors"),

      netsimr_page_intro("Sliced severity analysis",
                     "Splice one or two Pareto tails onto a LogNormal body at chosen slicing points."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "Choose the claim sizes and the slicing points.",
          netsimr_column_select("sliced_sev_var", "Claim size column"),
          dft_run_button("execute_sliced_sev_analysis"),
          netsimr_help("Missing, negative, zero and non-numeric values are left out."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Slicing points"),
          dft_slicing_point("slicing_point_left", "First slicing point", max = 10, value = 5),
          dft_slicing_point("slicing_point_right", "Second slicing point", max = 20, value = 10),
          netsimr_help("Once the analysis has run, the sliders cover the claims up to the 99.5th percentile; ",
                   "a point beyond that, or an exact value, can be typed in the box beside the slider. ",
                   "The second point must be above the first."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Chart options"),
          dft_log_switch("sev_cens_fit_log_scale")
        ),
        bslib::card(
          netsimr_card_header("table-cells", "Fitted distributions",
                          "Body and tail parameters, with the Kolmogorov-Smirnov distance of each model."),
          bslib::card_body(uiOutput("slc_sev_fitted_param_summary"))
        ),
        netsimr_result_tabs(
          id = "sliced_results_tabs",
          bslib::nav_panel("CDF fit", netsimr_plot_output("sliced_sev_cdf_plot")),
          bslib::nav_panel("Mean excess", netsimr_plot_output("mean_excess_func_plot"))
        )
      )
    ),

    # ------------------------------------------------------- Piecewise Pareto
    bslib::nav_panel(
      title = "Piecewise Pareto",
      value = "piecewise",
      icon = icon("layer-group"),

      netsimr_page_intro("Piecewise Pareto fit",
                     "Fit a Pareto alpha between each pair of thresholds, from the smallest claim upwards."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "Choose the claim sizes and the thresholds.",
          netsimr_column_select("piecewise_pareto_var", "Claim size column"),
          dft_run_button("execute_piecewise_sev_analysis"),
          netsimr_help("Missing, negative, zero and non-numeric values are left out."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Thresholds"),
          sliderInput("num_pareto_slices", "Number of thresholds", min = 1, max = 6, value = 3, step = 1),
          div(class = "dft-sliders", uiOutput("pareto_slider_inputs")),
          netsimr_help("The first threshold is always the smallest claim. Thresholds must increase."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Chart options"),
          dft_log_switch("piecewise_pareto_fit_log_scale")
        ),
        uiOutput("piecewise_pareto_ks_test"),
        bslib::card(
          netsimr_card_header("table-cells", "Fitted alphas", "One Pareto alpha for each layer between thresholds."),
          bslib::card_body(uiOutput("fitted_sliced_pareto"))
        ),
        bslib::card(
          netsimr_card_header("chart-line", "CDF fit", "Empirical against fitted cumulative distribution."),
          bslib::card_body(netsimr_plot_output("piecewise_pareto_cdf_plot"))
        )
      )
    ),

    # ------------------------------------------------------- To the simulator
    bslib::nav_panel(
      #(a short title: with a longer one the tabs squeezed the tool's name in the navbar)
      title = "Export",
      value = "export",
      icon = icon("paper-plane"),

      netsimr_page_intro("Send a fit to the simulator",
                     paste("Save fitted models as a settings file that NetSimulator and NetTreaty's Programmes tab load,",
                           "or the fits as a report.")),

      netsimr_layout(
        div(class = "dft-settings-stack", netsimr_settings_card(
          "sliders", "Settings", "Choose the fitted models to send.",
          selectInput("export_frequency", "Frequency",
                      choices = c("Leave the simulator's" = "none", names(dft_export_frequency_models)),
                      selectize = FALSE),
          selectInput("export_severity", "Severity",
                      choices = c("Leave the simulator's" = "none", names(dft_export_severity_models)),
                      selectize = FALSE),
          netsimr_help("Each model is the one fitted on its tab, with that tab's columns and slicing points:",
                   "run the analysis there first."),
          uiOutput("export_threshold_option"),
          uiOutput("export_download")
        ),
        netsimr_settings_card(
          "file-lines", "Report", "The fits of every analysis run, as one HTML page.",
          uiOutput("report_download")
        )),
        bslib::card(
          netsimr_card_header("table-cells", "Settings to send", "What the file sets in the simulator."),
          bslib::card_body(uiOutput("export_summary"))
        ),
        bslib::card(
          netsimr_card_header("route", "Where the file goes"),
          bslib::card_body(
            p(tags$b("NetSimulator", .noWS = "after"), ": open the claims simulator with ", tags$code("run_shiny_simulator()"),
              " and, in its Simulation card, load the file with ", tags$b("Load settings", .noWS = "after"), "."),
            p(tags$b("NetTreaty", .noWS = "after"), ": open the reinsurance pricing tool with ",
              tags$code("run_shiny_reinsurance_pricing_tool()"), " and, on its Programmes tab, load the file with ",
              tags$b("Load a fitted model", .noWS = "after"), ": the reinsurance programmes are then compared on the ",
              "fitted claims."),
            p(class = "sim-muted mb-0",
              "The settings the file leaves out, such as the number of simulations and the layers, keep the values ",
              "they have in the tool. A sliced or piecewise Pareto model becomes Pareto slices, a tail above the ",
              "slicing points.")
          )
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(netsimr_theme_switch())
  )
}
