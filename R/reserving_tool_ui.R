#' User interface of the Shiny reserving tool
#'
#' @description The page of the reserving application: a welcome tab, the data,
#'   the development factors, the ultimates of each method and the uncertainty of
#'   the reserves, with the settings beside the results;
#'   \code{\link{run_shiny_reserving_tool}} pairs it with
#'   \code{reserving_tool_Server}. Like the package's other applications, the page
#'   is built each time the application starts, not when the package is
#'   installed, so that its theme compiles from where bslib is installed on the
#'   machine running it.
#' @param request The request of the browser, passed by shiny; not used.
#' @return The user interface of the application, a bslib navbar page.
#' @keywords internal
reserving_tool_UI <- function(request) {
  dot <- paste0(" ", intToUtf8(183), " ")
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", icon("hourglass-half")),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "NetReserve"),
        tags$span(class = "sim-brand-subtitle", "Claims reserving by NetSimR")
      )
    ),
    window_title = "NetReserve \u2013 Claims Reserving Tool",
    id = "rsv_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#431407",
      theme = "dark",
      underline = FALSE
    ),

    theme = netsimr_bs_theme("#c2410c"),

    header = tagList(
      netsimr_tool_head("netsimr-reserving-theme", netsimr_css(c("tools", "analysis", "reserving")), tags$script(HTML(rsv_ratio_js))),
      #tells the server which theme is showing, so the charts are drawn in matching colours
      tags$script(HTML(netsimr_theme_input_js("app_theme"))),
      netsimr_busy_indicators("#fdba74", "#ea580c")
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
          div(class = "sim-hero-eyebrow", "NetReserve"),
          div(class = "sim-hero-title", "Claims Reserving Tool"),
          p(
            class = "sim-hero-text",
            "Load a claims triangle, choose its development factors and tail, compare the chain ladder, ",
            "Bornhuetter-Ferguson, Benktander and Cape Cod ultimates and loss ratios of each origin, and ",
            "measure the uncertainty of the reserves with Mack's method and the bootstrap."
          ),
          tags$button(
            type = "button",
            class = "btn btn-light",
            onclick = "document.querySelector('.navbar a[data-value=\"data\"]').click();",
            icon("table"), " Load data"
          )
        ),

        bslib::card(
          netsimr_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Load a triangle"),
                          div(class = "sim-step-text", "An example, or a file of paid or incurred claims."))),
              tags$li(div(div(class = "sim-step-title", "Select the factors"),
                          div(class = "sim-step-text", "Choose the average, leave out link ratios, add a tail."))),
              tags$li(div(div(class = "sim-step-title", "Choose the ultimates"),
                          div(class = "sim-step-text", "Compare the methods, origin by origin, with premiums."))),
              tags$li(div(div(class = "sim-step-title", "Measure the uncertainty"),
                          div(class = "sim-step-text", "Mack's standard errors and a bootstrap distribution.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("table-cells"), "Inputs"),
          div(class = "sim-stat-value", paste(c("Examples", "Long or wide tables", "Paid or incurred", "Premiums"), collapse = dot))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("calculator"), "Methods"),
          div(class = "sim-stat-value", paste(c("Chain ladder", "Bornhuetter-Ferguson", "Benktander", "Cape Cod",
                                                "Munich chain ladder"), collapse = dot))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("chart-area"), "Uncertainty"),
          div(class = "sim-stat-value", paste(c("Mack standard errors", "One-year view", "Bootstrap (ODP)",
                                                "Percentiles to 99.5%"), collapse = dot))
        )
      ),

      netsimr_about_card("Reserving tool", tagList(
        "The functions behind the tool, such as ", tags$code("chain_ladder()"), " and ",
            tags$code("bootstrap_chain_ladder()"), ", can also be used from R."))
    ),

    # ------------------------------------------------------------------- Data
    bslib::nav_panel(
      title = "Data",
      value = "data",
      icon = icon("table"),

      netsimr_page_intro("Data", "Load an example triangle, or upload a file of claims by origin and development period."),

      netsimr_layout(
        div(class = "rsv-settings-stack", netsimr_settings_card(
          "file-arrow-up", "Triangle", "Where the claims come from.",
          div(class = "sim-section-label", "Source"),
          div(class = "pill-radio",
              netsimr_hidden_label(radioButtons("data_source", "Source", c("Example" = "example", "Upload a file" = "upload"),
                                            "example", inline = TRUE))),
          conditionalPanel(
            condition = "input.data_source == 'example'",
            selectInput("example", "Example", choices = rsv_examples, selectize = FALSE)
          ),
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput("file1", "File", accept = netsimr_data_accept,
                      placeholder = "No file selected"),
            netsimr_private_note(),
            netsimr_file_options("data_includes_header", "sep", "dec", "quote"),
            tags$hr(class = "sim-divider"),
            div(class = "sim-section-label", "Layout"),
            div(class = "pill-radio",
                netsimr_hidden_label(radioButtons("layout", "Layout", c("A row per cell" = "long", "A row per origin" = "wide"),
                                              "long", inline = TRUE))),
            netsimr_help("A row per cell: one row for each origin and development period (or each payment), with columns for",
                     "the origin, the development period and the amount. A row per origin: a column for each development",
                     "period, in order, empty for the future."),
            netsimr_column_select("origin_col", "Origin column"),
            conditionalPanel(
              condition = "input.layout == 'long'",
              netsimr_column_select("development_col", "Development period column"),
              netsimr_column_select("value_col", "Amount column"),
              selectizeInput("incurred_col", "Incurred column (optional)", choices = NULL,
                             options = list(placeholder = "Upload a data file first")),
              netsimr_help("With paid amounts in the amount column, an incurred column adds the Paid & incurred tab.")
            ),
            selectizeInput("premium_col", "Premium column (optional)", choices = NULL,
                           options = list(placeholder = "Upload a data file first")),
            div(class = "sim-section-label mt-2", "Amounts"),
            div(class = "pill-radio",
                netsimr_hidden_label(radioButtons("cumulative", "Amounts", c("Cumulative" = "cumulative", "Incremental" = "incremental"),
                                              "cumulative", inline = TRUE))),
            netsimr_help("Incremental amounts of the same origin and development period are added up, so payments can be",
                     "uploaded as they are. The origin and development periods must be consecutive.")
          )
        ),
        netsimr_settings_card(
          "floppy-disk", "Settings file", "Keep this session's choices and restore them later.",
          downloadButton("save_settings", "Save settings", class = "btn-outline-rsv"),
          fileInput("load_settings", "Load settings", accept = ".txt", placeholder = "No file selected"),
          netsimr_help("The file keeps the data options, the development factors, the link ratios left out, each origin's",
                   "premium, loss ratio and method, and the bootstrap settings. An uploaded data file is not kept:",
                   "upload it again, and its columns are chosen as they were.")
        )),
        uiOutput("data_overview"),
        netsimr_result_tabs(
          id = "data_tabs",
          bslib::nav_panel("Cumulative triangle", uiOutput("triangle_table")),
          bslib::nav_panel("Incremental", uiOutput("incremental_table")),
          bslib::nav_panel("Data", uiOutput("data_table"))
        )
      )
    ),

    # ------------------------------------------------------------ Development
    bslib::nav_panel(
      title = "Development",
      value = "development",
      icon = icon("arrow-trend-up"),

      netsimr_page_intro("Development factors", "Choose how the link ratios are averaged into factors, and the tail beyond the triangle."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "The factors update as you change them.",
          div(class = "sim-section-label", "Average"),
          div(class = "pill-radio",
              netsimr_hidden_label(radioButtons("average", "Average", c("Volume-weighted" = "volume", "Simple" = "simple",
                                                                    "Regression" = "regression"), "volume", inline = TRUE))),
          selectInput("periods", "Calendar periods used", choices = c("All" = "all"), selectize = FALSE),
          netsimr_help("Click a link ratio in the table to leave it out of its factor, and again to bring it back."),
          actionButton("include_all", "Include all link ratios", icon = icon("rotate-left"), class = "btn-outline-rsv mt-2 mb-3"),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Tail"),
          selectInput("tail_method", "Tail factor", selectize = FALSE,
                      choices = c("None" = "none", "Exponential curve" = "exponential", "Inverse power curve" = "inverse power",
                                  "Enter a factor" = "factor")),
          conditionalPanel(
            condition = "input.tail_method == 'exponential' || input.tail_method == 'inverse power'",
            selectInput("tail_from", "Fit the curve from factor", choices = c("1" = 1), selectize = FALSE),
            numericInput("tail_periods", "Extrapolate over (periods)", value = 100, min = 1, max = 1000, step = 10),
            netsimr_help("The curve is fitted to the factors above 1 from this one on, and extrapolated over these periods.",
                     "The Tail curve tab compares the tail of each curve.")
          ),
          conditionalPanel(
            condition = "input.tail_method == 'factor'",
            numericInput("tail_factor", "Tail factor", value = 1.01, min = 0, step = 0.001)
          ),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Standard errors"),
          div(class = "pill-radio",
              netsimr_hidden_label(radioButtons("sigma", "Sigma of a single link ratio", c("Log-linear" = "log-linear", "Mack's rule" = "Mack"),
                                            "log-linear", inline = TRUE))),
          netsimr_help("How Mack's sigma is found for a factor with one link ratio, such as the last.")
        ),
        uiOutput("development_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Link ratios",
                          "Each origin's ratio of its claims at one period to the period before."),
          bslib::card_body(uiOutput("link_ratio_table"), uiOutput("development_notes"))
        ),
        netsimr_result_tabs(
          id = "development_tabs",
          bslib::nav_panel("Development", netsimr_plot_output("development_plot"),
                           p(class = "dft-table-note", "Each line is an origin, from the oldest (lightest) to the latest (darkest).")),
          bslib::nav_panel("Link ratios", netsimr_plot_output("link_ratio_plot")),
          bslib::nav_panel("Tail curve", netsimr_plot_output("tail_plot"), uiOutput("tail_comparison")),
          bslib::nav_panel("Projected triangle", uiOutput("projected_table"))
        )
      )
    ),

    # -------------------------------------------------------------- Ultimates
    bslib::nav_panel(
      title = "Ultimates",
      value = "ultimates",
      icon = icon("flag-checkered"),

      netsimr_page_intro("Ultimates", "Compare the methods for each origin, and choose the ultimate of each."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "For all origins; change any origin below.",
          numericInput("prior_loss_ratio", "A priori loss ratio (%)", value = 70, min = 0, step = 0.5),
          netsimr_help("The expected loss ratio of the Bornhuetter-Ferguson and Benktander methods. Changing it here sets it",
                   "for every origin."),
          sliderInput("decay", "Cape Cod decay", min = 0, max = 1, value = 1, step = 0.05),
          netsimr_help("1 for one Cape Cod loss ratio for all origins; below 1, each origin's loss ratio weighs the nearby",
                   "origins most."),
          selectInput("method_all", "Method for all origins", choices = rsv_methods, selectize = FALSE),
          netsimr_help("The Bornhuetter-Ferguson, Benktander and Cape Cod methods need premiums: they come from the data's",
                   "premium column, or can be entered below.")
        ),
        uiOutput("ultimate_stats"),
        bslib::card(
          netsimr_card_header("list-check", "By origin", "Each origin's premium, a priori loss ratio and method."),
          bslib::card_body(uiOutput("origin_inputs"))
        ),
        bslib::card(
          netsimr_card_header("table-cells", "Ultimates", "The ultimate of each method, and the one selected."),
          bslib::card_body(
            uiOutput("ultimate_table"),
            div(class = "rsv-downloads",
                downloadButton("download_results", "Results (CSV)", class = "btn-outline-rsv"),
                downloadButton("download_triangle", "Projected triangle (CSV)", class = "btn-outline-rsv"),
                downloadButton("download_report", "Report (HTML)", class = "btn-outline-rsv"),
                netsimr_excel_button("download_excel", class = "btn-outline-rsv"))
          )
        ),
        netsimr_result_tabs(
          id = "ultimate_tabs",
          bslib::nav_panel("Ultimates", netsimr_plot_output("ultimate_plot")),
          bslib::nav_panel("Loss ratios", netsimr_plot_output("loss_ratio_plot"))
        )
      )
    ),

    # --------------------------------------------------------- More methods
    bslib::nav_panel(
      title = "More methods",
      value = "more",
      icon = icon("layer-group"),

      netsimr_page_intro("More methods",
                     "Check the chain ladder against the latest period, fit Clark's growth curves, and allow for claims inflation."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Settings", "For the growth curve and the claims inflation.",
          div(class = "sim-section-label", "Clark's growth curve"),
          selectInput("clark_curve", "Curve", c("Log-logistic" = "loglogistic", "Weibull" = "weibull"), selectize = FALSE),
          div(class = "pill-radio",
              radioButtons("clark_method", NULL, c("LDF" = "ldf", "Cape Cod" = "cape cod"), "ldf", inline = TRUE)),
          numericInput("clark_truncation", "Truncate at development period (empty for none)", value = NA, min = 1, step = 1),
          netsimr_help("The LDF method fits an ultimate to each origin, the Cape Cod method one loss ratio to the premiums.",
                   "The curves can have long tails: a truncation counts the claims as fully developed at that age."),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Claims inflation"),
          div(class = "param-grid-2",
              numericInput("inflation_past", "Past (% a period)", value = 0, step = 0.5),
              numericInput("inflation_future", "Future (% a period)", value = 0, step = 0.5)),
          netsimr_help("Past payments are restated in today's money by the past inflation, developed by the chain ladder,",
                   "and the future payments inflated by the future inflation.")
        ),
        uiOutput("more_stats"),
        netsimr_result_tabs(
          id = "more_tabs",
          bslib::nav_panel("Actual vs expected", uiOutput("ave_table"), netsimr_plot_output("ave_plot")),
          bslib::nav_panel("Clark", uiOutput("clark_table"), netsimr_plot_output("clark_plot")),
          bslib::nav_panel("Claims inflation", uiOutput("inflation_table"))
        )
      )
    ),

    # --------------------------------------------------- IFRS 17 & Solvency II
    bslib::nav_panel(
      title = "IFRS 17 & SII",
      value = "ifrs17",
      icon = icon("building-columns"),

      netsimr_page_intro("IFRS 17 and Solvency II",
                     "The reserves discounted with a yield curve, the risk adjustment, and the standard formula's reserve risk."),

      netsimr_layout(
        div(class = "rsv-settings-stack",
          netsimr_settings_card(
            "percent", "Discounting", "The payments of the chain ladder, discounted.",
            textInput("yield_curve", "Spot rates for 1, 2, 3, ... years (%)", value = "3.0, 3.1, 3.2, 3.3, 3.4"),
            netsimr_help("One rate for a flat curve. Terms between them are interpolated, beyond the last the last rate is used."),
            selectInput("period_length", "Length of a development period", selectize = FALSE,
                        c("A year" = "1", "Half a year" = "0.5", "A quarter" = "0.25", "A month" = "0.0833333333333333")),
            div(class = "sim-section-label", "Payments made"),
            div(class = "pill-radio",
                netsimr_hidden_label(radioButtons("payment_timing", "Payments made", c("Mid-period" = "0.5", "At the start" = "0",
                                                                                    "At the end" = "1"), "0.5", inline = TRUE))),
            netsimr_help("The triangle should be of paid claims: the chain ladder's increments are the future payments.")
          ),
          netsimr_settings_card(
            "scale-balanced", "Risk adjustment", "IFRS 17's risk adjustment, or Solvency II's risk margin.",
            div(class = "pill-radio",
                netsimr_hidden_label(radioButtons("ra_method", "Method", c("Percentile" = "percentile", "Cost of capital" = "cost of capital"),
                                              "percentile", inline = TRUE))),
            conditionalPanel(
              condition = "input.ra_method == 'percentile'",
              numericInput("ra_level", "Confidence level (%)", value = 75, min = 50, max = 99.9, step = 5)
            ),
            conditionalPanel(
              condition = "input.ra_method == 'cost of capital'",
              numericInput("coc_rate", "Cost of capital (% a year)", value = 6, min = 0, step = 0.5),
              div(class = "sim-section-label", "Capital"),
              div(class = "pill-radio",
                  netsimr_hidden_label(radioButtons("ra_capital", "Capital", c("Standard formula" = "sii", "Bootstrap 99.5%" = "bootstrap"),
                                                "sii", inline = TRUE)))
            ),
            selectInput("sii_segment", "Solvency II segment", selectize = FALSE,
                        stats::setNames(sii_nonlife_segments$segment,
                                        paste0(sii_nonlife_segments$segment, ". ", sii_nonlife_segments$name))),
            netsimr_help("The percentile and the confidence level need the bootstrap, run on the Uncertainty tab. The standard",
                     "formula's reserve risk is three times the segment's reserve standard deviation times the discounted",
                     "best estimate, the reserve volume of the segment on its own.")
          )
        ),
        uiOutput("ifrs_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "By origin", "Each origin's reserve, discounted."),
          bslib::card_body(uiOutput("ifrs_table"))
        ),
        netsimr_result_tabs(
          id = "ifrs_tabs",
          bslib::nav_panel("Cash flows", netsimr_plot_output("cash_flow_plot"), uiOutput("cash_flow_table")),
          bslib::nav_panel("Risk adjustment", uiOutput("ra_table"))
        )
      )
    ),

    # -------------------------------------------------------- Paid & incurred
    bslib::nav_panel(
      title = "Paid & incurred",
      value = "munich",
      icon = icon("scale-balanced"),

      netsimr_page_intro("Paid and incurred", "The Munich chain ladder: paid and incurred claims projected together, so that their ultimates agree."),

      netsimr_layout(
        netsimr_settings_card(
          "circle-info", "How it works", "Quarg and Mack's Munich chain ladder.",
          p(class = "mb-2",
            "The chain ladders of paid and of incurred claims often disagree: an origin whose paid claims are low",
            "against its incurred claims is projected low on paid and high on incurred."),
          p(class = "mb-2",
            "The Munich chain ladder corrects each factor by how far the origin's paid to incurred ratio is from",
            "the average of its development period, so the two projections meet."),
          netsimr_help("It uses the volume-weighted factors of every link ratio, without a tail, and the sigma setting",
                   "of the Development tab. It needs the incurred claims: an incurred column on the Data tab, or the",
                   "motor or Quarg and Mack example.")
        ),
        uiOutput("munich_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Ultimates", "The separate chain ladders and the Munich chain ladder, by origin."),
          bslib::card_body(
            uiOutput("munich_table"),
            div(class = "rsv-downloads", downloadButton("download_munich", "Paid & incurred (CSV)", class = "btn-outline-rsv"))
          )
        ),
        netsimr_result_tabs(
          id = "munich_tabs",
          bslib::nav_panel("Ultimates", netsimr_plot_output("munich_plot")),
          bslib::nav_panel("Paid to incurred", netsimr_plot_output("munich_ratio_plot"))
        )
      )
    ),

    # ------------------------------------------------------------ Uncertainty
    bslib::nav_panel(
      title = "Uncertainty",
      value = "uncertainty",
      icon = icon("chart-area"),

      netsimr_page_intro("Uncertainty", "Mack's standard errors of the chain ladder, and a bootstrap of its reserves."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Bootstrap", "England and Verrall's over-dispersed Poisson bootstrap.",
          netsimr_amount_input("boot_n", "Number of simulations", value = 5000, min = 100, max = 1e5),
          div(class = "sim-section-label", "Process distribution"),
          div(class = "pill-radio",
              netsimr_hidden_label(radioButtons("boot_process", "Process distribution",
                                            c("Gamma" = "gamma", "Over-dispersed Poisson" = "odp"), "gamma", inline = TRUE))),
          bslib::input_switch("boot_tail", "Apply the tail factor", value = TRUE),
          numericInput("boot_seed", "Seed (optional)", value = NA, min = 0, step = 1),
          actionButton("run_bootstrap", "Run bootstrap", icon = icon("play"), class = "btn-run"),
          netsimr_help("The bootstrap refits the volume-weighted chain ladder of the whole triangle to each simulated",
                   "triangle, so it leaves out the choices of the Development tab other than the tail.")
        ),
        uiOutput("uncertainty_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Mack's standard errors",
                          "The chain ladder of the Development tab: its IBNR, standard error and their parts."),
          bslib::card_body(uiOutput("mack_table"))
        ),
        bslib::card(
          netsimr_card_header("calendar-check", "One-year view",
                          "Merz and W\u00fcthrich's standard error of the claims development result of the next year."),
          bslib::card_body(uiOutput("one_year_table"))
        ),
        bslib::card(
          netsimr_card_header("table-cells", "Bootstrap", "The mean, standard deviation and percentiles of the simulated IBNR."),
          bslib::card_body(
            uiOutput("bootstrap_table"),
            div(class = "rsv-downloads", uiOutput("bootstrap_download", inline = TRUE))
          )
        ),
        netsimr_result_tabs(
          id = "uncertainty_tabs",
          bslib::nav_panel("Distribution", netsimr_plot_output("bootstrap_plot")),
          bslib::nav_panel("Residuals by origin", netsimr_plot_output("residual_origin_plot")),
          bslib::nav_panel("by development", netsimr_plot_output("residual_development_plot")),
          bslib::nav_panel("by calendar period", netsimr_plot_output("residual_calendar_plot"))
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(netsimr_theme_switch())
  )
}
