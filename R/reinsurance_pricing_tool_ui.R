#' User interface of the Shiny reinsurance pricing tool
#'
#' @description The page of the reinsurance pricing application: a welcome tab, the
#'   layer, the experience rating, the exposure rating and the pricing, with the
#'   settings beside the results; \code{\link{run_shiny_reinsurance_pricing_tool}}
#'   pairs it with \code{reinsurance_pricing_tool_Server}. Like the package's other
#'   applications, the page is built each time the application starts, not when the
#'   package is installed, so that its theme compiles from where bslib is installed
#'   on the machine running it.
#' @param request The request of the browser, passed by shiny; not used.
#' @return The user interface of the application, a bslib navbar page.
#' @keywords internal
reinsurance_pricing_tool_UI <- function(request) {
  dot <- paste0(" ", intToUtf8(183), " ")
  # the condition of a parameter's input: the distributions of the select with that many parameters
  with_params <- function(id, models, n) {
    names <- names(models)[vapply(models, function(m) length(m$params) >= n, logical(1))]
    paste0("[", paste0("'", names, "'", collapse = ", "), "].indexOf(input.", id, ") >= 0")
  }
  accept <- netsimr_data_accept
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", icon("shield-halved")),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "NetTreaty"),
        tags$span(class = "sim-brand-subtitle", "Reinsurance pricing by NetSimR")
      )
    ),
    window_title = "NetTreaty \u2013 Reinsurance Pricing Tool",
    id = "xlp_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#4c0519",
      theme = "dark",
      underline = FALSE
    ),

    theme = netsimr_bs_theme("#be123c"),

    header = tagList(
      netsimr_tool_head("netsimr-reinsurance-theme", netsimr_css(c("tools", "analysis", "reinsurance"))),
      #tells the server which theme is showing, so the charts are drawn in matching colours
      tags$script(HTML(netsimr_theme_input_js("app_theme"))),
      netsimr_busy_indicators("#fda4af", "#e11d48")
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
          div(class = "sim-hero-eyebrow", "NetTreaty"),
          div(class = "sim-hero-title", "Reinsurance Pricing Tool"),
          p(
            class = "sim-hero-text",
            "Price an excess of loss layer on its loss experience, with the burning cost and a Pareto model of the ",
            "large losses, and on its exposure, with a risk profile and an exposure curve; blend the two, and get ",
            "the technical premium with reinstatements, rate on line and payback."
          ),
          tags$button(
            type = "button",
            class = "btn btn-light",
            onclick = "document.querySelector('.navbar a[data-value=\"layer\"]').click();",
            icon("layer-group"), " Set the layer"
          )
        ),

        bslib::card(
          netsimr_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Set the layer"),
                          div(class = "sim-step-text", "Its deductible, limit, reinstatements and rating year."))),
              tags$li(div(div(class = "sim-step-title", "Rate the experience"),
                          div(class = "sim-step-text", "Index, develop and put the large losses through the layer."))),
              tags$li(div(div(class = "sim-step-title", "Rate the exposure"),
                          div(class = "sim-step-text", "A risk profile and an exposure curve or claim size distribution."))),
              tags$li(div(div(class = "sim-step-title", "Price the layer"),
                          div(class = "sim-step-text", "Blend the rates and add the loadings and reinstatements.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("clock-rotate-left"), "Experience"),
          div(class = "sim-stat-value", paste(c("Burning cost", "Indexation", "Development", "As-if layer", "Pareto"), collapse = dot))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("building"), "Exposure"),
          div(class = "sim-stat-value", paste(c("Swiss Re Y1 to Y4", "Lloyd's", "MBBEFD", "Casualty ILFs"), collapse = dot))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("tags"), "Pricing"),
          div(class = "sim-stat-value", paste(c("Credibility", "Loadings", "Reinstatements", "Rate on line", "Payback"),
                                              collapse = dot))
        )
      ),

      netsimr_about_card("Reinsurance pricing tool", tagList(
        "The functions behind the tool, such as ", tags$code("burning_cost()"), ", ",
            tags$code("property_exposure_rating()"), " and ", tags$code("technical_premium()"), ", can also be used from R."))
    ),

    # ------------------------------------------------------------------ Layer
    bslib::nav_panel(
      title = "Layer",
      value = "layer",
      icon = icon("layer-group"),

      netsimr_page_intro("Layer", "The layer priced and the rating year, used by every method."),

      netsimr_layout(
        div(class = "xlp-settings-stack",
          netsimr_settings_card(
            "layer-group", "Layer", "Each and every loss, with its annual terms.",
            netsimr_amount_input("deductible", "Deductible", value = 1e6, min = 0),
            netsimr_amount_input("limit", "Limit", value = 4e6, min = 0),
            numericInput("reinstatements", "Reinstatements (empty for unlimited)", value = 1, min = 0, step = 1),
            numericInput("reinstatement_rate", "Reinstatement rate (%)", value = 100, min = 0, step = 5),
            netsimr_amount_input("aggregate_deductible", "Annual aggregate deductible", value = 0, min = 0),
            netsimr_help("The annual aggregate limit is the limit times one plus the reinstatements. Reinstatements are paid",
                     "pro rata as to amount, at the rate of the upfront premium.")
          ),
          netsimr_settings_card(
            "calendar", "Rating year", "The year priced and its premium.",
            numericInput("rating_year", "Rating year", value = 2025, step = 1),
            netsimr_amount_input("rating_premium", "Subject premium of the rating year", value = 6e7, min = 0),
            numericInput("loading", "Loading for expenses, brokerage and profit (%)", value = 25, min = 0, max = 95, step = 1),
            netsimr_help("The rates of every method are shares of the subject premium; the expected losses are the rates",
                     "times the rating year's subject premium.")
          ),
          netsimr_settings_card(
            "floppy-disk", "Settings file", "Keep this session's choices and restore them later.",
            downloadButton("save_settings", "Save settings", class = "btn-outline-xlp"),
            fileInput("load_settings", "Load settings", accept = ".txt", placeholder = "No file selected"),
            netsimr_help("The file keeps the layer, the options of each tab and each year's premium and factors. Uploaded",
                     "files are not kept: upload them again, and their columns are chosen as they were.")
          )
        ),
        uiOutput("layer_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Rates at a glance", "Each method's rate and expected loss for this layer."),
          bslib::card_body(uiOutput("glance_table"))
        ),
        netsimr_result_tabs(
          id = "layer_tabs",
          bslib::nav_panel("Losses against the layer", netsimr_plot_output("layer_losses_plot"))
        )
      )
    ),

    # ------------------------------------------------------------- Experience
    bslib::nav_panel(
      title = "Experience",
      value = "experience",
      icon = icon("clock-rotate-left"),

      netsimr_page_intro("Experience rating", "The past large losses, brought to the rating year and put through the layer."),

      netsimr_layout(
        div(class = "xlp-settings-stack",
          netsimr_settings_card(
            "file-arrow-up", "Large losses", "Where the losses come from.",
            div(class = "sim-section-label", "Source"),
            netsimr_pill_radio("loss_source", "Source", c("Example" = "example", "Upload a file" = "upload"), "example"),
            conditionalPanel(
              condition = "input.loss_source == 'upload'",
              fileInput("loss_file", "File", accept = accept, placeholder = "No file selected"),
              netsimr_private_note(),
              netsimr_file_options("loss_header", "loss_sep", "loss_dec"),
              netsimr_column_select("loss_year_col", "Year column"),
              netsimr_column_select("loss_amount_col", "Amount column"),
              netsimr_help("A row per loss, with its year (of occurrence or underwriting) and its amount in the money of",
                       "that year.")
            ),
            tags$hr(class = "sim-divider"),
            numericInput("first_year", "First year", value = NA, step = 1),
            numericInput("last_year", "Last year", value = NA, step = 1),
            numericInput("inflation", "Claims inflation (% a year)", value = 4, step = 0.5),
            netsimr_amount_input("threshold", "Reporting threshold", value = 5e5, min = 0),
            netsimr_help("The losses are indexed to the rating year with the inflation. The threshold is the smallest",
                     "loss in the data, for the warning when indexed losses below it could reach the layer.")
          ),
          netsimr_settings_card(
            "sliders", "Burning cost and Pareto", "How the years and the large losses are used.",
            div(class = "sim-section-label", "Weights of the years"),
            netsimr_pill_radio("weights", "Weights of the years", c("By premium" = "premium", "Equal" = "equal"), "premium"),
            tags$hr(class = "sim-divider"),
            netsimr_amount_input("pareto_threshold", "Pareto threshold", value = 7.5e5, min = 0),
            numericInput("pareto_alpha", "Pareto alpha (empty to estimate)", value = NA, min = 0, step = 0.05),
            netsimr_help("The Pareto is fitted to the indexed losses above its threshold, which must be at most the",
                     "deductible; enter an alpha to use a market one instead.")
          )
        ),
        uiOutput("experience_stats"),
        bslib::card(
          netsimr_card_header("list-check", "By year", "Each year's subject premium, rate index, development and inclusion."),
          bslib::card_body(uiOutput("year_inputs"))
        ),
        bslib::card(
          netsimr_card_header("table-cells", "Burning cost", "Each year's losses in the layer, developed, over its on-level premium."),
          bslib::card_body(
            uiOutput("burning_cost_table"),
            div(class = "xlp-downloads",
                downloadButton("download_burning_cost", "Burning cost by year (CSV)", class = "btn-outline-xlp"),
                downloadButton("download_losses", "Indexed losses (CSV)", class = "btn-outline-xlp"))
          )
        ),
        netsimr_result_tabs(
          id = "experience_tabs",
          bslib::nav_panel("Burning cost by year", netsimr_plot_output("burning_cost_plot")),
          bslib::nav_panel("Losses", netsimr_plot_output("losses_plot")),
          bslib::nav_panel("Pareto fit", netsimr_plot_output("pareto_plot"), uiOutput("pareto_note"))
        )
      )
    ),

    # --------------------------------------------------------------- Exposure
    bslib::nav_panel(
      title = "Exposure",
      value = "exposure",
      icon = icon("building"),

      netsimr_page_intro("Exposure rating", "The layer's share of each band's expected loss, from a risk profile."),

      netsimr_layout(
        div(class = "xlp-settings-stack",
          netsimr_settings_card(
            "file-arrow-up", "Risk profile", "Where the profile comes from.",
            div(class = "sim-section-label", "Source"),
            netsimr_pill_radio("profile_source", "Source", c("Example" = "example", "Upload a file" = "upload"), "example"),
            conditionalPanel(
              condition = "input.profile_source == 'example'",
              selectInput("profile_example", "Example", choices = xlp_profile_examples, selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.profile_source == 'upload'",
              div(class = "sim-section-label", "Business"),
              netsimr_pill_radio("profile_type", "Business", c("Property" = "property", "Casualty" = "casualty"), "property"),
              fileInput("profile_file", "File", accept = accept, placeholder = "No file selected"),
              netsimr_private_note(),
              netsimr_file_options("profile_header", "profile_sep", "profile_dec"),
              selectizeInput("band_col", "Band label column (optional)", choices = NULL,
                             options = list(placeholder = "Upload a data file first")),
              netsimr_column_select("size_col", "Sum insured or policy limit column"),
              netsimr_column_select("premium_col", "Premium column"),
              selectizeInput("loss_ratio_col", "Loss ratio column (optional)", choices = NULL,
                             options = list(placeholder = "Upload a data file first")),
              netsimr_help("A row per band (or policy): for property its sum insured per risk, often the band's average, or",
                       "its maximum possible loss; for casualty its policy limit. Loss ratios as shares, such as 0.65.")
            ),
            numericInput("loss_ratio", "Expected loss ratio (%)", value = 55, min = 0, step = 1),
            netsimr_help("The ground-up loss ratio of the bands, unless the profile has a loss ratio column.")
          ),
          netsimr_settings_card(
            "chart-line", "Curve", "How a band's expected loss is shared with the layer.",
            conditionalPanel(
              condition = "output.profile_type == 'property'",
              selectInput("curve", "Exposure curve", choices = xlp_curves, selected = "Y3", selectize = FALSE),
              conditionalPanel(
                condition = "input.curve == 'c'",
                numericInput("curve_c", "Swiss Re c", value = 3, min = 0, step = 0.1)
              ),
              conditionalPanel(
                condition = "input.curve == 'bg'",
                numericInput("curve_b", "b", value = 3.67, min = 0, step = 0.1),
                numericInput("curve_g", "g", value = 30.6, min = 1, step = 1)
              ),
              netsimr_help("Y1 suits personal lines and Y4 large industrial risks; the Lloyd's curve is for industrial risks.")
            ),
            conditionalPanel(
              condition = "output.profile_type == 'casualty'",
              selectInput("severity", "Claim size distribution", choices = names(xlp_severities), selected = "LogNormal",
                          selectize = FALSE),
              numericInput("severity_p1", "meanlog", value = 10, step = 0.1),
              conditionalPanel(
                condition = "input.severity != 'Exponential'",
                numericInput("severity_p2", "sdlog", value = 2, step = 0.1)
              ),
              netsimr_help("The ground-up claim sizes of the business, e.g. fitted with the distribution fitting tool.")
            )
          )
        ),
        uiOutput("exposure_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "By band", "Each band's expected loss and the layer's share of it."),
          bslib::card_body(
            uiOutput("exposure_table"),
            div(class = "xlp-downloads", downloadButton("download_exposure", "Exposure rating (CSV)", class = "btn-outline-xlp"))
          )
        ),
        netsimr_result_tabs(
          id = "exposure_tabs",
          bslib::nav_panel("Layer loss by band", netsimr_plot_output("band_plot")),
          bslib::nav_panel("Curve", netsimr_plot_output("curve_plot"))
        )
      )
    ),

    # ---------------------------------------------------------------- Pricing
    bslib::nav_panel(
      title = "Pricing",
      value = "pricing",
      icon = icon("tags"),

      netsimr_page_intro("Pricing", "The experience and exposure rates blended, and the technical premium of the layer."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Blend", "How much weight the experience gets.",
          div(class = "sim-section-label", "Experience rate"),
          netsimr_pill_radio("experience_method", "Experience rate", c("Burning cost" = "burning_cost", "Pareto" = "pareto"), "burning_cost"),
          sliderInput("experience_weight", "Weight of the experience (%)", min = 0, max = 100, value = 50, step = 1),
          tags$hr(class = "sim-divider"),
          numericInput("full_credibility", "Losses for full credibility", value = 1082, min = 1, step = 1),
          actionButton("use_credibility", "Use the credibility weight", icon = icon("scale-balanced"), class = "btn-outline-xlp"),
          netsimr_help("Limited fluctuation credibility: the square root of the losses in the layer (or above the Pareto",
                   "threshold) over the number for full credibility, at most 100%.")
        ),
        uiOutput("pricing_stats"),
        bslib::card(
          netsimr_card_header("table-cells", "Methods", "Each method's rate, expected loss and technical premium."),
          bslib::card_body(
            uiOutput("pricing_table"),
            div(class = "xlp-downloads", downloadButton("download_report", "Report (HTML)", class = "btn-outline-xlp"),
                downloadButton("download_pricing", "Pricing (CSV)", class = "btn-outline-xlp"),
                netsimr_excel_button("download_excel", class = "btn-outline-xlp"))
          )
        ),
        netsimr_result_tabs(
          id = "pricing_tabs",
          bslib::nav_panel("Expected loss by method", netsimr_plot_output("method_plot"))
        )
      )
    ),

    # ------------------------------------------------------------- Programmes
    bslib::nav_panel(
      title = "Programmes",
      value = "programmes",
      icon = icon("code-compare"),

      netsimr_page_intro("Programmes", "Compare reinsurance programmes by what they cost and the capital they save."),

      netsimr_layout(
        netsimr_settings_card(
          "sliders", "Gross portfolio", "The claims every programme sees.",
          selectInput("prog_frequency", "Claim count", names(xlp_prog_frequencies), selectize = FALSE),
          div(class = "param-grid-2",
              numericInput("prog_f1", "Mean (lambda)", value = 200, min = 0, step = 10),
              conditionalPanel(condition = with_params("prog_frequency", xlp_prog_frequencies, 2),
                               numericInput("prog_f2", "beta (scale)", value = 2, min = 0, step = 0.5)),
              conditionalPanel(condition = with_params("prog_frequency", xlp_prog_frequencies, 3),
                               numericInput("prog_f3", "Extra zeros (probability)", value = 0.1, min = 0, max = 1, step = 0.05))),
          selectInput("prog_severity", "Claim size", names(xlp_prog_severities), selectize = FALSE),
          div(class = "param-grid-2",
              numericInput("prog_s1", "meanlog", value = 11, step = 0.1),
              conditionalPanel(condition = with_params("prog_severity", xlp_prog_severities, 2),
                               numericInput("prog_s2", "sdlog", value = 1.5, step = 0.1)),
              conditionalPanel(condition = with_params("prog_severity", xlp_prog_severities, 3),
                               numericInput("prog_s3", "scale", value = 2e5, step = 0.1))),
          uiOutput("prog_tail"),
          fileInput("prog_fit", "Load a fitted model", accept = ".txt", placeholder = "No file selected"),
          netsimr_help("A settings file saved on the Export tab of NetDisFit, the distribution fitting tool, or by",
                   "NetSimulator: it sets the claim count and the claim size, with any Pareto tail fitted above",
                   "slicing points."),
          netsimr_help("The subject premium is the rating year's, on the Layer tab: the expected gross claims should be",
                   "below it."),
          tags$hr(class = "sim-divider"),
          netsimr_amount_input("prog_n", "Simulated years", value = 20000, min = 1000, max = 2e5),
          selectInput("prog_level", "Capital at", c("99.5% (1 in 200)" = "0.995", "99% (1 in 100)" = "0.99",
                                                     "90% (1 in 10)" = "0.9"), selectize = FALSE),
          div(class = "param-grid-2",
              numericInput("prog_loading", "Reinsurer's loading (%)", value = 25, min = 0, max = 95, step = 5),
              numericInput("prog_coc", "Cost of capital (%)", value = 10, min = 0, step = 1)),
          numericInput("prog_seed", "Seed (optional)", value = 1, min = 0, step = 1),
          actionButton("run_programmes", "Compare", icon = icon("play"), class = "btn-run"),
          netsimr_help("A layer or stop loss without a premium is priced at its expected loss, the loading being the share of",
                   "the premium for the reinsurer's expenses and profit.")
        ),
        uiOutput("programme_stats"),
        bslib::card(
          netsimr_card_header("list-check", "Programmes", "Up to five, each a surplus treaty, a quota share, a layer, a stop loss or a mix."),
          bslib::card_body(uiOutput("programme_inputs"))
        ),
        bslib::card(
          netsimr_card_header("table-cells", "Comparison", "Each programme's cost, net result and capital."),
          bslib::card_body(
            uiOutput("programme_table"),
            div(class = "xlp-downloads", downloadButton("download_programmes", "Comparison (CSV)", class = "btn-outline-xlp"))
          )
        ),
        netsimr_result_tabs(
          id = "programme_tabs",
          bslib::nav_panel("Cost and capital", netsimr_plot_output("programme_plot")),
          bslib::nav_panel("Net result by return period", netsimr_plot_output("programme_return_plot"))
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(netsimr_theme_switch())
  )
}
