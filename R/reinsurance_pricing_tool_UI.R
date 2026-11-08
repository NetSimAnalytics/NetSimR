#' Styles for the reinsurance pricing tool
#'
#' Added after the simulator styles (sim_ui_css) and the distribution fitting tool
#' styles (dft_ui_css), whose cards, inputs, theme switch, tiles, tables and light /
#' dark tokens the tool shares; this block gives the tool its rose accent and styles
#' the parts only it has.
#' @noRd
xlp_ui_css <- "
:root, [data-bs-theme='light'] {
  --dft-accent: #e11d48;
  --dft-accent-strong: #be123c;
  --dft-accent-shadow: rgba(225, 29, 72, 0.26);
  --sim-accent-soft: #fff1f2;
  --sim-accent-text: #be123c;
  --dft-row-hover: #fff5f6;
  /* white text on the slider's value: the darker rose in both themes */
  --dft-slider-value: #be123c;
}

[data-bs-theme='dark'] {
  --dft-accent: #fb7185;
  --dft-accent-strong: #f43f5e;
  --dft-accent-shadow: rgba(251, 113, 133, 0.28);
  --sim-accent-soft: rgba(251, 113, 133, 0.16);
  --sim-accent-text: #fda4af;
  --dft-row-hover: rgba(251, 113, 133, 0.08);
  --dft-slider-value: #be123c;
  --bs-link-color: #fda4af;
  --bs-link-color-rgb: 253, 164, 175;
  --bs-link-hover-color: #fecdd3;
  --bs-link-hover-color-rgb: 254, 205, 211;
}

/* ---------- Accent ---------- */
.navbar {
  background: linear-gradient(90deg, #4c0519 0%, #881337 100%) !important;
}

.sim-brand-mark {
  background: linear-gradient(135deg, #fb7185, #be123c);
  box-shadow: 0 6px 18px rgba(225, 29, 72, 0.35);
}

.theme-btn.active,
.sim-hero .btn-light {
  color: #4c0519;
}

.theme-btn:focus-visible,
.pill-radio .radio-inline:has(input:focus-visible) {
  outline-color: #fda4af !important;
}

.form-control:focus, .form-select:focus, .selectize-input.focus,
.form-check-input:focus {
  box-shadow: 0 0 0 0.2rem rgba(225, 29, 72, 0.18) !important;
}

/* white on the button: the darker roses, as white on the lighter ones is under 4.5:1 */
.btn-run {
  background: linear-gradient(135deg, #be123c, #9f1239);
}

.btn-run:hover, .btn-run:focus {
  background: linear-gradient(135deg, #9f1239, #881337);
}

.sim-hero {
  background:
    radial-gradient(circle at 85% 20%, rgba(251, 113, 133, 0.34), transparent 45%),
    linear-gradient(135deg, #4c0519 0%, #881337 55%, #be123c 130%);
}

.sim-hero-eyebrow {
  color: #fda4af;
}

.btn-outline-xlp {
  border: 1px solid var(--sim-border-strong);
  background: var(--sim-input-bg);
  color: var(--sim-label);
  font-size: 0.85rem;
  padding: 0.4rem 0.75rem;
}

.btn-outline-xlp:hover, .btn-outline-xlp:focus {
  border-color: var(--dft-accent);
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
}

.xlp-downloads {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-top: 0.9rem;
}

/* a settings column of two or three cards, one above the other */
.xlp-settings-stack {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  min-width: 0;
}

.xlp-settings-stack > .card {
  margin-bottom: 0;
}

/* ---------- Inputs by year ---------- */
/* the grid scrolls sideways on a narrow screen; positioned, so that the inputs' hidden
   labels (absolutely positioned) scroll with it instead of widening the page */
.xlp-year-wrap {
  overflow-x: auto;
  position: relative;
}

.xlp-year-grid {
  display: grid;
  grid-template-columns: minmax(3.5rem, auto) minmax(9rem, 1.4fr) minmax(6rem, 0.8fr) minmax(6rem, 0.8fr) minmax(4.5rem, auto);
  gap: 0.45rem 0.75rem;
  align-items: center;
  min-width: 480px;
}

.xlp-year-grid .shiny-input-container {
  margin-bottom: 0 !important;
  width: 100% !important;
}

.xlp-year-grid .form-check {
  margin-bottom: 0;
}

.xlp-year-head {
  font-size: 0.7rem;
  font-weight: 800;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--sim-muted);
}

.xlp-year-name {
  font-weight: 700;
  color: var(--sim-heading);
}

.xlp-year-grid .form-control {
  padding-top: 0.3rem;
  padding-bottom: 0.3rem;
  font-size: 0.88rem;
}

.dft-table tr.xlp-total td {
  font-weight: 800;
  border-top: 1px solid var(--sim-border-strong);
  background: var(--sim-accent-soft);
}

.dft-table tr.xlp-excluded td {
  color: var(--sim-muted);
}
"

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
  pill <- function(id, label, choices, selected) {
    div(class = "pill-radio", sim_hidden_label(radioButtons(id, label, choices, selected, inline = TRUE)))
  }
  file_options <- function(prefix) {
    tagList(
      bslib::input_switch(paste0(prefix, "_header"), "First row holds column names", value = TRUE),
      div(class = "sim-section-label mt-2", "Separator"),
      pill(paste0(prefix, "_sep"), "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
      div(class = "sim-section-label mt-2", "Decimal mark"),
      pill(paste0(prefix, "_dec"), "Decimal mark", c("Point (1.5)" = ".", "Comma (1,5)" = ","), ".")
    )
  }
  accept <- c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt", ".tsv")
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
    window_title = "NetSim Reinsurance Pricing Tool",
    id = "xlp_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#4c0519",
      theme = "dark",
      underline = FALSE
    ),

    theme = bslib::bs_theme(
      version = 5,
      primary = "#be123c",
      secondary = "#64748b",
      success = "#16a34a",
      info = "#0891b2",
      warning = "#d97706",
      danger = "#dc2626",
      base_font = bslib::font_collection(
        bslib::font_google("Inter", local = FALSE),
        "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
      ),
      heading_font = bslib::font_collection(
        bslib::font_google("Inter", local = FALSE),
        "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
      )
    ),

    header = tagList(
      tags$head(
        tags$script(HTML(netsimr_theme_js("netsimr-reinsurance-theme"))),
        tags$style(HTML(sim_ui_css)),
        tags$style(HTML(dft_ui_css)),
        tags$style(HTML(xlp_ui_css)),
        #the binding of the amount inputs (deductible, limit, premiums)
        tags$script(HTML(sim_amount_input_js))
      ),
      #tells the server which theme is showing, so the charts are drawn in matching colours
      tags$script(HTML(sim_compare_theme_js("app_theme"))),
      useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
      busyIndicatorOptions(
        pulse_background = "linear-gradient(90deg, #fda4af, #e11d48, #fda4af)",
        pulse_height = "4px"
      )
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
          sim_card_header("route", "How it works"),
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

      dft_page_intro("Layer", "The layer priced and the rating year, used by every method."),

      dft_layout(
        div(class = "xlp-settings-stack",
          dft_settings_card(
            "layer-group", "Layer", "Each and every loss, with its annual terms.",
            sim_amount_input("deductible", "Deductible", value = 1e6, min = 0),
            sim_amount_input("limit", "Limit", value = 4e6, min = 0),
            numericInput("reinstatements", "Reinstatements (empty for unlimited)", value = 1, min = 0, step = 1),
            numericInput("reinstatement_rate", "Reinstatement rate (%)", value = 100, min = 0, step = 5),
            sim_amount_input("aggregate_deductible", "Annual aggregate deductible", value = 0, min = 0),
            dft_help("The annual aggregate limit is the limit times one plus the reinstatements. Reinstatements are paid",
                     "pro rata as to amount, at the rate of the upfront premium.")
          ),
          dft_settings_card(
            "calendar", "Rating year", "The year priced and its premium.",
            numericInput("rating_year", "Rating year", value = 2025, step = 1),
            sim_amount_input("rating_premium", "Subject premium of the rating year", value = 6e7, min = 0),
            numericInput("loading", "Loading for expenses, brokerage and profit (%)", value = 25, min = 0, max = 95, step = 1),
            dft_help("The rates of every method are shares of the subject premium; the expected losses are the rates",
                     "times the rating year's subject premium.")
          ),
          dft_settings_card(
            "floppy-disk", "Settings file", "Keep this session's choices and restore them later.",
            downloadButton("save_settings", "Save settings", class = "btn-outline-xlp"),
            fileInput("load_settings", "Load settings", accept = ".txt", placeholder = "No file selected"),
            dft_help("The file keeps the layer, the options of each tab and each year's premium and factors. Uploaded",
                     "files are not kept: upload them again, and their columns are chosen as they were.")
          )
        ),
        uiOutput("layer_stats"),
        bslib::card(
          sim_card_header("table-cells", "Rates at a glance", "Each method's rate and expected loss for this layer."),
          bslib::card_body(uiOutput("glance_table"))
        ),
        dft_tabs(
          id = "layer_tabs",
          bslib::nav_panel("Losses against the layer", dft_plot_output("layer_losses_plot"))
        )
      )
    ),

    # ------------------------------------------------------------- Experience
    bslib::nav_panel(
      title = "Experience",
      value = "experience",
      icon = icon("clock-rotate-left"),

      dft_page_intro("Experience rating", "The past large losses, brought to the rating year and put through the layer."),

      dft_layout(
        div(class = "xlp-settings-stack",
          dft_settings_card(
            "file-arrow-up", "Large losses", "Where the losses come from.",
            div(class = "sim-section-label", "Source"),
            pill("loss_source", "Source", c("Example" = "example", "Upload a file" = "upload"), "example"),
            conditionalPanel(
              condition = "input.loss_source == 'upload'",
              fileInput("loss_file", "File", accept = accept, placeholder = "No file selected"),
              file_options("loss"),
              dft_column_select("loss_year_col", "Year column"),
              dft_column_select("loss_amount_col", "Amount column"),
              dft_help("A row per loss, with its year (of occurrence or underwriting) and its amount in the money of",
                       "that year.")
            ),
            tags$hr(class = "sim-divider"),
            numericInput("first_year", "First year", value = NA, step = 1),
            numericInput("last_year", "Last year", value = NA, step = 1),
            numericInput("inflation", "Claims inflation (% a year)", value = 4, step = 0.5),
            sim_amount_input("threshold", "Reporting threshold", value = 5e5, min = 0),
            dft_help("The losses are indexed to the rating year with the inflation. The threshold is the smallest",
                     "loss in the data, for the warning when indexed losses below it could reach the layer.")
          ),
          dft_settings_card(
            "sliders", "Burning cost and Pareto", "How the years and the large losses are used.",
            div(class = "sim-section-label", "Weights of the years"),
            pill("weights", "Weights of the years", c("By premium" = "premium", "Equal" = "equal"), "premium"),
            tags$hr(class = "sim-divider"),
            sim_amount_input("pareto_threshold", "Pareto threshold", value = 7.5e5, min = 0),
            numericInput("pareto_alpha", "Pareto alpha (empty to estimate)", value = NA, min = 0, step = 0.05),
            dft_help("The Pareto is fitted to the indexed losses above its threshold, which must be at most the",
                     "deductible; enter an alpha to use a market one instead.")
          )
        ),
        uiOutput("experience_stats"),
        bslib::card(
          sim_card_header("list-check", "By year", "Each year's subject premium, rate index, development and inclusion."),
          bslib::card_body(uiOutput("year_inputs"))
        ),
        bslib::card(
          sim_card_header("table-cells", "Burning cost", "Each year's losses in the layer, developed, over its on-level premium."),
          bslib::card_body(
            uiOutput("burning_cost_table"),
            div(class = "xlp-downloads",
                downloadButton("download_burning_cost", "Burning cost by year (CSV)", class = "btn-outline-xlp"),
                downloadButton("download_losses", "Indexed losses (CSV)", class = "btn-outline-xlp"))
          )
        ),
        dft_tabs(
          id = "experience_tabs",
          bslib::nav_panel("Burning cost by year", dft_plot_output("burning_cost_plot")),
          bslib::nav_panel("Losses", dft_plot_output("losses_plot")),
          bslib::nav_panel("Pareto fit", dft_plot_output("pareto_plot"), uiOutput("pareto_note"))
        )
      )
    ),

    # --------------------------------------------------------------- Exposure
    bslib::nav_panel(
      title = "Exposure",
      value = "exposure",
      icon = icon("building"),

      dft_page_intro("Exposure rating", "The layer's share of each band's expected loss, from a risk profile."),

      dft_layout(
        div(class = "xlp-settings-stack",
          dft_settings_card(
            "file-arrow-up", "Risk profile", "Where the profile comes from.",
            div(class = "sim-section-label", "Source"),
            pill("profile_source", "Source", c("Example" = "example", "Upload a file" = "upload"), "example"),
            conditionalPanel(
              condition = "input.profile_source == 'example'",
              selectInput("profile_example", "Example", choices = xlp_profile_examples, selectize = FALSE)
            ),
            conditionalPanel(
              condition = "input.profile_source == 'upload'",
              div(class = "sim-section-label", "Business"),
              pill("profile_type", "Business", c("Property" = "property", "Casualty" = "casualty"), "property"),
              fileInput("profile_file", "File", accept = accept, placeholder = "No file selected"),
              file_options("profile"),
              selectizeInput("band_col", "Band label column (optional)", choices = NULL,
                             options = list(placeholder = "Upload a data file first")),
              dft_column_select("size_col", "Sum insured or policy limit column"),
              dft_column_select("premium_col", "Premium column"),
              selectizeInput("loss_ratio_col", "Loss ratio column (optional)", choices = NULL,
                             options = list(placeholder = "Upload a data file first")),
              dft_help("A row per band (or policy): for property its sum insured per risk, often the band's average, or",
                       "its maximum possible loss; for casualty its policy limit. Loss ratios as shares, such as 0.65.")
            ),
            numericInput("loss_ratio", "Expected loss ratio (%)", value = 55, min = 0, step = 1),
            dft_help("The ground-up loss ratio of the bands, unless the profile has a loss ratio column.")
          ),
          dft_settings_card(
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
              dft_help("Y1 suits personal lines and Y4 large industrial risks; the Lloyd's curve is for industrial risks.")
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
              dft_help("The ground-up claim sizes of the business, e.g. fitted with the distribution fitting tool.")
            )
          )
        ),
        uiOutput("exposure_stats"),
        bslib::card(
          sim_card_header("table-cells", "By band", "Each band's expected loss and the layer's share of it."),
          bslib::card_body(
            uiOutput("exposure_table"),
            div(class = "xlp-downloads", downloadButton("download_exposure", "Exposure rating (CSV)", class = "btn-outline-xlp"))
          )
        ),
        dft_tabs(
          id = "exposure_tabs",
          bslib::nav_panel("Layer loss by band", dft_plot_output("band_plot")),
          bslib::nav_panel("Curve", dft_plot_output("curve_plot"))
        )
      )
    ),

    # ---------------------------------------------------------------- Pricing
    bslib::nav_panel(
      title = "Pricing",
      value = "pricing",
      icon = icon("tags"),

      dft_page_intro("Pricing", "The experience and exposure rates blended, and the technical premium of the layer."),

      dft_layout(
        dft_settings_card(
          "sliders", "Blend", "How much weight the experience gets.",
          div(class = "sim-section-label", "Experience rate"),
          pill("experience_method", "Experience rate", c("Burning cost" = "burning_cost", "Pareto" = "pareto"), "burning_cost"),
          sliderInput("experience_weight", "Weight of the experience (%)", min = 0, max = 100, value = 50, step = 1),
          tags$hr(class = "sim-divider"),
          numericInput("full_credibility", "Losses for full credibility", value = 1082, min = 1, step = 1),
          actionButton("use_credibility", "Use the credibility weight", icon = icon("scale-balanced"), class = "btn-outline-xlp"),
          dft_help("Limited fluctuation credibility: the square root of the losses in the layer (or above the Pareto",
                   "threshold) over the number for full credibility, at most 100%.")
        ),
        uiOutput("pricing_stats"),
        bslib::card(
          sim_card_header("table-cells", "Methods", "Each method's rate, expected loss and technical premium."),
          bslib::card_body(
            uiOutput("pricing_table"),
            div(class = "xlp-downloads", downloadButton("download_pricing", "Pricing (CSV)", class = "btn-outline-xlp"))
          )
        ),
        dft_tabs(
          id = "pricing_tabs",
          bslib::nav_panel("Expected loss by method", dft_plot_output("method_plot"))
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(sim_theme_switch())
  )
}
