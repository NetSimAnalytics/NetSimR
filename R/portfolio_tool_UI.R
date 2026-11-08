#' Styles for the portfolio capital tool
#'
#' Added after the simulator styles (sim_ui_css) and the distribution fitting tool
#' styles (dft_ui_css), whose cards, inputs, theme switch, tiles, tables and light /
#' dark tokens the tool shares; this block gives the tool its green accent and styles
#' the parts only it has.
#' @noRd
pfo_ui_css <- "
:root, [data-bs-theme='light'] {
  --dft-accent: #15803d;
  --dft-accent-strong: #166534;
  --dft-accent-shadow: rgba(21, 128, 61, 0.26);
  --sim-accent-soft: #f0fdf4;
  --sim-accent-text: #166534;
  --dft-row-hover: #f3fdf6;
  --dft-slider-value: #166534;
}

[data-bs-theme='dark'] {
  --dft-accent: #4ade80;
  --dft-accent-strong: #22c55e;
  --dft-accent-shadow: rgba(74, 222, 128, 0.28);
  --sim-accent-soft: rgba(74, 222, 128, 0.14);
  --sim-accent-text: #86efac;
  --dft-row-hover: rgba(74, 222, 128, 0.08);
  --dft-slider-value: #166534;
  --bs-link-color: #86efac;
  --bs-link-color-rgb: 134, 239, 172;
  --bs-link-hover-color: #bbf7d0;
  --bs-link-hover-color-rgb: 187, 247, 208;
}

.navbar {
  background: linear-gradient(90deg, #052e16 0%, #14532d 100%) !important;
}

.sim-brand-mark {
  background: linear-gradient(135deg, #4ade80, #15803d);
  box-shadow: 0 6px 18px rgba(21, 128, 61, 0.35);
}

.theme-btn.active,
.sim-hero .btn-light {
  color: #052e16;
}

.theme-btn:focus-visible,
.pill-radio .radio-inline:has(input:focus-visible) {
  outline-color: #86efac !important;
}

.form-control:focus, .form-select:focus, .selectize-input.focus,
.form-check-input:focus {
  box-shadow: 0 0 0 0.2rem rgba(21, 128, 61, 0.18) !important;
}

/* white on the button: the darker greens, as white on the lighter ones is under 4.5:1 */
.btn-run {
  background: linear-gradient(135deg, #15803d, #166534);
}

.btn-run:hover, .btn-run:focus {
  background: linear-gradient(135deg, #166534, #14532d);
}

.sim-hero {
  background:
    radial-gradient(circle at 85% 20%, rgba(74, 222, 128, 0.30), transparent 45%),
    linear-gradient(135deg, #052e16 0%, #14532d 55%, #15803d 130%);
}

.sim-hero-eyebrow {
  color: #86efac;
}

.btn-outline-pfo {
  border: 1px solid var(--sim-border-strong);
  background: var(--sim-input-bg);
  color: var(--sim-label);
  font-size: 0.85rem;
  padding: 0.4rem 0.75rem;
}

.btn-outline-pfo:hover, .btn-outline-pfo:focus {
  border-color: var(--dft-accent);
  background: var(--sim-accent-soft);
  color: var(--sim-accent-text);
}

.pfo-downloads {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-top: 0.9rem;
}

.pfo-settings-stack {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  min-width: 0;
}

.pfo-settings-stack > .card {
  margin-bottom: 0;
}

/* ---------- The grids of lines and correlations ---------- */
/* they scroll sideways on a narrow screen; positioned, so that the inputs' hidden labels
   (absolutely positioned) scroll with them instead of widening the page */
.pfo-grid-wrap {
  overflow-x: auto;
  position: relative;
}

.pfo-line-grid {
  display: grid;
  grid-template-columns: minmax(8rem, 1.3fr) minmax(11rem, 1fr) minmax(5.5rem, 0.7fr) minmax(5.5rem, 0.7fr)
                         minmax(8rem, 1fr) minmax(5.5rem, 0.7fr) minmax(6rem, 0.8fr);
  gap: 0.45rem 0.6rem;
  align-items: center;
  min-width: 800px;
}

.pfo-corr-grid {
  display: grid;
  gap: 0.45rem 0.6rem;
  align-items: center;
}

.pfo-line-grid .shiny-input-container,
.pfo-corr-grid .shiny-input-container {
  margin-bottom: 0 !important;
  width: 100% !important;
}

.pfo-line-grid .form-control, .pfo-line-grid .form-select,
.pfo-corr-grid .form-control {
  padding-top: 0.3rem;
  padding-bottom: 0.3rem;
  font-size: 0.88rem;
}

.pfo-head {
  font-size: 0.7rem;
  font-weight: 800;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--sim-muted);
}

.pfo-corr-name {
  font-weight: 700;
  color: var(--sim-heading);
  font-size: 0.88rem;
}

.pfo-corr-one {
  color: var(--sim-muted);
  text-align: center;
}

.dft-table tr.pfo-total td {
  font-weight: 800;
  border-top: 1px solid var(--sim-border-strong);
  background: var(--sim-accent-soft);
}
"

#' User interface of the Shiny portfolio capital tool
#'
#' @description The page of the portfolio capital application: a welcome tab, the
#'   lines, their dependence and the capital, with the settings beside the results;
#'   \code{\link{run_shiny_portfolio_tool}} pairs it with \code{portfolio_tool_Server}.
#'   Like the package's other applications, the page is built each time the
#'   application starts, not when the package is installed.
#' @param request The request of the browser, passed by shiny; not used.
#' @return The user interface of the application, a bslib navbar page.
#' @keywords internal
portfolio_tool_UI <- function(request) {
  dot <- paste0(" ", intToUtf8(183), " ")
  pill <- function(id, label, choices, selected) {
    div(class = "pill-radio", sim_hidden_label(radioButtons(id, label, choices, selected, inline = TRUE)))
  }
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", icon("cubes")),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "NetCapital"),
        tags$span(class = "sim-brand-subtitle", "Portfolio capital by NetSimR")
      )
    ),
    window_title = "NetSim Portfolio Capital Tool",
    id = "pfo_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(bg = "#052e16", theme = "dark", underline = FALSE),

    theme = bslib::bs_theme(
      version = 5,
      primary = "#15803d",
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
        tags$script(HTML(netsimr_theme_js("netsimr-portfolio-theme"))),
        tags$style(HTML(sim_ui_css)),
        tags$style(HTML(dft_ui_css)),
        tags$style(HTML(pfo_ui_css)),
        #the binding of the amount inputs (the number of simulations)
        tags$script(HTML(sim_amount_input_js))
      ),
      #tells the server which theme is showing, so the charts are drawn in matching colours
      tags$script(HTML(sim_compare_theme_js("app_theme"))),
      useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
      busyIndicatorOptions(
        pulse_background = "linear-gradient(90deg, #86efac, #15803d, #86efac)",
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
          div(class = "sim-hero-eyebrow", "NetCapital"),
          div(class = "sim-hero-title", "Portfolio Capital Tool"),
          p(
            class = "sim-hero-text",
            "Simulate the annual losses of several lines of business, join them with a Gaussian or t copula, and ",
            "measure the portfolio's value at risk and tail value at risk, the diversification between the lines ",
            "and each line's share of the capital."
          ),
          tags$button(
            type = "button",
            class = "btn btn-light",
            onclick = "document.querySelector('.navbar a[data-value=\"lines\"]').click();",
            icon("layer-group"), " Set the lines"
          )
        ),
        bslib::card(
          sim_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Set the lines"),
                          div(class = "sim-step-text", "A frequency and severity for each, or upload simulated losses."))),
              tags$li(div(div(class = "sim-step-title", "Set the dependence"),
                          div(class = "sim-step-text", "A Gaussian or t copula, with a correlation for each pair."))),
              tags$li(div(div(class = "sim-step-title", "Measure the capital"),
                          div(class = "sim-step-text", "VaR, TVaR, diversification and each line's capital."))),
              tags$li(div(div(class = "sim-step-title", "Share it"),
                          div(class = "sim-step-text", "A report, the summary and the simulated losses.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(class = "sim-stat",
            div(class = "sim-stat-label", icon("layer-group"), "Lines"),
            div(class = "sim-stat-value", paste(c("Poisson", "Negative Binomial", "LogNormal", "Gamma", "Pareto", "Uploads"),
                                                collapse = dot))),
        div(class = "sim-stat",
            div(class = "sim-stat-label", icon("link"), "Dependence"),
            div(class = "sim-stat-value", paste(c("Gaussian copula", "t copula", "Correlation matrix"), collapse = dot))),
        div(class = "sim-stat",
            div(class = "sim-stat-label", icon("scale-balanced"), "Capital"),
            div(class = "sim-stat-value", paste(c("VaR", "TVaR", "Diversification", "Co-TVaR allocation"), collapse = dot)))
      ),

      netsimr_about_card("Portfolio capital tool", tagList(
        "The function behind the tool, ", tags$code("simulate_portfolio()"), ", can also be used from R."))
    ),

    # ------------------------------------------------------------------ Lines
    bslib::nav_panel(
      title = "Lines",
      value = "lines",
      icon = icon("layer-group"),

      dft_page_intro("Lines", "The lines of business and the annual losses of each."),

      dft_layout(
        div(class = "pfo-settings-stack",
          dft_settings_card(
            "layer-group", "Lines", "Where each line's losses come from.",
            div(class = "sim-section-label", "Source"),
            pill("line_source", "Source", c("Frequency and severity" = "model", "Upload simulated losses" = "upload"), "model"),
            conditionalPanel(
              condition = "input.line_source == 'model'",
              numericInput("n_lines", "Number of lines", value = 3, min = 2, max = pfo_max_lines, step = 1),
              dft_help("Each line's claims are simulated with simulate_claims(): a claim count and a claim size for each",
                       "claim, every year.")
            ),
            conditionalPanel(
              condition = "input.line_source == 'upload'",
              fileInput("upload_file", "File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt",
                                                          ".tsv"), placeholder = "No file selected"),
              bslib::input_switch("upload_header", "First row holds column names", value = TRUE),
              div(class = "sim-section-label mt-2", "Separator"),
              pill("upload_sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
              div(class = "sim-section-label mt-2", "Decimal mark"),
              pill("upload_dec", "Decimal mark", c("Point (1.5)" = ".", "Comma (1,5)" = ","), "."),
              selectizeInput("upload_columns", "A column per line", choices = NULL, multiple = TRUE,
                             options = list(placeholder = "Upload a data file first")),
              dft_help("A row per simulated year and a column per line, such as the simulations downloaded from the",
                       "claims simulator. The copula reorders each column, so each line keeps its own distribution.")
            )
          ),
          dft_settings_card(
            "floppy-disk", "Settings file", "Keep this session's choices and restore them later.",
            downloadButton("save_settings", "Save settings", class = "btn-outline-pfo"),
            fileInput("load_settings", "Load settings", accept = ".txt", placeholder = "No file selected"),
            dft_help("The file keeps the lines, the dependence and the simulation settings. An uploaded file is not",
                     "kept: upload it again, and its columns are chosen as they were.")
          )
        ),
        uiOutput("lines_stats"),
        conditionalPanel(
          condition = "input.line_source == 'model'",
          bslib::card(
            sim_card_header("list-check", "By line", "Each line's name, claim count and claim size distributions."),
            bslib::card_body(uiOutput("line_inputs"))
          )
        ),
        bslib::card(
          sim_card_header("table-cells", "Expected losses", "Each line's expected annual claims and losses."),
          bslib::card_body(uiOutput("lines_table"))
        )
      )
    ),

    # ------------------------------------------------------------- Dependence
    bslib::nav_panel(
      title = "Dependence",
      value = "dependence",
      icon = icon("link"),

      dft_page_intro("Dependence", "How the lines' bad years come together."),

      dft_layout(
        dft_settings_card(
          "link", "Copula", "The dependence between the lines' ranks.",
          div(class = "sim-section-label", "Copula"),
          pill("copula", "Copula", c("Gaussian" = "gaussian", "t" = "t"), "gaussian"),
          conditionalPanel(
            condition = "input.copula == 't'",
            numericInput("copula_df", "Degrees of freedom", value = 4, min = 1, step = 1),
            dft_help("The fewer the degrees of freedom, the more often the lines have their worst years together.")
          ),
          tags$hr(class = "sim-divider"),
          div(class = "sim-section-label", "Correlations"),
          pill("corr_mode", "Correlations", c("One for every pair" = "all", "A matrix" = "matrix"), "all"),
          conditionalPanel(
            condition = "input.corr_mode == 'all'",
            numericInput("corr_all", "Correlation", value = 0.25, min = -0.99, max = 0.99, step = 0.05)
          ),
          dft_help("The correlations of the copula's normal draws: their ranks' correlation is a little lower, about",
                   "6 / pi times asin(r / 2)."),
          dft_help("A correlation of 0.25 between two lines is common for lines hit by the same economy or weather.")
        ),
        uiOutput("dependence_stats"),
        conditionalPanel(
          condition = "input.corr_mode == 'matrix'",
          bslib::card(
            sim_card_header("table-cells", "Correlation matrix", "Each pair's correlation, above the diagonal."),
            bslib::card_body(uiOutput("corr_inputs"))
          )
        ),
        bslib::card(
          sim_card_header("circle-info", "How the lines are joined"),
          bslib::card_body(
            p(class = "mb-2",
              "Each line is simulated on its own. The copula then draws, for every simulated year, a rank for each line,",
              "and each line's losses are put in that order: so each line keeps exactly its own distribution, and the",
              "ranks of the lines follow the copula (the reordering of Iman and Conover)."),
            p(class = "mb-0",
              "With a Gaussian copula, extreme years of two lines rarely coincide; a t copula, with the same correlation,",
              "makes them more likely, and leaves less diversification at the 99.5% level.")
          )
        )
      )
    ),

    # ---------------------------------------------------------------- Capital
    bslib::nav_panel(
      title = "Capital",
      value = "capital",
      icon = icon("scale-balanced"),

      dft_page_intro("Capital", "The portfolio's value at risk, its diversification and each line's capital."),

      dft_layout(
        dft_settings_card(
          "sliders", "Simulation", "The run and the level of the capital.",
          sim_amount_input("n_sims", "Number of simulations", value = 20000, min = 1000, max = 1e6),
          selectInput("level", "Level", choices = pfo_levels, selected = "0.995", selectize = FALSE),
          numericInput("seed", "Seed (optional)", value = 1, min = 0, step = 1),
          actionButton("run", "Run", icon = icon("play"), class = "btn-run"),
          dft_help("With uploaded losses, the number of simulations is the file's rows. At least 10,000 simulations",
                   "are recommended for figures at 99.5%.")
        ),
        uiOutput("capital_stats"),
        bslib::card(
          sim_card_header("table-cells", "By line", "Each line's and the total's mean, VaR, TVaR and capital."),
          bslib::card_body(
            uiOutput("capital_table"),
            div(class = "pfo-downloads",
                downloadButton("download_summary", "Summary (CSV)", class = "btn-outline-pfo"),
                downloadButton("download_losses", "Simulated losses (CSV)", class = "btn-outline-pfo"),
                downloadButton("download_report", "Report (HTML)", class = "btn-outline-pfo"))
          )
        ),
        dft_tabs(
          id = "capital_tabs",
          bslib::nav_panel("Capital by line", dft_plot_output("capital_plot")),
          bslib::nav_panel("Total losses", dft_plot_output("total_plot")),
          bslib::nav_panel("Dependence",
                           div(class = "param-grid-2 mt-2",
                               selectInput("rank_x", "Line across", choices = NULL, selectize = FALSE),
                               selectInput("rank_y", "Line up", choices = NULL, selectize = FALSE)),
                           dft_plot_output("rank_plot", height = "480px"),
                           p(class = "dft-table-note",
                             "Each point is a simulated year, at the ranks of the two lines' losses. The square at the top",
                             "right holds the years in the worst 5% of both."))
        )
      )
    ),

    bslib::nav_spacer(),
    bslib::nav_item(sim_theme_switch())
  )
}
