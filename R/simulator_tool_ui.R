#' Script that shows busy states on the Run and Report buttons
#'
#' Server-side label updates only reach the browser after a run has finished,
#' so the busy state is set in the browser on click and cleared by a server message.
#' @noRd
sim_run_state_js <- "
(function () {
  function setBusy(el, label) {
    if (!el.hasAttribute('data-idle-html')) el.setAttribute('data-idle-html', el.innerHTML);
    var spinner = document.createElement('span');
    spinner.className = 'spinner-border spinner-border-sm me-2';
    spinner.setAttribute('aria-hidden', 'true');
    el.innerHTML = '';
    el.appendChild(spinner);
    el.appendChild(document.createTextNode(label));
    el.setAttribute('aria-busy', 'true');
  }

  function setIdle(el) {
    if (!el) return;
    if (el.hasAttribute('data-idle-html')) {
      el.innerHTML = el.getAttribute('data-idle-html');
      el.removeAttribute('data-idle-html');
    }
    el.removeAttribute('aria-busy');
  }

  document.addEventListener('click', function (event) {
    if (!event.target.closest) return;

    /* Shiny's own click handler on the button has already run by the time this
       document-level listener fires, so the click is counted before the button is
       disabled. The busy state must be set right away: the server's reply can
       arrive before any deferred timer runs, especially in a background tab. */
    var run = event.target.closest('#RunSimulations');
    if (run && !run.disabled) {
      run.disabled = true;
      setBusy(run, 'Running...');
    }

    var report = event.target.closest('#downloadReportHandler');
    if (report) {
      if (report.classList.contains('disabled')) { event.preventDefault(); return; }
      report.classList.add('disabled');
      setBusy(report, 'Preparing report...');
    }
  });

  /* Shiny requires message handlers to take exactly one argument */
  Shiny.addCustomMessageHandler('netsimr-run-finished', function (message) {
    var run = document.getElementById('RunSimulations');
    if (run) { run.disabled = false; setIdle(run); }
  });

  Shiny.addCustomMessageHandler('netsimr-report-finished', function (message) {
    var report = document.getElementById('downloadReportHandler');
    if (report) { report.classList.remove('disabled'); setIdle(report); }
  });
})();
"

#' The Pareto slices section of the Tail adjustments card
#'
#' All slice rows are in the page; the hidden number input pareto_slice_times decides
#' which are shown. The add button and the remove button of each row change it on the
#' server, and a saved settings file restores it like any other number.
#'
#' @return A tag list.
#' @noRd
sim_pareto_slices_ui <- function() {
  count <- "(Number(input.pareto_slice_times) || 0)"
  tagList(
    div(
      class = "sim-slices-head",
      span(class = "sim-slices-title", "Pareto slices"),
      conditionalPanel(
        condition = paste(count, "<", max_number_of_pareto_slices),
        actionButton("add_pareto_slice", "Add slice", icon = icon("plus"), class = "btn-sm btn-outline-primary")
      )
    ),
    helpText("Splices a Pareto tail onto the severity distribution above each threshold. Thresholds must increase from one slice to the next."),
    div(
      class = "sim-hidden-input",
      numericInput("pareto_slice_times", "Number of Pareto slices", value = 0, min = 0,
                   max = max_number_of_pareto_slices, step = 1)
    ),
    conditionalPanel(
      condition = paste(count, "== 0"),
      div(class = "sim-slices-note", "No slices: the severity distribution is used as it is.")
    ),
    lapply(seq_len(max_number_of_pareto_slices), function(i) {
      conditionalPanel(
        condition = paste(count, ">=", i),
        div(
          class = "sim-slice-row",
          div(class = "sim-slice-badge", i, title = paste("Slice", i)),
          div(
            class = "param-grid-2 sim-slice-fields",
            #the threshold first, the order actuaries enter a slice in (the ids keep alpha odd)
            netsimr_amount_input(paste0("slice_pareto_param_", 2 * i), "Threshold (x_m)", value = NULL, min = 0),
            numericInput(paste0("slice_pareto_param_", 2 * i - 1), "Alpha", value = NULL, min = 0)
          ),
          tags$button(
            id = paste0("remove_pareto_slice_", i), type = "button",
            class = "btn btn-sm btn-outline-secondary action-button sim-slice-remove",
            title = paste("Remove slice", i), `aria-label` = paste("Remove slice", i),
            icon("xmark")
          )
        )
      )
    }),
    conditionalPanel(
      condition = paste(count, ">=", max_number_of_pareto_slices),
      div(class = "sim-slices-note", paste("The maximum of", max_number_of_pareto_slices, "slices is reached."))
    )
  )
}

#' User interface of the Shiny NetSimR simulator
#'
#' @description The page the simulator opens in: a welcome panel and the
#'   simulator itself, where the claim, reinsurance and run settings are
#'   entered and the results, charts and report are shown.
#'   The page is built each time the application starts, not when the package
#'   is installed: its theme compiles Bootstrap's SCSS files from where bslib is
#'   installed on the machine running it, and a page built at installation kept
#'   the paths of the machine that built the package (for a CRAN binary, CRAN's
#'   build machine), where nobody else's bslib is.
#' @param request The request of the browser, passed by shiny; not used.
#' @return The user interface of the application, a bslib navbar page, which
#'   \code{\link{run_shiny_simulator}} pairs with \code{shiny_simulator_server}.
#' @keywords internal
shiny_simulator_ui <- function(request) {
  bslib::page_navbar(
    title = div(
      class = "sim-brand",
      div(class = "sim-brand-mark", icon("dice")),
      div(
        class = "sim-brand-text",
        tags$span(class = "sim-brand-title", "NetSimulator"),
        tags$span(class = "sim-brand-subtitle", "Claims simulation by NetSimR")
      )
    ),
    window_title = "NetSimulator \u2013 Claims & Reinsurance Simulator",
    id = "sim_navbar",
    selected = "welcome",
    fillable = FALSE,

    navbar_options = bslib::navbar_options(
      bg = "#0b1f3d",
      theme = "dark",
      underline = FALSE
    ),

    theme = netsimr_bs_theme("#2563eb"),

    header = tagList(
      netsimr_tool_head("netsimr-simulator-theme", netsimr_css("tools")),
      netsimr_busy_indicators("#60a5fa", "#2563eb"),
      tags$script(HTML(sim_run_state_js)),
      tags$script(HTML(sim_run_phone_js))
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
          div(class = "sim-hero-eyebrow", "NetSimulator"),
          div(class = "sim-hero-title", "Claims & Reinsurance Simulator"),
          p(
            class = "sim-hero-text",
            "Simulate insurance claims from frequency and severity distributions, ",
            "reshape the tail, apply each-and-every-loss and aggregate reinsurance ",
            "structures, then export the results and a full report."
          ),
          tags$button(
            type = "button",
            class = "btn btn-light",
            onclick = "document.querySelector('.navbar a[data-value=\"simulator\"]').click();",
            icon("play"), " Open the simulator"
          )
        ),

        bslib::card(
          netsimr_card_header("route", "How it works"),
          bslib::card_body(
            tags$ol(
              class = "sim-steps",
              tags$li(div(div(class = "sim-step-title", "Frequency"),
                          div(class = "sim-step-text", "How many claims occur per period."))),
              tags$li(div(div(class = "sim-step-title", "Severity"),
                          div(class = "sim-step-text", "How large each claim is."))),
              tags$li(div(div(class = "sim-step-title", "Tail adjustments"),
                          div(class = "sim-step-text", "Optional Pareto slices and claim caps."))),
              tags$li(div(div(class = "sim-step-title", "Reinsurance"),
                          div(class = "sim-step-text", "EEL and aggregate layers, with reinstatements."))),
              tags$li(div(div(class = "sim-step-title", "Export"),
                          div(class = "sim-step-text", "Download the simulated data or an HTML report.")))
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, md = c(4, 4, 4)),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("chart-bar"), "Frequency models"),
          div(class = "sim-stat-value",
              paste(unname(vapply(freq_dist_options, function(x) x@distr_label, character(1))), collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("chart-line"), "Severity models"),
          div(class = "sim-stat-value",
              paste(unname(vapply(sev_dist_options, function(x) x@distr_label, character(1))), collapse = paste0(" ", intToUtf8(183), " ")))
        ),
        div(
          class = "sim-stat",
          div(class = "sim-stat-label", icon("shield-halved"), "Reinsurance"),
          div(class = "sim-stat-value",
              paste(setdiff(reinsurance_structures_options, "No Reinsurance Structure"), collapse = paste0(" ", intToUtf8(183), " ")))
        )
      ),

      netsimr_about_card("Claims simulator", tagList(
        "The model behind the tool, ", tags$code("simulate_claims()"), ", can also be used from R."))
    ),

    # ------------------------------------------------------- Claims simulator
    bslib::nav_panel(
      title = "Claims Simulator",
      value = "simulator",
      icon = icon("dice"),

      div(
        class = "sim-page-intro",
        div(
          h2(class = "sim-page-title", "Claims simulator"),
          p(class = "sim-page-subtitle",
            "Set the claim frequency, the claim severity and the reinsurance, then run the simulation.")
        )
      ),

      bslib::layout_columns(
        class = "sim-layout",
        col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8), xxl = c(3, 9)),

        # ---- Run panel
        bslib::card(
          netsimr_card_header("gear", "Simulation", "Run settings and outputs"),
          bslib::card_body(
            #the same limits as the validation in find_missing_simulation_settings()
            netsimr_amount_input('numberOfSimulations', 'Number of simulations',
                             value = 50000L, min = 1L, max = max_number_of_simulations),
            bslib::input_switch('seedSetBinary', 'Custom seed', value = FALSE),
            uiOutput("seed_value"),
            #(worker processes cannot be started in a web browser)
            if (!netsimr_in_browser()) {
              tagList(
                radioButtons('processing', 'Processing', inline = TRUE, selected = "auto",
                             choices = c("Auto" = "auto", "Single" = "single", "Parallel" = "parallel")),
                helpText("Auto runs the chunks on parallel workers when that is faster. Parallel runs show no live progress.")
              )
            },

            uiOutput("expected_gross_claims"),

            tags$hr(class = "sim-divider"),

            actionButton("RunSimulations", "Run Simulations", icon = icon("play"), class = "btn-run"),

            tags$hr(class = "sim-divider"),

            div(class = "sim-section-label", "Results"),
            div(
              class = "sim-downloads",
              uiOutput("downloadDataButton"),
              uiOutput("downloadReportButton"),
              div(class = "sim-downloads-hint", icon("circle-info"),
                  "Run a simulation to enable the downloads.")
            ),

            #save and load settings, and built-in examples (R/simulator_tool_settings.R)
            tags$hr(class = "sim-divider"),
            sim_settings_io_ui()
          )
        ),

        # ---- Model configuration
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, md = c(6, 6, 12, 6, 6)),

          bslib::card(
            netsimr_card_header("chart-bar", "Frequency", "How many claims occur per simulated period."),
            bslib::card_body(
              div(class = "sim-section-label", "Distribution"),
              div(
                class = "pill-radio",
                netsimr_hidden_label(radioButtons(
                  inputId = 'freqDistr',
                  label = 'Frequency distribution',
                  inline = TRUE,
                  choiceNames = unname(vapply(freq_dist_options, function(x) x@distr_label, character(1))),
                  choiceValues = unname(vapply(freq_dist_options, function(x) x@distrID, character(1)))
                ))
              ),
              div(class = "sim-section-label mt-2", "Parameters"),
              div(
                class = "param-grid",
                lapply(seq_len(max(vapply(freq_dist_options, function(x) length(x@paramIDs), integer(1)))),
                       function(i){ uiOutput(paste0('freq_param_', i)) })
              ),
              uiOutput("freq_implied_moments")
            )
          ),

          bslib::card(
            netsimr_card_header("chart-line", "Severity", "The size of each individual claim."),
            bslib::card_body(
              div(class = "sim-section-label", "Distribution"),
              div(
                class = "pill-radio",
                netsimr_hidden_label(radioButtons(
                  inputId = 'sevDistr',
                  label = 'Severity distribution',
                  inline = TRUE,
                  choiceNames = unname(vapply(sev_dist_options, function(x) x@distr_label, character(1))),
                  choiceValues = unname(vapply(sev_dist_options, function(x) x@distrID, character(1)))
                ))
              ),
              div(class = "sim-section-label mt-2", "Parameters"),
              div(
                class = "param-grid",
                lapply(seq_len(max(vapply(sev_dist_options, function(x) length(x@paramIDs), integer(1)))),
                       function(i){ uiOutput(paste0('sev_param_', i)) })
              ),
              uiOutput("sev_implied_moments"),
              conditionalPanel(
                condition = "input.sevDistr == 'Normal'",
                class = "sim-truncate",
                bslib::input_switch('sevTruncateAtZero', 'Truncate at zero', value = FALSE),
                helpText("Draws claims from the Normal distribution conditional on being positive, so no claim is negative.")
              )
            )
          ),

          bslib::card(
            netsimr_card_header("scissors", "Tail adjustments", "Optionally reshape or cap the severity tail."),
            bslib::card_body(
              bslib::layout_columns(
                col_widths = bslib::breakpoints(sm = 12, md = c(7, 5)),
                div(
                  sim_pareto_slices_ui()
                ),
                div(
                  bslib::input_switch('sevCapBinary', 'Severity cap', value = FALSE),
                  helpText("Caps any single claim at a maximum amount."),
                  uiOutput("sev_cap_amount_ui")
                )
              )
            )
          ),

          bslib::card(
            netsimr_card_header("shield-halved", "Each & every loss (EEL)",
                            "Applied to each claim before it is summed into the total."),
            bslib::card_body(
              div(class = "sim-section-label", "Structure"),
              div(
                class = "pill-radio pill-radio-grid",
                netsimr_hidden_label(radioButtons('reinsuranceStructureEEL', 'Each and every loss structure',
                                              choices = reinsurance_structures_options, inline = TRUE))
              ),
              div(
                class = "param-grid mt-3",
                #the limit first, as a layer is quoted ("limit xs deductible")
                uiOutput("reinsuranceStructureLimitEEL"),
                uiOutput("reinsuranceStructureDeductibleEEL")
              ),
              uiOutput("reinsuranceStructureLimitedReinstatements_ui"),
              uiOutput("reinsuranceStructureReinstatementLimit_ui")
            )
          ),

          bslib::card(
            netsimr_card_header("umbrella", "Aggregate layer (AL)",
                            "Applied to each simulation's total after EEL: deductible first, then the limit and reinstatement cap."),
            bslib::card_body(
              div(class = "sim-section-label", "Structure"),
              div(
                class = "pill-radio pill-radio-grid",
                netsimr_hidden_label(radioButtons('reinsuranceStructureAL', 'Aggregate layer structure',
                                              choices = reinsurance_structures_options, inline = TRUE))
              ),
              div(
                class = "param-grid mt-3",
                uiOutput("reinsuranceStructureLimitAL"),
                uiOutput("reinsuranceStructureDeductibleAL")
              )
            )
          ),

          #on a narrow screen the Run panel is above all the settings: the button again, at their foot
          div(
            class = "sim-run-phone",
            tags$button(id = "RunSimulationsPhone", type = "button", class = "btn btn-default btn-run",
                        icon("play"), "Run Simulations")
          )
        )
      )
    ),

    # ------------------------------------------------ Report and Compare tabs
    #defined in R/simulator_tool_tabs.R
    sim_report_tab_ui("report"),
    sim_compare_tab_ui("compare"),

    bslib::nav_spacer(),
    bslib::nav_item(netsimr_theme_switch())
  )
}
