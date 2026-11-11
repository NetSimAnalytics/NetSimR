#' Styles of the six tools on a phone
#'
#' Added after each tool's own styles, so that these rules win where both apply. Most
#' visitors of the website open the tools on a phone: the notifications move to the top of
#' the screen, clear of the Run buttons at the foot of the settings; the small upper case
#' labels and the sliders' ticks are a little larger; the controls that were under a
#' finger's width are taller; and a table marked to stack shows each row as a card, a
#' line per column, instead of scrolling sideways.
#' @noRd
netsimr_phone_css <- "
/* the results a Run button scrolls to stop a little below the top of the screen */
.sim-layout > * {
  scroll-margin-top: 0.75rem;
}

/* the Run button repeated at the foot of the simulator's settings, where the Run panel
   is a screen or two above */
.sim-run-phone {
  display: none;
}

@media (max-width: 991.98px) {
  .sim-run-phone {
    display: block;
  }
}

@media (max-width: 767.98px) {
  /* (the theme writes the panel's id twice in its own rule, which a single id would not outrank) */
  #shiny-notification-panel#shiny-notification-panel {
    top: 0;
    bottom: auto;
    left: 0;
    right: 0;
    width: auto;
    max-width: none;
    padding: 0.25rem;
  }

  .shiny-notification.shiny-notification {
    padding: 0.9rem 2.25rem 0.9rem 1rem;
    margin: 0.5rem;
  }

  .sim-stat-label,
  .dft-stat-label,
  .dft-table th,
  .dft-badge,
  .sim-run-id,
  .pfo-head,
  .xlp-year-head,
  .rsv-origin-head {
    font-size: 0.75rem;
  }

  .irs.irs--shiny .irs-grid-text {
    font-size: 11px;
  }

  .irs.irs--shiny .irs-min,
  .irs.irs--shiny .irs-max,
  .irs.irs--shiny .irs-single,
  .irs.irs--shiny .irs-from,
  .irs.irs--shiny .irs-to {
    font-size: 12px;
  }

  /* taller controls: the link ratios, the links of the About card, the file inputs and
     the small buttons */
  .rsv-ratio {
    padding: 0.4rem 0.5rem;
    margin: -0.2rem -0.35rem;
  }

  .netsimr-feedback-link,
  .btn-sm {
    padding: 0.45rem 0.85rem;
    font-size: 0.875rem;
  }

  .input-group .btn-file,
  .input-group .form-control[readonly] {
    min-height: 2.6rem;
    display: flex;
    align-items: center;
  }
}

@media (max-width: 575.98px) {
  /* a table marked to stack: each row a card, each cell a line with its column's name */
  .dft-table-stack .dft-table,
  .dft-table-stack .dft-table tbody,
  .dft-table-stack .dft-table tr,
  .dft-table-stack .dft-table td {
    display: block;
  }

  .dft-table-stack .dft-table thead {
    display: none;
  }

  .dft-table-stack .dft-table tr {
    border: 1px solid var(--sim-border);
    border-radius: 12px;
    padding: 0.55rem 0.8rem;
    margin-bottom: 0.6rem;
  }

  .dft-table-stack .dft-table tbody tr:last-child {
    margin-bottom: 0;
  }

  .dft-table-stack .dft-table td,
  .dft-table-stack .dft-table tbody tr:last-child td {
    display: flex;
    align-items: baseline;
    gap: 0.4rem;
    padding: 0.2rem 0;
    border: 0;
    white-space: normal;
    text-align: right;
    background: transparent;
  }

  .dft-table-stack .dft-table td::before {
    content: attr(data-label);
    flex: none;
    font-size: 0.75rem;
    font-weight: 700;
    letter-spacing: 0.05em;
    text-transform: uppercase;
    color: var(--sim-muted);
    text-align: left;
    margin-right: auto;
    padding-right: 0.6rem;
  }

  .dft-table-stack .dft-table td:first-child,
  .dft-table-stack .dft-table tbody tr:last-child td:first-child {
    display: block;
    text-align: left;
    font-size: 1rem;
    padding-bottom: 0.3rem;
  }

  /* a parameter the distribution of the row does not have takes no line */
  .dft-table-stack .dft-table td[data-label^='Parameter']:has(> .dft-na:only-child) {
    display: none;
  }

  .dft-table-stack .dft-table td:first-child::before {
    content: none;
  }

  /* the programmes of the reinsurance tool: a card each, two inputs to a line, each under
     its own name, in place of a grid twelve columns wide */
  .xlp-prog-grid.xlp-year-grid {
    display: block;
    min-width: 0 !important;
  }

  .xlp-prog-grid > .xlp-year-head {
    display: none;
  }

  .xlp-prog-grid .xlp-prog-row {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 0.5rem 0.75rem;
    align-items: end;
    border: 1px solid var(--sim-border);
    border-radius: 12px;
    padding: 0.75rem 0.8rem;
    margin-bottom: 0.75rem;
  }

  .xlp-prog-grid .xlp-prog-row:last-child {
    margin-bottom: 0;
  }

  .xlp-prog-grid .xlp-prog-cell:first-child {
    grid-column: 1 / -1;
  }

  .xlp-prog-grid .xlp-prog-cell::before {
    content: attr(data-label);
    display: block;
    font-size: 0.75rem;
    font-weight: 700;
    letter-spacing: 0.05em;
    text-transform: uppercase;
    color: var(--sim-muted);
    margin-bottom: 0.15rem;
  }
}
"

#' Script that brings the results into view after a Run button on a narrow screen
#'
#' Below the width at which the settings sit beside the results, the results are under
#' the settings, off the screen: a click of a Run button did its work out of sight. The
#' script remembers the part of the layout after the one holding the button, and scrolls
#' to it once the server has finished, unless it answered with an error message (the
#' settings to correct are where the reader already is). The simulator is left out: its
#' own Run button moves to the Results tab.
#' @noRd
netsimr_results_scroll_js <- "
(function () {
  var target = null, busy = false;

  document.addEventListener('click', function (event) {
    if (!event.target.closest || window.innerWidth >= 992) return;
    var button = event.target.closest('button.btn-run');
    if (!button || button.id === 'RunSimulations' || button.id === 'RunSimulationsPhone') return;
    var layout = button.closest('.sim-layout');
    if (!layout) return;
    var item = button;
    while (item.parentElement && item.parentElement !== layout) item = item.parentElement;
    target = item.nextElementSibling;
    busy = false;
  });

  document.addEventListener('DOMContentLoaded', function () {
    if (!window.jQuery) return;
    window.jQuery(document).on('shiny:busy', function () { if (target) busy = true; });
    window.jQuery(document).on('shiny:idle', function () {
      if (!target || !busy) return;
      var results = target;
      target = null;
      busy = false;
      if (document.querySelector('#shiny-notification-panel .shiny-notification-error')) return;
      window.setTimeout(function () {
        if (results.getBoundingClientRect().top > 80) results.scrollIntoView({behavior: 'smooth', block: 'start'});
      }, 120);
    });
  });
})();
"

#' Script of the Run button repeated at the foot of the simulator's settings
#'
#' The button clicks the Run button of the Run panel, and shows what that one shows
#' (Running..., and disabled, during a run). A run it started ends on the Report tab,
#' where the results are, as a Run button of the other tools ends at its results.
#' @noRd
sim_run_phone_js <- "
document.addEventListener('DOMContentLoaded', function () {
  var run = document.getElementById('RunSimulations'), copy = document.getElementById('RunSimulationsPhone');
  if (!run || !copy) return;
  var fromCopy = false;
  copy.addEventListener('click', function () { fromCopy = true; run.click(); });
  /* a run started from the foot of the settings ends on the Report tab, where its results are */
  Shiny.addCustomMessageHandler('netsimr-run-succeeded', function (message) {
    var started = fromCopy;
    fromCopy = false;
    if (!started || window.innerWidth >= 992) return;
    var link = document.querySelector('.navbar a[data-value=\"report\"]');
    if (link) { link.click(); window.scrollTo(0, 0); }
  });
  if (!window.MutationObserver) return;
  new MutationObserver(function () {
    copy.disabled = run.disabled;
    copy.innerHTML = run.innerHTML;
    /* the run is over, whether or not it succeeded (the message of a success comes first) */
    if (!run.disabled) fromCopy = false;
  }).observe(run, {attributes: true, childList: true, subtree: true});
});
"

#' The phone styles and the scroll to the results, for the head of a tool's page
#' @noRd
netsimr_phone_head <- function() {
  tagList(tags$style(HTML(netsimr_phone_css)), tags$script(HTML(netsimr_results_scroll_js)))
}
