##############
#The building blocks of the tools' pages
##############

# Shared by the six Shiny tools: the start-up options, the theme switch and its script, card
# headers, labels for screen readers, amount inputs with thousands separators, and the layout
# of an analysis tab (its settings beside its results). The tools' own pages are in
# R/<tool>_ui.R; the styles these use are in the tools' style sheets.

#' The header of a card: an icon, a title and an optional subtitle
#'
#' @param icon_name Font Awesome icon name.
#' @param title Card title.
#' @param subtitle Optional short description shown under the title.
#' @noRd
netsimr_card_header <- function(icon_name, title, subtitle = NULL) {
  bslib::card_header(
    class = "sim-card-header",
    div(class = "sim-card-icon", icon(icon_name)),
    div(
      class = "sim-card-titles",
      div(class = "sim-card-title", title),
      if (!is.null(subtitle)) div(class = "sim-card-subtitle", subtitle)
    )
  )
}

#' Hide an input's label from sight, not from screen readers
#'
#' Inputs whose heading is drawn separately (the pill radio groups under a section label,
#' the run name boxes of the Compare tab) still need a label for their accessible name: a
#' label of NULL leaves the group announced without one. Bootstrap's visually-hidden class
#' keeps the label for screen readers and takes it out of the layout.
#' @param input A shiny input whose label is one of its direct children, e.g. from
#'   radioButtons() or textInput().
#' @return The input with the class added to its label.
#' @noRd
netsimr_hidden_label <- function(input) {
  input$children <- lapply(input$children, function(child) {
    if (inherits(child, "shiny.tag") && identical(child$name, "label")) {
      tagAppendAttributes(child, class = "visually-hidden")
    } else {
      child
    }
  })
  input
}

#' A number input that shows thousands separators
#'
#' A browser's number input cannot show "1,000,000", so amounts are text inputs that the
#' script \code{netsimr_amount_input_js} gives a shiny input binding: the field shows the
#' amount with thousands separators once it is entered (on leaving the field), and shiny
#' receives the number, as from \code{numericInput()}: NA when the field is empty or does
#' not hold a number. \code{updateNumericInput()} updates it as it updates a number input.
#'
#' @param inputId The input id.
#' @param label The label.
#' @param value The starting number, or NULL or NA for an empty field.
#' @param min,max The bounds, NA for none; shown as attributes, checked by the server.
#' @return A shiny input container.
#' @noRd
netsimr_amount_input <- function(inputId, label, value = NULL, min = NA, max = NA) {
  no_sci <- function(x) format(x, scientific = FALSE, trim = TRUE, digits = 15)
  shown <- if (length(value) == 1 && is.numeric(value) && is.finite(value)) netsimr_amount_text(value) else ""
  div(
    class = "form-group shiny-input-container",
    tags$label(class = "control-label", id = paste0(inputId, "-label"), `for` = inputId, label),
    tags$input(
      id = inputId, type = "text", class = "form-control sim-amount-input", value = shown,
      #a keypad with a minus sign only where an amount can be negative
      inputmode = if (isTRUE(min >= 0)) "decimal" else "text", autocomplete = "off",
      min = if (!is.na(min)) no_sci(min), max = if (!is.na(max)) no_sci(max)
    )
  )
}

#' An amount as the amount inputs show it: thousands separators below 1e15, the number as R
#' writes it otherwise (1e+308 rather than 309 digits)
#' @noRd
netsimr_amount_text <- function(x) {
  if (abs(x) >= 1e15) return(format(x, digits = 15))
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 15)
}

#' The shiny input binding of the amount inputs (see netsimr_amount_input())
#' @noRd
netsimr_amount_input_js <- "
(function () {
  if (!window.Shiny || !Shiny.inputBindings) return;
  /* a whole number written with thousands separators */
  function group(digits) {
    var out = '';
    for (var i = 0; i < digits.length; i++) {
      if (i > 0 && (digits.length - i) % 3 === 0) out += ',';
      out += digits.charAt(i);
    }
    return out;
  }
  function digits_only(s) { return /^[0-9]+$/.test(s); }
  /* the number in a field's text, or null when it is empty or not a number. Commas and
     spaces (as in much of Europe) are thousands separators only where they group the whole
     part in threes: 1,000,000 and 1 000 000 are a million, but 2,50 is not read as 250,
     a decimal comma that would silently make an amount a hundred times larger */
  function parse(text) {
    var t = String(text === null || text === undefined ? '' : text).trim();
    [' ', String.fromCharCode(160), String.fromCharCode(8239)].forEach(function (space) {
      t = t.split(space).join(',');
    });
    if (t === '') return null;
    if (t.indexOf(',') >= 0) {
      var sign = /^[+-]/.test(t) ? t.charAt(0) : '';
      var parts = t.slice(sign.length).split('.');
      if (parts.length > 2) return null;
      var groups = parts[0].split(',');
      var grouped = groups[0].length >= 1 && groups[0].length <= 3 && groups.every(function (g, i) {
        return digits_only(g) && (i === 0 || g.length === 3);
      });
      if (!grouped || (parts.length === 2 && parts[1] !== '' && !digits_only(parts[1]))) return null;
      t = sign + groups.join('') + (parts.length === 2 ? '.' + parts[1] : '');
    }
    var n = Number(t);
    return isNaN(n) || !isFinite(n) ? null : n;
  }
  /* a number as the field shows it, as netsimr_amount_text() writes it in R */
  function format(x) {
    if (typeof x !== 'number' || !isFinite(x)) return '';
    var s = String(x);
    if (Math.abs(x) >= 1e15 || s.indexOf('e') >= 0) return s;
    var sign = s.charAt(0) === '-' ? '-' : '';
    if (sign) s = s.slice(1);
    var dot = s.indexOf('.');
    return sign + group(dot < 0 ? s : s.slice(0, dot)) + (dot < 0 ? '' : s.slice(dot));
  }
  /* once a number is entered the field shows it with its separators; text that is not a
     number is left as typed, for the user to correct */
  function tidy(el) {
    var n = parse(el.value);
    if (n !== null) el.value = format(n);
  }
  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function (scope) { return $(scope).find('input.sim-amount-input'); },
    getValue: function (el) { return parse(el.value); },
    /* as a number input: shiny turns an empty field into NA */
    getType: function () { return 'shiny.number'; },
    setValue: function (el, value) {
      el.value = typeof value === 'number' ? format(value) : format(parse(value));
    },
    subscribe: function (el, callback) {
      $(el).on('input.simAmount', function () { callback(true); });
      $(el).on('change.simAmount', function () { tidy(el); callback(false); });
      /* Enter shows the separators at once, as leaving the field does */
      $(el).on('keydown.simAmount', function (event) { if (event.key === 'Enter') tidy(el); });
    },
    unsubscribe: function (el) { $(el).off('.simAmount'); },
    getRatePolicy: function () { return { policy: 'debounce', delay: 250 }; },
    /* the messages of updateNumericInput() */
    receiveMessage: function (el, data) {
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);
      ['min', 'max'].forEach(function (key) {
        if (data.hasOwnProperty(key)) el.setAttribute(key, data[key]);
      });
      if (typeof data.label === 'string') $('#' + el.id + '-label').text(data.label);
      $(el).trigger('change');
    },
    getState: function (el) { return { label: $('#' + el.id + '-label').text(), value: parse(el.value) }; }
  });
  /* before shiny's text binding, which would otherwise claim these text inputs */
  Shiny.inputBindings.register(binding, 'netsimr.amountInput', 10);
})();
"

#' The theme switch of a tool's navbar (light / dark / system)
#'
#' @noRd
netsimr_theme_switch <- function() {
  theme_button <- function(value, icon_name, label) {
    tags$button(
      type = "button",
      class = "theme-btn",
      `data-theme-value` = value,
      title = paste(label, "theme"),
      `aria-label` = paste(label, "theme"),
      `aria-pressed` = "false",
      icon(icon_name),
      tags$span(class = "theme-btn-label", label)
    )
  }
  div(
    class = "theme-switch",
    role = "group",
    `aria-label` = "Colour theme",
    theme_button("light", "sun", "Light"),
    theme_button("dark", "moon", "Dark"),
    theme_button("system", "circle-half-stroke", "System")
  )
}

#' Script that applies and remembers the light / dark / system theme choice
#'
#' Each tool remembers its own choice. With no stored choice the app follows the system setting.
#' @param storage_key The localStorage key the choice is saved under.
#' @noRd
netsimr_theme_js <- function(storage_key) {
  sub("__STORAGE_KEY__", storage_key, netsimr_theme_js_template, fixed = TRUE)
}

netsimr_theme_js_template <- "
(function () {
  var KEY = '__STORAGE_KEY__';
  var media = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;

  function storedMode() {
    try { return localStorage.getItem(KEY) || 'system'; } catch (e) { return 'system'; }
  }

  function applyTheme(mode) {
    var dark = mode === 'dark' || (mode === 'system' && media && media.matches);
    document.documentElement.setAttribute('data-bs-theme', dark ? 'dark' : 'light');
    var buttons = document.querySelectorAll('.theme-switch .theme-btn');
    for (var i = 0; i < buttons.length; i++) {
      var on = buttons[i].getAttribute('data-theme-value') === mode;
      buttons[i].classList.toggle('active', on);
      buttons[i].setAttribute('aria-pressed', on ? 'true' : 'false');
    }
  }

  applyTheme(storedMode());

  if (media) {
    var onSystemChange = function () { if (storedMode() === 'system') applyTheme('system'); };
    if (media.addEventListener) media.addEventListener('change', onSystemChange);
    else if (media.addListener) media.addListener(onSystemChange);
  }

  document.addEventListener('DOMContentLoaded', function () { applyTheme(storedMode()); });

  document.addEventListener('click', function (event) {
    var button = event.target.closest && event.target.closest('.theme-switch .theme-btn');
    if (!button) return;
    var mode = button.getAttribute('data-theme-value');
    try { localStorage.setItem(KEY, mode); } catch (e) {}
    applyTheme(mode);
  });
})();
"

#' Script that tells the server which theme the page is showing
#'
#' The charts are drawn on the server with colours for that theme, on a transparent
#' background, and redrawn when the theme changes.
#' @param input_id The namespaced id of the theme input.
#' @noRd
netsimr_theme_input_js <- function(input_id) {
  paste0("
(function () {
  var inputId = '", input_id, "';
  function send() {
    if (!window.Shiny || !window.Shiny.setInputValue) return;
    var theme = document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'dark' : 'light';
    window.Shiny.setInputValue(inputId, theme);
  }
  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected shiny:sessioninitialized', send);
  }
  if (window.MutationObserver) {
    new MutationObserver(send).observe(document.documentElement, {
      attributes: true,
      attributeFilter: ['data-bs-theme']
    });
  }
  send();
})();
")
}

#' Page title and subtitle
#'
#' @noRd
netsimr_page_intro <- function(title, subtitle) {
  div(
    class = "sim-page-intro",
    div(
      h2(class = "sim-page-title", title),
      p(class = "sim-page-subtitle", subtitle)
    )
  )
}

#' Two-column page layout: settings on the left, results on the right
#'
#' @noRd
netsimr_layout <- function(settings, ...) {
  bslib::layout_columns(
    class = "sim-layout",
    col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8), xxl = c(3, 9)),
    settings,
    div(class = "dft-results", ...)
  )
}

#' The settings card of an analysis tab
#'
#' @param icon_name Font Awesome icon name.
#' @param title Card title.
#' @param subtitle Short description shown under the title.
#' @param ... Card body contents.
#' @noRd
netsimr_settings_card <- function(icon_name, title, subtitle, ...) {
  bslib::card(
    class = "dft-settings",
    netsimr_card_header(icon_name, title, subtitle),
    bslib::card_body(...)
  )
}

#' Muted help line with an icon
#'
#' @noRd
netsimr_help <- function(...) {
  div(class = "dft-help", icon("circle-info"), div(...))
}

#' Column selector, empty until a file is uploaded
#'
#' @noRd
netsimr_column_select <- function(id, label) {
  selectizeInput(id, label, choices = NULL,
                 options = list(placeholder = "Upload a data file first"))
}

#' Card with underlined tabs for the results of an analysis
#'
#' @noRd
netsimr_result_tabs <- function(...) {
  tagAppendAttributes(bslib::navset_card_underline(...), class = "dft-tabs")
}

#' A chart output in the tools' style
#'
#' The chart is an image drawn on the server to the width of its card, and
#' drawn again when the card or the theme changes.
#' @noRd
netsimr_plot_output <- function(id, height = "440px") {
  div(class = "dft-plot", plotOutput(id, height = height))
}

# Maximum upload size (in bytes) accepted by the file inputs of the tools. Shiny's default is 5 MB, which is too small for
# claims datasets; 64 GB was the limit introduced in version 0.1.5 and is kept
# so that no previously accepted file is rejected. In practice the available
# memory limits what read.csv() can load, not this setting.
shiny_tool_max_request_size <- 64 * 1024^3

# onStart hook of the tools: raises the
# upload limit only while the app runs and restores the user's option when it
# stops, so that loading the package does not change the R session.
shiny_tool_on_start <- function() {
  old_options <- options(shiny.maxRequestSize = shiny_tool_max_request_size)
  onStop(function() options(old_options))
}

# The style sheets of the tools and their reports (inst/css), one string for each name given:
# "tools" (all six tools), "analysis" (the five that analyse data), a tool's own ("glm",
# "reserving", "reinsurance", "portfolio", "simulator-tabs", "simulator-settings") and "report".
# They are read when a page or a report is built, from where the package is installed.
netsimr_css <- function(names) {
  vapply(names, function(name) {
    file <- system.file("css", paste0(name, ".css"), package = "NetSimR")
    if (!nzchar(file)) stop("The style sheet ", name, ".css of NetSimR is missing: reinstall the package.", call. = FALSE)
    paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

# The theme of a tool's page: Bootstrap 5 with the tool's accent colour as its primary colour, set
# in Inter, with code in JetBrains Mono when `code` is TRUE (the fonts come with the package, see
# R/fonts.R). The theme is built when the page is (R/<tool>_ui.R), not when the package is
# installed (tests/testthat/test-ui-portability.R).
netsimr_bs_theme <- function(primary, code = FALSE) {
  text_font <- bslib::font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif")
  bslib::bs_theme(
    version = 5,
    primary = primary,
    secondary = "#64748b",
    success = "#16a34a",
    info = "#0891b2",
    warning = "#d97706",
    danger = "#dc2626",
    base_font = text_font,
    heading_font = text_font,
    code_font = if (code) bslib::font_collection("JetBrains Mono", "ui-monospace", "SFMono-Regular", "Menlo", "Consolas", "monospace")
  )
}

# The head of a tool's page: the script of its theme switch, which keeps the choice under
# `storage_key`; the styles (the tools' shared ones, then its own); the fonts (with the code
# font when `code` is TRUE); the layout on a phone; the binding of the amount inputs, unless
# `amount_inputs` is FALSE; and the tool's own scripts, in `...`.
netsimr_tool_head <- function(storage_key, styles, ..., code = FALSE, amount_inputs = TRUE) {
  tags$head(
    tags$script(HTML(netsimr_theme_js(storage_key))),
    lapply(styles, function(css) tags$style(HTML(css))),
    netsimr_fonts_head(code = code),
    netsimr_phone_head(),
    if (amount_inputs) tags$script(HTML(netsimr_amount_input_js)),
    ...
  )
}

# Shiny's busy indicators, which follow the page's theme: a bar across the top of the page while
# the server works, in the tool's accent colour (`strong`, between two of its lighter tints,
# `light`), and spinners on the outputs being recalculated.
netsimr_busy_indicators <- function(light, strong) {
  tagList(
    useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
    busyIndicatorOptions(
      pulse_background = paste0("linear-gradient(90deg, ", light, ", ", strong, ", ", light, ")"),
      pulse_height = "4px"
    )
  )
}

# A row of pill-shaped radio buttons. Their label is for screen readers only: a section label
# above them names them on the page.
netsimr_pill_radio <- function(id, label, choices, selected) {
  div(class = "pill-radio", netsimr_hidden_label(radioButtons(id, label, choices, selected, inline = TRUE)))
}

# The options a delimited text file is read with, each under a section label: whether its first
# row holds the column names, its separator, its decimal mark and, given an id for it, its quote
# character. The arguments are the inputs' ids.
netsimr_file_options <- function(header, sep, dec, quote = NULL) {
  tagList(
    bslib::input_switch(header, "First row holds column names", value = TRUE),
    div(class = "sim-section-label mt-2", "Separator"),
    netsimr_pill_radio(sep, "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
    div(class = "sim-section-label mt-2", "Decimal mark"),
    netsimr_pill_radio(dec, "Decimal mark", c("Point (1.5)" = ".", "Comma (1,5)" = ","), "."),
    if (!is.null(quote)) {
      tagList(
        div(class = "sim-section-label mt-2", "Quote"),
        netsimr_pill_radio(quote, "Quote", c("Double" = '"', "Single" = "'", "None" = ""), '"')
      )
    }
  )
}
