#The settings files of the tools list their inputs and how each is restored (its kind); the
#pages define the inputs. These checks keep the two in step: an input renamed or removed on a
#page, or given another kind, would otherwise be skipped without a word when a file is loaded.

#the inputs of a page: for each id, the kinds of setting it can hold
page_input_kinds <- function(page) {
  found <- list()
  add <- function(id, kind) if (!is.null(id)) found[[id]] <<- unique(c(found[[id]], kind))
  has_class <- function(attribs, class) {
    classes <- unlist(strsplit(paste(unlist(attribs[names(attribs) == "class"]), collapse = " "), "\\s+"))
    class %in% classes
  }
  walk <- function(x) {
    if (inherits(x, "shiny.tag")) {
      a <- x$attribs
      id <- a$id
      if (identical(x$name, "div") && has_class(a, "shiny-input-radiogroup")) add(id, "radio")
      if (identical(x$name, "select")) add(id, c("select", "column"))
      if (identical(x$name, "input")) {
        type <- if (is.null(a$type)) "text" else a$type
        if (type == "checkbox") add(id, c("switch", "checkbox"))
        if (type == "number" || has_class(a, "sim-amount-input")) add(id, "numeric")
        if (has_class(a, "js-range-slider")) add(id, "slider")
        if (type == "text" && !has_class(a, "sim-amount-input") && !has_class(a, "js-range-slider")) add(id, "text")
      }
      if (identical(x$name, "textarea")) add(id, "textarea")
      lapply(x$children, walk)
    } else if (is.list(x)) {
      lapply(x, walk)
    }
    invisible(NULL)
  }
  walk(page)
  found
}

expect_settings_on_page <- function(page, kinds, tool) {
  inputs <- page_input_kinds(page)
  for (id in names(kinds)) {
    expect_true(id %in% names(inputs), label = paste0("the ", tool, "'s setting '", id, "' is an input of its page"))
    if (id %in% names(inputs)) {
      expect_true(kinds[[id]] %in% inputs[[id]],
                  label = paste0("the ", tool, "'s setting '", id, "' (", kinds[[id]], ") is an input of its kind"))
    }
  }
}

test_that("each tool's settings are inputs of its page, of the kind the file restores", {
  expect_settings_on_page(distribution_fitting_tool_UI(NULL), dft_settings_inputs, "distribution fitting tool")
  expect_settings_on_page(reserving_tool_UI(NULL), rsv_settings_inputs, "reserving tool")
  expect_settings_on_page(reinsurance_pricing_tool_UI(NULL), xlp_settings_inputs, "reinsurance pricing tool")
  expect_settings_on_page(portfolio_tool_UI(NULL), pfo_settings_inputs, "portfolio tool")
  expect_settings_on_page(GLMFittingToolUI(NULL), stats::setNames(glm_settings_inputs$kind, glm_settings_inputs$id),
                          "GLM fitting tool")
})

test_that("a loaded setting is checked against the choices the page offers", {
  choices <- page_input_choices(reserving_tool_UI(NULL))
  expect_identical(choices$sep, c(",", ";", "\t"))
  expect_identical(choices$quote, c("\"", "'", ""))
  expect_identical(choices$tail_method, c("none", "exponential", "inverse power", "factor"))
  expect_identical(choices$sii_segment, as.character(sii_nonlife_segments$segment))
  expect_true(setting_usable("Mack", "radio", choices$sigma))
  expect_false(setting_usable("Mack's", "radio", choices$sigma))
  #a choice that names an entity of HTML is read as the page shows it
  page <- shiny::selectInput("x", "X", c("A & B" = "a&b", "<c>" = "<c>"))
  expect_identical(page_input_choices(page)$x, c("a&b", "<c>"))
})

test_that("the values saved are each input's single value, with empty columns left out", {
  input <- list(sep = ";", quote = "", origin_col = "", value_col = "paid", periods = c("1", "2"), prior = NA_real_)
  kinds <- c(sep = "radio", quote = "radio", origin_col = "column", value_col = "column", periods = "select",
             prior = "numeric", missing = "numeric")
  expect_identical(collect_setting_values(input, kinds),
                   list(sep = ";", quote = "", value_col = "paid", prior = NA_real_))
})

test_that("a tool tells the values it set in an input from a change the user made", {
  own <- netsimr_own_updates()
  expect_false(own$is_echo("severity", "Gamma"))
  own$expect("severity", "Pareto")
  #the user chose another before the page reported the tool's value: nothing is skipped after
  expect_false(own$is_echo("severity", "Gamma"))
  expect_false(own$is_echo("severity", "Pareto"))
  own$expect("prior", 0.65)
  own$expect("prior", 0.7)
  expect_true(own$is_echo("prior", 0.65))
  expect_true(own$is_echo("prior", "0.7"))
  expect_false(own$is_echo("prior", 0.7))
})
