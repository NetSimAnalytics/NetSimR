##############
#Generalised linear models of the loss cost
##############

# What the GLM tool and its validation (R/glm_validation.R) share: the functions a model formula
# may call and the check of a formula, the family of a model (with the Tweedie family of
# R/tweedie.R), bands of a numeric variable, and the relativities of a fitted model.

# Functions a formula may call, besides the operators + - * : / ^ and brackets. A
# formula is evaluated by model.frame() and glm(), so without this list the terms
# typed in the formula box, or read from a shared settings file, could run any R
# code on the machine the tool runs on (a settings file is plain text, and Shiny
# apps are deployed on servers). These transform a column or mark a term and
# reach nothing beyond their arguments.
glm_formula_functions <- c("I", "log", "exp", "sqrt", "pmin", "pmax", "poly", "ns", "bs", "cut", "c", "factor", "as.factor",
                           "interaction", "offset")

# The environment of the tool's formulas: the global environment, with the spline bases of
# the splines package, ns() (natural cubic splines) and bs() (B-splines), so a formula finds
# them without the package attached. A model keeps its formula's environment, and with it
# these two functions, so a downloaded model predicts on new data in any R session (the
# splines' knots are kept in the model's terms, as for any spline term).
glm_formula_env <- function() {
  env <- new.env(parent = globalenv())
  env$ns <- splines::ns
  env$bs <- splines::bs
  env
}

# What stops a formula from being fitted, or NULL when nothing does: a formula (or
# the text of its terms) may only use the columns of the data (any name when
# columns is NULL, before the data is imported), "." for every other column,
# numbers, the operators and the functions above. The contents of I() are checked
# like everything else. Text that does not parse runs nothing, so it is left for
# the fit to report.
glm_formula_problem <- function(expr, columns = NULL) {
  if (is.character(expr)) {
    expr <- tryCatch(str2lang(expr), error = function(e) NULL)
    if (is.null(expr)) return(NULL)
  }
  allowed <- paste("it may only use the data's columns with + - * : / ^ and", glm_formula_functions_text())
  check <- function(e) {
    if (is.name(e)) {
      name <- as.character(e)
      if (name %in% c("", ".") || is.null(columns) || name %in% columns) return(NULL)
      return(paste0("'", name, "' is not a column of the data"))
    }
    if (is.call(e)) {
      fun <- e[[1]]
      name <- if (is.name(fun)) as.character(fun) else paste(deparse(fun), collapse = "")
      if (!is.name(fun) || !name %in% c("~", "+", "-", "*", ":", "/", "^", "(", glm_formula_functions)) {
        return(paste0("the formula may not call ", name, "(); ", allowed))
      }
      for (i in seq_along(e)[-1]) {
        # an empty argument, as in poly(x, ), cannot be passed on
        if (identical(e[[i]], quote(expr = ))) next
        problem <- check(e[[i]])
        if (!is.null(problem)) return(problem)
      }
      return(NULL)
    }
    if ((is.numeric(e) || is.logical(e)) && length(e) == 1) return(NULL)
    paste0("the formula may not contain ", paste(deparse(e), collapse = ""), "; ", allowed)
  }
  check(expr)
}

# The family object of a model, such as poisson(link = "log"), built in the base
# environment. The functions of a family keep the environment of the call that
# built it (poisson() and the other families leave their link argument
# unevaluated), so a family built by glm() inside the server kept the server, and
# the Shiny session with its inputs, such as a database password, in every model
# saved to a file. The Tweedie family (tweedie_family()) keeps only its power and link.
glm_family_object <- function(family, link, power = 1.5) {
  if (identical(family, "tweedie")) return(tweedie_family(glm_tweedie_power(power), link))
  do.call(getExportedValue("stats", family), list(link = link), envir = baseenv())
}

# The variance power of a Tweedie model: a number between 1 and 2, 1.5 when there is none.
glm_tweedie_power <- function(power) {
  power <- suppressWarnings(as.numeric(power))
  if (length(power) != 1 || !is.finite(power) || power <= 1 || power >= 2) 1.5 else power
}

# The family as the tool writes it: "poisson", or "Tweedie, power 1.5".
glm_family_label <- function(spec) {
  if (identical(spec$family, "tweedie")) paste0("Tweedie, power ", format(glm_tweedie_power(spec$power))) else spec$family
}

# Whether a column is text with so many different values that "." in a formula
# leaves it out: more than 100 values, or more than 10 and more than half the
# rows with a value (IDs, dates or free text). Each value is a coefficient and
# a column of the model matrix, so a text ID of 2,000 rows makes a 2,000 x 2,000
# matrix that takes minutes to fit, and estimates from one or two rows each mean
# little. A column named in the formula is still used.
glm_many_values <- function(x) {
  if (!is.character(x) && !is.factor(x)) return(FALSE)
  present <- x[!is.na(x)]
  values <- length(unique(present))
  values > 100 || (values > 10 && values > length(present) / 2)
}

# Whether a column looks like an identifier, of no use as a response or as the
# variable of the chart: text with a different value in more than half the rows,
# or whole numbers that are all different and run through about as many values
# as there are rows (a row number or a policy number).
glm_id_like <- function(x) {
  present <- x[!is.na(x)]
  n <- length(present)
  if (n < 2) return(FALSE)
  if (is.character(x) || is.factor(x)) return(length(unique(present)) > n / 2)
  if (!is.numeric(x) || any(present != round(present)) || anyDuplicated(present) > 0) return(FALSE)
  isTRUE(diff(range(present)) < 2 * n)
}

# Breaks of the bands of a numeric variable in the actual vs predicted chart:
# `bands` bands of equal width, or of about the same number of rows each
# (quantiles, fewer bands where they coincide). The equal widths are explicit:
# cut(breaks = n) computes the same breaks but moves the outer two 0.1% out,
# which gives labels below zero for a variable that starts at zero.
glm_band_breaks <- function(x, bands, method = "quantile") {
  if (identical(method, "width")) {
    seq.int(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = bands + 1)
  } else {
    unique(stats::quantile(x, seq(0, 1, length.out = bands + 1), na.rm = TRUE, names = FALSE))
  }
}

# Labels of the bands that cut(x, breaks, include.lowest = TRUE) makes, easier
# to read than "(18.0006,23.9723]": for a whole-number variable the integers
# each band holds ("19-24", the first band including its lower edge),
# otherwise the edges with the fewest significant digits (at least 2) that keep
# them apart ("18-24"). The dash is an en dash, or " to " when a break is
# negative, where it would read as a minus sign. NULL when two labels would be
# the same, which leaves cut() its own labels.
glm_band_labels <- function(breaks, whole = FALSE) {
  n <- length(breaks) - 1
  separator <- if (any(breaks < 0)) " to " else intToUtf8(8211)
  if (whole) {
    low <- c(ceiling(breaks[1]), floor(breaks[-c(1, n + 1)]) + 1)
    high <- floor(breaks[-1])
    text <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
    labels <- ifelse(low >= high, text(high), paste0(text(low), separator, text(high)))
  } else {
    digits <- 2
    while (digits < 15 && anyDuplicated(signif(breaks, digits))) digits <- digits + 1
    edges <- netsimr_fmt(breaks, digits)
    labels <- paste0(edges[-(n + 1)], separator, edges[-1])
  }
  if (anyDuplicated(labels)) NULL else labels
}

# Labels of the values of a numeric variable with few values, one bar each,
# with thousands separators ("25,000" rather than "25000"); the values as they
# are when two would look the same.
glm_value_labels <- function(values) {
  labels <- netsimr_fmt(values, 15)
  if (anyDuplicated(labels)) as.character(values) else labels
}

# The rating factors of a fitted model, a row per coefficient as pricing teams load them:
# the intercept, a row per level of each factor (its base level first, with coefficient 0
# and relativity 1) and a row per unit of each numeric variable. With a log link the
# relativity is exp(coefficient), with a 95% interval; with another link exp(coefficient)
# is not a relativity, so the relativity columns are NA and the coefficients are given.
# Terms that are neither a single factor nor a single number (interactions, poly()) get a
# row per coefficient, named as glm() names them. An aliased coefficient is NA.
glm_relativity_table <- function(model) {
  coefs <- stats::coef(model)
  ses <- rep(NA_real_, length(coefs))
  names(ses) <- names(coefs)
  estimable <- summary(model)$coefficients
  ses[rownames(estimable)] <- estimable[, "Std. Error"]
  matrix <- stats::model.matrix(model)
  assign <- attr(matrix, "assign")
  columns <- colnames(matrix)
  labels <- attr(stats::terms(model), "term.labels")
  levels_of <- model$xlevels
  row <- function(variable, level, base, coefficient, se) {
    data.frame(variable = variable, level = level, base = base, coefficient = coefficient,
               std_error = se, stringsAsFactors = FALSE)
  }
  rows <- list()
  if (0 %in% assign) {
    rows[[1]] <- row("(Intercept)", "", FALSE, coefs[["(Intercept)"]], ses[["(Intercept)"]])
  }
  for (t in seq_along(labels)) {
    in_term <- columns[assign == t]
    if (!length(in_term)) next
    variable <- gsub("^`|`$", "", labels[t])
    levels <- levels_of[[variable]]
    if (!is.null(levels) && length(in_term) == length(levels) - 1) {
      #treatment contrasts: the columns are the levels after the base level, in order
      rows[[length(rows) + 1]] <- row(variable, levels[1], TRUE, 0, NA_real_)
      rows[[length(rows) + 1]] <- row(rep(variable, length(in_term)), levels[-1], FALSE,
                                      unname(coefs[in_term]), unname(ses[in_term]))
    } else if (length(in_term) == 1 && identical(in_term, labels[t])) {
      rows[[length(rows) + 1]] <- row(variable, "(per unit)", FALSE, coefs[[in_term]], ses[[in_term]])
    } else {
      rows[[length(rows) + 1]] <- row(rep(variable, length(in_term)), in_term, FALSE,
                                      unname(coefs[in_term]), unname(ses[in_term]))
    }
  }
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  log_link <- identical(model$family$link, "log")
  relativity <- function(x) if (log_link) exp(x) else rep(NA_real_, length(x))
  table$relativity <- relativity(table$coefficient)
  table$relativity_lower_95 <- relativity(table$coefficient - stats::qnorm(0.975) * table$std_error)
  table$relativity_upper_95 <- relativity(table$coefficient + stats::qnorm(0.975) * table$std_error)
  #the base level has no uncertainty of its own: its relativity is 1 by definition
  table$relativity_lower_95[table$base] <- table$relativity[table$base]
  table$relativity_upper_95[table$base] <- table$relativity[table$base]
  table
}

# The functions written for a message: "I(), log(), ... and offset()".
glm_formula_functions_text <- function() {
  calls <- paste0(glm_formula_functions, "()")
  paste(paste(calls[-length(calls)], collapse = ", "), "and", calls[length(calls)])
}

# the offset term of a formula, which the tool adds to the terms
glm_offset_term <- function(offset, offset_log) {
  if (is.null(offset) || identical(offset, "None")) return(NULL)
  if (isTRUE(offset_log)) paste0("offset(log(", glm_term(offset), "))") else paste0("offset(", glm_term(offset), ")")
}

# The fit of the GLM tool, apart from the tool: `spec` is the model the tool's inputs describe
# (response, family, link, Tweedie power, formula terms, offset and whether it is logged,
# weights, the share of rows held out and its seed) and `model_data` the imported data. It
# checks the spec against the data, stopping with a message the tool shows, and returns the
# model, the spec (with the link used), the data, the rows held out, the warnings and the
# columns "." left out.
glm_fit_model <- function(spec, model_data) {
  if (is.null(model_data)) stop("import data on the Data tab first")
  columns <- names(model_data)
  if (is.null(spec$response) || !spec$response %in% columns) stop("choose the response column")
  for (column in c(spec$offset, spec$weights)) {
    if (column != "None" && !column %in% columns) stop("the column '", column, "' is not in the data")
  }
  links <- glm_family_links[[spec$family]]
  if (is.null(links)) stop("unknown family '", spec$family, "'")
  link <- if (!is.null(spec$link) && spec$link %in% links) spec$link else links[1]
  if (spec$offset != "None") {
    offset_values <- model_data[[spec$offset]]
    if (!is.numeric(offset_values)) stop("the offset column must be numeric")
    if (spec$offset_log && any(offset_values <= 0, na.rm = TRUE)) {
      stop("the log of the offset needs positive values; '", spec$offset, "' has zero or negative values")
    }
  }
  if (grepl("~", spec$formula, fixed = TRUE)) {
    stop("enter only the terms after ~; the response is chosen above")
  }
  terms_text <- if (spec$formula == "") "1" else spec$formula
  rhs <- paste(c(glm_offset_term(spec$offset, spec$offset_log), terms_text), collapse = " + ")
  model_formula <- tryCatch(
    stats::as.formula(paste(glm_term(spec$response), "~", rhs), env = glm_formula_env()),
    error = function(e) {
      # the parser's message starts with "<text>:2:0:" and repeats the text on more lines
      stop("the formula could not be read: ", sub("^<text>:[0-9]+:[0-9]+: ", "", strsplit(conditionMessage(e), "\n")[[1]][1]),
           call. = FALSE)
    }
  )
  # model.frame() and glm() run whatever the formula calls: only the data's columns and the
  # formula functions are allowed (see glm_formula_problem()), I() included
  problem <- glm_formula_problem(model_formula, columns)
  if (!is.null(problem)) stop(problem, call. = FALSE)
  warnings <- character(0)
  left_out <- character(0)
  if ("." %in% all.vars(model_formula)) {
    # "." is every other column except the offset and weights columns, and the columns that
    # cannot be predictors: no values, a single text value, or text with too many values
    candidates <- setdiff(columns, c(spec$response, spec$offset, spec$weights))
    left_out <- unlist(lapply(candidates, function(column) {
      x <- model_data[[column]]
      present <- x[!is.na(x)]
      reason <- if (length(present) == 0) {
        "no values"
      } else if ((is.character(x) || is.factor(x) || is.logical(x)) && length(unique(present)) < 2) {
        "a single value"
      } else if (glm_many_values(x)) {
        paste(format(length(unique(present)), big.mark = ",", scientific = FALSE), "different values")
      }
      if (!is.null(reason)) stats::setNames(reason, column)
    }))
    # a column the formula names (". - id", "id + .") is not left out: terms() needs it in the data
    left_out <- left_out[!names(left_out) %in% all.vars(model_formula)]
    allowed <- setdiff(columns, c(spec$offset, spec$weights, names(left_out)))
    model_formula <- stats::formula(stats::terms(model_formula, data = model_data[allowed]))
    environment(model_formula) <- glm_formula_env()
  }
  # text columns named in the formula are used, however many values they have
  labels <- attr(stats::terms(model_formula), "term.labels")
  predictors <- if (length(labels) > 0) all.vars(str2lang(paste(labels, collapse = " + "))) else character(0)
  for (column in setdiff(intersect(predictors, columns), spec$response)) {
    if (glm_many_values(model_data[[column]])) {
      warnings <- c(warnings, paste0("'", column, "' is text with ", format(length(unique(stats::na.omit(model_data[[column]]))), big.mark = ",", scientific = FALSE),
                                     " different values, each with its own coefficient"))
    }
  }
  response_values <- model_data[[spec$response]]
  if (spec$family == "tweedie" && is.numeric(response_values) && any(response_values < 0, na.rm = TRUE)) {
    stop("the Tweedie family needs zero or positive responses; '", spec$response, "' has negative values")
  }
  if (spec$family != "binomial" && !is.numeric(response_values) && !is.logical(response_values)) {
    stop("the response '", spec$response, "' has text values, such as '", stats::na.omit(response_values)[1],
         "'; the ", spec$family, " family needs numbers (only a binomial response can be text)")
  }
  # the rows the fit can use have every column of the model, and the weight, present
  used_columns <- intersect(c(all.vars(model_formula), if (spec$weights != "None") spec$weights), columns)
  complete <- stats::complete.cases(model_data[used_columns])
  if (!any(complete)) {
    empty <- used_columns[vapply(model_data[used_columns], function(x) all(is.na(x)), logical(1))]
    stop("no row has a value in every column the model uses",
         if (length(empty) > 0) paste0(": ", paste0("'", empty, "'", collapse = ", "), if (length(empty) == 1) " has" else " have", " no values"))
  }
  # the rows held out for testing: the model is fitted to the others, the training rows
  test <- glm_holdout_rows(nrow(model_data), or_default(spec$holdout, "0"), or_default(spec$seed, 1L))
  if (!any(complete & !test)) stop("no training row has a value in every column the model uses")
  # a text predictor with a single value in those rows has no contrasts, which glm() reports as
  # "contrasts can be applied only to factors with 2 or more levels" without naming it
  frame <- stats::model.frame(model_formula, data = model_data[complete & !test, used_columns, drop = FALSE])
  for (column in names(frame)[-1]) {
    x <- frame[[column]]
    if ((is.character(x) || is.factor(x) || is.logical(x)) && length(unique(x)) < 2) {
      stop("'", column, "' has a single value ('", x[1], "') in the rows the model uses, so it cannot be a predictor; ",
           "remove it from the formula")
    }
  }
  # a text response of a binomial model is a factor: glm() takes its first level as failure and
  # every other level as success; a logical response is left as it is, TRUE being success
  if (spec$family == "binomial" && is.character(model_data[[spec$response]])) {
    model_data[[spec$response]] <- factor(model_data[[spec$response]])
    outcomes <- levels(model_data[[spec$response]])
    if (length(outcomes) > 2) {
      warnings <- c(warnings, paste0("the response has ", length(outcomes), " values: '", outcomes[1],
                                     "' is failure and every other value is success"))
    }
  }
  # the call is written out, so that the summary shows the real formula, family and weights; it
  # refers to the data as model_data, so update() on a downloaded model needs the data assigned to
  # model_data or passed as data =. The weights are a column of the data, so the rows left out
  # for missing values leave out their weight too
  family_call <- if (spec$family == "tweedie") {
    # NetSimR's own family, so that update() on a downloaded model finds it
    as.call(list(quote(NetSimR::tweedie_family), power = glm_tweedie_power(spec$power), link = link))
  } else {
    call(spec$family, link = link)
  }
  weights_arg <- if (spec$weights != "None") as.name(spec$weights) else NULL
  fit_call <- if (is.null(weights_arg)) {
    bquote(stats::glm(.(model_formula), family = family_object, data = model_data, na.action = stats::na.exclude))
  } else {
    bquote(stats::glm(.(model_formula), family = family_object, data = model_data, weights = .(weights_arg),
                      na.action = stats::na.exclude))
  }
  if (spec$offset != "None" && spec$offset_log && link != "log") {
    warnings <- c(warnings, "an exposure offset only multiplies the mean with the log link")
  }
  # nothing of the server may be reachable from the model, which is saved to a file with
  # everything it refers to: the call is evaluated where only the data and the family (built
  # apart, see glm_family_object()) are, and the formula's environment is the global one with
  # the spline bases (glm_formula_env())
  fit_env <- new.env(parent = globalenv())
  fit_env$model_data <- model_data[!test, , drop = FALSE]
  fit_env$family_object <- glm_family_object(spec$family, link, spec$power)
  model <- withCallingHandlers(
    eval(fit_call, fit_env),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  model$call$family <- family_call
  spec$link <- link
  list(model = model, spec = spec, data = model_data, test = test, warnings = unique(warnings), left_out = left_out, error = NULL)
}

#' Links offered for each GLM family, the first being the family's default
#'
#' @noRd
glm_family_links <- list(
  gaussian = c("identity", "log", "inverse"),
  poisson = c("log", "identity", "sqrt"),
  binomial = c("logit", "probit", "cloglog", "cauchit", "log"),
  Gamma = c("inverse", "log", "identity"),
  inverse.gaussian = c("1/mu^2", "inverse", "log", "identity"),
  tweedie = c("log", "identity", "sqrt", "inverse")
)

# A column name as it must be written in a formula.
glm_term <- function(column) if (identical(make.names(column), column)) column else paste0("`", gsub("`", "\\\\`", column), "`")
