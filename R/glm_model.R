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
