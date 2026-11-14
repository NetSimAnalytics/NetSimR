# Rating tables and scoring of the GLM fitting tool: the fitted model as a base rate and a
# table of relativities for each rating factor, which pricing systems load, and the model's
# predictions for a new file of policies.

# The values a rating table shows for a variable of the data: each level of a factor (or
# text, or TRUE/FALSE), and for a number each of its values when it has at most `max_values`
# of them (100 by default), otherwise `max_values` quantiles from the smallest to the largest, rounded to
# three significant digits. `exact` says whether the values are all the data's.
glm_rating_values <- function(x, max_values = 100, levels = NULL) {
  if (!is.null(levels)) return(list(values = levels, exact = TRUE))
  if (is.logical(x)) return(list(values = c(FALSE, TRUE), exact = TRUE))
  if (is.character(x) || is.factor(x)) {
    return(list(values = if (is.factor(x)) levels(droplevels(x)) else sort(unique(x[!is.na(x)])), exact = TRUE))
  }
  present <- sort(unique(x[is.finite(x)]))
  if (length(present) <= max_values) return(list(values = present, exact = TRUE))
  grid <- unique(signif(stats::quantile(x, seq(0, 1, length.out = max_values), na.rm = TRUE, names = FALSE), 3))
  # rounding can step past the range of the data, where a spline is extrapolated
  grid <- pmin(pmax(grid, present[1]), present[length(present)])
  list(values = unique(grid), exact = FALSE)
}

# The base value of a variable, at which its relativity is 1: the most common level of a
# factor, and the value closest to the median of a number, among the values shown.
glm_rating_base <- function(x, values) {
  if (is.numeric(x) && is.numeric(values)) {
    return(values[which.min(abs(values - stats::median(x, na.rm = TRUE)))])
  }
  counts <- table(factor(as.character(x), levels = as.character(values)))
  values[which.max(counts)]
}

# The rating table of a fitted GLM: its terms grouped by the columns of the data they use
# (age and I(age^2), or ns(age, 4), make one factor; age:region another), the effect of each
# group on the linear predictor at each value of its columns, and the base rate. The
# effects come from predict(type = "terms"), so any term the formula allows is covered:
# splines, polynomials, bands and interactions. With the log link the relativities
# multiply: the prediction of a policy is the base rate times its relativity in every
# rating factor (times its exposure, with an offset), exactly so for values the table
# shows. With the logit link exp(effect) is an odds ratio; with other links the effects
# add up on the scale of the link and there is no relativity.
#
# model is a fit of the tool, data the rows it was fitted to. Returns a list: `factors`,
# one data frame per rating factor (a column per variable, then effect, relativity, base and
# rows, the number of rows of the data at that value, NA for a number shown at quantiles);
# `base`, the base value of each variable; `base_rate`, the prediction at the base values
# per unit of exposure (or the base linear predictor's inverse with another link);
# `constant`, the base linear predictor; `link` and `relativity`, the name of exp(effect)
# ("relativity", "odds ratio" or NA); and `exact`, whether every number's values are all the
# data's (then the table reproduces every prediction).
glm_rate_table <- function(model, data, max_values = 100) {
  tt <- stats::terms(model)
  labels <- attr(tt, "term.labels")
  offsets <- attr(tt, "offset")
  offset_vars <- if (length(offsets)) {
    unique(unlist(lapply(as.list(attr(tt, "variables"))[-1][offsets], all.vars)))
  } else character(0)
  link <- model$family$link
  relativity_name <- switch(link, log = "relativity", logit = "odds ratio", NA_character_)
  columns <- names(data)
  # the columns of the data each term uses
  term_vars <- lapply(labels, function(label) intersect(all.vars(str2lang(label)), columns))
  keys <- vapply(term_vars, function(v) paste(sort(v), collapse = "\r"), character(1))
  groups <- unique(keys[nzchar(keys)])
  variables <- unique(unlist(term_vars))
  used <- intersect(c(variables, offset_vars), columns)
  complete <- data[stats::complete.cases(data[used]), , drop = FALSE]
  if (nrow(complete) == 0) stop("no row has every column the model uses")
  # the values and base of each variable; the factors' levels are those the model knows
  levels_of <- function(v) {
    known <- model$xlevels[[v]]
    if (is.null(known)) return(NULL)
    if (is.logical(complete[[v]])) as.logical(known) else known
  }
  shown <- lapply(stats::setNames(variables, variables), function(v) glm_rating_values(complete[[v]], max_values, levels_of(v)))
  base <- lapply(stats::setNames(variables, variables), function(v) glm_rating_base(complete[[v]], shown[[v]]$values))
  # a row of the data at the base values, whose other columns (the offset) do not matter:
  # the effects of the terms leave out the offset
  reference <- complete[1, , drop = FALSE]
  for (v in variables) reference[[v]] <- base[[v]]
  at <- function(grid) {
    rows <- reference[rep(1, nrow(grid)), , drop = FALSE]
    for (v in names(grid)) {
      value <- grid[[v]]
      rows[[v]] <- if (is.factor(complete[[v]])) factor(value, levels = levels(complete[[v]])) else value
    }
    rownames(rows) <- NULL
    rows
  }
  term_effects <- function(rows) {
    p <- suppressWarnings(stats::predict(model, newdata = rows, type = "terms"))
    list(effects = p, constant = attr(p, "constant"))
  }
  base_terms <- term_effects(at(data.frame(row.names = 1)[1, , drop = FALSE]))
  constant <- unname(base_terms$constant) + sum(base_terms$effects)
  factors <- lapply(groups, function(key) {
    vars <- strsplit(key, "\r", fixed = TRUE)[[1]]
    in_group <- labels[keys == key]
    # a number in an interaction shows fewer values, so the table stays readable
    values <- lapply(stats::setNames(vars, vars), function(v) {
      s <- shown[[v]]
      if (length(vars) > 1 && is.numeric(s$values) && length(s$values) > 12) {
        s <- glm_rating_values(complete[[v]], 12)
      }
      s$values
    })
    grid <- expand.grid(values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    effects <- term_effects(at(grid))$effects
    effect <- rowSums(effects[, in_group, drop = FALSE])
    is_base <- Reduce(`&`, lapply(vars, function(v) {
      if (is.numeric(grid[[v]])) grid[[v]] == base[[v]] else as.character(grid[[v]]) == as.character(base[[v]])
    }))
    # the base of an interaction may not be in its grid (a number shown at fewer values)
    reference_effect <- if (any(is_base)) effect[which(is_base)[1]] else sum(base_terms$effects[, in_group])
    effect <- unname(effect - reference_effect)
    exact <- all(vapply(vars, function(v) !is.numeric(grid[[v]]) || length(values[[v]]) == length(unique(complete[[v]])),
                        logical(1)))
    rows <- if (exact) {
      key_of <- function(d) do.call(paste, c(lapply(vars, function(v) as.character(d[[v]])), sep = "\r"))
      as.numeric(table(factor(key_of(complete), levels = key_of(grid))))
    } else NA_real_
    out <- grid
    out$effect <- effect
    out$relativity <- if (is.na(relativity_name)) NA_real_ else exp(effect)
    out$base <- is_base
    out$rows <- rows
    attr(out, "terms") <- in_group
    attr(out, "exact") <- exact
    out
  })
  names(factors) <- vapply(groups, function(key) paste(strsplit(key, "\r", fixed = TRUE)[[1]], collapse = " \u00d7 "), character(1))
  # the order of the formula: a group in the place of its first term
  factors <- factors[order(match(groups, keys))]
  list(factors = factors, base = base, constant = constant, base_rate = model$family$linkinv(constant),
       link = link, relativity = relativity_name, offset = offset_vars,
       exact = all(vapply(factors, function(f) isTRUE(attr(f, "exact")), logical(1))))
}

# A rating table as one data frame for a CSV file: the base rate first, then a row per value
# of each rating factor (its variables' values joined with "; " for an interaction).
glm_rate_table_long <- function(rates) {
  base_text <- paste(vapply(names(rates$base), function(v) paste0(v, " = ", format(rates$base[[v]])), character(1)),
                     collapse = "; ")
  head <- data.frame(rating_factor = "Base rate", level = base_text, effect = rates$constant,
                     relativity = rates$base_rate, base = TRUE, rows = NA_real_, stringsAsFactors = FALSE)
  rows <- lapply(names(rates$factors), function(name) {
    f <- rates$factors[[name]]
    vars <- setdiff(names(f), c("effect", "relativity", "base", "rows"))
    level <- if (length(vars) == 1) as.character(f[[vars]]) else {
      do.call(paste, c(lapply(vars, function(v) paste0(v, " = ", as.character(f[[v]]))), sep = "; "))
    }
    data.frame(rating_factor = name, level = level, effect = f$effect, relativity = f$relativity, base = f$base,
               rows = f$rows, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(head), rows))
  names(out)[names(out) == "relativity"] <- if (is.na(rates$relativity)) "relativity" else gsub(" ", "_", rates$relativity)
  rownames(out) <- NULL
  out
}

# The predictions of a model for a new file of policies: the columns the model needs are
# checked first (the offset's included), text that is mostly numbers is read as numbers as
# on import, and a row with a level the model has not seen, or a missing value, gets NA.
# Returns the data with the prediction column added, the number of rows predicted and the
# reasons for those that are not.
glm_score <- function(model, data, response = NULL, column = "prediction") {
  tt <- stats::delete.response(stats::terms(model))
  needed <- setdiff(all.vars(tt), response)
  missing <- setdiff(needed, names(data))
  if (length(missing) > 0) {
    stop("the file has no column ", paste0("'", missing, "'", collapse = ", "), ", which the model needs", call. = FALSE)
  }
  predicted <- glm_predict_all(model, data)
  out <- data
  name <- if (column %in% names(out)) make.unique(c(names(out), paste0("model_", column)))[ncol(out) + 1] else column
  out[[name]] <- predicted$predicted
  incomplete <- !stats::complete.cases(data[needed])
  list(data = out, column = name, predicted = sum(is.finite(predicted$predicted)), unseen = predicted$unseen,
       incomplete = sum(incomplete), total = nrow(data))
}
