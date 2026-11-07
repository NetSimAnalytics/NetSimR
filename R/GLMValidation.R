# Validation and variable selection of the GLM fitting tool: rows held out for testing,
# predictions for rows the model was not fitted to, the deviance per row, the Gini
# coefficient and its Lorenz curve, lift and double lift tables, and stepwise selection

# The shares of the rows that can be held out for testing, as the Model tab offers them.
glm_holdout_choices <- c("None" = "0", "10%" = "0.1", "20%" = "0.2", "30%" = "0.3")

# A seed for the holdout rows: a whole number, 1 when there is none.
glm_holdout_seed <- function(seed) {
  seed <- suppressWarnings(as.numeric(seed))
  if (length(seed) != 1 || !is.finite(seed) || abs(seed) > .Machine$integer.max) return(1L)
  as.integer(round(seed))
}

# Which of n rows are held out for testing: a random share of them (at least one row and
# at least one left to fit to), the same for the same seed, without touching the caller's
# random numbers. All FALSE when the share is 0 or not a share.
glm_holdout_rows <- function(n, share, seed = 1) {
  share <- suppressWarnings(as.numeric(share))
  test <- rep(FALSE, n)
  if (length(share) != 1 || !is.finite(share) || share <= 0 || n < 2) return(test)
  size <- min(n - 1, max(1, round(min(share, 0.5) * n)))
  global <- globalenv()
  saved <- if (exists(".Random.seed", envir = global, inherits = FALSE)) get(".Random.seed", envir = global)
  kind <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(kind[1], kind[2], kind[3]))
    if (is.null(saved)) {
      if (exists(".Random.seed", envir = global, inherits = FALSE)) rm(".Random.seed", envir = global)
    } else {
      assign(".Random.seed", saved, envir = global)
    }
  }, add = TRUE)
  set.seed(glm_holdout_seed(seed), kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  test[sample.int(n, size)] <- TRUE
  test
}

# A response as glm() codes it: a factor is 0 for its first level and 1 for every other,
# TRUE is 1 and FALSE 0, and numbers are as they are.
glm_response_coded <- function(x) {
  if (is.factor(x)) return(as.numeric(as.integer(x) != 1L))
  as.numeric(x)
}

# The predictions of a model (on the response scale) for every row of data, including rows
# it was not fitted to. A row with a value of a factor that the model has not seen, which
# has no coefficient, gets NA; unseen is how many rows that is.
glm_predict_all <- function(model, data) {
  newdata <- data
  unseen <- rep(FALSE, nrow(data))
  for (variable in names(model$xlevels)) {
    values <- if (variable %in% names(data)) {
      data[[variable]]
    } else {
      # a term such as factor(region): the formula holds only the data's columns and the
      # formula functions (glm_formula_problem()), so evaluating it runs nothing else
      tryCatch(eval(str2lang(variable), data, baseenv()), error = function(e) NULL)
    }
    if (is.null(values) || length(values) != nrow(data)) next
    new_value <- !is.na(values) & !(as.character(values) %in% model$xlevels[[variable]])
    if (!any(new_value)) next
    unseen <- unseen | new_value
    columns <- if (variable %in% names(data)) variable else intersect(all.vars(str2lang(variable)), names(data))
    for (column in columns) newdata[[column]][new_value] <- NA
  }
  predicted <- unname(stats::predict(model, newdata = newdata, type = "response", na.action = stats::na.pass))
  list(predicted = as.numeric(predicted), unseen = sum(unseen))
}

# The deviance per row of a model's family (per unit of weight): the mean of the unit
# deviances, comparable between the rows a model was fitted to and the rows held out.
glm_unit_deviance <- function(family, actual, predicted, weight = 1) {
  weight <- rep_len(weight, length(actual))
  ok <- is.finite(actual) & is.finite(predicted) & is.finite(weight) & weight > 0
  if (!any(ok)) return(NA_real_)
  sum(family$dev.resids(actual[ok], predicted[ok], weight[ok])) / sum(weight[ok])
}

# The Lorenz curve of a model and its Gini coefficient. The rows are sorted from the lowest
# predicted rate (prediction per unit of exposure) to the highest; the curve runs through
# the cumulative shares of the exposure (x) and of the actual response (y), each row
# counted with its weight. Rows with the same prediction are one step, so their order does
# not matter. The Gini coefficient is 1 - 2 x the area under the curve: 0 for a model that
# ranks no better than chance, higher the better it separates the low risks from the high.
# NULL when fewer than two rows or no response are left.
glm_gini_curve <- function(actual, predicted, exposure = 1, weight = 1) {
  n <- length(actual)
  exposure <- rep_len(exposure, n)
  weight <- rep_len(weight, n)
  ok <- is.finite(actual) & is.finite(predicted) & is.finite(exposure) & is.finite(weight) & exposure > 0 & weight > 0
  if (sum(ok) < 2) return(NULL)
  rate <- predicted[ok] / exposure[ok]
  o <- order(rate)
  e <- (weight * exposure)[ok][o]
  a <- (weight * actual)[ok][o]
  if (sum(e) <= 0 || sum(a) <= 0) return(NULL)
  group <- cumsum(c(TRUE, diff(rate[o]) != 0))
  e <- rowsum(e, group, reorder = FALSE)[, 1]
  a <- rowsum(a, group, reorder = FALSE)[, 1]
  x <- c(0, cumsum(e) / sum(e))
  y <- c(0, cumsum(a) / sum(a))
  area <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  list(x = unname(x), y = unname(y), gini = 1 - 2 * area)
}

# A Lorenz curve with at most `points` points, for a chart: every row of a large test set
# would draw thousands of segments the eye cannot tell apart.
glm_thin_curve <- function(curve, points = 400) {
  n <- length(curve$x)
  if (n <= points) return(curve)
  keep <- unique(round(seq(1, n, length.out = points)))
  curve$x <- curve$x[keep]
  curve$y <- curve$y[keep]
  curve
}

# A lift table: the rows sorted by sort_by and cut into `bands` bands of about the same
# exposure (each row counted with its weight), with each band's exposure and its actual
# and predicted rates per unit of exposure; others is a named list of further predictions
# (a second model, for a double lift), whose rates are added as columns of those names.
glm_lift_table <- function(actual, predicted, sort_by, exposure = 1, weight = 1, bands = 10, others = list()) {
  n <- length(actual)
  exposure <- rep_len(exposure, n)
  weight <- rep_len(weight, n)
  ok <- is.finite(actual) & is.finite(predicted) & is.finite(sort_by) & is.finite(exposure) & is.finite(weight) &
    exposure > 0 & weight > 0
  for (other in others) ok <- ok & is.finite(other)
  if (!any(ok)) return(NULL)
  o <- order(sort_by[ok])
  e <- (weight * exposure)[ok][o]
  share <- (cumsum(e) - e / 2) / sum(e)
  band <- pmin(bands, floor(share * bands) + 1)
  w <- weight[ok][o]
  total <- function(x) as.numeric(tapply(x, factor(band, levels = seq_len(bands)), sum))
  denominator <- total(e)
  table <- data.frame(band = seq_len(bands), exposure = denominator,
                      actual = total(w * actual[ok][o]) / denominator,
                      predicted = total(w * predicted[ok][o]) / denominator)
  for (name in names(others)) table[[name]] <- total(w * others[[name]][ok][o]) / denominator
  table[is.finite(table$exposure) & table$exposure > 0, , drop = FALSE]
}

# A column name as it must be written in a formula.
glm_term <- function(column) if (identical(make.names(column), column)) column else paste0("`", gsub("`", "\\\\`", column), "`")

# Stepwise selection of a model's terms, by AIC or BIC, in both directions: from the
# model's terms, each step adds one of the candidate columns or drops a term, whichever
# lowers the criterion most, until none does (stats::step()). The steps are fitted to the
# rows with every candidate present, as every model of a selection must use the same rows;
# test, the rows held out, gives each step's deviance per row on data it was not fitted to.
# model is a fit of the tool (its call refers to model_data), data the rows it was fitted
# to. Returns the path (each step's change, criterion and test deviance), the suggested
# terms, the number of rows left out and the warnings of the fits.
glm_stepwise <- function(model, data, candidates, criterion = c("AIC", "BIC"), test = NULL) {
  criterion <- match.arg(criterion)
  call <- model$call
  # the call refers to the data and the family as model_data and family_object (see fit_glm())
  call$family <- quote(family_object)
  weights <- if (!is.null(call$weights)) as.character(call$weights)
  start_formula <- stats::formula(model)
  used <- intersect(unique(c(all.vars(start_formula), candidates, weights)), names(data))
  complete <- stats::complete.cases(data[used])
  if (sum(complete) < 2) stop("no rows have every candidate column present")
  env <- new.env(parent = globalenv())
  env$model_data <- data[complete, , drop = FALSE]
  env$family_object <- glm_family_object(model$family$family, model$family$link)
  warnings <- character(0)
  quietly <- function(expr) withCallingHandlers(expr, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  # add1() and drop1() refit in the environment of the model's formula, so the formulas of
  # the selection live in its own environment, which has the data and the family
  fit_formula <- function(f) {
    environment(f) <- env
    call$formula <- f
    quietly(eval(call, env))
  }
  start <- fit_formula(start_formula)
  labels <- attr(stats::terms(start), "term.labels")
  added <- setdiff(vapply(candidates, glm_term, character(1)), labels)
  upper <- stats::as.formula(paste("~", if (length(c(labels, added))) paste(c(labels, added), collapse = " + ") else "1"),
                             env = env)
  env$start <- start
  env$scope <- list(lower = ~1, upper = upper)
  env$k <- if (criterion == "BIC") log(stats::nobs(start)) else 2
  selected <- quietly(eval(quote(stats::step(start, scope = scope, direction = "both", k = k, trace = 0)), env))
  steps <- trimws(as.character(selected$anova$Step))
  formulas <- vector("list", length(steps))
  f <- start_formula
  for (i in seq_along(steps)) {
    if (i > 1) f <- stats::update(f, stats::as.formula(paste("~ .", steps[i]), env = env))
    formulas[[i]] <- f
  }
  test_deviance <- rep(NA_real_, length(steps))
  if (!is.null(test) && nrow(test) > 0) {
    actual <- glm_response_coded(eval(start_formula[[2]], test, baseenv()))
    weight <- if (!is.null(weights)) test[[weights]] else 1
    for (i in seq_along(steps)) {
      fit <- if (i == length(steps)) selected else fit_formula(formulas[[i]])
      test_deviance[i] <- glm_unit_deviance(fit$family, actual, glm_predict_all(fit, test)$predicted, weight)
    }
  }
  final <- attr(stats::terms(selected), "term.labels")
  list(
    path = data.frame(change = ifelse(steps == "", "Start", steps), criterion = selected$anova$AIC,
                      test_deviance = test_deviance, stringsAsFactors = FALSE),
    terms = if (length(final)) paste(final, collapse = " + ") else "1",
    criterion = criterion,
    rows = sum(complete),
    left_out = sum(!complete),
    warnings = unique(warnings)
  )
}

# The columns a selection may add: every column except the response, offset and weights
# that can be a predictor (some values, more than one value, and text with not too many
# values, see glm_many_values()).
glm_candidate_columns <- function(data, exclude = character(0)) {
  columns <- setdiff(names(data), exclude)
  columns[vapply(data[columns], function(x) {
    present <- x[!is.na(x)]
    length(present) > 0 && length(unique(present)) > 1 && !glm_many_values(x) && !glm_id_like(x)
  }, logical(1))]
}
