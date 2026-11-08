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
  if (!is.finite(stats::AIC(model))) {
    stop("stepwise selection compares the ", criterion, ", which a ", model$family$family,
         " model does not have (its likelihood has no closed form); compare models by their deviance ",
         "on the rows held out, on the Validation tab")
  }
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

# The penalties a selection can use, as the elastic net mixing parameter of glmnet: 1 is
# the lasso, 0 ridge regression.
glm_penalty_alpha <- c(lasso = 1, elastic = 0.5, ridge = 0)

# A penalised fit of a model's terms and candidate columns: the lasso, the elastic net or
# ridge regression (glmnet::cv.glmnet(), a suggested package), its penalty chosen by
# cross-validation on the rows the model was fitted to. Each factor has a column for every
# level, so the penalty does not depend on the base level. Two penalties are reported: the
# one with the lowest cross-validated deviance (lambda.min) and the largest within a
# standard error of it (lambda.1se, fewer terms), each with the terms it keeps (a factor is
# kept when any of its levels is) and, given test rows, its deviance per row on them. The
# fold of each row is drawn with seed, without touching the caller's random numbers.
glm_penalised <- function(model, data, candidates, penalty = c("lasso", "elastic", "ridge"), test = NULL,
                          folds = 5, seed = 1) {
  penalty <- match.arg(penalty)
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("the lasso, the elastic net and ridge regression need the glmnet package: install.packages(\"glmnet\")")
  }
  formula <- stats::formula(model)
  model_terms <- stats::terms(model)
  weights <- if (!is.null(model$call$weights)) as.character(model$call$weights)
  labels <- attr(model_terms, "term.labels")
  added <- setdiff(vapply(candidates, glm_term, character(1)), labels)
  all_terms <- c(labels, added)
  if (length(all_terms) < 2) stop("a penalised fit needs at least two terms or candidate columns")
  offset_positions <- attr(model_terms, "offset")
  offset_text <- if (length(offset_positions)) {
    vapply(as.list(attr(model_terms, "variables"))[-1][offset_positions],
           function(e) paste(deparse(e), collapse = ""), character(1))
  }
  full <- stats::as.formula(paste(paste(deparse(formula[[2]]), collapse = ""), "~",
                                  paste(c(offset_text, all_terms), collapse = " + ")), env = globalenv())
  used <- intersect(unique(c(all.vars(full), weights)), names(data))
  rows <- data[stats::complete.cases(data[used]), , drop = FALSE]
  if (nrow(rows) < 10 * folds) stop("too few rows with every column present for ", folds, "-fold cross-validation")
  frame <- stats::model.frame(full, rows, drop.unused.levels = TRUE)
  factors <- names(frame)[-1][vapply(frame[-1], function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
  for (f in factors) frame[[f]] <- factor(frame[[f]])
  every_level <- stats::setNames(lapply(factors, function(f) stats::contrasts(frame[[f]], contrasts = FALSE)), factors)
  matrix_terms <- stats::delete.response(stats::terms(frame))
  design <- function(f) {
    x <- stats::model.matrix(matrix_terms, f, contrasts.arg = if (length(every_level)) every_level)
    structure(x[, -1, drop = FALSE], assign = attr(x, "assign")[-1])
  }
  x <- design(frame)
  assign <- attr(x, "assign")
  y <- glm_response_coded(stats::model.response(frame))
  offset <- stats::model.offset(frame)
  weight <- if (!is.null(weights)) rows[[weights]] else rep(1, nrow(rows))
  family <- model$family
  canonical <- c(gaussian = "identity", poisson = "log", binomial = "logit")
  glmnet_family <- if (family$family %in% names(canonical) && identical(family$link, canonical[[family$family]])) {
    family$family
  } else {
    glm_family_object(if (identical(family$family, "Tweedie")) "tweedie" else family$family, family$link,
                      if (is.null(family$variance_power)) 1.5 else family$variance_power)
  }
  fold <- glm_fold_ids(nrow(x), folds, seed)
  warnings <- character(0)
  fit <- withCallingHandlers(
    glmnet::cv.glmnet(x, y, family = glmnet_family, weights = weight, offset = offset,
                      alpha = glm_penalty_alpha[[penalty]], foldid = fold, type.measure = "deviance"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  term_names <- attr(matrix_terms, "term.labels")
  kept_terms <- function(s) {
    beta <- as.numeric(stats::coef(fit, s = s))[-1]
    term_names[sort(unique(assign[beta != 0]))]
  }
  test_deviance <- function(s) {
    if (is.null(test) || nrow(test) == 0) return(NA_real_)
    test_rows <- test[stats::complete.cases(test[used]), , drop = FALSE]
    if (nrow(test_rows) == 0) return(NA_real_)
    test_frame <- stats::model.frame(full, test_rows)
    # a level the training rows did not have has no column: those rows are left out
    for (f in factors) test_frame[[f]] <- factor(test_frame[[f]], levels = levels(frame[[f]]))
    seen <- stats::complete.cases(test_frame)
    test_frame <- test_frame[seen, , drop = FALSE]
    if (nrow(test_frame) == 0) return(NA_real_)
    newx <- design(test_frame)
    newoffset <- stats::model.offset(test_frame)
    predicted <- as.numeric(stats::predict(fit, newx = newx, s = s, type = "response",
                                           newoffset = if (is.null(newoffset)) rep(0, nrow(newx)) else newoffset))
    test_weight <- if (!is.null(weights)) test_rows[[weights]][seen] else 1
    glm_unit_deviance(family, glm_response_coded(stats::model.response(test_frame)), predicted, test_weight)
  }
  choices <- c("lambda.min", "lambda.1se")
  summary <- data.frame(
    choice = c("Lowest CV deviance", "Within one standard error"),
    lambda = c(fit$lambda.min, fit$lambda.1se),
    terms = vapply(choices, function(s) length(kept_terms(s)), integer(1)),
    cv_deviance = fit$cvm[match(c(fit$lambda.min, fit$lambda.1se), fit$lambda)],
    test_deviance = vapply(choices, test_deviance, numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  kept <- kept_terms("lambda.1se")
  list(
    summary = summary,
    terms = if (length(kept)) paste(kept, collapse = " + ") else "1",
    dropped = setdiff(term_names, kept),
    path = data.frame(log_lambda = log(fit$lambda), cv_deviance = fit$cvm, upper = fit$cvup, lower = fit$cvlo),
    lambda = c(min = fit$lambda.min, `1se` = fit$lambda.1se),
    penalty = penalty,
    rows = nrow(rows),
    left_out = nrow(data) - nrow(rows),
    warnings = unique(warnings)
  )
}

# The cross-validation fold of each of n rows: as even as can be, in a random order that is
# the same for the same seed, without touching the caller's random numbers.
glm_fold_ids <- function(n, folds, seed = 1) {
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
  sample(rep_len(seq_len(folds), n))
}
