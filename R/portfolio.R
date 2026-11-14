# Several lines of business with dependence between them: each line's annual losses are
# simulated on their own, then reordered so that their ranks follow a copula.

#' Simulate a portfolio of dependent lines
#'
#' @description Simulates the annual losses of several lines of business, each with its
#'   own frequency and severity, and joins them with a Gaussian or t copula, for the
#'   capital of the portfolio: its value at risk and tail value at risk, the
#'   diversification between the lines, and each line's share of the capital.
#'
#' @details Each line is simulated on its own, by \code{\link{simulate_claims}} (or
#'   given as a vector of simulated losses). The lines are then joined by
#'   reordering: \code{n} draws from the copula give, for each line, the rank its
#'   losses take in each simulation, and the line's simulated losses are put in that
#'   order. Each line keeps exactly the distribution it was simulated with, and the
#'   ranks of the lines follow the copula: with the Gaussian copula, normal draws
#'   with the given correlation matrix; with the t copula, the same divided by a
#'   shared \eqn{\sqrt{W / \nu}}, \eqn{W} chi-squared with \eqn{\nu} degrees of
#'   freedom, which makes large losses in several lines at once more likely (tail
#'   dependence). This is the reordering used in capital models (the method of
#'   Iman and Conover with the copula's ranks).
#'
#'   The value at risk of the total at level \eqn{p} is its \eqn{p} quantile, and the
#'   tail value at risk the mean of the totals at or above it. The diversification
#'   benefit is the sum of the lines' VaR less the total's VaR. Each line's capital
#'   is its mean loss in the simulations whose total is at or above the total's VaR
#'   (co-TVaR, the Euler allocation of the TVaR), so the lines' capitals add up to
#'   the total's TVaR.
#' @param lines A named list with one element per line: a list of arguments of
#'   \code{\link{simulate_claims}} (without \code{n_sims} and \code{seed}), whose
#'   \code{total_claims} are the line's losses, or a numeric vector of \code{n}
#'   simulated losses.
#' @param correlation The correlation between the lines' copula draws: a matrix with a
#'   row and a column per line, symmetric and positive definite with ones on the
#'   diagonal, or one number for every pair of lines.
#' @param copula \code{"gaussian"} (the default) or \code{"t"}.
#' @param df The degrees of freedom of the t copula: the lower, the stronger the
#'   dependence in the tail.
#' @param n The number of simulations.
#' @param level The level of the value at risk and the tail value at risk, such as
#'   0.995 (the default) for 1 in 200 years.
#' @param seed A whole number for a reproducible run, or \code{NULL} (the default) to
#'   use the current random number stream. The stream of the R session is restored
#'   after a run with a seed.
#' @return An object of class \code{netsimr_portfolio}, a list with \code{losses}, a
#'   data frame with a column per line and the \code{total}, one row per simulation;
#'   \code{summary}, a data frame with the mean, standard deviation, VaR and TVaR of
#'   each line and of the total, each line's capital (co-TVaR) and its share;
#'   \code{diversification}, the benefit in amount and as a share of the lines' VaR;
#'   and the copula settings.
#' @references Iman, R. L. and Conover, W. J. (1982). A distribution-free approach to
#'   inducing rank correlation among input variables. Communications in Statistics -
#'   Simulation and Computation 11(3), 311-334.
#' @seealso \code{\link{simulate_claims}}.
#' @export
#' @examples
#' lines <- list(
#'   motor = list(frequency = "Poisson", frequency_params = 50,
#'                severity = "LogNormal", severity_params = c(8, 1)),
#'   property = list(frequency = "Poisson", frequency_params = 5,
#'                   severity = "Pareto", severity_params = c(1.8, 20000))
#' )
#' portfolio <- simulate_portfolio(lines, correlation = 0.3, n = 20000, seed = 1)
#' portfolio
#'
#' # stronger dependence in the tail with a t copula
#' tail <- simulate_portfolio(lines, correlation = 0.3, copula = "t", df = 3, n = 20000, seed = 1)
#' tail$diversification
simulate_portfolio <- function(lines, correlation = 0, copula = c("gaussian", "t"), df = 4, n = 10000, level = 0.995,
                               seed = NULL) {
  copula <- match.arg(copula)
  if (!is.list(lines) || length(lines) < 2 || is.null(names(lines)) || any(!nzchar(names(lines))) ||
      anyDuplicated(names(lines))) {
    stop("lines must be a list of at least two lines, each with its own name.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 2 || n != round(n)) {
    stop("n must be a whole number of at least 2.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be a probability between 0 and 1.", call. = FALSE)
  }
  if (copula == "t" && (!is.numeric(df) || length(df) != 1 || !is.finite(df) || df <= 0)) {
    stop("df must be a positive number.", call. = FALSE)
  }
  k <- length(lines)
  R <- portfolio_correlation(correlation, names(lines))

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed != round(seed)) {
      stop("seed must be a whole number, or NULL.", call. = FALSE)
    }
    netsimr_local_seed(seed)
  }

  # each line's losses, on their own (a line's own seed comes from this stream)
  losses <- lapply(names(lines), function(name) {
    line <- lines[[name]]
    if (is.numeric(line)) {
      if (length(line) != n || any(!is.finite(line))) {
        stop("The losses of line '", name, "' must be ", n, " finite numbers, one per simulation.", call. = FALSE)
      }
      return(as.numeric(line))
    }
    if (!is.list(line)) stop("Line '", name, "' must be a list of simulate_claims() arguments or a numeric vector.", call. = FALSE)
    if (any(c("n_sims", "seed") %in% names(line))) {
      stop("Line '", name, "' must not set n_sims or seed: simulate_portfolio() sets them.", call. = FALSE)
    }
    simulated <- tryCatch(
      do.call(simulate_claims, c(list(n_sims = n, seed = sample.int(.Machine$integer.max, 1L)), line)),
      error = function(e) stop("Line '", name, "' could not be simulated: ", conditionMessage(e), call. = FALSE)
    )
    simulated$total_claims
  })
  names(losses) <- names(lines)

  # the copula's draws, and each line's losses in the order of their ranks
  z <- matrix(stats::rnorm(n * k), n, k) %*% chol(R)
  if (copula == "t") z <- z / sqrt(stats::rchisq(n, df) / df)
  joined <- lapply(seq_len(k), function(j) sort(losses[[j]])[rank(z[, j], ties.method = "first")])
  joined <- as.data.frame(stats::setNames(joined, names(lines)), check.names = FALSE)
  joined$total <- rowSums(joined)

  # risk measures and the allocation of the TVaR
  var_of <- function(x) stats::quantile(x, level, names = FALSE, type = 7)
  tvar_of <- function(x) mean(x[x >= var_of(x)])
  tail <- joined$total >= var_of(joined$total)
  columns <- c(names(lines), "total")
  summary <- data.frame(
    line = columns,
    mean = vapply(columns, function(cn) mean(joined[[cn]]), numeric(1)),
    sd = vapply(columns, function(cn) stats::sd(joined[[cn]]), numeric(1)),
    var = vapply(columns, function(cn) var_of(joined[[cn]]), numeric(1)),
    tvar = vapply(columns, function(cn) tvar_of(joined[[cn]]), numeric(1)),
    capital = vapply(columns, function(cn) mean(joined[[cn]][tail]), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  summary$capital_share <- summary$capital / summary$capital[k + 1]
  stand_alone <- sum(summary$var[seq_len(k)])
  benefit <- stand_alone - summary$var[k + 1]
  structure(list(
    losses = joined, summary = summary,
    diversification = c(stand_alone_var = stand_alone, total_var = summary$var[k + 1], benefit = benefit,
                        benefit_share = benefit / stand_alone),
    correlation = R, copula = copula, df = if (copula == "t") df, level = level, n = n
  ), class = "netsimr_portfolio")
}

# The correlation matrix of the copula, from one number for every pair or a matrix.
portfolio_correlation <- function(correlation, names) {
  k <- length(names)
  if (is.numeric(correlation) && length(correlation) == 1 && is.null(dim(correlation))) {
    if (!is.finite(correlation) || correlation <= -1 / (k - 1) || correlation >= 1) {
      stop("A single correlation for ", k, " lines must be above ", format(-1 / (k - 1), digits = 3),
           " and below 1.", call. = FALSE)
    }
    R <- matrix(correlation, k, k)
    diag(R) <- 1
  } else {
    R <- correlation
    if (!is.matrix(R) || !is.numeric(R) || !identical(dim(R), c(k, k)) || any(!is.finite(R))) {
      stop("correlation must be one number or a ", k, " by ", k, " matrix.", call. = FALSE)
    }
    if (!isTRUE(all.equal(R, t(R), check.attributes = FALSE)) || any(abs(diag(R) - 1) > 1e-12) || any(abs(R) > 1)) {
      stop("correlation must be symmetric, with ones on the diagonal and values from -1 to 1.", call. = FALSE)
    }
  }
  if (inherits(tryCatch(chol(R), error = function(e) e), "error")) {
    stop("correlation must be positive definite: the correlations are not consistent with one another.", call. = FALSE)
  }
  dimnames(R) <- list(names, names)
  R
}

#' Print a simulated portfolio
#'
#' @param x The result of \code{\link{simulate_portfolio}}.
#' @param ... Not used.
#' @return \code{x}, invisibly.
#' @export
#' @keywords internal
print.netsimr_portfolio <- function(x, ...) {
  cat("Portfolio of ", length(x$summary$line) - 1, " lines, ", format(x$n, big.mark = ",", scientific = FALSE), " simulations, ",
      if (x$copula == "t") paste0("t copula (", format(x$df), " degrees of freedom)") else "Gaussian copula", "\n\n", sep = "")
  s <- x$summary
  amount <- function(v) formatC(v, format = "f", digits = 0, big.mark = ",")
  shown <- data.frame(line = s$line, mean = amount(s$mean), sd = amount(s$sd), amount(s$var), amount(s$tvar),
                      capital = amount(s$capital), share = formatC(s$capital_share, format = "f", digits = 3),
                      check.names = FALSE, stringsAsFactors = FALSE)
  names(shown)[4:5] <- paste0(c("VaR ", "TVaR "), format(100 * x$level), "%")
  print(shown, row.names = FALSE, right = TRUE)
  d <- x$diversification
  cat("\nDiversification benefit: ", amount(d[["benefit"]]), " (", formatC(100 * d[["benefit_share"]], format = "f", digits = 1),
      "% of the lines' VaR of ", amount(d[["stand_alone_var"]]), ")\n", sep = "")
  invisible(x)
}
