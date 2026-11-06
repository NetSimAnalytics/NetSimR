#' The one-year view of the reserves: Merz and Wuthrich's claims development result
#'
#' @description The standard error of the claims development result (CDR) of the
#'   next year: how much the chain ladder's ultimate can move in one year, as the
#'   year's claims emerge and the factors are estimated again. It is the reserve
#'   risk over one year that Solvency II measures, against Mack's standard error of
#'   the reserves until they are all paid.
#'
#' @details The formulas of Merz and Wuthrich (2008) hold for the volume-weighted
#'   chain ladder of every link ratio without a tail, which the function fits; with
#'   Mack's \eqn{\sigma_j} and the shares
#'   \eqn{\alpha_j = C_{I-j,j} / \sum_{i \le I-j} C_{ij}}, the mean squared error of
#'   each origin's CDR is its next year's process error,
#'   \eqn{\hat C_{iJ}^2 \hat\sigma_j^2 / (\hat f_j^2 C_{ij})}, plus the error of the
#'   factors, which counts each later factor's error by the share \eqn{\alpha} of its
#'   estimate that the year's claims change; the total adds the correlation of the
#'   origins. The results match the ChainLadder package's \code{CDR()}, and for the
#'   Taylor and Ashe triangle Merz and Wuthrich's paper.
#' @inheritParams chain_ladder
#' @return An object of class \code{netsimr_reserve}: a list with \code{by_origin}, a
#'   data frame with each origin's latest claims, ultimate, IBNR, the standard error
#'   of its one-year CDR and Mack's standard error until ultimate; \code{total}, the
#'   same for all origins; and \code{chain_ladder}, the chain ladder they come from.
#' @references Merz, M. and Wuthrich, M. V. (2008). Modelling the claims development
#'   result for solvency purposes. CAS E-Forum, Fall 2008, 542-568.
#' @seealso \code{\link{chain_ladder}} for Mack's standard error until ultimate, and
#'   \code{\link{bootstrap_chain_ladder}}.
#' @export
#' @examples
#' merz_wuthrich(taylor_ashe_triangle, sigma = "Mack")
merz_wuthrich <- function(triangle, sigma = c("log-linear", "Mack")) {
  cl <- chain_ladder(triangle, sigma = match.arg(sigma))
  C <- cl$triangle
  full <- cl$full_triangle[, seq_len(ncol(C)), drop = FALSE]
  latest <- cl$latest_period
  m <- nrow(C)
  n <- ncol(C)
  f <- unname(cl$factors)
  ratio <- unname(cl$sigma[seq_len(n - 1)])^2 / f^2
  # the column sums of the factors' estimates (the origins with a link ratio at j), and the
  # share of the next estimate that the latest diagonal adds
  estimate_sum <- vapply(seq_len(n - 1), function(j) sum(C[latest > j, j]), numeric(1))
  share <- vapply(seq_len(n - 1), function(j) sum(C[latest == j, j]) / sum(C[latest >= j, j]), numeric(1))
  ultimate <- unname(full[, n])
  process <- numeric(m)
  parameter <- numeric(m)
  for (i in which(latest < n)) {
    j <- latest[i]
    process[i] <- ultimate[i]^2 * ratio[j] / full[i, j]
    later <- if (j < n - 1) (j + 1):(n - 1) else integer(0)
    parameter[i] <- ratio[j] / estimate_sum[j] + sum(share[later] * ratio[later] / estimate_sum[later])
  }
  se <- sqrt(process + parameter * ultimate^2)
  # the total: each pair of origins shares the parameter error of the older one's later factors
  open <- which(latest < n)
  total_parameter <- 0
  for (i in open) for (k in open) {
    older <- if (latest[i] >= latest[k]) i else k
    total_parameter <- total_parameter + parameter[older] * ultimate[i] * ultimate[k]
  }
  total_se <- sqrt(sum(process) + total_parameter)
  by_origin <- data.frame(origin = cl$by_origin$origin, latest = cl$by_origin$latest, ultimate = cl$by_origin$ultimate,
                          ibnr = cl$by_origin$ibnr, one_year_se = se, mack_se = cl$by_origin$se,
                          stringsAsFactors = FALSE, row.names = NULL)
  total <- c(latest = cl$total[["latest"]], ultimate = cl$total[["ultimate"]], ibnr = cl$total[["ibnr"]],
             one_year_se = total_se, mack_se = cl$total[["se"]])
  structure(list(method = "Merz-Wuthrich one-year claims development result", by_origin = by_origin, total = total,
                 chain_ladder = cl), class = "netsimr_reserve")
}

#' Munich chain ladder of paid and incurred claims
#'
#' @description Projects a paid and an incurred triangle together, so that their
#'   ultimates agree: where an origin's paid to incurred ratio is below the average,
#'   its paid claims develop faster and its incurred claims slower than the chain
#'   ladder's, and the other way round.
#'
#' @details Following Quarg and Mack (2004), the chain ladder factor of each triangle
#'   is corrected by the origin's deviation from the average paid to incurred ratio
#'   (\eqn{q_j}, or its inverse for the paid triangle) of the development period:
#'   \eqn{\hat f^P_j + \lambda^P \hat\sigma^P_j / \hat\rho^P_j (I_{ij} / P_{ij} - 1 / q_j)},
#'   and likewise for incurred, where \eqn{\lambda} is the slope of the link ratio
#'   residuals on the ratio residuals. The \eqn{\sigma} of a development period with
#'   a single link ratio or ratio is extrapolated log-linearly, as in the ChainLadder
#'   package's \code{MunichChainLadder()}, whose results the function reproduces.
#' @param paid,incurred Triangles of cumulative paid and incurred claims, as for
#'   \code{\link{chain_ladder}}, with values in the same cells.
#' @param sigma How the chain ladders' \eqn{\sigma} is found for a period with a
#'   single link ratio: \code{"log-linear"} or \code{"Mack"}.
#' @return An object of class \code{netsimr_reserve}: a list with \code{by_origin}, a
#'   data frame with each origin's latest paid and incurred claims, their ratio, the
#'   Munich chain ladder ultimates of both and their ratio, and the separate chain
#'   ladder ultimates; \code{total}; the completed triangles \code{paid_full} and
#'   \code{incurred_full}; \code{lambda} (paid and incurred), \code{q} (the average
#'   paid to incurred ratio of each development period) and the two chain ladders.
#' @references Quarg, G. and Mack, T. (2004). Munich chain ladder. Blatter DGVFM
#'   26(4), 597-630.
#' @seealso \code{\link{chain_ladder}}.
#' @export
#' @examples
#' munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)
munich_chain_ladder <- function(paid, incurred, sigma = c("log-linear", "Mack")) {
  sigma <- match.arg(sigma)
  P <- reserving_check_triangle(paid)
  I <- reserving_check_triangle(incurred)
  if (!identical(dim(P$triangle), dim(I$triangle)) || !identical(P$latest, I$latest)) {
    stop("The paid and incurred triangles must have values in the same cells.", call. = FALSE)
  }
  latest <- P$latest
  P <- P$triangle
  I <- I$triangle
  if (any(P <= 0, na.rm = TRUE) || any(I <= 0, na.rm = TRUE)) {
    stop("The Munich chain ladder needs positive paid and incurred claims.", call. = FALSE)
  }
  m <- nrow(P)
  n <- ncol(P)
  cl_paid <- chain_ladder(P, sigma = sigma)
  cl_incurred <- chain_ladder(I, sigma = sigma)
  # the average ratio of each development period, the ratios weighted by the denominator,
  # and its sigma, extrapolated log-linearly where a period has a single ratio
  ratio_model <- function(numerator, denominator) {
    q <- numeric(n)
    rho <- rep(NA_real_, n)
    for (j in seq_len(n)) {
      rows <- latest >= j
      q[j] <- sum(numerator[rows, j]) / sum(denominator[rows, j])
      if (sum(rows) > 1) {
        rho[j] <- sqrt(sum(denominator[rows, j] * (numerator[rows, j] / denominator[rows, j] - q[j])^2) / (sum(rows) - 1))
      }
    }
    known <- which(is.finite(rho) & rho > 0)
    if (anyNA(rho)) {
      if (length(known) < 2) stop("Too few origins to estimate the Munich chain ladder.", call. = FALSE)
      fit <- stats::lm.fit(cbind(1, known), log(rho[known]))$coefficients
      rho[is.na(rho)] <- exp(fit[1] + fit[2] * which(is.na(rho)))
    }
    list(q = q, rho = rho)
  }
  q_incurred <- ratio_model(P, I)  # paid over incurred, rho^I
  q_paid <- ratio_model(I, P)      # incurred over paid, rho^P
  # the residuals of the link ratios and of the ratios, paired cell by cell (as in
  # ChainLadder, the last development factor's are left out)
  pairs <- which(!is.na(cl_paid$link_ratios) & col(cl_paid$link_ratios) <= n - 2, arr.ind = TRUE)
  residual_link <- function(cl, C) {
    j <- pairs[, 2]
    (cl$link_ratios[pairs] - cl$factors[j]) / cl$sigma[j] * sqrt(C[pairs])
  }
  residual_ratio <- function(model, numerator, denominator) {
    j <- pairs[, 2]
    (numerator[pairs] / denominator[pairs] - model$q[j]) / model$rho[j] * sqrt(denominator[pairs])
  }
  slope <- function(y, x) sum(x * y) / sum(x^2)
  lambda_paid <- slope(residual_link(cl_paid, P), residual_ratio(q_paid, I, P))
  lambda_incurred <- slope(residual_link(cl_incurred, I), residual_ratio(q_incurred, P, I))
  # project both triangles together, period by period
  full_P <- P
  full_I <- I
  fP <- unname(cl_paid$factors)
  fI <- unname(cl_incurred$factors)
  sP <- unname(cl_paid$sigma)
  sI <- unname(cl_incurred$sigma)
  for (j in seq_len(n - 1)) {
    rows <- which(latest <= j)
    if (length(rows) == 0) next
    p <- full_P[rows, j]
    r <- full_I[rows, j]
    full_P[rows, j + 1] <- p * (fP[j] + lambda_paid * sP[j] / q_paid$rho[j] * (r / p - q_paid$q[j]))
    full_I[rows, j + 1] <- r * (fI[j] + lambda_incurred * sI[j] / q_incurred$rho[j] * (p / r - q_incurred$q[j]))
  }
  latest_paid <- P[cbind(seq_len(m), latest)]
  latest_incurred <- I[cbind(seq_len(m), latest)]
  by_origin <- data.frame(
    origin = rownames(P), latest_paid = latest_paid, latest_incurred = latest_incurred,
    latest_ratio = latest_paid / latest_incurred, ultimate_paid = full_P[, n], ultimate_incurred = full_I[, n],
    ultimate_ratio = full_P[, n] / full_I[, n], chain_ladder_paid = cl_paid$by_origin$ultimate,
    chain_ladder_incurred = cl_incurred$by_origin$ultimate, stringsAsFactors = FALSE, row.names = NULL
  )
  total <- c(latest_paid = sum(latest_paid), latest_incurred = sum(latest_incurred),
             latest_ratio = sum(latest_paid) / sum(latest_incurred), ultimate_paid = sum(full_P[, n]),
             ultimate_incurred = sum(full_I[, n]), ultimate_ratio = sum(full_P[, n]) / sum(full_I[, n]),
             chain_ladder_paid = cl_paid$total[["ultimate"]], chain_ladder_incurred = cl_incurred$total[["ultimate"]])
  structure(list(method = "Munich chain ladder", by_origin = by_origin, total = total, paid_full = full_P,
                 incurred_full = full_I, lambda = c(paid = lambda_paid, incurred = lambda_incurred),
                 q = q_incurred$q, chain_ladder_paid = cl_paid, chain_ladder_incurred = cl_incurred),
            class = "netsimr_reserve")
}
