# Claims reserving: chain ladder with Mack's standard errors, Bornhuetter-Ferguson,
# Benktander, Cape Cod and England and Verrall's bootstrap of the over-dispersed
# Poisson chain ladder. A triangle is a numeric matrix of cumulative claims, one row
# per origin period and one column per development period, with NA for the future.

# The cumulative triangle as a numeric matrix with origin and development names, and
# the latest development period of each origin. Each origin's values must start at
# the first development period and have no gaps; the last development period must
# have a value.
reserving_check_triangle <- function(triangle) {
  if (is.data.frame(triangle)) triangle <- as.matrix(triangle)
  if (!is.matrix(triangle) || !(is.numeric(triangle) || is.logical(triangle))) {
    stop("triangle must be a numeric matrix of cumulative claims, one row per origin period and one column per ",
         "development period.", call. = FALSE)
  }
  storage.mode(triangle) <- "double"
  if (nrow(triangle) < 1 || ncol(triangle) < 2) {
    stop("triangle needs at least one origin period and two development periods.", call. = FALSE)
  }
  if (is.null(rownames(triangle))) rownames(triangle) <- seq_len(nrow(triangle))
  if (is.null(colnames(triangle))) colnames(triangle) <- seq_len(ncol(triangle))
  dimnames(triangle) <- list(origin = rownames(triangle), development = colnames(triangle))
  observed <- !is.na(triangle)
  latest <- rowSums(observed)
  gaps <- vapply(seq_len(nrow(triangle)), function(i) !all(observed[i, ] == (seq_len(ncol(triangle)) <= latest[i])), logical(1))
  if (any(latest == 0)) {
    stop("Origin ", rownames(triangle)[which(latest == 0)[1]], " has no values.", call. = FALSE)
  }
  if (any(gaps)) {
    stop("Origin ", rownames(triangle)[which(gaps)[1]], " has a gap: each origin's values must run from the first ",
         "development period to its latest, with none missing in between.", call. = FALSE)
  }
  if (any(!is.finite(triangle[observed]))) stop("triangle has values that are not finite numbers.", call. = FALSE)
  if (max(latest) < ncol(triangle)) {
    stop("The last development period has no values: leave it out of the triangle.", call. = FALSE)
  }
  list(triangle = triangle, latest = unname(latest))
}

# The link ratios C[i, j + 1] / C[i, j] of a cumulative triangle, NA where either is
# missing or the claims at j are not positive.
reserving_link_ratios <- function(triangle) {
  n <- ncol(triangle)
  from <- triangle[, -n, drop = FALSE]
  to <- triangle[, -1, drop = FALSE]
  ratios <- to / from
  ratios[!(is.finite(ratios) & from > 0)] <- NA
  dimnames(ratios) <- list(origin = rownames(triangle),
                           development = paste(colnames(triangle)[-n], colnames(triangle)[-1], sep = "-"))
  ratios
}

# The development periods (columns of the link ratios) whose ratios end on one of the
# latest `periods` calendar diagonals.
reserving_recent_ratios <- function(ratios, latest, periods) {
  diagonal <- row(ratios) + col(ratios)
  newest <- max(seq_along(latest) + latest - 1)
  diagonal > newest - periods
}

# The tail factor of a curve fitted to the development factors of periods from
# `from` onwards that are above 1: an exponential decay of log(f - 1) with the
# period, or an inverse power (log(f - 1) linear in the log of the period),
# extrapolated over the next 100 periods.
reserving_tail_curve <- function(factors, curve = c("exponential", "inverse power"), from = 1) {
  curve <- match.arg(curve)
  n <- length(factors)
  j <- seq_len(n)
  use <- j >= from & is.finite(factors) & factors > 1
  if (sum(use) < 2) {
    stop("A tail curve needs at least two development factors above 1 from the period it is fitted from.", call. = FALSE)
  }
  x <- if (curve == "exponential") j[use] else log(j[use])
  fit <- stats::lm.fit(cbind(1, x), log(factors[use] - 1))
  a <- unname(fit$coefficients[1])
  b <- unname(fit$coefficients[2])
  if (!is.finite(b) || b >= 0) {
    stop("The development factors do not fall towards 1, so no tail curve can be fitted to them: enter a tail ",
         "factor instead.", call. = FALSE)
  }
  later <- (n + 1):(n + 100)
  excess <- if (curve == "exponential") exp(a + b * later) else exp(a) * later^b
  tail <- prod(1 + excess)
  if (!is.finite(tail) || tail > 2) {
    stop("The fitted tail factor is over 2, which would more than double the claims: fit the curve from a later ",
         "period, or enter a tail factor.", call. = FALSE)
  }
  list(tail = tail, curve = curve, from = from, intercept = a, slope = b, used = which(use),
       fitted = function(period) 1 + if (curve == "exponential") exp(a + b * period) else exp(a) * period^b)
}

#' Chain ladder reserves with Mack's standard errors
#'
#' @description Projects a triangle of cumulative claims to ultimate with the chain
#'   ladder, and estimates the standard error of each origin's reserve and of the
#'   total with Mack's distribution-free model, including the tail.
#'
#' @details The development factor from period \eqn{j} to \eqn{j + 1} is the weighted
#'   average of the link ratios \eqn{F_{ij} = C_{i,j+1} / C_{ij}} that are used,
#'   with weights \eqn{C_{ij}^\alpha}: \eqn{\alpha = 1} for the volume-weighted
#'   average (the usual chain ladder), 0 for the simple average and 2 for the
#'   regression through the origin. Link ratios can be left out with
#'   \code{exclude}, or by using only the latest \code{periods} calendar periods.
#'   Link ratios from claims that are zero or negative are always left out.
#'
#'   The standard errors follow Mack (1993, 1999): the variance parameter of each
#'   period is \eqn{\sigma_j^2 = \sum w_{ij} C_{ij}^\alpha (F_{ij} - f_j)^2 / (n_j - 1)},
#'   and the reserve's mean squared error is the sum of the process and the
#'   parameter error, found recursively; the total adds the correlation of the
#'   origins through the shared factors. Where a period has a single link ratio, its
#'   \eqn{\sigma} is extrapolated: by a log-linear regression on the others
#'   (\code{"log-linear"}, the default, falling back to Mack's rule when that
#'   regression is not significant at 5\%), or by Mack's rule
#'   \eqn{\sigma_{n-1}^2 = \min(\sigma_{n-2}^4 / \sigma_{n-3}^2, \sigma_{n-3}^2, \sigma_{n-2}^2)}
#'   (\code{"Mack"}). With a tail factor, the tail's standard error and
#'   \eqn{\sigma} are extrapolated by log-linear regressions to where a curve
#'   through the factors reaches the tail factor. These choices are those of the
#'   ChainLadder package's \code{MackChainLadder()}, whose results the function
#'   reproduces.
#'
#' @param triangle A numeric matrix of cumulative claims: one row per origin period,
#'   oldest first, and one column per development period, with \code{NA} for the
#'   future. A data frame is converted with \code{as.matrix()}; see
#'   \code{\link{as_triangle}} to build one from a table of claims.
#' @param average \code{"volume"} (the default) for volume-weighted development
#'   factors, \code{"simple"} for simple averages of the link ratios, or
#'   \code{"regression"} for least squares through the origin.
#' @param periods The number of latest calendar periods (diagonals) whose link
#'   ratios are used, or \code{NULL} (the default) for all of them.
#' @param exclude Link ratios to leave out: a logical matrix with a row per origin
#'   and a column per development factor (\code{TRUE} to leave out), or
#'   \code{NULL} (the default) for none.
#' @param tail The tail factor from the last development period to ultimate: a
#'   positive number (1, the default, for none), or \code{"exponential"} or
#'   \code{"inverse power"} for a curve fitted to the development factors above 1
#'   and extrapolated over 100 periods.
#' @param tail_from The first development factor the tail curve is fitted to.
#' @param sigma How \eqn{\sigma} is found for a period with a single link ratio:
#'   \code{"log-linear"} or \code{"Mack"}.
#' @return An object of class \code{netsimr_chain_ladder}, a list with:
#'   \item{by_origin}{a data frame with, for each origin, the latest claims, the
#'     share developed (latest over ultimate), the ultimate, the IBNR (ultimate less
#'     latest: for a paid triangle, the IBNR and case reserves), its Mack standard
#'     error and coefficient of variation, and the process and parameter parts of the
#'     standard error;}
#'   \item{total}{the same for all origins together;}
#'   \item{factors, cdf, tail}{the development factors, the cumulative development
#'     factor from each period to ultimate (with the tail) and the tail factor;}
#'   \item{sigma, factor_se}{Mack's \eqn{\sigma} and the standard error of each
#'     factor, the tail's last;}
#'   \item{link_ratios, used}{the link ratios and which of them the factors use;}
#'   \item{triangle, full_triangle}{the triangle and the triangle completed with the
#'     projections, with an ultimate column;}
#'   \item{notes}{messages on the choices made, such as an extrapolated \eqn{\sigma}.}
#' @references Mack, T. (1993). Distribution-free calculation of the standard error
#'   of chain ladder reserve estimates. ASTIN Bulletin 23(2), 213-225.
#'
#'   Mack, T. (1999). The standard error of chain ladder reserve estimates:
#'   recursive calculation and inclusion of a tail factor. ASTIN Bulletin 29(2),
#'   361-366.
#' @seealso \code{\link{bornhuetter_ferguson}}, \code{\link{cape_cod}} and
#'   \code{\link{bootstrap_chain_ladder}}; \code{\link{run_shiny_reserving_tool}}, an
#'   application for all of them.
#' @export
#' @examples
#' cl <- chain_ladder(raa_triangle)
#' cl
#' cl$factors
#'
#' # simple averages of the latest five years, with a fitted tail
#' chain_ladder(raa_triangle, average = "simple", periods = 5, tail = "exponential")$total
chain_ladder <- function(triangle, average = c("volume", "simple", "regression"), periods = NULL, exclude = NULL,
                         tail = 1, tail_from = 1, sigma = c("log-linear", "Mack")) {
  average <- match.arg(average)
  sigma_method <- match.arg(sigma)
  checked <- reserving_check_triangle(triangle)
  C <- checked$triangle
  latest <- checked$latest
  m <- nrow(C)
  n <- ncol(C)
  alpha <- c(volume = 1, simple = 0, regression = 2)[[average]]
  notes <- character()

  ratios <- reserving_link_ratios(C)
  available <- !is.na(ratios)
  used <- available
  if (!is.null(periods)) {
    if (!is.numeric(periods) || length(periods) != 1 || !is.finite(periods) || periods < 1 || periods != round(periods)) {
      stop("periods must be a whole number of at least 1, or NULL for all.", call. = FALSE)
    }
    used <- used & reserving_recent_ratios(ratios, latest, periods)
  }
  if (!is.null(exclude)) {
    if (!is.matrix(exclude) || !is.logical(exclude) || !identical(dim(exclude), dim(ratios))) {
      stop("exclude must be a logical matrix with a row for each origin and a column for each development factor (",
           m, " by ", n - 1, ").", call. = FALSE)
    }
    used <- used & !(exclude %in% TRUE)
  }
  dimnames(used) <- dimnames(ratios)
  count <- colSums(used)
  if (any(count == 0)) {
    stop("The development factor ", colnames(ratios)[which(count == 0)[1]], " has no link ratios to use: ",
         "include at least one.", call. = FALSE)
  }

  weight <- ifelse(used, C[, -n, drop = FALSE]^alpha, 0)
  F0 <- ifelse(used, ratios, 0)
  factors <- colSums(weight * F0) / colSums(weight)
  names(factors) <- colnames(ratios)
  deviation <- ifelse(used, (ratios - rep(factors, each = m))^2, 0)
  sigma <- ifelse(count > 1, sqrt(colSums(weight * deviation) / pmax(count - 1, 1)), NA_real_)

  # sigma for the periods with one link ratio
  missing <- which(is.na(sigma))
  if (length(missing) > 0) {
    method <- sigma_method
    if (method == "log-linear") {
      known <- which(is.finite(sigma) & sigma > 0)
      p_value <- NA_real_
      if (length(known) >= 2) {
        fit <- stats::lm(log(sigma[known]) ~ known)
        p_value <- tryCatch(summary(fit)$coefficients[2, 4], error = function(e) NA_real_)
      }
      if (is.finite(p_value) && p_value <= 0.05) {
        sigma[missing] <- exp(stats::predict(fit, newdata = data.frame(known = missing)))
        notes <- c(notes, paste0("Sigma of ", paste(names(factors)[missing], collapse = ", "),
                                 " (a single link ratio) extrapolated log-linearly."))
      } else {
        method <- "Mack"
        notes <- c(notes, "The log-linear fit of sigma is not significant at 5%, so Mack's rule extrapolates sigma.")
      }
    }
    if (method == "Mack") {
      for (j in missing) {
        sigma[j] <- if (j >= 3 && all(is.finite(sigma[j - 1:2]))) {
          ratio <- sigma[j - 1]^4 / sigma[j - 2]^2
          candidates <- c(sigma[j - 2]^2, sigma[j - 1]^2, if (is.finite(ratio)) ratio)
          sqrt(min(candidates))
        } else if (j >= 2 && is.finite(sigma[j - 1])) {
          sigma[j - 1]
        } else {
          0
        }
      }
      notes <- c(notes, paste0("Sigma of ", paste(names(factors)[missing], collapse = ", "),
                               " (a single link ratio) set by Mack's rule."))
    }
  }
  factor_se <- sigma / sqrt(colSums(weight))

  # the tail
  tail_curve <- NULL
  if (is.character(tail)) {
    if (length(tail) != 1 || !(tail %in% c("exponential", "inverse power"))) {
      stop("tail must be a positive number, \"exponential\" or \"inverse power\".", call. = FALSE)
    }
    tail_curve <- reserving_tail_curve(factors, tail, tail_from)
    tail_factor <- tail_curve$tail
  } else {
    if (!is.numeric(tail) || length(tail) != 1 || !is.finite(tail) || tail <= 0) {
      stop("tail must be a positive number, \"exponential\" or \"inverse power\".", call. = FALSE)
    }
    tail_factor <- tail
  }
  tail_se <- 0
  tail_sigma <- 0
  if (tail_factor != 1) {
    j <- seq_along(factors)
    above <- is.finite(factors) & factors > 1
    estimated <- FALSE
    if (sum(above) >= 2 && tail_factor > 1) {
      curve <- stats::lm.fit(cbind(1, j[above]), log(factors[above] - 1))$coefficients
      if (is.finite(curve[2]) && curve[2] < 0) {
        position <- (log(tail_factor - 1) - curve[1]) / curve[2]
        extrapolate <- function(values) {
          ok <- is.finite(values) & values > 0
          if (sum(ok) < 2) return(0)
          fit <- stats::lm.fit(cbind(1, j[ok]), log(values[ok]))$coefficients
          unname(exp(fit[1] + fit[2] * position))
        }
        tail_se <- extrapolate(factor_se)
        tail_sigma <- extrapolate(sigma)
        estimated <- TRUE
      }
    }
    if (!estimated) notes <- c(notes, "The tail factor's standard error could not be extrapolated and is taken as 0.")
  }

  # projection, and Mack's recursion of the process and parameter variances
  f <- unname(factors)
  fse <- unname(factor_se)
  sig <- unname(sigma)
  full <- C
  process <- numeric(m)
  parameter <- numeric(m)
  for (i in seq_len(m)) {
    if (latest[i] < n) {
      for (k in latest[i]:(n - 1)) {
        process[i] <- process[i] * f[k]^2 + sig[k]^2 * full[i, k]^(2 - alpha)
        parameter[i] <- parameter[i] * f[k]^2 + full[i, k]^2 * fse[k]^2
        full[i, k + 1] <- full[i, k] * f[k]
      }
    }
  }
  ultimate <- unname(full[, n]) * tail_factor
  if (tail_factor != 1) {
    process <- process * tail_factor^2 + tail_sigma^2 * full[, n]^(2 - alpha)
    parameter <- parameter * tail_factor^2 + full[, n]^2 * tail_se^2
  }
  total_parameter <- 0
  for (k in seq_len(n - 1)) {
    projected <- sum(full[latest <= k, k])
    total_parameter <- total_parameter * f[k]^2 + projected^2 * fse[k]^2
  }
  if (tail_factor != 1) total_parameter <- total_parameter * tail_factor^2 + sum(full[, n])^2 * tail_se^2

  latest_claims <- C[cbind(seq_len(m), latest)]
  ibnr <- ultimate - latest_claims
  se <- sqrt(process + parameter)
  by_origin <- data.frame(
    origin = rownames(C), latest = latest_claims, developed = latest_claims / ultimate, ultimate = ultimate,
    ibnr = ibnr, se = se, cv = ifelse(ibnr != 0, se / ibnr, NA_real_),
    process_se = sqrt(process), parameter_se = sqrt(parameter),
    stringsAsFactors = FALSE, row.names = NULL
  )
  total_se <- sqrt(sum(process) + total_parameter)
  total <- c(latest = sum(latest_claims), developed = sum(latest_claims) / sum(ultimate), ultimate = sum(ultimate),
             ibnr = sum(ibnr), se = total_se, cv = if (sum(ibnr) != 0) total_se / sum(ibnr) else NA_real_,
             process_se = sqrt(sum(process)), parameter_se = sqrt(total_parameter))
  cdf <- rev(cumprod(rev(c(factors, tail_factor))))
  names(cdf) <- c(colnames(C))
  full <- cbind(full, Ultimate = ultimate)
  names(dimnames(full)) <- c("origin", "development")

  structure(list(
    method = "Chain ladder", by_origin = by_origin, total = total,
    factors = factors, cdf = cdf, tail = tail_factor, tail_curve = tail_curve,
    sigma = c(sigma, tail = tail_sigma), factor_se = c(factor_se, tail = tail_se),
    link_ratios = ratios, used = used, triangle = C, full_triangle = full, latest_period = latest,
    average = average, alpha = alpha, notes = notes
  ), class = c("netsimr_chain_ladder", "netsimr_reserve"))
}

# The chain ladder of a triangle, or the one given.
reserving_development <- function(triangle, ...) {
  if (inherits(triangle, "netsimr_chain_ladder")) triangle else chain_ladder(triangle, ...)
}

# Premiums or loss ratios checked against the number of origins.
reserving_per_origin <- function(x, m, name, positive = TRUE, single = TRUE) {
  if (!is.numeric(x) || !(length(x) == m || (single && length(x) == 1)) || any(!is.finite(x)) ||
      any(if (positive) x <= 0 else x < 0)) {
    stop(name, " must be ", if (positive) "positive" else "non-negative", " numbers, one for each origin",
         if (single) " (or one for all)", ".", call. = FALSE)
  }
  rep_len(as.numeric(x), m)
}

#' Bornhuetter-Ferguson and Benktander reserves
#'
#' @description Reserves from an a priori expected loss ratio: the latest claims
#'   plus the share of the expected ultimate still to emerge, by the chain ladder's
#'   development pattern.
#'
#' @details With \eqn{p_i = 1 / CDF_i} the share developed of origin \eqn{i} (the
#'   inverse of its chain ladder cumulative development factor, with the tail) and
#'   \eqn{U_0 = P_i \times LR_i} the a priori ultimate, each iteration gives
#'   \eqn{U_{k} = C_i + (1 - p_i) U_{k-1}}. One iteration is the
#'   Bornhuetter-Ferguson method, two the Benktander (iterated
#'   Bornhuetter-Ferguson) method, and as the iterations increase the ultimate
#'   tends to the chain ladder's.
#' @param triangle A triangle of cumulative claims, as for
#'   \code{\link{chain_ladder}}, or the result of \code{chain_ladder()}, whose
#'   development pattern is then used.
#' @param premium The premium (or other exposure) of each origin.
#' @param loss_ratio The a priori expected loss ratio: one for all origins or one
#'   for each, as a share of the premium (0.7 for 70\%).
#' @param iterations 1 (the default) for Bornhuetter-Ferguson, 2 for Benktander.
#' @param ... Arguments passed to \code{chain_ladder()} for the development pattern,
#'   such as \code{average} and \code{tail}, when \code{triangle} is a triangle.
#' @return An object of class \code{netsimr_reserve}: a list with \code{by_origin},
#'   a data frame with the latest claims, premium, a priori loss ratio, share
#'   developed, a priori ultimate, ultimate, IBNR and ultimate loss ratio of each
#'   origin, \code{total}, the same for all origins together, and
#'   \code{chain_ladder}, the development pattern.
#' @references Bornhuetter, R. L. and Ferguson, R. E. (1972). The actuary and IBNR.
#'   Proceedings of the Casualty Actuarial Society 59, 181-195.
#'
#'   Mack, T. (2000). Credible claims reserves: the Benktander method. ASTIN
#'   Bulletin 30(2), 333-347.
#' @seealso \code{\link{cape_cod}}, which estimates the loss ratio from the
#'   triangle, and \code{\link{chain_ladder}}.
#' @export
#' @examples
#' premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
#' bornhuetter_ferguson(raa_triangle, premium, loss_ratio = 0.35)
#'
#' # Benktander, with the chain ladder of simple averages
#' cl <- chain_ladder(raa_triangle, average = "simple")
#' bornhuetter_ferguson(cl, premium, loss_ratio = 0.35, iterations = 2)$total
bornhuetter_ferguson <- function(triangle, premium, loss_ratio, iterations = 1, ...) {
  cl <- reserving_development(triangle, ...)
  m <- nrow(cl$triangle)
  premium <- reserving_per_origin(premium, m, "premium", single = FALSE)
  loss_ratio <- reserving_per_origin(loss_ratio, m, "loss_ratio", positive = FALSE)
  if (!is.numeric(iterations) || length(iterations) != 1 || !is.finite(iterations) || iterations < 1 ||
      iterations != round(iterations)) {
    stop("iterations must be a whole number of at least 1.", call. = FALSE)
  }
  latest <- cl$by_origin$latest
  developed <- 1 / cl$cdf[cl$latest_period]
  prior <- premium * loss_ratio
  ultimate <- prior
  for (k in seq_len(iterations)) ultimate <- latest + (1 - developed) * ultimate
  method <- if (iterations == 1) "Bornhuetter-Ferguson" else if (iterations == 2) "Benktander" else
    paste0("Iterated Bornhuetter-Ferguson (", iterations, " iterations)")
  reserving_expected_result(method, cl, premium, loss_ratio, developed, prior, ultimate)
}

#' Cape Cod reserves
#'
#' @description Bornhuetter-Ferguson reserves whose expected loss ratio is
#'   estimated from the triangle: the latest claims over the premium already
#'   earned by development (the used-up premium).
#'
#' @details The expected loss ratio is
#'   \eqn{LR = \sum_i C_i / \sum_i P_i p_i}, with \eqn{p_i = 1 / CDF_i} the share
#'   developed of origin \eqn{i}, and each origin's ultimate is
#'   \eqn{C_i + (1 - p_i) P_i LR}. With \code{decay} below 1 (the generalised Cape
#'   Cod of Gluck), each origin has its own loss ratio, in which origin \eqn{k}
#'   weighs \eqn{decay^{|i - k|}}, so that nearby origins count most; with decay 0
#'   the method is the chain ladder.
#' @inheritParams bornhuetter_ferguson
#' @param decay The weight of an origin one period away in an origin's loss ratio,
#'   from 0 to 1 (the default, the classical Cape Cod with one loss ratio).
#' @return An object of class \code{netsimr_reserve}, as for
#'   \code{\link{bornhuetter_ferguson}}, whose loss ratios are the estimated ones.
#' @references Stanard, J. N. (1985). A simulation test of prediction errors of loss
#'   reserve estimation techniques. Proceedings of the Casualty Actuarial Society
#'   72, 124-148.
#'
#'   Gluck, S. M. (1997). Balancing development and trend in loss reserve analysis.
#'   Proceedings of the Casualty Actuarial Society 84, 482-532.
#' @export
#' @examples
#' premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
#' cape_cod(raa_triangle, premium)
#' cape_cod(raa_triangle, premium, decay = 0.75)$by_origin$loss_ratio
cape_cod <- function(triangle, premium, decay = 1, ...) {
  cl <- reserving_development(triangle, ...)
  m <- nrow(cl$triangle)
  premium <- reserving_per_origin(premium, m, "premium", single = FALSE)
  if (!is.numeric(decay) || length(decay) != 1 || !is.finite(decay) || decay < 0 || decay > 1) {
    stop("decay must be a number from 0 to 1.", call. = FALSE)
  }
  latest <- cl$by_origin$latest
  developed <- 1 / cl$cdf[cl$latest_period]
  used_premium <- premium * developed
  distance <- abs(outer(seq_len(m), seq_len(m), "-"))
  weights <- decay^distance
  loss_ratio <- as.vector((weights %*% latest) / (weights %*% used_premium))
  if (any(!is.finite(loss_ratio))) {
    stop("The used-up premium is zero, so the Cape Cod loss ratio cannot be estimated.", call. = FALSE)
  }
  prior <- premium * loss_ratio
  ultimate <- latest + (1 - developed) * prior
  method <- if (decay == 1) "Cape Cod" else paste0("Generalised Cape Cod (decay ", format(decay), ")")
  result <- reserving_expected_result(method, cl, premium, loss_ratio, developed, prior, ultimate)
  result$total[["loss_ratio"]] <- if (decay == 1) loss_ratio[1] else sum(prior) / sum(premium)
  result
}

# The result of an expected loss ratio method.
reserving_expected_result <- function(method, cl, premium, loss_ratio, developed, prior, ultimate) {
  latest <- cl$by_origin$latest
  by_origin <- data.frame(
    origin = cl$by_origin$origin, latest = latest, premium = premium, loss_ratio = loss_ratio,
    developed = unname(developed), prior_ultimate = prior, ultimate = ultimate, ibnr = ultimate - latest,
    ultimate_loss_ratio = ultimate / premium, stringsAsFactors = FALSE, row.names = NULL
  )
  total <- c(latest = sum(latest), premium = sum(premium), loss_ratio = sum(prior) / sum(premium),
             prior_ultimate = sum(prior), ultimate = sum(ultimate), ibnr = sum(ultimate - latest),
             ultimate_loss_ratio = sum(ultimate) / sum(premium))
  structure(list(method = method, by_origin = by_origin, total = total, chain_ladder = cl),
            class = "netsimr_reserve")
}

#' Bootstrap of the chain ladder reserves
#'
#' @description Simulates the distribution of the reserves with England and
#'   Verrall's bootstrap of the over-dispersed Poisson chain ladder: resampling
#'   its residuals for the estimation error, then simulating each future
#'   payment for the process error.
#'
#' @details The volume-weighted chain ladder factors of the whole triangle give
#'   fitted incremental claims \eqn{m_{ij}}, working back from the latest
#'   diagonal. The Pearson residuals \eqn{(X_{ij} - m_{ij}) / \sqrt{|m_{ij}|}}
#'   give the scale parameter \eqn{\phi}, their sum of squares over \eqn{N - p}
#'   (\eqn{N} incremental values and \eqn{p} parameters, one per origin and one
#'   per development period less one), and, scaled by \eqn{\sqrt{N / (N - p)}},
#'   the residuals that are resampled; the residuals that are zero by
#'   construction (an origin with one value, a development period with one
#'   origin) are left out of the sampling. Each simulation builds a pseudo
#'   triangle \eqn{m_{ij} + r^*_{ij} \sqrt{|m_{ij}|}}, refits the chain ladder to
#'   it and projects its latest diagonal, then draws each future increment from a
#'   gamma or over-dispersed Poisson distribution with that mean and variance
#'   \eqn{\phi} times it. A tail factor scales each simulated ultimate.
#' @param triangle A triangle of cumulative claims, as for \code{\link{chain_ladder}}.
#' @param n The number of simulations.
#' @param process The distribution of the future increments: \code{"gamma"} (the
#'   default) or \code{"odp"}, the over-dispersed Poisson.
#' @param tail A tail factor applied to each simulated ultimate (1, the default,
#'   for none).
#' @param probs The probabilities of the percentiles reported.
#' @param seed A whole number for a reproducible run, or \code{NULL} (the default)
#'   to use the current random number stream. The stream of the R session is
#'   restored after a run with a seed.
#' @return An object of class \code{netsimr_bootstrap}, a list with:
#'   \item{by_origin}{a data frame with each origin's latest claims, mean ultimate,
#'     mean IBNR, its standard deviation and coefficient of variation, and the
#'     percentiles of the IBNR;}
#'   \item{total}{the same for the total;}
#'   \item{reserves}{the simulated IBNR, a matrix with a row per simulation and a
#'     column per origin;}
#'   \item{total_reserves}{the simulated total IBNR;}
#'   \item{residuals, fitted, scale, factors}{the scaled Pearson residuals, the
#'     fitted incremental claims, \eqn{\phi} and the chain ladder factors.}
#' @references England, P. D. and Verrall, R. J. (1999). Analytic and bootstrap
#'   estimates of prediction errors in claims reserving. Insurance: Mathematics
#'   and Economics 25(3), 281-293.
#'
#'   England, P. D. and Verrall, R. J. (2002). Stochastic claims reserving in
#'   general insurance. British Actuarial Journal 8(3), 443-518.
#' @export
#' @examples
#' boot <- bootstrap_chain_ladder(raa_triangle, n = 1000, seed = 1)
#' boot$total
#' hist(boot$total_reserves, main = "Total IBNR", xlab = NULL)
bootstrap_chain_ladder <- function(triangle, n = 1000, process = c("gamma", "odp"), tail = 1,
                                   probs = c(0.75, 0.9, 0.95, 0.99, 0.995), seed = NULL) {
  process <- match.arg(process)
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 2 || n != round(n)) {
    stop("n must be a whole number of at least 2.", call. = FALSE)
  }
  if (!is.numeric(tail) || length(tail) != 1 || !is.finite(tail) || tail <= 0) {
    stop("tail must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(probs) || length(probs) == 0 || any(!is.finite(probs)) || any(probs < 0 | probs > 1)) {
    stop("probs must be probabilities from 0 to 1.", call. = FALSE)
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed != round(seed)) {
      stop("seed must be a whole number, or NULL.", call. = FALSE)
    }
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else
      rm(".Random.seed", envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  checked <- reserving_check_triangle(triangle)
  C <- checked$triangle
  latest <- checked$latest
  m <- nrow(C)
  d <- ncol(C)
  observed <- !is.na(C)

  # volume-weighted factors of the whole triangle
  volume_factors <- function(cum) {
    vapply(seq_len(d - 1), function(j) {
      rows <- latest >= j + 1
      sum(cum[rows, j + 1]) / sum(cum[rows, j])
    }, numeric(1))
  }
  factors <- volume_factors(C)
  if (any(!is.finite(factors)) || any(factors <= 0)) {
    stop("The claims of a development period add up to zero or less, so the chain ladder cannot be bootstrapped.",
         call. = FALSE)
  }
  # fitted cumulative claims, back from the latest diagonal, and the increments
  backward <- rev(cumprod(rev(factors)))
  to_ultimate <- c(backward, 1)
  fitted_cum <- C
  latest_claims <- C[cbind(seq_len(m), latest)]
  for (i in seq_len(m)) {
    fitted_cum[i, seq_len(latest[i])] <- latest_claims[i] * to_ultimate[latest[i]] / to_ultimate[seq_len(latest[i])]
  }
  increments <- function(cum) cbind(cum[, 1], cum[, -1, drop = FALSE] - cum[, -d, drop = FALSE])
  X <- increments(C)
  fitted <- increments(fitted_cum)
  N <- sum(observed)
  p <- m + d - 1
  if (N - p < 1) stop("The triangle has too few values to bootstrap: it needs more than one per parameter.", call. = FALSE)
  root <- sqrt(abs(fitted))
  residuals <- ifelse(observed & root > 0, (X - fitted) / root, NA_real_)
  residuals[observed & root == 0] <- 0
  scale <- sum(residuals^2, na.rm = TRUE) / (N - p)
  adjusted <- residuals * sqrt(N / (N - p))
  structural <- abs(X - fitted) <= 1e-9 * max(abs(C[observed]), 1)
  pool <- adjusted[observed & !structural]
  if (length(pool) == 0 || scale <= 0) {
    stop("The chain ladder fits the triangle exactly, so there is no variation to bootstrap.", call. = FALSE)
  }
  dimnames(adjusted) <- dimnames(fitted) <- dimnames(C)

  cells <- which(observed)
  future <- which(!observed)
  root_cells <- root[cells]
  fitted_cells <- fitted[cells]
  reserves <- matrix(0, n, m, dimnames = list(NULL, rownames(C)))
  chunk <- max(1, floor(4e6 / (m * d)))
  start <- 1
  while (start <= n) {
    s <- min(chunk, n - start + 1)
    # pseudo increments: origins x development x simulations
    pseudo <- array(0, c(m, d, s))
    draws <- matrix(pool[sample.int(length(pool), length(cells) * s, replace = TRUE)], length(cells), s)
    offsets <- rep((seq_len(s) - 1) * m * d, each = length(cells))
    pseudo[cells + offsets] <- fitted_cells + draws * root_cells
    for (j in seq_len(d)[-1]) pseudo[, j, ] <- pseudo[, j - 1, ] + pseudo[, j, ]
    projected <- pseudo
    for (j in seq_len(d - 1)) {
      rows <- latest >= j + 1
      f <- colSums(pseudo[rows, j + 1, , drop = FALSE]) / colSums(pseudo[rows, j, , drop = FALSE])
      f[!is.finite(f)] <- factors[j]
      grow <- latest <= j
      if (any(grow)) {
        projected[grow, j + 1, ] <- projected[grow, j, , drop = FALSE] * rep(f, each = sum(grow))
      }
    }
    expected <- projected
    expected[, -1, ] <- projected[, -1, , drop = FALSE] - projected[, -d, , drop = FALSE]
    mean_future <- expected[future + rep((seq_len(s) - 1) * m * d, each = length(future))]
    shape <- abs(mean_future) / scale
    simulated <- if (process == "gamma") {
      sign(mean_future) * stats::rgamma(length(mean_future), shape = shape, scale = scale)
    } else {
      sign(mean_future) * scale * stats::rpois(length(mean_future), shape)
    }
    simulated[shape == 0] <- 0
    future_array <- array(0, c(m, d, s))
    future_array[future + rep((seq_len(s) - 1) * m * d, each = length(future))] <- simulated
    reserves[start:(start + s - 1), ] <- t(apply(future_array, c(1, 3), sum))
    start <- start + s
  }
  if (tail != 1) reserves <- (rep(latest_claims, each = n) + reserves) * tail - rep(latest_claims, each = n)
  totals <- rowSums(reserves)

  summarise <- function(x, latest_value) {
    c(latest = latest_value, ultimate = latest_value + mean(x), ibnr = mean(x), se = stats::sd(x),
      cv = if (mean(x) != 0) stats::sd(x) / mean(x) else NA_real_,
      stats::setNames(stats::quantile(x, probs, names = FALSE), paste0("p", formatC(100 * probs, format = "fg"))))
  }
  by_origin <- do.call(rbind, lapply(seq_len(m), function(i) summarise(reserves[, i], latest_claims[i])))
  by_origin <- data.frame(origin = rownames(C), by_origin, stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)
  structure(list(
    method = paste0("Bootstrap chain ladder (", if (process == "gamma") "gamma" else "over-dispersed Poisson", " process, ",
                    format(n, big.mark = ","), " simulations)"),
    by_origin = by_origin, total = summarise(totals, sum(latest_claims)),
    reserves = reserves, total_reserves = totals, residuals = adjusted, fitted = fitted, scale = scale,
    factors = stats::setNames(factors, colnames(reserving_link_ratios(C))), tail = tail, process = process,
    probs = probs, triangle = C
  ), class = c("netsimr_bootstrap", "netsimr_reserve"))
}

#' Print a reserving result
#'
#' @param x The result of \code{\link{chain_ladder}}, \code{\link{bornhuetter_ferguson}},
#'   \code{\link{cape_cod}}, \code{\link{bootstrap_chain_ladder}}, \code{\link{merz_wuthrich}},
#'   \code{\link{munich_chain_ladder}}, \code{\link{actual_vs_expected}}, \code{\link{clark_growth}}
#'   or \code{\link{inflation_adjusted_chain_ladder}}.
#' @param ... Not used.
#' @return \code{x}, invisibly.
#' @export
#' @keywords internal
print.netsimr_reserve <- function(x, ...) {
  cat(x$method, "\n\n")
  table <- x$by_origin
  total <- lapply(names(table), function(name) {
    if (name == "origin") "Total" else if (name %in% names(x$total)) x$total[[name]] else NA_real_
  })
  names(total) <- names(table)
  table <- rbind(table, as.data.frame(total, stringsAsFactors = FALSE, check.names = FALSE))
  ratios <- names(table) %in% c("developed", "cv", "loss_ratio", "ultimate_loss_ratio", "latest_ratio", "ultimate_ratio",
                                "actual_to_expected", "z")
  shown <- table
  for (j in which(names(table) != "origin")) {
    shown[[j]] <- if (ratios[j]) formatC(table[[j]], format = "f", digits = 3) else
      formatC(table[[j]], format = "f", digits = 0, big.mark = ",")
    shown[[j]][!is.finite(table[[j]])] <- ""
  }
  print(shown, row.names = FALSE, right = TRUE)
  invisible(x)
}

#' Build a claims triangle from a table
#'
#' @description Makes the triangle of cumulative claims that the reserving functions
#'   take, from a table with a row per origin and development period (such as
#'   payments by accident year and development year), or from a table with a row
#'   per origin and a column per development period.
#'
#' @details In the long form, the origin and development periods are each sorted
#'   and taken to be consecutive: the development periods can be numbered (1, 2,
#'   ...) or ages (12, 24, ... months). Incremental values of the same origin and
#'   development period are added up, so claim transactions can be given as they
#'   are; a cell of the past with no value is then zero. The past is every cell on
#'   or before the latest calendar diagonal in the data. Cumulative values must have
#'   one value per cell of the past.
#'
#'   In the wide form, the values of each origin run from the first development
#'   period, with \code{NA} (or empty) for the future.
#' @param data A data frame: in the long form with origin, development and value
#'   columns; in the wide form with a row per origin and a column per development
#'   period (and the origin in the column named by \code{origin}, or in the row
#'   names).
#' @param origin,development,value The names of the origin, development period and
#'   value columns of the long form. For the wide form, give \code{development =
#'   NULL} and \code{value = NULL}, and \code{origin} the name of the origin column
#'   or \code{NULL} for the row names.
#' @param cumulative \code{TRUE} (the default) if the values are cumulative,
#'   \code{FALSE} if they are incremental.
#' @return A numeric matrix of cumulative claims, one row per origin and one column
#'   per development period, with \code{NA} for the future.
#' @export
#' @examples
#' head(simulated_motor_claims)
#' as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
as_triangle <- function(data, origin = "origin", development = "development", value = "value", cumulative = TRUE) {
  if (!is.data.frame(data) && !is.matrix(data)) stop("data must be a data frame.", call. = FALSE)
  if (!is.logical(cumulative) || length(cumulative) != 1 || is.na(cumulative)) {
    stop("cumulative must be TRUE or FALSE.", call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  long <- !is.null(development) || !is.null(value)
  if (long) {
    columns <- c(origin = origin, development = development, value = value)
    if (length(columns) != 3) stop("Give the origin, development and value columns.", call. = FALSE)
    missing_columns <- setdiff(columns, names(data))
    if (length(missing_columns) > 0) stop("data has no column ", missing_columns[1], ".", call. = FALSE)
    return(reserving_long_triangle(data[[origin]], data[[development]], data[[value]], cumulative))
  }
  if (is.null(origin)) {
    labels <- rownames(data)
    values <- data
  } else {
    if (!(origin %in% names(data))) stop("data has no column ", origin, ".", call. = FALSE)
    labels <- as.character(data[[origin]])
    values <- data[setdiff(names(data), origin)]
  }
  numbers <- vapply(values, function(column) is.numeric(column) || all(is.na(column)), logical(1))
  if (!all(numbers)) stop("The development period column ", names(values)[!numbers][1], " is not numeric.", call. = FALSE)
  reserving_wide_triangle(as.matrix(values), labels, names(values), cumulative)
}

# The triangle of a table with a row per origin and development period.
reserving_long_triangle <- function(origin, development, value, cumulative) {
  development <- suppressWarnings(as.numeric(development))
  value <- suppressWarnings(as.numeric(value))
  keep <- !is.na(origin) & !is.na(development) & !is.na(value)
  if (!any(keep)) stop("The table has no rows with an origin, a numeric development period and a numeric value.", call. = FALSE)
  if (any(!is.finite(value[keep]))) stop("The values must be finite numbers.", call. = FALSE)
  origin <- origin[keep]
  development <- development[keep]
  value <- value[keep]
  origins <- reserving_sorted_levels(origin)
  ages <- sort(unique(development))
  i <- match(as.character(origin), origins)
  j <- match(development, ages)
  m <- length(origins)
  d <- length(ages)
  if (d < 2) stop("The table needs at least two development periods.", call. = FALSE)
  newest <- max(i + j)
  past <- outer(seq_len(m), seq_len(d), "+") <= newest
  cell <- (j - 1) * m + i
  triangle <- matrix(NA_real_, m, d, dimnames = list(origin = origins, development = format(ages, trim = TRUE)))
  if (cumulative) {
    duplicated_cells <- duplicated(cell)
    if (any(duplicated_cells)) {
      k <- which(duplicated_cells)[1]
      stop("Origin ", origin[k], " has more than one value at development period ", format(development[k]),
           ": cumulative values need one per cell (give incremental values to add them up).", call. = FALSE)
    }
    triangle[cell] <- value
    gaps <- which(past & is.na(triangle), arr.ind = TRUE)
    if (nrow(gaps) > 0) {
      stop("Origin ", origins[gaps[1, 1]], " has no value at development period ", format(ages[gaps[1, 2]]),
           ", which is in the past.", call. = FALSE)
    }
  } else {
    sums <- tapply(value, cell, sum)
    increments <- matrix(0, m, d)
    increments[as.integer(names(sums))] <- sums
    increments[!past] <- NA
    triangle[] <- t(apply(increments, 1, cumsum))
  }
  reserving_trim_triangle(triangle)
}

# The triangle of a table with a row per origin and a column per development period.
reserving_wide_triangle <- function(values, labels, ages, cumulative) {
  storage.mode(values) <- "double"
  m <- nrow(values)
  d <- ncol(values)
  if (d < 2) stop("The table needs at least two development period columns.", call. = FALSE)
  observed <- !is.na(values)
  latest <- apply(observed, 1, function(x) if (any(x)) max(which(x)) else 0L)
  rows <- latest > 0
  values <- values[rows, , drop = FALSE]
  latest <- latest[rows]
  labels <- labels[rows]
  if (nrow(values) == 0) stop("The table has no values.", call. = FALSE)
  past <- col(values) <= latest
  if (cumulative) {
    gaps <- which(past & is.na(values), arr.ind = TRUE)
    if (nrow(gaps) > 0) {
      stop("Origin ", labels[gaps[1, 1]], " has no value at development period ", ages[gaps[1, 2]],
           ", before its latest value.", call. = FALSE)
    }
  } else {
    values[past & is.na(values)] <- 0
    values[past] <- t(apply(ifelse(past, values, 0), 1, cumsum))[past]
  }
  values[!past] <- NA
  dimnames(values) <- list(origin = labels, development = ages)
  reserving_trim_triangle(values)
}

# Drops development periods after the last one with a value.
reserving_trim_triangle <- function(triangle) {
  last <- max(which(colSums(!is.na(triangle)) > 0))
  triangle[, seq_len(last), drop = FALSE]
}

# Origins sorted as numbers when they all are, otherwise as text.
reserving_sorted_levels <- function(x) {
  levels <- unique(as.character(x))
  numbers <- suppressWarnings(as.numeric(levels))
  if (!anyNA(numbers)) levels[order(numbers)] else sort(levels)
}
