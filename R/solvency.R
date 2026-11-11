##############
#Discounted reserves, the risk adjustment and the Solvency II standard formula
##############

#' Discounted reserves from the chain ladder's payment pattern
#'
#' @description The future payments of a paid triangle, as the chain ladder projects
#'   them, by calendar period, discounted with a yield curve: the best estimate of the
#'   liability for incurred claims under IFRS 17, or of the claims provision under
#'   Solvency II (before expenses).
#'
#' @details Each origin's projected payments of each future development period fall
#'   in a future calendar period, the first being the one after the triangle's latest
#'   diagonal. A payment of calendar period \eqn{t} is taken to be made \code{timing}
#'   of the way through it, at \eqn{(t - 1 + \code{timing}) \times \code{period}} years,
#'   and discounted with the spot rate of that term, \eqn{(1 + r)^{-\textrm{term}}}. The
#'   claims of a tail factor beyond the triangle are paid evenly over the
#'   \code{tail_periods} periods after its last development period.
#'
#'   The spot rates of the yield curve are given for terms of 1, 2, 3, ... years; a
#'   term between two is interpolated linearly, one before the first takes the first
#'   rate and one beyond the last the last rate. One rate gives a flat curve.
#' @param x A triangle of cumulative paid claims, or its \code{\link{chain_ladder}}.
#' @param yield_curve The annual spot rates, as shares (0.03 for 3\%), for terms of 1,
#'   2, 3, ... years, or one rate for all terms.
#' @param period The length of a development period in years: 1 (the default) for an
#'   annual triangle, 0.25 for a quarterly one.
#' @param timing When the payments of a period are made, as a share of the period: 0.5
#'   (the default) for the middle, 0 for its start, 1 for its end.
#' @param tail_periods The number of periods over which the claims of a tail factor are
#'   paid.
#' @param ... Arguments of \code{\link{chain_ladder}} when \code{x} is a triangle.
#' @return An object of class \code{netsimr_discounted}, a list with:
#'   \item{by_origin}{a data frame with each origin's reserve (its IBNR: the
#'     undiscounted future payments), the reserve discounted, the discount and the
#'     mean term of its payments in years;}
#'   \item{cash_flows}{a data frame with a row per future calendar period: its payments,
#'     their term, the spot rate, the discount factor and their present value;}
#'   \item{total}{the reserve, the discounted reserve, the discount and the mean term.}
#' @seealso \code{\link{risk_adjustment}}, \code{\link{sii_premium_reserve_risk}} and
#'   \code{\link{chain_ladder}}; \code{\link{run_shiny_reserving_tool}}, an application
#'   for them.
#' @export
#' @examples
#' paid <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid",
#'                     cumulative = FALSE)
#' # a rising yield curve: 3% at one year to 3.8% at five years and beyond
#' be <- discount_reserves(paid, yield_curve = c(0.030, 0.032, 0.034, 0.036, 0.038))
#' be$total
#' head(be$cash_flows)
discount_reserves <- function(x, yield_curve = 0, period = 1, timing = 0.5, tail_periods = 1, ...) {
  cl <- reserving_development(x, ...)
  if (!is.numeric(yield_curve) || length(yield_curve) == 0 || any(!is.finite(yield_curve)) || any(yield_curve <= -1)) {
    stop("yield_curve must be one or more spot rates above -1, as shares (0.03 for 3%).", call. = FALSE)
  }
  if (!is.numeric(period) || length(period) != 1 || !is.finite(period) || period <= 0) {
    stop("period must be the length of a development period in years, above 0.", call. = FALSE)
  }
  if (!is.numeric(timing) || length(timing) != 1 || !is.finite(timing) || timing < 0 || timing > 1) {
    stop("timing must be a share of a period, from 0 to 1.", call. = FALSE)
  }
  if (!is.numeric(tail_periods) || length(tail_periods) != 1 || !is.finite(tail_periods) || tail_periods < 1 ||
      tail_periods != round(tail_periods)) {
    stop("tail_periods must be a whole number of at least 1.", call. = FALSE)
  }
  full <- cl$full_triangle
  n <- ncol(cl$triangle)
  m <- nrow(full)
  latest <- cl$latest_period
  # each origin's future payments by future calendar period: its development periods after
  # the latest, then the tail spread over the tail periods
  horizon <- n + tail_periods
  payments <- matrix(0, m, horizon)
  for (i in seq_len(m)) {
    if (latest[i] < n) {
      for (k in (latest[i] + 1):n) payments[i, k - latest[i]] <- full[i, k] - full[i, k - 1]
    }
    tail <- full[i, n + 1] - full[i, n]
    if (tail != 0) {
      offset <- n - latest[i]
      payments[i, offset + seq_len(tail_periods)] <- payments[i, offset + seq_len(tail_periods)] + tail / tail_periods
    }
  }
  used <- which(colSums(abs(payments)) > 0)
  last <- if (length(used)) max(used) else 1
  payments <- payments[, seq_len(last), drop = FALSE]
  term <- (seq_len(last) - 1 + timing) * period
  rate <- solvency_spot_rate(yield_curve, term)
  factor <- (1 + rate)^(-term)
  present <- sweep(payments, 2, factor, `*`)
  reserve <- rowSums(payments)
  discounted <- rowSums(present)
  mean_term <- ifelse(reserve != 0, as.vector(payments %*% term) / reserve, NA_real_)
  by_origin <- data.frame(origin = rownames(cl$triangle), reserve = reserve, discounted = discounted,
                          discount = reserve - discounted, mean_term = mean_term, stringsAsFactors = FALSE, row.names = NULL)
  cash_flows <- data.frame(period = seq_len(last), payments = colSums(payments), term = term, spot_rate = rate,
                           discount_factor = factor, present_value = colSums(present))
  total_reserve <- sum(reserve)
  structure(list(
    by_origin = by_origin, cash_flows = cash_flows,
    total = c(reserve = total_reserve, discounted = sum(discounted), discount = total_reserve - sum(discounted),
              mean_term = if (total_reserve != 0) sum(cash_flows$payments * term) / total_reserve else NA_real_),
    yield_curve = yield_curve, period = period, timing = timing, chain_ladder = cl
  ), class = "netsimr_discounted")
}

# The spot rate of each term (in years) from the rates of terms 1, 2, 3, ... years, linear
# between them and flat before the first and beyond the last.
solvency_spot_rate <- function(curve, term) {
  if (length(curve) == 1) return(rep(curve, length(term)))
  stats::approx(seq_along(curve), curve, xout = pmax(term, 1), rule = 2)$y
}

#' @export
print.netsimr_discounted <- function(x, ...) {
  t <- x$total
  cat("Reserve ", format(round(t[["reserve"]]), big.mark = ",", scientific = FALSE), ", discounted ", format(round(t[["discounted"]]), big.mark = ",", scientific = FALSE),
      " (a discount of ", format(round(t[["discount"]]), big.mark = ","), ", mean term ", formatC(t[["mean_term"]], format = "f", digits = 2),
      " years)\n", sep = "")
  invisible(x)
}

#' Risk adjustment for non-financial risk
#'
#' @description The compensation for bearing the uncertainty of the claims, the risk
#'   adjustment of IFRS 17 or the risk margin of Solvency II, by one of two methods: a
#'   percentile of the distribution of the reserves, or the cost of the capital held
#'   until the claims are paid.
#'
#' @details The percentile method takes the bootstrap's total reserves: the risk
#'   adjustment is their \code{level} quantile less their mean, and the confidence
#'   level is \code{level}.
#'
#'   The cost of capital method projects the capital \code{capital} held now over the
#'   run-off of the reserves, in proportion to the reserve still to be paid at the start
#'   of each period, and charges \code{coc_rate} of it for each period, discounted to
#'   now: \eqn{RA = \code{coc\_rate} \sum_t C_t (1 + r_t)^{-t}}, as the Solvency II risk
#'   margin is found. With the bootstrap given too, the confidence level that IFRS 17
#'   asks to disclose is the share of the simulated reserves at or below the mean plus
#'   the risk adjustment.
#'
#'   The discounting of the reserves applies to the risk adjustment too: with a
#'   discounted best estimate (\code{\link{discount_reserves}}), the percentile risk
#'   adjustment is scaled by the ratio of the discounted to the undiscounted reserve.
#' @param method \code{"percentile"} or \code{"cost of capital"}.
#' @param boot The \code{\link{bootstrap_chain_ladder}} of the reserves: needed for the
#'   percentile method, and for the confidence level of the cost of capital method.
#' @param level The confidence level of the percentile method, such as 0.75.
#' @param discounted The \code{\link{discount_reserves}} of the reserves: its payments
#'   give the run-off of the cost of capital method, its yield curve the discounting,
#'   and its ratio of discounted to undiscounted reserve scales the percentile method.
#'   Needed for the cost of capital method.
#' @param capital The capital held now for the reserves, such as their Solvency II
#'   reserve risk (\code{\link{sii_premium_reserve_risk}}) or the bootstrap's 99.5\%
#'   quantile less its mean. Needed for the cost of capital method.
#' @param coc_rate The cost of capital rate a year: 6\% under Solvency II before 2027.
#' @return A list with the method, the risk adjustment, the confidence level (NA without
#'   the bootstrap for the cost of capital method), and for the cost of capital method
#'   the projected capital and its discounted cost by period.
#' @seealso \code{\link{discount_reserves}}, \code{\link{bootstrap_chain_ladder}}.
#' @export
#' @examples
#' paid <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid",
#'                     cumulative = FALSE)
#' boot <- bootstrap_chain_ladder(paid, n = 2000, seed = 1)
#' be <- discount_reserves(paid, yield_curve = 0.03)
#' risk_adjustment("percentile", boot = boot, level = 0.75, discounted = be)$risk_adjustment
#' # the cost of 6% a year of the capital at 99.5%, over the run-off
#' capital <- boot$total[["p99.5"]] - boot$total[["ibnr"]]
#' ra <- risk_adjustment("cost of capital", boot = boot, discounted = be, capital = capital)
#' c(ra$risk_adjustment, ra$confidence_level)
risk_adjustment <- function(method = c("percentile", "cost of capital"), boot = NULL, level = 0.75, discounted = NULL,
                            capital = NULL, coc_rate = 0.06) {
  method <- match.arg(method)
  if (!is.null(boot) && !inherits(boot, "netsimr_bootstrap") && is.null(boot$total_reserves)) {
    stop("boot must be a bootstrap_chain_ladder() result.", call. = FALSE)
  }
  if (!is.null(discounted) && !inherits(discounted, "netsimr_discounted")) {
    stop("discounted must be a discount_reserves() result.", call. = FALSE)
  }
  scale <- if (!is.null(discounted) && discounted$total[["reserve"]] != 0) {
    discounted$total[["discounted"]] / discounted$total[["reserve"]]
  } else 1
  if (method == "percentile") {
    if (is.null(boot)) stop("The percentile method needs the bootstrap of the reserves (boot).", call. = FALSE)
    if (!is.numeric(level) || length(level) != 1 || !is.finite(level) || level <= 0 || level >= 1) {
      stop("level must be a probability between 0 and 1.", call. = FALSE)
    }
    totals <- boot$total_reserves
    ra <- (stats::quantile(totals, level, names = FALSE) - mean(totals)) * scale
    return(list(method = "Percentile", risk_adjustment = ra, confidence_level = level, scale = scale))
  }
  if (is.null(discounted)) stop("The cost of capital method needs the discounted reserves (discounted).", call. = FALSE)
  if (!is.numeric(capital) || length(capital) != 1 || !is.finite(capital) || capital < 0) {
    stop("capital must be the capital held now for the reserves, at least 0.", call. = FALSE)
  }
  if (!is.numeric(coc_rate) || length(coc_rate) != 1 || !is.finite(coc_rate) || coc_rate < 0) {
    stop("coc_rate must be a rate of at least 0.", call. = FALSE)
  }
  cf <- discounted$cash_flows
  total <- sum(cf$payments)
  # the reserve still to be paid at the start of each period, as a share of the reserve now
  outstanding <- if (total != 0) (total - c(0, cumsum(cf$payments)[-nrow(cf)])) / total else rep(0, nrow(cf))
  years <- seq_len(nrow(cf)) * discounted$period
  factor <- (1 + solvency_spot_rate(discounted$yield_curve, years))^(-years)
  projected <- capital * outstanding
  cost <- coc_rate * projected * discounted$period * factor
  ra <- sum(cost)
  confidence <- if (!is.null(boot)) mean(boot$total_reserves * scale <= mean(boot$total_reserves) * scale + ra) else NA_real_
  list(method = "Cost of capital", risk_adjustment = ra, confidence_level = confidence, coc_rate = coc_rate,
       capital = capital, run_off = data.frame(period = seq_len(nrow(cf)), capital = projected, discount_factor = factor,
                                               cost = cost))
}

#' The Solvency II standard formula's non-life segments
#'
#' @description The twelve segments of the Solvency II standard formula for non-life
#'   premium and reserve risk, with the standard deviations of their premium (gross)
#'   and reserve risk, as Annex II of Commission Delegated Regulation (EU) 2015/35 sets
#'   them after its amendment by Delegated Regulation (EU) 2019/981; and the correlations
#'   between the segments, Annex IV.
#' @format \code{sii_nonlife_segments}: a data frame with 12 rows and 4 columns: the
#'   segment's number, its name, and the standard deviations of premium risk and of
#'   reserve risk, as shares. \code{sii_nonlife_correlation}: the 12 by 12 matrix of
#'   correlations between the segments.
#' @source Commission Delegated Regulation (EU) 2015/35, Annexes II and IV, as amended by
#'   Commission Delegated Regulation (EU) 2019/981.
#' @seealso \code{\link{sii_premium_reserve_risk}}.
#' @name sii_nonlife_segments
#' @export
sii_nonlife_segments <- data.frame(
  segment = 1:12,
  name = c("Motor vehicle liability", "Other motor", "Marine, aviation and transport", "Fire and other damage to property",
           "General liability", "Credit and suretyship", "Legal expenses", "Assistance", "Miscellaneous financial loss",
           "Non-proportional casualty reinsurance", "Non-proportional marine, aviation and transport reinsurance",
           "Non-proportional property reinsurance"),
  premium_sd = c(0.10, 0.08, 0.15, 0.08, 0.14, 0.19, 0.083, 0.064, 0.13, 0.17, 0.17, 0.17),
  reserve_sd = c(0.09, 0.08, 0.11, 0.10, 0.11, 0.172, 0.055, 0.22, 0.20, 0.20, 0.20, 0.20),
  stringsAsFactors = FALSE
)

#' @rdname sii_nonlife_segments
#' @export
sii_nonlife_correlation <- matrix(c(
  1, 0.5, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.25, 0.25,
  0.5, 1, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25,
  0.5, 0.25, 1, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.25, 0.5, 0.25,
  0.25, 0.25, 0.25, 1, 0.25, 0.25, 0.25, 0.5, 0.5, 0.25, 0.5, 0.5,
  0.5, 0.25, 0.25, 0.25, 1, 0.5, 0.5, 0.25, 0.5, 0.5, 0.25, 0.25,
  0.25, 0.25, 0.25, 0.25, 0.5, 1, 0.5, 0.25, 0.5, 0.5, 0.25, 0.25,
  0.5, 0.5, 0.25, 0.25, 0.5, 0.5, 1, 0.25, 0.5, 0.5, 0.25, 0.25,
  0.25, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 1, 0.5, 0.25, 0.25, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 0.25, 0.5, 0.25,
  0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.25, 0.25, 1, 0.25, 0.25,
  0.25, 0.25, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25, 0.5, 0.25, 1, 0.25,
  0.25, 0.25, 0.25, 0.5, 0.25, 0.25, 0.25, 0.5, 0.25, 0.25, 0.25, 1
), 12, 12, byrow = TRUE, dimnames = list(1:12, 1:12))

#' Solvency II standard formula: non-life premium and reserve risk
#'
#' @description The capital requirement for non-life premium and reserve risk of the
#'   Solvency II standard formula (Articles 115 to 117 of Commission Delegated
#'   Regulation (EU) 2015/35), from the volume measures of each segment.
#'
#' @details The standard deviation of a segment combines those of its premium and
#'   reserve risk with a correlation of 50\%:
#'   \deqn{\sigma_s = \frac{\sqrt{(\sigma_{p,s} V_{p,s})^2 + \sigma_{p,s} \sigma_{r,s}
#'   V_{p,s} V_{r,s} + (\sigma_{r,s} V_{r,s})^2}}{V_{p,s} + V_{r,s}},}
#'   and its volume is \eqn{V_s = (V_{p,s} + V_{r,s}) (0.75 + 0.25 DIV_s)}, with
#'   \eqn{DIV_s} its geographical diversification (1 for business in a single region).
#'   The segments are joined with the correlations \eqn{CorrS} of Annex IV:
#'   \eqn{\sigma = \sqrt{\sum_{s,t} CorrS_{s,t} \sigma_s V_s \sigma_t V_t} / V}, with
#'   \eqn{V = \sum_s V_s}, and the capital requirement is \eqn{3 \sigma V}.
#'
#'   The premium volume of a segment is, under Article 116, the larger of the earned
#'   premium of the next year and of the last year, plus the expected premiums of
#'   existing and future contracts beyond the next year; the reserve volume is its best
#'   estimate of the claims provision, net of reinsurance. They are given here; the
#'   function does not derive them. The adjustment for non-proportional reinsurance (80\%
#'   of the premium standard deviation of segments 1, 4 and 5, Article 117(3)) is applied
#'   to the segments asked for, where the undertaking's reinsurance qualifies.
#' @param volumes A data frame with a row per segment the undertaking writes: its
#'   \code{segment} (the number, 1 to 12, of \code{\link{sii_nonlife_segments}}), its
#'   \code{premium} and \code{reserve} volume measures, and optionally its geographical
#'   \code{diversification} (1 when not given).
#' @param np_adjustment The segments whose premium standard deviation takes the 80\%
#'   adjustment for non-proportional reinsurance: \code{NULL} (the default) for none, or
#'   some of 1, 4 and 5.
#' @param premium_sd,reserve_sd Undertaking-specific standard deviations replacing the
#'   standard ones, named by segment number (such as \code{c("4" = 0.06)}), or
#'   \code{NULL}.
#' @return A list with \code{by_segment}, a data frame with each segment's volumes, its
#'   standard deviations, its volume measure and its capital on its own (\eqn{3
#'   \sigma_s V_s}); \code{total}, the volume measure, the standard deviation, the
#'   capital requirement, the sum of the segments' capital and the diversification
#'   between them; and \code{reserve_risk} and \code{premium_risk}, the capital of each
#'   risk alone (the same formula with the other volume set to zero).
#' @references Commission Delegated Regulation (EU) 2015/35, Articles 115 to 117 and
#'   Annexes II and IV, as amended by Commission Delegated Regulation (EU) 2019/981.
#' @seealso \code{\link{sii_nonlife_segments}}, \code{\link{risk_adjustment}}.
#' @export
#' @examples
#' volumes <- data.frame(segment = c(1, 2, 4), premium = c(40e6, 25e6, 30e6),
#'                       reserve = c(60e6, 10e6, 20e6))
#' sii <- sii_premium_reserve_risk(volumes)
#' sii$total
#' sii$by_segment[, c("name", "sd", "volume", "capital")]
sii_premium_reserve_risk <- function(volumes, np_adjustment = NULL, premium_sd = NULL, reserve_sd = NULL) {
  if (!is.data.frame(volumes) || !all(c("segment", "premium", "reserve") %in% names(volumes))) {
    stop("volumes must be a data frame with the columns segment, premium and reserve.", call. = FALSE)
  }
  seg <- volumes$segment
  if (!is.numeric(seg) || any(!seg %in% 1:12) || anyDuplicated(seg)) {
    stop("Each segment must be a number from 1 to 12, given once.", call. = FALSE)
  }
  vp <- volumes$premium
  vr <- volumes$reserve
  if (!is.numeric(vp) || !is.numeric(vr) || any(!is.finite(c(vp, vr))) || any(c(vp, vr) < 0)) {
    stop("The premium and reserve volumes must be amounts of at least 0.", call. = FALSE)
  }
  div <- if (!is.null(volumes$diversification)) volumes$diversification else rep(1, length(seg))
  if (!is.numeric(div) || any(!is.finite(div)) || any(div < 0 | div > 1)) {
    stop("The geographical diversification must be from 0 to 1.", call. = FALSE)
  }
  if (!is.null(np_adjustment) && !all(np_adjustment %in% c(1, 4, 5))) {
    stop("The adjustment for non-proportional reinsurance applies to segments 1, 4 and 5 only.", call. = FALSE)
  }
  standard <- sii_nonlife_segments[match(seg, sii_nonlife_segments$segment), ]
  sd_of <- function(given, column) {
    out <- standard[[column]]
    if (!is.null(given)) {
      at <- match(names(given), as.character(seg))
      if (anyNA(at)) stop("Undertaking-specific standard deviations must be named by a segment in volumes.", call. = FALSE)
      out[at] <- given
    }
    out
  }
  sp <- sd_of(premium_sd, "premium_sd") * ifelse(seg %in% np_adjustment, 0.8, 1)
  sr <- sd_of(reserve_sd, "reserve_sd")
  # the capital of the given volumes, and the details by segment
  compute <- function(vp, vr) {
    gross <- vp + vr
    sd <- ifelse(gross > 0, sqrt((sp * vp)^2 + sp * sr * vp * vr + (sr * vr)^2) / gross, 0)
    v <- gross * (0.75 + 0.25 * div)
    corr <- sii_nonlife_correlation[seg, seg, drop = FALSE]
    sv <- sd * v
    total_v <- sum(v)
    combined <- sqrt(as.numeric(t(sv) %*% corr %*% sv))
    list(sd = sd, v = v, sigma = if (total_v > 0) combined / total_v else 0, volume = total_v, capital = 3 * combined)
  }
  both <- compute(vp, vr)
  by_segment <- data.frame(segment = seg, name = standard$name, premium = vp, reserve = vr, premium_sd = sp, reserve_sd = sr,
                           sd = both$sd, volume = both$v, capital = 3 * both$sd * both$v, stringsAsFactors = FALSE,
                           row.names = NULL)
  standalone <- sum(by_segment$capital)
  list(
    by_segment = by_segment,
    total = c(volume = both$volume, sd = both$sigma, capital = both$capital, standalone = standalone,
              diversification = max(standalone - both$capital, 0)),
    premium_risk = compute(vp, 0 * vr)$capital,
    reserve_risk = compute(0 * vp, vr)$capital
  )
}
