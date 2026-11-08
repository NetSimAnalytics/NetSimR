##############
#Pricing monitoring: on-level premiums, the indicated rate change and credibility
##############

# A date as a decimal year: 2024-07-01 is about 2024.5. Numbers are taken as decimal years.
pricing_decimal_year <- function(x) {
  if (inherits(x, "Date")) {
    year <- as.numeric(format(x, "%Y"))
    start <- as.Date(paste0(year, "-01-01"))
    end <- as.Date(paste0(year + 1, "-01-01"))
    return(year + as.numeric(x - start) / as.numeric(end - start))
  }
  if (!is.numeric(x)) stop("dates must be Dates or decimal years.", call. = FALSE)
  x
}

#' Premiums at today's rates: the parallelogram method
#'
#' @description Brings each calendar year's earned premium to the current rate level,
#'   allowing for the rate changes made since: the on-level factor of a year is the
#'   current rate level over the average rate level of the premium the year earned.
#'
#' @details Policies are taken to be written evenly over time, each for \code{term}
#'   years, and to earn their premium evenly over their term. The premium a calendar
#'   year earns comes from the policies written in the \code{term} years before its end;
#'   its average rate level weighs the rate level at each writing date by the part of
#'   that policy's term within the year. This is the parallelogram method, worked out
#'   exactly for any dates of change and any term (numerically, on a fine grid).
#' @param premium The earned premium of each calendar year.
#' @param years The calendar years, such as 2020:2024.
#' @param changes A data frame of the rate changes: their \code{date} (a Date, or a
#'   decimal year such as 2023.5) and the \code{change} (0.05 for +5\%), applying to the
#'   policies written from that date.
#' @param term The length of the policies in years: 1 (the default) for annual policies.
#' @return A data frame with each year's earned premium, its average rate level (the
#'   rate level of the policies written before the first change being 1), the on-level
#'   factor and the premium at the current rate level; the current rate level is in its
#'   attribute \code{current_level}.
#' @seealso \code{\link{indicated_rate_change}} and \code{\link{buhlmann_straub}}.
#' @export
#' @examples
#' changes <- data.frame(date = as.Date(c("2021-07-01", "2023-01-01", "2024-04-01")),
#'                       change = c(0.05, -0.02, 0.08))
#' on_level_premium(c(9.2e6, 9.8e6, 10.4e6, 11.1e6), 2021:2024, changes)
on_level_premium <- function(premium, years, changes, term = 1) {
  if (!is.numeric(premium) || !is.numeric(years) || length(premium) != length(years) || any(!is.finite(c(premium, years)))) {
    stop("premium and years must be numbers of the same length.", call. = FALSE)
  }
  if (!is.data.frame(changes) || !all(c("date", "change") %in% names(changes))) {
    stop("changes must be a data frame with the columns date and change.", call. = FALSE)
  }
  if (!is.numeric(term) || length(term) != 1 || !is.finite(term) || term <= 0) stop("term must be a length in years, above 0.", call. = FALSE)
  dates <- pricing_decimal_year(changes$date)
  change <- changes$change
  if (any(!is.finite(dates)) || !is.numeric(change) || any(!is.finite(change)) || any(change <= -1)) {
    stop("Each rate change needs a date and a change above -1 (-100%).", call. = FALSE)
  }
  ord <- order(dates)
  dates <- dates[ord]
  levels <- cumprod(1 + change[ord])
  level_at <- function(w) c(1, levels)[findInterval(w, dates) + 1]
  average <- vapply(years, function(y) {
    # the writing dates w whose policies earn in year y run from y - term to y + 1, and a
    # policy written at w earns the part of its term within the year. Between the knots
    # (the corners of that overlap and the dates of change) the rate level is constant and
    # the overlap linear, so the trapezoid rule on each piece is exact
    overlap <- function(w) pmax(0, pmin(w + term, y + 1) - pmax(w, y))
    knots <- sort(unique(c(y - term, y, y + 1 - term, y + 1, dates[dates > y - term & dates < y + 1])))
    knots <- knots[knots >= y - term & knots <= y + 1]
    lo <- knots[-length(knots)]
    hi <- knots[-1]
    area <- (overlap(lo) + overlap(hi)) / 2 * (hi - lo)
    sum(level_at((lo + hi) / 2) * area) / sum(area)
  }, numeric(1))
  current <- if (length(levels)) levels[length(levels)] else 1
  out <- data.frame(year = years, premium = premium, average_level = average, on_level_factor = current / average,
                    on_level_premium = premium * current / average)
  attr(out, "current_level") <- current
  out
}

#' The indicated rate change: the loss ratio method
#'
#' @description Projects each year's ultimate losses to the period the new rates will
#'   apply to, with a claims trend, divides them by the year's on-level premium, and
#'   compares the projected loss ratio with the permissible one, which leaves room for
#'   the expenses and the profit.
#'
#' @details Each year's losses are trended from the middle of the year, \eqn{y + 0.5}
#'   for calendar or accident year \eqn{y}, to the average date of the claims of the
#'   policies the new rates will price: the date they take effect, plus half the period
#'   they will be in force, plus half a policy term. The experience loss ratio weighs
#'   the years by their on-level premium (or \code{weights}); with \code{credibility}
#'   below 1 it is blended with the \code{complement}, such as the loss ratio the
#'   current rates assume. The indicated change is that loss ratio over the
#'   permissible loss ratio, \eqn{1 - \textrm{expense ratio} - \textrm{profit}}, less 1.
#' @param losses The ultimate losses of each year, such as from \code{\link{chain_ladder}}.
#' @param premium The on-level earned premium of each year (\code{\link{on_level_premium}}).
#' @param years The years.
#' @param trend The annual claims trend (inflation of the loss cost), as a share.
#' @param effective The date the new rates take effect: a Date or a decimal year.
#' @param rate_period How long the new rates will be in force, in years.
#' @param term The length of the policies in years.
#' @param expense_ratio The expenses (commissions and other costs varying with the
#'   premium) as a share of the premium.
#' @param profit The profit and contingencies provision, as a share of the premium.
#' @param weights Weights of the years, such as to give the latest more weight; by
#'   default their on-level premium.
#' @param credibility The credibility of the experience, from 0 to 1.
#' @param complement The loss ratio blended with the experience when its credibility is
#'   below 1.
#' @return A list with \code{by_year} (each year's losses, trend factor, trended losses,
#'   premium and projected loss ratio), the experience loss ratio, the blended loss
#'   ratio, the permissible loss ratio and the indicated rate change.
#' @seealso \code{\link{on_level_premium}}, \code{\link{buhlmann_straub}} and
#'   \code{\link{credibility_weight}}.
#' @export
#' @examples
#' changes <- data.frame(date = as.Date(c("2021-07-01", "2023-01-01", "2024-04-01")),
#'                       change = c(0.05, -0.02, 0.08))
#' onl <- on_level_premium(c(9.2e6, 9.8e6, 10.4e6, 11.1e6), 2021:2024, changes)
#' rate <- indicated_rate_change(c(6.3e6, 6.6e6, 7.4e6, 7.6e6), onl$on_level_premium, 2021:2024,
#'                               trend = 0.04, effective = as.Date("2026-01-01"),
#'                               expense_ratio = 0.27, profit = 0.05)
#' rate[c("experience_loss_ratio", "indicated_change")]
indicated_rate_change <- function(losses, premium, years, trend = 0, effective, rate_period = 1, term = 1, expense_ratio = 0,
                                  profit = 0, weights = NULL, credibility = 1, complement = NULL) {
  n <- length(years)
  if (!is.numeric(losses) || !is.numeric(premium) || length(losses) != n || length(premium) != n ||
      any(!is.finite(c(losses, premium))) || any(premium <= 0)) {
    stop("losses, premium and years must have the same length, with premiums above 0.", call. = FALSE)
  }
  if (!is.numeric(trend) || length(trend) != 1 || !is.finite(trend) || trend <= -1) stop("trend must be a rate above -1.", call. = FALSE)
  permissible <- 1 - expense_ratio - profit
  if (!is.finite(permissible) || permissible <= 0) stop("The expenses and the profit must leave a permissible loss ratio above 0.", call. = FALSE)
  if (!is.numeric(credibility) || length(credibility) != 1 || credibility < 0 || credibility > 1) {
    stop("credibility must be from 0 to 1.", call. = FALSE)
  }
  if (credibility < 1 && (!is.numeric(complement) || length(complement) != 1 || !is.finite(complement))) {
    stop("A credibility below 1 needs the complement, a loss ratio.", call. = FALSE)
  }
  target <- pricing_decimal_year(effective) + rate_period / 2 + term / 2
  factor <- (1 + trend)^(target - (years + 0.5))
  trended <- losses * factor
  ratio <- trended / premium
  w <- if (is.null(weights)) premium else weights
  if (!is.numeric(w) || length(w) != n || any(!is.finite(w)) || any(w < 0) || sum(w) == 0) stop("weights must be at least 0, one per year.", call. = FALSE)
  experience <- sum(w * ratio) / sum(w)
  blended <- if (credibility < 1) credibility * experience + (1 - credibility) * complement else experience
  list(
    by_year = data.frame(year = years, losses = losses, trend_factor = factor, trended_losses = trended, premium = premium,
                         loss_ratio = ratio),
    experience_loss_ratio = experience, loss_ratio = blended, permissible_loss_ratio = permissible,
    indicated_change = blended / permissible - 1, target = target
  )
}

#' Buhlmann-Straub credibility
#'
#' @description The credibility estimates of the risk of several groups (schemes,
#'   fleets, regions, ...) from their experience over several periods, each weighted by
#'   its volume: each group's own mean, shrunk towards the collective by its
#'   credibility.
#'
#' @details With \eqn{X_{ij}} the value (such as a loss ratio or a claim frequency) of
#'   group \eqn{i} in period \eqn{j} and \eqn{w_{ij}} its weight (premium, exposure),
#'   \eqn{w_i = \sum_j w_{ij}}, \eqn{\bar X_i} the weighted mean of the group and
#'   \eqn{\bar X} of all, the within-group variance is estimated by
#'   \deqn{s^2 = \frac{\sum_i \sum_j w_{ij} (X_{ij} - \bar X_i)^2}{\sum_i (n_i - 1)}}
#'   and the variance between the groups by
#'   \deqn{a = \frac{\sum_i w_i (\bar X_i - \bar X)^2 - (I - 1) s^2}{w - \sum_i w_i^2 / w},}
#'   with \eqn{w = \sum_i w_i}. A group's credibility is \eqn{Z_i = w_i / (w_i + s^2 / a)}
#'   (0 for all when \eqn{a \le 0}: the groups do not differ more than chance), the
#'   collective mean \eqn{\hat\mu = \sum_i Z_i \bar X_i / \sum_i Z_i} (the weighted mean
#'   when every Z is 0), and its estimate \eqn{\hat\mu + Z_i (\bar X_i - \hat\mu)}.
#' @param value The value of each group and period.
#' @param weight The weight of each value.
#' @param group The group of each value.
#' @return A list with \code{by_group} (each group's weight, number of periods, mean,
#'   credibility and estimate), the collective mean, the within and between variances
#'   (\code{s2}, \code{a}) and \code{k} \eqn{= s^2 / a}.
#' @references Buhlmann, H. and Straub, E. (1970). Glaubwurdigkeit fur Schadensatze.
#'   Mitteilungen der Vereinigung Schweizerischer Versicherungsmathematiker 70, 111-133.
#' @seealso \code{\link{credibility_weight}}, limited fluctuation credibility.
#' @export
#' @examples
#' set.seed(1)
#' schemes <- data.frame(scheme = rep(c("A", "B", "C", "D"), each = 5),
#'                       premium = round(runif(20, 1e5, 1e6)))
#' true <- c(A = 0.55, B = 0.65, C = 0.70, D = 0.80)[schemes$scheme]
#' schemes$loss_ratio <- rgamma(20, shape = schemes$premium / 2e4, rate = schemes$premium / 2e4 / true)
#' buhlmann_straub(schemes$loss_ratio, schemes$premium, schemes$scheme)$by_group
buhlmann_straub <- function(value, weight, group) {
  if (!is.numeric(value) || !is.numeric(weight) || length(value) != length(weight) || length(group) != length(value)) {
    stop("value, weight and group must have the same length.", call. = FALSE)
  }
  keep <- is.finite(value) & is.finite(weight) & !is.na(group)
  if (any(weight[keep] < 0)) stop("The weights must be at least 0.", call. = FALSE)
  value <- value[keep]
  weight <- weight[keep]
  group <- as.character(group[keep])
  groups <- unique(group)
  if (length(groups) < 2) stop("Credibility needs at least two groups.", call. = FALSE)
  wi <- tapply(weight, group, sum)[groups]
  ni <- as.numeric(table(group)[groups])
  if (any(wi <= 0)) stop("Each group needs a weight above 0.", call. = FALSE)
  if (sum(ni - 1) < 1) stop("Credibility needs at least one group with two or more periods.", call. = FALSE)
  xi <- tapply(weight * value, group, sum)[groups] / wi
  w <- sum(wi)
  x <- sum(wi * xi) / w
  s2 <- sum(weight * (value - xi[group])^2) / sum(ni - 1)
  a <- (sum(wi * (xi - x)^2) - (length(groups) - 1) * s2) / (w - sum(wi^2) / w)
  z <- if (a > 0) wi / (wi + s2 / a) else rep(0, length(groups))
  mu <- if (sum(z) > 0) sum(z * xi) / sum(z) else x
  list(
    by_group = data.frame(group = groups, weight = as.numeric(wi), periods = ni, mean = as.numeric(xi), credibility = as.numeric(z),
                          estimate = as.numeric(mu + z * (xi - mu)), stringsAsFactors = FALSE),
    collective_mean = mu, s2 = s2, a = a, k = if (a > 0) s2 / a else Inf
  )
}
