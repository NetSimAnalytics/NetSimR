#' Reinstatement premiums of an excess of loss layer
#'
#' @description The premium paid to reinstate the cover of an each-and-every-loss
#'   layer after claims have used it, for each simulated period: pro rata to the
#'   amount reinstated, as is usual in the market, and not to the time left in the
#'   period.
#'
#' @details Reinstatement \eqn{k} restores the layer's limit once the recoveries
#'   pass \eqn{k} limits. With \eqn{u} reinstatements used (the period's recoveries
#'   divided by the limit, capped at the number of reinstatements, as
#'   \code{number_of_reinstatements_used} of \code{\link{simulate_function}} counts
#'   them), the share of reinstatement \eqn{k} used is
#'   \eqn{\min(\max(u - (k - 1), 0), 1)}, and its premium is that share times its
#'   rate times the upfront premium. A rate of 1 charges the full upfront premium
#'   for a full reinstatement: two reinstatements at 100\% are \code{rates = 1}, and
#'   one at 100\% and one at 50\% are \code{rates = c(1, 0.5)}.
#'
#' @param reinstatements_used The number of reinstatements used in each period,
#'   non-negative and counted pro rata to the amount recovered; NA gives NA.
#' @param premium The upfront (deposit) premium of the layer, a non-negative number.
#' @param rates The premium rate of each reinstatement as a share of the upfront
#'   premium: one rate for every reinstatement, or one per reinstatement, the first
#'   reinstatement first. Defaults to 1 (100\%).
#' @return A numeric vector with the reinstatement premium of each period.
#' @seealso \code{\link{simulate_claims}} and \code{\link{simulate_function}}, which
#'   add these premiums to their results when given an upfront premium.
#' @export
#' @examples
#' # an upfront premium of 100,000 and reinstatements at 100%
#' reinstatement_premium(c(0, 0.25, 1, 1.5, 2), premium = 1e5)
#'
#' # one reinstatement at 100% and one at 50%
#' reinstatement_premium(c(0.5, 1, 1.5, 2), premium = 1e5, rates = c(1, 0.5))
reinstatement_premium <- function(reinstatements_used, premium, rates = 1) {
  if (!is.numeric(reinstatements_used) || any(reinstatements_used < 0, na.rm = TRUE)) {
    stop("reinstatements_used must be non-negative numbers.", call. = FALSE)
  }
  if (!is.numeric(premium) || length(premium) != 1 || !is.finite(premium) || premium < 0) {
    stop("premium must be one non-negative number.", call. = FALSE)
  }
  if (!is.numeric(rates) || length(rates) == 0 || any(!is.finite(rates)) || any(rates < 0)) {
    stop("rates must be non-negative numbers, one for every reinstatement or one for each.", call. = FALSE)
  }
  if (length(rates) == 1) return(premium * rates * reinstatements_used)
  if (any(reinstatements_used > length(rates), na.rm = TRUE)) {
    stop("rates gives ", length(rates), " reinstatements, but more than that are used.", call. = FALSE)
  }
  total <- numeric(length(reinstatements_used))
  for (k in seq_along(rates)) {
    total <- total + rates[k] * pmin(pmax(reinstatements_used - (k - 1), 0), 1)
  }
  premium * total
}
