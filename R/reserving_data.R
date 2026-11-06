#' RAA triangle of incurred claims
#'
#' @description The cumulative incurred claims of the Reinsurance Association of
#'   America for accident years 1981 to 1990, in thousands: the triangle of Mack
#'   (1994), much used to compare reserving methods.
#' @format A numeric matrix with 10 rows (accident years 1981 to 1990) and 10
#'   columns (development years 1 to 10), \code{NA} for the future.
#' @source Mack, T. (1994). Measuring the variability of chain ladder reserve
#'   estimates. Casualty Actuarial Society Forum, Spring 1994, 101-182.
#' @seealso \code{\link{chain_ladder}}.
#' @examples
#' raa_triangle
#' chain_ladder(raa_triangle)$total
"raa_triangle"

#' Taylor and Ashe triangle of paid claims
#'
#' @description The cumulative paid claims of Taylor and Ashe (1983) for ten
#'   accident years: the triangle of Mack's (1993) paper on the standard error of
#'   the chain ladder.
#' @format A numeric matrix with 10 rows (accident years) and 10 columns
#'   (development years), \code{NA} for the future.
#' @source Taylor, G. C. and Ashe, F. R. (1983). Second moments of estimates of
#'   outstanding claims. Journal of Econometrics 23(1), 37-61.
#' @seealso \code{\link{chain_ladder}}.
#' @examples
#' chain_ladder(taylor_ashe_triangle, sigma = "Mack")$total
"taylor_ashe_triangle"

#' Simulated motor claims with premiums
#'
#' @description The paid and incurred claims of a simulated motor portfolio, by
#'   accident year 2014 to 2025 and development year, with each accident year's
#'   earned premium: a table in the long form that \code{\link{as_triangle}} turns
#'   into triangles. Each year's loss ratio is around 68\%, and its payments follow a
#'   development pattern with over-dispersed (gamma) noise; the incurred claims add
#'   case reserves that each year sets a little high or low. The script that made it
#'   is in the package's source, under data-raw.
#' @format A data frame with 78 rows, one per accident and development year of the
#'   past, and 5 columns:
#'   \describe{
#'     \item{accident_year}{the accident year, 2014 to 2025;}
#'     \item{development_year}{the development year, 1 for the accident year itself;}
#'     \item{paid}{the claims paid in the development year (incremental);}
#'     \item{incurred}{the change in the incurred claims (paid claims and case
#'       reserves) over the development year (incremental);}
#'     \item{earned_premium}{the accident year's earned premium, on each of its rows.}
#'   }
#' @seealso \code{\link{as_triangle}}, \code{\link{bornhuetter_ferguson}},
#'   \code{\link{cape_cod}} and \code{\link{munich_chain_ladder}}.
#' @examples
#' triangle <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid",
#'                         cumulative = FALSE)
#' premium <- unique(simulated_motor_claims[c("accident_year", "earned_premium")])$earned_premium
#' cape_cod(triangle, premium)$total
"simulated_motor_claims"

#' Paid and incurred triangles of the Munich chain ladder paper
#'
#' @description The cumulative paid and incurred claims of seven accident years
#'   with which Quarg and Mack (2004) introduce the Munich chain ladder.
#' @format Numeric matrices with 7 rows (accident years) and 7 columns (development
#'   years), \code{NA} for the future, with values in the same cells.
#' @source Quarg, G. and Mack, T. (2004). Munich chain ladder. Blatter DGVFM 26(4),
#'   597-630.
#' @seealso \code{\link{munich_chain_ladder}}.
#' @examples
#' munich_chain_ladder(quarg_mack_paid, quarg_mack_incurred)$by_origin
#' @name quarg_mack
"quarg_mack_paid"

#' @rdname quarg_mack
"quarg_mack_incurred"
