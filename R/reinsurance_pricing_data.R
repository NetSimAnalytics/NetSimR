#' Simulated large losses of an excess of loss treaty
#'
#' @description The large losses of a simulated portfolio for the years 2015 to
#'   2024, those of at least 500,000 in the money of their year: the experience of
#'   the examples of \code{\link{burning_cost}} and \code{\link{pareto_rating}}, with
#'   the premiums of \code{\link{xl_premium}}. In the money of 2025, at 4\% claims
#'   inflation a year, they are Pareto with alpha 1.7 above 500,000, about 0.16 per
#'   million of on-level premium, with fewer in the recent years, whose losses are not
#'   all reported yet. The script that made it is in the package's source, under
#'   data-raw.
#' @format A data frame with a row per loss and 3 columns:
#' \describe{
#'   \item{claim}{The claim's reference.}
#'   \item{year}{The year of the loss.}
#'   \item{amount}{The loss, in the money of its year.}
#' }
#' @source Simulated.
#' @seealso \code{\link{xl_premium}}, \code{\link{burning_cost}} and
#'   \code{\link{pareto_rating}}.
#' @examples
#' table(xl_large_losses$year)
"xl_large_losses"

#' Subject premiums of an excess of loss treaty
#'
#' @description The subject premium of each year 2015 to 2024 of the portfolio of
#'   \code{\link{xl_large_losses}}, with the rate index that brings it to the rates of
#'   2025 and the development factor of its layer losses.
#' @format A data frame with 10 rows and 4 columns:
#' \describe{
#'   \item{year}{The year.}
#'   \item{premium}{The subject premium of the year.}
#'   \item{rate_index}{The factor bringing the year's premium to the rates of 2025.}
#'   \item{development}{The development factor of the year's layer losses, for the
#'     losses not reported yet and the development of those reported.}
#' }
#' @source Simulated.
#' @seealso \code{\link{burning_cost}}.
#' @examples
#' xl_premium
"xl_premium"

#' Risk profile of a property portfolio
#'
#' @description The bands of sum insured of a simulated property portfolio, with the
#'   number of risks, average sum insured and premium of each: the example of
#'   \code{\link{property_exposure_rating}}.
#' @format A data frame with 9 rows and 6 columns:
#' \describe{
#'   \item{band}{The band, as a label.}
#'   \item{lower, upper}{The band's lowest and highest sum insured.}
#'   \item{risks}{The number of risks in the band.}
#'   \item{sum_insured}{The average sum insured of the band's risks.}
#'   \item{premium}{The premium of the band.}
#' }
#' @source Simulated.
#' @seealso \code{\link{property_exposure_rating}}.
#' @examples
#' xl_property_profile
"xl_property_profile"

#' Risk profile of a casualty portfolio
#'
#' @description The premium written at each policy limit by a simulated liability
#'   portfolio: the example of \code{\link{casualty_exposure_rating}}.
#' @format A data frame with 6 rows and 2 columns:
#' \describe{
#'   \item{policy_limit}{The policy limit.}
#'   \item{premium}{The premium of the policies with that limit.}
#' }
#' @source Simulated.
#' @seealso \code{\link{casualty_exposure_rating}}.
#' @examples
#' xl_casualty_profile
"xl_casualty_profile"
