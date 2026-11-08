#' Simulated motor policies
#'
#' @description A simulated year of 10,000 motor policies, with their rating
#'   factors, exposure, claim counts and claim costs: the example of the GLM fitting
#'   tool and of the claim count fits of the distribution fitting tool. Claims are
#'   most frequent for the youngest drivers, in the North and for motorcycles, and
#'   cost more for vans and dearer vehicles. The script that made it is in the
#'   package's source, under data-raw.
#' @format A data frame with 10,000 rows, one per policy, and 8 columns:
#'   \describe{
#'     \item{policy_id}{the policy's identifier;}
#'     \item{age}{the driver's age, 18 to 85;}
#'     \item{region}{North, South, East or West;}
#'     \item{vehicle}{car, van or motorcycle;}
#'     \item{vehicle_value}{the vehicle's value;}
#'     \item{exposure}{the share of the year the policy was in force;}
#'     \item{claim_count}{the number of claims in the year;}
#'     \item{claim_cost}{the total cost of the claims, 0 without a claim.}
#'   }
#' @seealso \code{\link{run_shiny_glm_fitting_tool}}, \code{\link{tweedie_family}}.
#' @examples
#' frequency <- glm(claim_count ~ offset(log(exposure)) + region + vehicle,
#'                  family = poisson(), data = motor_policies)
#' round(exp(coef(frequency)), 3)
"motor_policies"

#' Simulated liability claims at their policy limits
#'
#' @description 2,500 simulated liability claims of four accident years, each paid
#'   up to its policy limit, so the largest are cut at the limits: the example of the
#'   severity fits of the distribution fitting tool, which fits them as censored.
#'   The script that made it is in the package's source, under data-raw.
#' @format A data frame with 2,500 rows, one per claim, and 4 columns:
#'   \describe{
#'     \item{claim_id}{the claim's identifier;}
#'     \item{accident_year}{2021 to 2024;}
#'     \item{amount}{the amount paid, at most the policy limit;}
#'     \item{policy_limit}{the policy's limit: 100,000, 250,000, 500,000 or 1,000,000.}
#'   }
#' @seealso \code{\link{run_shiny_distribution_fitting_tool}}.
#' @examples
#' # the claims paid at their limit
#' mean(liability_claims$amount >= liability_claims$policy_limit)
"liability_claims"
